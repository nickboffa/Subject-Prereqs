# dataframe supplied at run-time
is_completed    <- \(df, code, min_mark = 0)  any(!is.na(df$grade[df$code == code]) &
                                                    df$grade[df$code == code] >= min_mark)

is_concurrent   <- \(df, code)                any(is.na(df$grade[df$code == code]))

total_units     <- \(df)                      sum(df$units, na.rm = TRUE)

level_units     <- \(df, lvl)                 sum(df$units[grepl(sprintf("%d$", lvl), df$code)],
                                                  na.rm = TRUE)

Parser <- R6::R6Class(
  "Parser",
  public = list(
    tokens = TOKENS,
    
    precedence = list(
      c('right', 'NOT'),          # weakest
      c('left',  'OR'),
      c('left',  'AND') # highest
    ),
    
    # --------------------- statements -----------------------------------
    
    p_statement = function(doc = "
  statement : expression
            | statement PERIOD expression",
                            p) {
      
      if (p$length() == 2) {            # just a single expression
        p$set(1, p$get(2))
        
      } else {                          # expr '.' expr
        p$set(1, list(op = 'and',
                      args = list(p$get(1), p$get(3))))
      }
    },
    
    # ───── course_list  ────────────────────────────────────────────────
    # course_list : COURSE
    #             | course_list AND COURSE
    p_course_list = function(doc = "
  course_list : COURSE
              | course_list AND COURSE
              | course_list OR COURSE",          # ← one blank between OR and COURSE
                             p) {
      
      if (p$length() == 2) {                         # single course
        p$set(1, list(type  = 'course',
                      code  = p$get(2),
                      min_mark = 0,
                      status   = 'any'))
        
      } else {                                       # … AND/OR another course
        # we collapse both connectors into logical OR,
        # De Morgan is handled later when NOT wraps this node
        p$set(1, list(op   = 'or',
                      args = list(
                        p$get(2),
                        list(type  = 'course',
                             code  = p$get(4),
                             min_mark = 0,
                             status   = 'any'))))
      }
    },
    
    p_expression_not_courses = function(doc="
      expression : NOT course_list", p) {
      p$set(1, list(op='not', args=list(p$get(3))))
    },
    
    # --------------------- logic ----------------------------------------
    p_expression_binop = function(
    doc="expression : expression OR expression
                     | expression AND expression", p) {
      op   <- tolower(p$get(3))
      p$set(1, list(op = op, args = list(p$get(2), p$get(4))))
    },
    p_expression_not = function(doc='expression : NOT expression', p)
      p$set(1, list(op = "not", args = list(p$get(3)))),
    
    # --------------------- courses --------------------------------------
    p_expression_course = function(
    doc='expression : COURSE
                      | COURSE MARK', p) {
      mark <- if (p$length() == 3) p$get(3) else 0
      p$set(1, list(type="course", code=p$get(2), min_mark=mark,
                    status="any"))          # any = completed OR concurrent
    },
    
    p_expression_completed_course = function(
    doc='expression : COMPLETED COURSE
                      | COMPLETED COURSE MARK', p) {
      mark <- if (p$length() == 4) p$get(4) else 0
      p$set(1, list(type="course", code=p$get(3), min_mark=mark,
                    status="completed"))
    },
    
    p_expression_concurrent_course = function(
    doc='expression : CONCURRENT COURSE', p) {
      p$set(1, list(type="course", code=p$get(3), min_mark=0,
                    status="concurrent"))
    },
    
    # --------------------- units / level --------------------------------
    p_expression_units = function(doc='expression : NUMBER UNITS', p)
      p$set(1, list(type="units", n=p$get(2))),
    
    p_expression_level = function(doc='expression : NUMBER LEVEL', p)
      p$set(1, list(type="level_units", n=p$get(2))),
    
    # --------------------- catalog/program leaf -------------------------
    p_expression_program = function(doc='expression : PROGRAM', p)
      p$set(1, list(type="program", name=p$get(2))),  # store it; you may use later
    
    # --------------------- simple flags ---------------------------------
    p_expression_permission  = function(doc='expression : PERMISSION',  p)
      p$set(1, list(type="permission")),
    p_expression_proficiency = function(doc='expression : PROFICIENCY', p)
      p$set(1, list(type="proficiency")),
    
    p_error = function(p) {
      if (is.null(p)) stop("syntax error at <EOF> (ends with an OR/AND maybe?)")
      else stop(sprintf("syntax error near token '%s' (%s)", p$value, p$type))
    }
  )
)
parser <- yacc(Parser)

eval_tree <- function(node, df, context = list()) {
  
  if (is.null(node))                return(TRUE)     # empty clause is trivially true
  if (is.character(node))          return(TRUE)     # ignore unmatched text
  
  if (!is.null(node$op)) {                         # logic nodes
    vals <- vapply(node$args, eval_tree, logical(1), df, context)
    switch(node$op,
           and = all(vals),
           or  = any(vals),
           not = if (length(vals) == 0) TRUE else !vals[1]
    )
    
  } else {                                          # leaf nodes
    switch(node$type,
           course = {
             code       <- node$code
             need       <- node$status
             mark       <- node$min_mark
             completed  <- is_completed(df, code, mark)
             concurrent <- is_concurrent(df, code)
             (need == "any"        && (completed || concurrent)) ||
               (need == "completed"  && completed)                 ||
               (need == "concurrent" && concurrent)
           },
           units        = total_units(df)         >= node$n,
           level_units  = level_units(df, node$n) >= node$n,
           permission   = TRUE,
           proficiency  = TRUE,
           program      = TRUE
    )
  }
}

pretty_tree <- function(node, depth = 0) {
  indent <- strrep("  ", depth)
  
  # 1) scalar (character, logical, etc.)  ───────────────────────────
  if (!is.list(node)) {
    cat(indent, as.character(node), "\n", sep = "")
    return(invisible())
    
    # 2) logic node  ──────────────────────────────────────────────────
  } else if (!is.null(node$op)) {
    cat(indent, toupper(node$op), "\n", sep = "")
    lapply(node$args, pretty_tree, depth + 1)
    
    # 3) leaf with a recognised type  ─────────────────────────────────
  } else if (!is.null(node$type)) {
    if (node$type == "course") {
      cat(indent,
          sprintf("COURSE %s  [%s, ≥%d]",
                  node$code, node$status, node$min_mark),
          "\n", sep = "")
    } else {
      cat(indent,
          sprintf("%s  %s",
                  toupper(node$type),
                  toString(node[names(node) != "type"])),
          "\n", sep = "")
    }
    
    # 4) anything else  ───────────────────────────────────────────────
  } else {
    cat(indent, "<unknown node>\n", sep = "")
  }
  
  invisible(NULL)
}

# doesn't work
test_simple <- function(code, transcript) {
  print(a(code))
  blurb <- course_info[course_info$code == code, "blurb"]
  clean <- fix_blurb(blurb)
  tree <- parser$parse(clean, lexer)
  print(tree)
  pretty_tree(tree)
  eval_tree(tree, transcript)
}

# works (split into sentences and evaluate individually)
test_code <- function(code, transcript) {
  print(a(code))
  blurb <- course_info[course_info$code == code, "blurb"]
  clean <- fix_blurb(blurb)
  sentences <- clean |>
    str_replace_all("(?<=\\S)\\.(?=\\S)", ". ") |>  # ensure space after in-word periods
    str_split("\\.\\s+") |>                         # split on “dot + space(s)”
    unlist() |>
    str_squish() |>
    discard(~ .x == "")                             # drop empty fragments

  print(sentences)
  trees <- map(sentences, ~ parser$parse(.x, lexer))

  full_tree <-
    reduce(trees, \(a, b) list(op = "and", args = list(a, b)))
  
  res <- eval_tree(full_tree, transcript)
  list(tree = full_tree,
       elig = res)
}


transcript <- data.frame(
  code  = c("BIOL1003", "HLTH1004", "HLTH2001", "MATH1013", "MATH1014"),
  units = c(6, 6, 6, 6, 6),
  grade = c(65, 78, 72, 80, 100)  # NA = still in progress (concurrent)
)

res <- test_code("BIOL2202", transcript)
res$tree |> pretty_tree()
res$elig





