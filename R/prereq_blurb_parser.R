# dataframe supplied at run-time
is_completed    <- \(df, code, min_mark = 50)  any(!is.na(df$grade[df$code == code]) &
                                                    df$grade[df$code == code] >= min_mark)

is_concurrent   <- \(df, code)                any(is.na(df$grade[df$code == code]))

total_units     <- \(df)                      sum(df$units, na.rm = TRUE)

level_units     <- \(df, lvl)                 sum(df$units[grepl(sprintf("%d$", lvl), df$code)],
                                                  na.rm = TRUE)
contains_not <- function(node) {
  if (is.null(node) || !is.list(node)) return(FALSE)
  if (!is.null(node$op) && node$op == "not") return(TRUE)
  if (!is.null(node$args))
    return(any(vapply(node$args, contains_not, logical(1))))
  FALSE
}

Parser <- R6::R6Class(
  "Parser",
  public = list(
    tokens = TOKENS,
    
    # precedence: low → high
    precedence = list(
      c("left",  "OR"),
      c("left",  "AND"),
      c("right", "NOT")
    ),
    
    # ─────────────────────────  SENTENCES  ───────────────────────────
    p_statement = function(doc = "
  statement : expression
            | statement PERIOD expression
            | statement PERIOD OR expression", p) {
      
      if (p$length() == 2) {                  # just an expression
        p$set(1, p$get(2))
        
      } else if (p$length() == 4) {           # S '.' E  → OR **or** AND
        rhs <- p$get(4)
        connector <- if (contains_not(rhs)) "and" else "or"
        p$set(1, list(op = connector, args = list(p$get(2), rhs)))
        
      } else {                                # S '.' OR E  → explicit OR
        p$set(1, list(op = "or", args = list(p$get(2), p$get(5))))
      }
    },
    
    # ────────────────────────  COURSE LISTS  ────────────────────────
    p_course_list = function(doc = "
      course_list : COURSE
                  | course_list AND COURSE
                  | course_list OR COURSE", p) {
      
      if (p$length() == 2) {                    # single course
        p$set(1, list(type  = 'course',
                      code  = p$get(2),
                      min_mark = 50,
                      status   = 'completed'))
        
      } else {                                  # chain of courses
        p$set(1, list(op   = 'or',              # AND/OR collapse to OR
                      args = list(
                        p$get(2),
                        list(type  = 'course',
                             code  = p$get(4),
                             min_mark = 50,
                             status   = 'completed'))))
      }
    },
    
    # allow a course_list anywhere an expression can appear
    p_expression_course_list = function(doc = "
      expression : course_list", p)
      p$set(1, p$get(2)),
    
    # ─────────────────────────  LOGIC  ──────────────────────────────
    p_expression_binop = function(doc = "
      expression : expression OR expression
                 | expression AND expression", p)
      p$set(1, list(op   = tolower(p$get(3)),
                    args = list(p$get(2), p$get(4)))),
    
    p_expression_not = function(doc = "
      expression : NOT expression", p)
      p$set(1, list(op = 'not', args = list(p$get(3)))),
    
    # ─────────────────────────  COURSES  ────────────────────────────
    p_expression_course = function(doc = "
  expression : COURSE
             | COURSE NUMBER", p) {                  # ← NUMBER after COURSE
      mark <- if (p$length() == 3) p$get(3) else 50
      p$set(1, list(type     = 'course',
                    code     = p$get(2),
                    min_mark = mark,
                    status   = 'completed'))
    },
    
    p_expression_completed_course = function(doc = "
  expression : COMPLETED COURSE
             | COMPLETED COURSE NUMBER", p) {        # ← NUMBER, not MARK
      mark <- if (p$length() == 4) p$get(4) else 50
      p$set(1, list(type     = 'course',
                    code     = p$get(3),
                    min_mark = mark,
                    status   = 'completed'))
    },
    
    # NEW: 80 in MATH1013  (NUMBER before COURSE)
    p_expression_number_course = function(doc = "
  expression : NUMBER COURSE", p) {
      p$set(1, list(type     = 'course',
                    code     = p$get(3),
                    min_mark = p$get(2),
                    status   = 'completed'))
    },
    
    p_expression_concurrent_course = function(doc = "
      expression : CONCURRENT COURSE", p)
      p$set(1, list(type   = 'course',
                    code   = p$get(3),
                    min_mark = NA,
                    status = 'concurrent')),
    
    p_expression_expr_concurrent = function(doc = "
  expression : expression CONCURRENT COURSE", p) {
      
      p$set(1, list(
        op   = 'and',
        args = list(
          p$get(2),                               # the left-hand expression
          list(type = 'course',                   # the new concurrent course
               code = p$get(4),
               min_mark = NA,
               status = 'concurrent')
        )))
    },
    
    # ───────────────────  NUMERIC / LEVEL LEAVES  ───────────────────
    p_expression_units = function(doc = "
      expression : NUMBER UNITS", p)
      p$set(1, list(type = 'units', n = p$get(2))),
    
    p_expression_level = function(doc = "
      expression : NUMBER LEVEL", p)
      p$set(1, list(type = 'level_units', n = p$get(2))),
    
    # ────────────────────────  FLAGS  ───────────────────────────────
    p_expression_program     = function(doc = "expression : PROGRAM",     p)
      p$set(1, list(type = 'program',     name = p$get(2))),
    p_expression_permission  = function(doc = "expression : PERMISSION",  p)
      p$set(1, list(type = 'permission')),
    p_expression_proficiency = function(doc = "expression : PROFICIENCY", p)
      p$set(1, list(type = 'proficiency')),
    p_expression_equivalent = function(doc = "
  expression : EQUIVALENT", p)
      p$set(1, list(type = "equivalent")),
    
    # ─────────────────────────  ERROR  ──────────────────────────────
    p_error = function(p) {
      if (is.null(p))
        stop("syntax error at <EOF> (dangling AND/OR?)")
      else
        stop(sprintf("syntax error near '%s' (%s)", p$value, p$type))
    }
  )
)

parser <- yacc(Parser)


eval_tree <- function(node, df, context = list()) {
  
  if (is.null(node))                return(TRUE)     # empty clause is trivially true
  if (is.character(node))          return(TRUE)     # ignore unmatched text
  
  if (is.null(node) || is.character(node)) return(TRUE)
  
  # ── Logical nodes ───────────────────────────────────────────
  if (!is.null(node$op)) {
    vals <- vapply(node$args, eval_tree, logical(1), df, context)
    vals[is.na(vals)] <- FALSE            # <- NEW LINE
    
    switch(node$op,
           and = all(vals),
           or  = any(vals),
           not = !vals[1])
  } else {                                          # leaf nodes
    switch(node$type,
           course = {
             code       <- node$code
             need       <- node$status
             mark       <- node$min_mark
             completed  <- is_completed(df, code, mark)
             concurrent <- is_concurrent(df, code)
             (need == "concurrent"        && (completed || concurrent)) ||
               (need == "completed"  && completed)
           },
           units        = total_units(df)         >= node$n,
           level_units  = level_units(df, node$n) >= node$n,
           permission   = TRUE,
           proficiency  = TRUE,
           program      = TRUE,
           equivalent = FALSE
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

test_simple <- function(code, transcript) {
  print(a(code))
  
  blurb  <- course_info[course_info$code == code, "blurb"]
  clean  <- fix_blurb(blurb)
  
  tree   <- parser$parse(clean, lexer)
  
  list(tree = tree,
       elig = eval_tree(tree, transcript))
}

transcript <- data.frame(
  code  = c("HLTH1001", "CHEM1101", "BIOL1008", "MATH1115",
            "BIOL2202", "CHEM1201", "BIOL1004", "MATH1116",
            "BIOL2161", "BIOL3207", "STAT2001", "MEDN2001",
            "POPH3000", "SCNC2101", "STAT2005", "MEDN2002",
            "STAT3040", "HLTH3001", "SCNC3101", "MATH2305"),
  units = 6,
  grade = 100  # NA = still in progress (concurrent)
)

eligible <- c()

u_codes <-  course_info |> 
  filter(career == "Undergraduate",
         college == "College of Science and Medicine",
         !code %in% transcript$code) |>
  pull(code)

for (code in u_codes) {
  res <- tryCatch({
    result <- test_simple(code, transcript)
    result$tree |> pretty_tree()
    result
  }, error = function(e) {
    message(sprintf("Error in %s: %s", code, e$message))
    NULL
  })
  
  if (!is.null(res) && isTRUE(res$elig)) {
    eligible <- c(eligible, code)
  }
}

eligible

View(course_info |> filter(code %in% eligible) |> select(code, title, college, school))



