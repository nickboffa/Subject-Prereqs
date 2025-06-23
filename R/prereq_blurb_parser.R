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

contains_program <- function(node) {
  if (is.null(node) || !is.list(node)) return(FALSE)
  if (!is.null(node$type) && node$type == "program") return(TRUE)
  if (!is.null(node$args))
    return(any(vapply(node$args, contains_program, logical(1))))
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
    # ─────────────────────────  p_statement  ───────────────────────────
    p_statement = function(doc = "
      statement : expression
      | statement PERIOD expression
      | statement PERIOD AND expression
      | statement PERIOD OR expression", p) {
      
      if (p$length() == 2) {                       # just one clause
        p$set(1, p$get(2))
        
      } else if (p$length() == 4) {                # S '.'  E
        rhs       <- p$get(4)                      # <statement> '.' <expr>
        
        lhs <- p$get(2)
        rhs <- p$get(4)
        
        if (contains_program(lhs) || contains_program(rhs)) {
          connector <- "and"
        } else if (contains_not(rhs)) {
          connector <- "and"
        } else {
          connector <- "or"
        }
        
        p$set(1, list(op = connector,
                      args = list(p$get(2), rhs)))
        
      } else {                                     # S '.' AND|OR E
        rhs      <- p$get(5)                       # <stmt> '.' AND <expr>
        explicit <- tolower(p$get(4))              # AND / OR  ← index **4**, not 3!
        
        starts_with_program <-
          is.list(rhs) &&
          ( (!is.null(rhs$type) && rhs$type == "program") ||
              (!is.null(rhs$op) && any(vapply(rhs$args,
                                              \(x) is.list(x) &&
                                                !is.null(x$type) &&
                                                x$type == "program",
                                              logical(1)))) )
        
        connector <- if (starts_with_program) {
          if (contains_not(rhs)) "and" else "or"
        } else {
          explicit
        }
        
        p$set(1, list(op = connector,
                      args = list(p$get(2), rhs)))
      }
    },
    
    # ─────────────────────────  LOGIC  ──────────────────────────────
    p_expression_binop = function(doc = "
  expression : expression OR expression
             | expression AND expression", p) {
      
      op <- tolower(p$get(3))
      lhs <- p$get(2)
      rhs <- p$get(4)
      
      if (op == "and") {
        is_not <- function(x) is.list(x) && !is.null(x$op) && x$op == "not"
        
        if (is_not(lhs) && !is_not(rhs)) {
          # AND(NOT A, B) → NOT(A OR B)
          p$set(1, list(
            op = "not",
            args = list(list(op = "or", args = c(lhs$args, list(rhs))))
          ))
          return()
        }
        
        if (!is_not(lhs) && is_not(rhs)) {
          # AND(A, NOT B) → NOT(A OR B)
          p$set(1, list(
            op = "not",
            args = list(list(op = "or", args = c(list(lhs), rhs$args)))
          ))
          return()
        }
      }
      
      # default: just build binary op
      p$set(1, list(op = op, args = list(lhs, rhs)))
    },
    
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
    
    p_expression_completed_or_concurrent = function(doc = "
  expression : COMPLETED OR CONCURRENT COURSE", p) {
      p$set(1, list(
        type   = 'course',
        code   = p$get(5),
        min_mark = 50,
        status = 'concurrent'
      ))
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

flatten_ands <- function(node) {
  if (!is.list(node)) return(list(node))
  if (!is.null(node$op) && node$op == "and") {
    # recurse into AND's args
    return(unlist(lapply(node$args, flatten_ands), recursive = FALSE))
  }
  return(list(node))
}

# Combines list of clauses separated by periods
combine_statements <- function(stmts) {
  if (!is.list(stmts) || length(stmts) == 0) return(stmts)
  
  # Split top-level list into three parts if it matches our pattern
  if (length(stmts) == 3 &&
      all(vapply(stmts[1:2], \(x) is.list(x) && !is.null(x$op) && x$op == "or", logical(1))) &&
      is.list(stmts[[3]]) && !is.null(stmts[[3]]$op) && stmts[[3]]$op == "and" &&
      any(vapply(stmts[[3]]$args, \(x) is.list(x) && x$type == "program", logical(1)))) {
    return(list(
      op = "and",
      args = list(
        list(op = "or", args = stmts[1:2]),
        stmts[[3]]
      )
    ))
  }
  
  # Otherwise: group by period → then split by AND
  grouped <- lapply(stmts, flatten_ands)
  ands_flat <- unlist(grouped, recursive = FALSE)
  list(op = "and", args = ands_flat)
}


eval_tree <- function(node, df, program, context = list()) {
  
  # trivial or malformed nodes
  if (is.null(node))                      return(TRUE)
  if (is.character(node))                 return(TRUE)
  if (is.list(node) && length(node) == 0) return(FALSE)
  
  # logic node -------------------------------------------------------
  if (!is.null(node$op)) {
    vals <- vapply(node$args, function(arg) {
      v <- eval_tree(arg, df, program, context)
      if (length(v) != 1 || is.na(v)) FALSE else v
    }, logical(1))
    
    switch(node$op,
           and = all(vals),
           or  = any(vals),
           not = !vals[1])
  }
  
  # leaf node --------------------------------------------------------
  else if (!is.null(node$type)) {
    switch(node$type,
           course = {
             completed  <- is_completed (df, node$code, node$min_mark)
             concurrent <- is_concurrent(df, node$code)
             need <- node$status
             (need == "completed"  && completed) ||
               (need == "concurrent" && (completed || concurrent))
           },
           units        = total_units(df)         >= node$n,
           level_units  = level_units(df, node$n) >= node$n,
           permission   = TRUE,
           proficiency  = TRUE,
           program = (tolower(program) == tolower(node$name)),
           equivalent   = FALSE,
           FALSE)   # default for any unrecognised leaf
  }
  
  # anything else ----------------------------------------------------
  else {
    FALSE
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

test_code <- function(code, transcript, program) {
  print(a(code))
  
  blurb  <- course_info[course_info$code == code, "blurb"]
  clean  <- fix_blurb(blurb)
  
  tree_raw <- parser$parse(clean, lexer)
  tree <- if (is.list(tree_raw) && is.null(tree_raw$op)) combine_statements(tree_raw) else tree_raw
  list(tree = tree,
       elig = eval_tree(tree, transcript, program))
}

test <- function(code, transcript, program) {
  res <- test_code(code, transcript, program)
  pretty_tree(res$tree)
  res$elig
}

transcript <- data.frame(
  code  = c("HLTH1001", "CHEM1101", "BIOL1008", "MATH1115",
            "BIOL2202", "CHEM1201", "BIOL1004", "MATH1116",
            "BIOL2161", "BIOL3207", "STAT2001", "MEDN2001",
            "POPH3000", "SCNC2101", "STAT2005", "MEDN2002",
            "STAT3040", "HLTH3001", "SCNC3101", "MATH2305", "EMET2007"),
  units = 6,
  grade = 100  # NA = still in progress (concurrent)
)

program <- "bachelor of finance, economics and statistics (honours)"
program <- "bachelor of health science"

## Testing area

test("BIOL2161", transcript, program)


# me
# 
# eligible <- c()
# 
# u_codes <-  course_info |> 
#   filter(career == "Undergraduate",
#          college == "College of Science and Medicine",
#          !code %in% transcript$code) |>
#   pull(code)
# 
# for (code in u_codes) {
#   res <- tryCatch({
#     result <- test_code(code, transcript)
#     result$tree |> pretty_tree()
#     result
#   }, error = function(e) {
#     message(sprintf("Error in %s: %s", code, e$message))
#     NULL
#   })
#   
#   if (!is.null(res) && isTRUE(res$elig)) {
#     eligible <- c(eligible, code)
#   }
# }
# 
# eligible
# 
# View(course_info |> filter(code %in% eligible) |> select(code, title, college, school))
# 


