# dataframe supplied at run-time
is_completed    <- \(df, code, min_mark = 50)  any(!is.na(df$grade[df$code == code]) &
                                                    df$grade[df$code == code] >= min_mark)

is_concurrent   <- \(df, code)                any(is.na(df$grade[df$code == code]))

total_units     <- \(df)                      sum(df$units, na.rm = TRUE)

level_units <- function(df, code) {
  stem  <- substr(code, 1, 4)     # e.g. "PSYC"
  level <- substr(code, 5, 8)     # e.g. "1000"
  
  sum(df$units[startsWith(df$code, stem) & grepl(paste0(level, "$"), df$code)],
      na.rm = TRUE)
}

contains_not <- function(node) {
  if (is.null(node) || !is.list(node)) return(FALSE)
  if (!is.null(node$op) && node$op == "not") return(TRUE)
  if (!is.null(node$args))
    return(any(vapply(node$args, contains_not, logical(1))))
  FALSE
}

contains_program <- function(node) { # program includes honours here
  if (is.null(node) || !is.list(node)) return(FALSE)
  if (!is.null(node$type) && (node$type == "program" || node$type == "honours")) return(TRUE)
  if (!is.null(node$args))
    return(any(vapply(node$args, contains_program, logical(1))))
  FALSE
}

# for collapsing e.g A, B, C, D, AND/OR E
collapse_chain <- function(lhs, rhs, op) {
  if (is.list(lhs) && !is.null(lhs$op) && lhs$op == op) {
    list(op = op, args = c(lhs$args, list(rhs)))
  } else {
    list(op = op, args = list(lhs, rhs))
  }
}

grade_thresholds <- c(
  "credit" = 60,
  "distinction" = 70,
  "high Distinction" = 80
)

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
      | statement SEMI_COLON expression
      | statement SEMI_COLON AND expression
      | statement NOT expression
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
                      args = list(lhs, rhs)))
        
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
    
    p_expression_not = function(doc = "
      expression : NOT expression", p)
      p$set(1, list(op = 'not', args = list(p$get(3)))),
    
    # ─────────────────────────  LOGIC  ──────────────────────────────
    p_expression_binop = function(doc = "
  expression : expression OR expression
             | expression AND expression", p) {
      
      op <- tolower(p$get(3))
      lhs <- p$get(2)
      rhs <- p$get(4)
      
      is_not <- function(x) is.list(x) && !is.null(x$op) && x$op == "not"
      
      # Apply transformation when one side is NOT, regardless of op (AND or OR)
      if (is_not(lhs) && !is_not(rhs)) {
        # NOT A <op> B → NOT (A OR B)
        p$set(1, list(
          op = "not",
          args = list(list(op = "or", args = c(lhs$args, list(rhs))))
        ))
        return()
      }
      
      if (!is_not(lhs) && is_not(rhs)) {
        # A <op> NOT B → NOT (A OR B)
        p$set(1, list(
          op = "not",
          args = list(list(op = "or", args = c(list(lhs), rhs$args)))
        ))
        return()
      }
      
      # Default: no special case
      p$set(1, list(op = op, args = list(lhs, rhs)))
    },
    
    # ─────────────────────────  BASIC  ──────────────────────────────
    
  #   p_expression_comma = function(doc = "
  # expression : expression COMMA expression", p) {
  #     p$set(1, collapse_chain(p$get(2), p$get(4), "and"))
  #   },
  #   
  #   p_expression_comma_and = function(doc = "
  # expression : expression COMMA AND expression", p) {
  #     p$set(1, collapse_chain(p$get(2), p$get(5), "and"))
  #   },
  #   
  #   p_expression_comma_or = function(doc = "
  # expression : expression COMMA OR expression", p) {
  #     p$set(1, collapse_chain(p$get(2), p$get(5), "or"))
  #   },
    
    # ─────────────────────────  COURSES  ────────────────────────────
    p_expression_course = function(doc = "
  expression : COURSE
             | COURSE GRADE
             | COURSE NUMBER", p) {                  # ← NUMBER after COURSE
      mark <- if (p$length() == 3) {
          if (p$slice[[3]]$type == "GRADE") {
            grade_thresholds[p$get(3)]
          } else if (p$slice[[3]]$type == "NUMBER") {
            p$get(3)
          } else {
            NA
          }
      } else {
        50
      }
        
      p$set(1, list(type     = 'course',
                    code     = p$get(2),
                    min_mark = mark,
                    status   = 'completed'))
    },
    
    p_expression_grouped = function(doc = "
  expression : OPEN_BRACKET expression CLOSE_BRACKET", p) {
      p$set(1, p$get(3))  # just return the inner expression
    },
    
    p_expression_empty_bracket = function(doc = "
    expression : OPEN_BRACKET CLOSE_BRACKET", p) {
      p$set(1, NULL) # empty
    },
    
    p_expression_completed_or_concurrent = function(doc = "
  expression : CONCURRENT COURSE
             | OR CONCURRENT COURSE", p) { # for 'completed or concurrently enrolled in course'
      i <- p$length()
      p$set(1, list(
        type   = 'course',
        code   = p$get(i),
        min_mark = NA,
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
    
    p_expression_number_then_expression = function(doc = "
  expression : NUMBER expression
             | OPEN_BRACKET CLOSE_BRACKET expression", p) {
      # Just drop the number if it's not a recognised construction
      p$set(1, p$get(3))
    },
  #   
  # #   p_expression_course_comma = function(doc = "
  # # expression : COURSE COMMA COURSE", p) {
  # #     lhs <- list(type = 'course', code = p$get(2), min_mark = 50, status = 'completed')
  # #     rhs <- list(type = 'course', code = p$get(4), min_mark = 50, status = 'completed')
  # #     p$set(1, list(op = 'or', args = list(lhs, rhs)))
  # #   },
  #   p_expression_course_comma_chain = function(doc = "
  # expression : COURSE COMMA expression", p) {
  #     lhs <- list(type = 'course', code = p$get(2), min_mark = 50, status = 'completed')
  #     rhs <- p$get(4)
  #     
  #     if (is.list(rhs) && !is.null(rhs$op) && rhs$op == "or") {
  #       p$set(1, list(op = "or", args = c(list(lhs), rhs$args)))
  #     } else {
  #       p$set(1, list(op = "or", args = list(lhs, rhs)))
  #     }
  #   },
  #   
  #   p_expression_number_course_list = function(doc = "
  # expression : WRITTEN_NUMBER COURSE COMMA expression", p) {
  #     # start building an OR chain from the first COURSE
  #     lhs <- list(type = 'course', code = p$get(3), min_mark = 50, status = 'completed')
  #     print(lhs)
  #     rhs <- p$get(5)
  #     
  #     if (is.list(rhs) && !is.null(rhs$op) && rhs$op == "or") {
  #       args <- c(list(lhs), rhs$args)
  #     } else {
  #       args <- list(lhs, rhs)
  #     }
  #     
  #     p$set(1, list(op = "or", args = args))
  #   },
    
    
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
    
    p_expression_units_course = function(doc = "
      expression : NUMBER UNITS COURSE", p) {
      
      p$set(1, list(
        op = "and",
        args = list(
          list(type = "units", n = p$get(2)),
          list(type = "course",
               code = p$get(4),
               min_mark = 50,
               status = "completed")
        )
      ))
    },
    
    p_expression_level = function(doc = "
  expression : WRITTEN_NUMBER UNITS LEVEL COURSE_STEM
             | NUMBER UNITS LEVEL COURSE_STEM", p) {
      
      level_code <- paste0(toupper(p$get(5)), p$get(4))  # PSYC + 1000 → PSYC1000
      
      p$set(1, list(
        type  = "level_units",
        code  = level_code,  # PSYC1000
        n     = p$get(2)     # 12
      ))
    },
  
  p_expression_concurrent_level_units = function(doc = "
  expression : CONCURRENT WRITTEN_NUMBER LEVEL COURSE_STEM", p) {
    
    level_code <- paste0(toupper(p$get(5)), p$get(4))  # e.g. PHYS + 3000
    
    p$set(1, list(
      op = "and",
      args = list(
        list(type = "level_units", code = level_code, n = p$get(3)),
        list(type = "course", code = level_code, min_mark = NA, status = "concurrent")
      )
    ))
  },
    
    # ────────────────────────  FLAGS  ───────────────────────────────
    p_expression_program     = function(doc = "expression : PROGRAM",     p)
      p$set(1, list(type = 'program', name = p$get(2))),
    p_expression_permission  = function(doc = "expression : PERMISSION",  p)
      p$set(1, list(type = 'permission')),
    p_expression_honours = function(doc = "expression : HONOURS",  p)
      p$set(1, list(type = 'honours')),
    p_expression_proficiency = function(doc = "expression : PROFICIENCY", p)
      p$set(1, list(type = 'proficiency')),
    p_expression_equivalent = function(doc = "
    expression : EQUIVALENT
               | OR EQUIVALENT", p) {
      p$set(1, list(type = "equivalent"))
    },
    
    p_expression_number_alone = function(doc = "
  expression : NUMBER", p) {
      # Do nothing — drop stray numbers
      p$set(1, NULL)
    },
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
  if (!is.list(stmts)) return(stmts)
  if (!is.null(stmts$op) || !is.null(stmts$type)) return(stmts)  # already a single expression
  
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
  
  # Otherwise: flatten ANDs
  grouped <- lapply(stmts, flatten_ands)
  ands_flat <- unlist(grouped, recursive = FALSE)
  list(op = "and", args = ands_flat)
}


eval_tree <- function(node, df, program, context = list()) {
  
  # trivial or malformed nodes
  if (is.null(node))                      return(FALSE)
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
           level_units  = level_units(df, node$code) >= node$n,
           permission   = FALSE,
           proficiency  = TRUE,
           program = (tolower(program) == tolower(node$name)),
           equivalent   = FALSE,
           honours = FALSE,
           FALSE)   # default for any unrecognised leaf
  }
  
  # anything else ----------------------------------------------------
  else {
    FALSE
  }
}

pretty_tree <- function(node, depth = 0) {
  indent <- strrep("  ", depth)
  
  if (is.null(node)) {
    cat(indent, "<null>\n")
    return(invisible())
  }
  
  # 1) scalar
  if (!is.list(node)) {
    cat(indent, as.character(node), "\n")
    return(invisible())
  }
  
  # 2) logic node (with flattening)
  if (!is.null(node$op)) {
    cat(indent, toupper(node$op), "\n")
    
    flatten_args <- function(args, op) {
      unlist(lapply(args, function(arg) {
        if (is.list(arg) && !is.null(arg$op) && arg$op == op) {
          flatten_args(arg$args, op)
        } else {
          list(arg)
        }
      }), recursive = FALSE)
    }
    
    flat_args <- flatten_args(node$args, node$op)
    lapply(flat_args, pretty_tree, depth + 1)
    return(invisible())
  }
  
  # 3) known leaf types
  if (!is.null(node$type)) {
    desc <- switch(
      node$type,
      course = sprintf("COURSE %s [%s, ≥%s]", node$code, node$status, node$min_mark),
      program = sprintf("PROGRAM [%s]", node$name),
      units = sprintf("UNITS [%s]", node$n),
      level_units = sprintf("%s LEVEL [%s units]", node$code, node$n),
      permission = "PERMISSION",
      proficiency = "PROFICIENCY",
      equivalent = "EQUIVALENT",
      sprintf("%s %s", toupper(node$type), toString(node[names(node) != "type"]))
    )
    cat(indent, desc, "\n")
    return(invisible())
  }
  
  # 4) fallback
  cat(indent, "<unknown node>\n")
  return(invisible())
}

test_code <- function(code, transcript, program) {
  cat("\n\n\n")
  
  blurb <- course_info$blurb_clean[match(code, course_info$code)]
  if (!is.na(blurb) && nzchar(blurb)) {
    a(code)
    tree_raw <- parser$parse(blurb, lexer)
    tree <- if (is.list(tree_raw) && is.null(tree_raw$op)) combine_statements(tree_raw) else tree_raw
    elig <- eval_tree(tree, transcript, program)
  } else {
    tree <- NA
    elig <- TRUE
  }
  
  list(tree = tree, elig = elig)
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

a("PNPI3001")
test_code("PSYC1005", transcript, program)

test("ENGN4205", transcript, program)

test("NEUR3112", transcript, program)

test("PSYC3023", transcript, program)

test("BIOL3130", transcript, program)

test("MATH2305", transcript, program)

course_info |> 
  filter(code == "PSYCH1005")
# me

eligible <- c()

u_codes <-  course_info |>
  filter(career == "Undergraduate",
         #college == "College of Science and Medicine",
         !code %in% transcript$code) |>
  pull(code)

successes <- 0
for (code in u_codes) {
  res <- tryCatch({
    result <- test_code(code, transcript, program)
    result$tree |> pretty_tree()
    successes <- successes + 1
    result
  }, error = function(e) {
    message(sprintf("Error in %s: %s", code, e$message))
    NULL
  })

  if (!is.null(res) && isTRUE(res$elig)) {
    eligible <- c(eligible, code)
  }
}

print(successes/length(u_codes))

View(course_info |> filter(code %in% eligible) |> select(code, title, college, school, blurb))


course_info |> 
  filter(code == "HMLD1022")
