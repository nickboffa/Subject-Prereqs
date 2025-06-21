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
      c("right","NOT"),
      c("left", "OR"),
      c("left", "AND")
    ),
    
    # --------------------- statements -----------------------------------
    p_statement = function(doc='statement : expression', p)
      p$set(1, p$get(2)),
    
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

eval_tree <- function(node, df, context=list()) {
  
  if (is.character(node))                return(TRUE)
  
  if (!is.null(node$op)) {               # logic nodes
    vals <- vapply(node$args, eval_tree, logical(1), df, context)
    switch(node$op,
           and = all(vals),
           or  = any(vals),
           not = !vals[1])
  } else {                               # leaf nodes
    switch(node$type,
           course = {
             code  <- node$code
             need  <- node$status
             mark  <- node$min_mark
             completed  <- is_completed(df, code, mark)
             concurrent <- is_concurrent(df, code)
             (need == "any"       && (completed || concurrent)) ||
               (need == "completed" && completed)                 ||
               (need == "concurrent" && concurrent)
           },
           units        = total_units(df)            >= node$n,
           level_units  = level_units(df, node$n)    >= node$n,
           permission   = TRUE,          # treat as always satisfied unless you track it
           proficiency  = TRUE,
           program      = TRUE           # ignore for now
    )
  }
}

transcript <- data.frame(
  code  = c("BIOL1003", "BIOL1004", "MATH1013", "MATH1014"),
  units = c(6, 6, 6, 6),
  grade = c(65, 78, 72, 100)  # NA = still in progress (concurrent)
)


blurb <- course_info[course_info$code == "BIOL2202", "blurb"]
clean <- fix_blurb(blurb)
print(clean)
tree   <- parser$parse(clean, lexer)        # parse → nested list
eval_tree(tree, transcript)                  # TRUE / FALSE

