library(rly)

TOKENS <- c(
  "COURSE", "MARK", "UNITS", "NUMBER",
  "AND", "OR", "NOT", "CONCURRENT",
  "PROGRAM", "PERMISSION", "PROFICIENCY",
  "COMPLETED", "LEVEL"
)

numbers <- c(
  one = 1, two = 2, three = 3, four = 4, five = 5, six = 6,
  seven = 7, eight = 8, nine = 9, ten = 10,
  eleven = 11, twelve = 12, thirteen = 13, fourteen = 14, fifteen = 15
)

fix_blurb <- function(blurb) {
  blurb |>
    # 1 ─ insert a space after any “.” that is jammed between two word chars
    str_replace_all("(?<=\\S)\\.(?=\\S)", ". ") |>
    
    # 2 ─ your other normalisations
    str_replace_all("([a-z])([A-Z])", "\\1 \\2") |>
    str_replace_all("([a-zA-Z])\\(", "\\1 (") |>
    str_replace_all("\\)([a-zA-Z])", ") \\1") |>
    str_replace_all("(or|and)([A-Z])", "\\1 \\2") |>
    
    str_squish() |>
    tolower()
}

BlurbLexer <- R6::R6Class(
  "BlurbLexer",
  
  public = list(
    
    tokens = TOKENS,
    
    # Keywords
    t_AND = function(re='\\band\\b|[.]|&', t) {
      
      if (t$value == ".") {
        input <- t$lexer$lexdata
        pos   <- t$lexpos          # zero-based
        # move to the NEXT character after the period
        remainder <- substr(input, pos + 2L, nchar(input))
        
        # emit AND only if there is a non-whitespace character ahead
        if (!str_detect(remainder, "\\S")) return(NULL)
      }
      
      t$type  <- "AND"
      t$value <- "AND"
      t
    },
    t_OR = function(
    #   1.  “,   or”   (any spaces)          ← longest, matched first
      #   2.  bare word  “or”
      #   3.  a comma that is *not* followed by “or”
      re = ",\\s*or\\b|\\bor\\b|,|/ ",
      t
    ) {
      t$type  <- "OR"
      t$value <- "OR"
      return(t)
    },
    
    t_NOT = function(re='\\bnot\\b|\\bincompatible\\b', t) t,
    
    t_COMPLETED = function(re='\\bcompleted\\b', t) t,
    
    t_CONCURRENT = function(re='\\bconcurrent(ly)?|currently\\b', t) t,
    
    t_LEVEL = function(
    re = "\\b(\\d{3,4})\\s+level\\b",   # e.g. “1000 level”, “6100 level”
    t
    ) {
      m <- regexec(re, t$value)
      t$value <- as.integer(regmatches(t$value, m)[[1]][2])  # 1000, 6100, …
      return(t)
    },
    
    t_NUMBER = function(
    # either a word-number or an explicit digit block
      re = paste0("\\b(", paste(names(numbers), collapse = "|"), "|\\d{1,3})\\b"),
      t
    ) {
      val <- t$value
      t$value <- if (val %in% names(numbers)) numbers[[val]] else as.integer(val)
      return(t)
    },
    
    # Course codes (works with 'math6212/3320')
    last_prefix = "",              # ← remembers the most-recent 4-letter tag
    
    # ---------------------------------------------------------------------
    t_COURSE = function(re = "[a-z]{4}\\d{4}|/\\d{4}", t) {
      
      if (substr(t$value, 1, 1) == "/") {            # case  "/3320"
        # inherit the previous 4-letter prefix
        t$value <- paste0(self$last_prefix, substr(t$value, 2, 5))
      } else {                                       # case "math6212"
        # store the new prefix for later use
        self$last_prefix <- substr(t$value, 1, 4)
      }
      return(t)
    },
    
    # not good :(
    t_PROGRAM = function(
    re = "(bachelor|master|mather|juris) of [a-z]+( [a-z]+){0,4}( \\(advanced\\))?( in [a-z]+( [a-z]+){0,1})?",
    t) {
      t$value <- str_trim(t$value)
      t
    },
    
    # Marks
    t_MARK = function(re='mark of (\\d{2,3}) (or )?above', t) {
      m <- regexec(re, t$value)
      num <- regmatches(t$value, m)[[1]][2]  # capture group 1 = the number
      t$value <- as.integer(num)
      t
    },
    
    t_PERMISSION = function(
    re = "convenor|convener",
    t
    ) {
      t$value <- "permission"
      return(t)
    },
    
    t_UNITS = function(re = "\\bunits?\\b", t) t,   # just flag the word itself
    
    t_PROFICIENCY = function(
    re = "proficiency",
    t
    ) {
      t$value <- "proficiency"
      return(t)
    },
    
    # Skip whitespace
    t_ignore = " \t\n",
    
    t_error = function(t) {
      # --- 1. Tell the user what went wrong (optional verbosity) -------------
      msg <- sprintf(
        "Illegal character '%s' at index %d (line %d)",
        t$value[1],         # the bad char
        t$lexpos,           # absolute index in input
        t$lexer$lineno      # line counter maintained by {rly}
      )
      warning(msg, call. = FALSE)   # or message()/cat() if you prefer quiet
      
      # --- 2. Skip this single char (you could loop for a run of bad chars) --
      t$lexer$skip(1)
      
      # --- 3. Return NULL so no token is produced ----------------------------
      return(NULL)
    },

    t_ignore_other = function(re = "[a-z0-9'\",;:()/]+", t) NULL
  )
)

lexer <- lex(BlurbLexer)

blurb <- "To enrol in this course you must have completed MATH1115 with a mark of 60 or above or MATH1113 with a mark of 80 or above. You may not enrol in MATH1116 if you are attempting to concurrently enrol or have completed MATH1014."

a <- function(code) {
  blurb <- course_info[course_info$code == code, "blurb"]
  blurb <- fix_blurb(blurb)
  lexer$input(blurb)
  
  print(blurb)
  while (!is.null(tok <- lexer$token())) {
    cat(sprintf("<%s: %s>\n", tok$type, tok$value))
  }
}


a("BIOL2202")
