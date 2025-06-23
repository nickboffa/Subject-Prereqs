library(rly)
library(stringdist)

TOKENS <- c(
  "COURSE", "GRADE", "UNITS", "NUMBER", "WRITTEN_NUMBER",
  "AND", "OR", "NOT", "CONCURRENT",
  "PROGRAM", "PERMISSION", "PROFICIENCY",
  "LEVEL", "PERIOD", "EQUIVALENT", "HONOURS", "OPEN_BRACKET", "CLOSE_BRACKET", "SEMI_COLON",
  "COURSE_STEM"
)

numbers <- c(
  one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, #either = 1, any = 1,
  seven = 7, eight = 8, nine = 9, ten = 10,
  eleven = 11, twelve = 12, thirteen = 13, fourteen = 14, fifteen = 15
)

valid_stems <- unique(substr(course_info$code, 1, 4)) |> tolower()



escape_rx <- \(x) str_replace_all(x, "([\\^$.|?*+()\\[\\]{}\\\\])", "\\\\\\1")

# 2. concatenate → (?i) = case-insensitive, \\b = whole word
PROGRAM_RX <- paste0(
  "(?i)\\b(",
  paste(
    c(escape_rx(program_info$code),
      escape_rx(program_info$title[order(nchar(program_info$title), decreasing = TRUE)])
    ),
    collapse = "|"
  ),
  ")\\b"
)

BlurbLexer <- R6::R6Class(
  "BlurbLexer",
  
  public = list(
    
    tokens = TOKENS,
    
    t_PROGRAM = function(re = "<<program:\\s*[^>]+>>", t) {
      # Extract the content between <<program: ... >>
      m <- regexec("<<program:\\s*([^>]+)>>", t$value)
      t$value <- regmatches(t$value, m)[[1]][2] |> str_trim()
      t
    },
    # Keywords
    t_AND = function(re='\\band\\b|&', t) {
      t$type  <- "AND"
      t$value <- "AND"
      t
    },
    t_PERIOD = function(re='[.]', t) t,
    t_SEMI_COLON = function(re='[;]', t) t,
    t_OPEN_BRACKET = function(re='[(]', t) t,
    t_CLOSE_BRACKET = function(re='[)]', t) t,
    
    # Course codes (works with 'math6212/3320')
    last_prefix = "",
    
    t_OR = function(re = ",\\s*or\\b|\\bor\\b|/|\\botherwise\\b", t){
      t$type <- t$value <- "OR"; t
    },
    
    t_COURSE = function(
    re = "[a-z]{4}[0-9]{4}", t) {
      
      if (nchar(t$value) == 4) {                     # digits only
        t$value <- paste0(self$last_prefix, t$value)
      } else {
        self$last_prefix <- substr(t$value, 1, 4)
      }
      
      t$value        <- toupper(t$value)             # ← normalise to upper-case
      self$last_prefix <- toupper(self$last_prefix)  # keep prefix in sync
      t
    },
    
    t_HONOURS = function(re = "\\b[a-z]{4}-[a-z]{4}\\s+honours(\\s+specialisation)?\\b|\\b[a-z]+ honours( specialisation)?\\b", t) t,
    
    t_COURSE_STEM = function(re = paste0("\\b(", paste(valid_stems, collapse = "|"), ")\\b"), t) t,
    
    t_NOT = function(
    re="\\b(incompatible|cannot\\s+enrol|may\\s+not\\s+enrol|not)\\b", t){
      t$type<-t$value<-"NOT"; t
    },
    
    t_CONCURRENT = function(re='\\bconcurrent(ly)?|currently\\b', t) t,
    
    t_LEVEL = function(
    re = "\\b(\\d{3,4})\\s+level\\b",   # e.g. “1000 level”, “6100 level”
    t
    ) {
      m <- regexec(re, t$value)
      t$value <- as.integer(regmatches(t$value, m)[[1]][2])  # 1000, 6100, …
      return(t)
    },
      
    t_WRITTEN_NUMBER = function(re = paste0("\\b(", paste(names(numbers), collapse = "|"), ")\\b"),
                                t) {
      val <- t$value
      t$value <- if (val %in% names(numbers)) numbers[[val]] else NULL
      return(t)
    },
    
    t_NUMBER = function(
    # either a word-number or an explicit digit block
      re = paste0("\\b(\\d{1,3})\\b"),
      t
    ) {
      val <- t$value
      t$value <- as.integer(val)
      return(t)
    },

    
    # Marks
    t_GRADE = function(re = "credit|high distinction|distinction", t) t,
    
    t_PERMISSION = function(
    re = "convenor|convener|permission",
    t
    ) t,
    
    t_EQUIVALENT = function(re = "equivalent|similar", t) t,
    
    t_UNITS = function(re = "\\bunits?\\b", t) t,   # just flag the word itself
    
    t_PROFICIENCY = function(
    re = "proficiency|(prior knowledge)",
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

    t_ignore_other = function(re = "[a-z0-9'\",:/]+", t) NULL
  )
)

lexer <- lex(BlurbLexer)

blurb <- "To enrol in this course you must have completed MATH1115 with a mark of 60 or above or MATH1113 with a mark of 80 or above. You may not enrol in MATH1116 if you are attempting to concurrently enrol or have completed MATH1014."

a <- function(code) {
  print(code)

  blurb <- course_info |> 
    filter(code == !!code) |> 
    pull(blurb_clean)

  print(blurb)
  if (length(blurb) == 0) stop(sprintf("No blurb found for code '%s'", code))
  
  lexer$input(blurb)
  while (!is.null(tok <- lexer$token())) {
    cat(sprintf("<%s: %s>\n", tok$type, tok$value))
  }
}

a("BIOL2161")
a("STAT3040")
a("CHEM1201")

a("PHYS6102")

a("PHYS6102")

a("MATH2322")
a("NEUR3112")

course_info |>
  filter(str_detect(tolower(blurb), "two of")) |>
  select(code, blurb) %>%
  .[1:10, ] |> 
  pull(blurb)
