#####################################################################
#  FULL COPY-PASTE SCRIPT
#  – prerequisite engine  +  comprehensive test-suite
#####################################################################

library(jsonlite)
library(stringr)

blurbs <- fromJSON("data/current/blurbs.txt")   # adjust path if needed

## ------------------------------------------------------------------
##  Helpers
## ------------------------------------------------------------------

.grade_band_cutoff <- c("high distinction" = 80,
                        "distinction"      = 70,
                        "credit"           = 60,
                        "pass"             = 50)

.get_grade_cutoff <- function(txt) {
  num <- str_match(txt, "(?i)(?:mark|grade)\\s+of\\s*(\\d+)")[,2]
  if (!is.na(num)) return(as.numeric(num))
  num <- str_match(txt, "(?i)(?:at least|minimum|min)\\s*(\\d+)")[,2]
  if (!is.na(num)) return(as.numeric(num))
  num <- str_match(txt, "(?i)(\\d+)\\s*or\\s*(?:above|higher|more)")[,2]
  if (!is.na(num)) return(as.numeric(num))
  for (b in names(.grade_band_cutoff))
    if (grepl(b, txt, ignore.case = TRUE))
      return(.grade_band_cutoff[b])
  NA_real_
}

.extract_codes <- function(x) unique(str_extract_all(x, "[A-Z]{4}\\d{3}")[[1]])

.word_split <- function(cl) {
  cl <- str_replace_all(cl, "(?i)one of", "or")
  cl <- str_replace_all(cl, ";", " and ")
  str_split(cl, "(?i)\\band\\b")[[1]] |> lapply(.extract_codes)
}

.program_matches <- function(block, user_program) {
  if (is.null(user_program)) return(FALSE)
  pats <- str_match_all(block,
                        "(?i)be\\s+enrolled\\s+in\\s+the\\s+(Bachelor|Master)[^;,\\.]+")[[1]][,1]
  if (length(pats) == 0) return(FALSE)
  any(vapply(pats,
             \(p) grepl(trimws(p), user_program, ignore.case = TRUE), logical(1)))
}

.eval_groups <- function(groups, completed, block, user_program) {
  for (codes in groups) {
    ok <- FALSE
    if (length(codes) && any(codes %in% completed)) ok <- TRUE
    if (.program_matches(block, user_program))       ok <- TRUE
    if (!ok) return(FALSE)
  }
  TRUE
}

sub_fixed <- \(pat,repl,s) sub(pat,repl,s,fixed=TRUE)

.split_or_blocks <- function(text) {
  prot <- text
  for (i in seq_along(m <- str_extract_all(text,"\\([^()]*\\)")[[1]]))
    prot <- sub_fixed(m[i], sprintf("##P%02d##", i), prot)
  blks <- str_split(prot, regex("[,;\\.]\\s+or\\s*", ignore_case = TRUE))[[1]]
  for (i in seq_along(m))
    blks <- str_replace_all(blks, sprintf("##P%02d##", i), m[i])
  trimws(blks)
}

.block_satisfied <- function(block, grades, courses, prog, units) {
  
  if (grepl("mark of|grade of|credit|distinction|high distinction|pass",
            block, ignore.case = TRUE)) {
    cut <- .get_grade_cutoff(block); if (is.na(cut)) cut <- 50
    if (!.eval_groups(.word_split(block), courses, block, prog)) return(FALSE)
    co <- .extract_codes(block); if (length(co)) {
      g <- grades[co]
      if (!any(!is.na(g) & g >= cut)) return(FALSE)
    }
  }
  
  if (grepl("completed", block, ignore.case = TRUE) &&
      !.eval_groups(.word_split(block), courses, block, prog))
    return(FALSE)
  
  if (grepl("\\d+ units", block) && !is.null(units)) {
    req <- as.numeric(str_extract(block, "\\d+(?=\\s*units)"))
    if (!is.na(req) && units < req) return(FALSE)
  }
  
  if (grepl("Bachelor|Master", block) &&
      grepl("must be|open only to|restricted to", block, ignore.case = TRUE) &&
      !.program_matches(block, prog))
    return(FALSE)
  
  TRUE
}

.requirements_satisfied <- function(txt, grades, courses, prog, units) {
  txt <- str_split(txt, "(?i)Incompatible")[[1]][1]
  
  # BIOL3303 pattern: ≥72 units & 2000-level BIOL/ENVS
  if (grepl("72[^\\d]*units",  txt, ignore.case = TRUE) &&
      grepl("2000[^\\d]*level", txt, ignore.case = TRUE) &&
      grepl("(BIOL|ENVS)",      txt, ignore.case = TRUE)) {
    return(!is.null(units) && units >= 72 &&
             length(grep("^(BIOL|ENVS)[23]\\d{3}$", courses)) > 0)
  }
  
  for (blk in .split_or_blocks(txt))
    if (.block_satisfied(blk, grades, courses, prog, units))
      return(TRUE)
  FALSE
}

## ------------------------------------------------------------------
##  Main user-facing function
## ------------------------------------------------------------------
satisfied_requirements <- function(course, completed_course_grades,
                                   user_program = NULL, all_units = NULL) {
  
  courses <- names(completed_course_grades)
  units   <- if (!is.null(all_units))
    sum(all_units[courses], na.rm = TRUE) else NULL
  blurb   <- blurbs[[course]]
  if (is.null(blurb) || blurb == "") return(TRUE)
  blurb   <- str_squish(blurb)
  
  if (grepl("Incompatible", blurb, ignore.case = TRUE)) {
    bad <- .extract_codes(str_match(blurb, "(?i)Incompatible[^.]*")[1])
    if (length(intersect(bad, courses))) return(FALSE)
  }
  
  .requirements_satisfied(blurb, completed_course_grades, courses,
                          user_program, units)
}

#####################################################################
#  4.  COMPREHENSIVE TEST SUITE
#####################################################################

all_units <- setNames(rep(6, 100), paste0("FAKE", seq_len(100)))
# add real units we care about
add_units <- c("BIOL1003","BIOL1004","CHEM1101","CHEM1201","STAT1003",
               "STAT1008","STAT2001","MATH1014","MATH1115","MATH1113",
               "PHYS1101","PHYS1201","ENVS2001","ENVS2002","ENVS3001","ENVS3002",
               "BIOL6303")
all_units[add_units] <- 6

tst <- function(lbl, course, grades, prog, exp) {
  ok <- satisfied_requirements(course, grades, prog, all_units)
  cat(sprintf("%-33s%-8s ⇒ %-5s (%s)\n",
              lbl, course, ok, if (ok==exp) "✓" else "✗"))
}

# ---- BIOL3303 ---------------------------------------------------
biol_pass <- setNames(rep(100, 12),
                      c("BIOL1003","BIOL1004","CHEM1101","CHEM1201",
                        "STAT1003","MATH1014","PHYS1101","PHYS1201",
                        "ENVS2001","ENVS2002","ENVS3001","ENVS3002"))

biol_fail_units <- biol_pass[-1]
biol_incompat   <- c(biol_pass, BIOL6303 = 100)

tst("BIOL3303 pass (72u)",      "BIOL3303", biol_pass,
    "Bachelor of Science", TRUE)
tst("BIOL3303 fail (<72u)",     "BIOL3303", biol_fail_units,
    "Bachelor of Science", FALSE)
tst("BIOL3303 incompat",        "BIOL3303", biol_incompat,
    "Bachelor of Science", FALSE)

# ---- MATH1116 ---------------------------------------------------
tst("M1116 mark ≥60", "MATH1116",
    setNames(100,"MATH1115"), "Bachelor of Science", TRUE)
tst("M1116 mark <60", "MATH1116",
    setNames(55,"MATH1115"),  "Bachelor of Science", FALSE)
tst("M1116 alt path 80", "MATH1116",
    setNames(82,"MATH1113"),  "Bachelor of Science", TRUE)

# ---- BIOL2161 ---------------------------------------------------
tst("BIOL2161 fail (59/60)", "BIOL2161",
    setNames(c(59,60), c("BIOL1003","CHEM1101")),
    "Bachelor of Science", FALSE)
tst("BIOL2161 pass (60/60)", "BIOL2161",
    setNames(c(60,60), c("BIOL1003","CHEM1101")),
    "Bachelor of Science", TRUE)

# ---- COMP2550 programme gate -----------------------------------
tst("COMP2550 good prog", "COMP2550", c(),
    "Bachelor of Advanced Computing (Research and Development) (Honours)", TRUE)
tst("COMP2550 wrong prog","COMP2550", c(),
    "Bachelor of Science", FALSE)

# ---- STAT2001 various paths ------------------------------------
tst("STAT2001 BADA 1014", "STAT2001",
    setNames(100,"MATH1014"), "Bachelor of Applied Data Analytics", TRUE)
tst("STAT2001 BADA 1115", "STAT2001",
    setNames(100,"MATH1115"), "Bachelor of Applied Data Analytics", TRUE)
tst("STAT2001 BADA no maths", "STAT2001",
    c(), "Bachelor of Applied Data Analytics", FALSE)
tst("STAT2001 non-BADA", "STAT2001",
    setNames(c(100, 100), c("STAT1008","MATH1115")),
    "Bachelor of Science", TRUE)
