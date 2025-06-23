
#### First, we will edit any incorrectly-specified or misspelled program titles to make them proper
all_words <- program_info$title |>
  str_extract_all("\\(?[a-zA-Z–—,'-]+\\)?") |>  # same regex as `blocks`
  unlist() |>
  str_to_lower() |>
  discard(~ .x == "") |>
  unique() |>
  sort()

allowed_endings <- program_info$title |>
  str_extract_all("\\(?[a-zA-Z–—,'-]+\\)?") |>  # same as for all_words
  map_chr(\(x) tolower(str_remove_all(tail(x, 1), "[[:punct:]()]"))) |> 
  unique() |>
  sort()

allowed_initials <- program_info$title |>
  str_trim() |>
  str_sub(1, 1) |>
  str_to_lower() |>
  unique() |> 
  sort()

get_phrases <- function(blocks, max_len = 10) {
  phrases <- c()
  n <- length(blocks)
  
  for (i in seq_len(n)) {
    non_dict_count <- 0
    for (j in i:min(i + max_len - 1, n)) {
      word <- blocks[j]
      clean_word <- tolower(str_remove_all(word, "[()]"))
      if (!(clean_word %in% all_words)) {
        non_dict_count <- non_dict_count + 1
      }
      if (non_dict_count > 1) break
      
      phrase <- paste(blocks[i:j], collapse = " ")
      
      if (str_count(phrase, "\\s") >= 2) {
        first_letter <- str_to_lower(str_sub(str_remove_all(blocks[i], "[()]"), 1, 1))
        last_word <- str_to_lower(str_remove_all(blocks[j], "[[:punct:]()]"))
        
        if (first_letter %in% allowed_initials &&
            str_detect(blocks[i], "^[A-Z]") &&
            last_word %in% allowed_endings) {
          
          phrases <- c(phrases, phrase)
        }
      }
    }
  }
  
  phrases
}



correct_word <- function(word, dict, max_dist = 2) {
  w <- tolower(word)
  d <- stringdist(w, dict, method = "osa")
  i <- which.min(d)
  if (length(i) && d[i] <= max_dist) dict[i] else word
}

normalise_phrase <- function(phrase, dict = all_words) {
  words <- str_extract_all(phrase, "[a-zA-Z()'-]+")[[1]]
  fixed <- vapply(words, correct_word, "", dict = dict)
  
  # Do a word-by-word replacement in order, safely
  for (i in seq_along(words)) {
    phrase <- str_replace_all(phrase, fixed(words[i]), fixed[i])
  }
  
  str_squish(phrase)
}

extract_phrases <- function(blurb, max_len = 10) {
  blocks <- str_extract_all(blurb, "\\(?[a-zA-Z–—,'-]+\\)?")[[1]]
  get_phrases(blocks, max_len)
}

extract_phrases(blurb)

for (phrase in phrases) {
  print(normalise_phrase(phrase))
}

extract_phrases(blurb)

match_program <- function(blurb, program_titles, max_dist = 0.2) {
  phrases <- extract_phrases(blurb)
  if (length(phrases) == 0) return(character(0))
  
  phrase_pos <- lapply(phrases, function(p) str_locate(blurb, fixed(p, ignore_case = TRUE))[1, ])
  valid <- !sapply(phrase_pos, function(x) anyNA(x))
  phrases <- phrases[valid]
  phrase_pos <- phrase_pos[valid]
  
  results <- list()
  for (i in seq_along(phrases)) {
    opt <- phrases[i]
    #corrected <- normalise_phrase(opt)
    corrected <- opt
    dists <- stringdist::stringdist(tolower(corrected), tolower(program_titles), method = "jw")
    best_idx <- which.min(dists)
    best_title <- program_titles[best_idx]
    best_dist <- dists[best_idx]
    
    if (best_dist < max_dist) {
      results[[length(results) + 1]] <- list(
        phrase = opt,
        corrected = corrected,
        title = best_title,
        dist = best_dist,
        len = nchar(opt),
        start = phrase_pos[[i]][1],
        end = phrase_pos[[i]][2]
      )
    }
  }
  
  if (length(results) == 0) return(character(0))
  df <- do.call(rbind, lapply(results, as.data.frame))
  df <- df[order(df$start, df$dist), , drop = FALSE]
  
  # Step 1: pick the best phrase at each unique start index
  best_per_start <- df |>
    dplyr::group_by(start) |>
    dplyr::mutate(is_perfect = dist < 1e-5) |>
    dplyr::slice(if (all(is_perfect)) which.max(len) else which.min(dist)) |>
    dplyr::ungroup()
  
  # Step 2: greedy non-overlapping selection in start order
  selected <- list()
  for (i in seq_len(nrow(best_per_start))) {
    this <- best_per_start[i, ]
    this_range <- c(this$start, this$end)
    
    overlaps <- any(sapply(selected, function(sel) {
      r <- c(sel$start, sel$end)
      !(this_range[2] < r[1] || this_range[1] > r[2])
    }))
    if (!overlaps) {
      selected[[length(selected) + 1]] <- this
    }
  }
  
  out <- vapply(selected, `[[`, "", "title")
  names(out) <- vapply(selected, `[[`, "", "phrase")
  out
}


blurb <- course_info[course_info$code == "MATH1115", "blurb"]
match_program(blurb, program_info$title)
blurb <- course_info[course_info$code == "STAT3040", "blurb"]
match_program(blurb, program_info$title)
blurb <- course_info[course_info$code == "PHYS6102", "blurb"]
print(blurb$blurb)
match_program(blurb, program_info$title)
blurb <- course_info[course_info$code == "COMP6466", "blurb"]
match_program(blurb, program_info$title)
blurb <- course_info[course_info$code == "PNPI3001", "blurb"]
match_program(blurb, program_info$title)

fix_blurb <- function(blurb, program_titles = program_info$title) {
  matched <- match_program(blurb, program_titles)
  fixed <- blurb  # Preserve original casing for replacement
  
  if (length(matched) > 0) {
    for (i in seq_along(matched)) {
      match_title <- matched[i]
      phrase <- names(matched)[i]
      
      if (!is.na(phrase) && nzchar(phrase)) {
        fixed <- str_replace(
          fixed,
          fixed(phrase, ignore_case = TRUE),
          paste0("<<PROGRAM:", match_title, ">>")
        )
      }
    }
  }
  
  fixed <- fixed |>
    str_replace_all("(?<=\\S)\\.(?=\\S)", ". ") |>
    str_replace_all("([a-z])([A-Z])", "\\1 \\2") |>
    str_replace_all("([a-zA-Z])\\(", "\\1 (") |>
    str_replace_all("\\)([a-zA-Z])", ") \\1") |>
    str_replace_all("(or|and)([A-Z])", "\\1 \\2") |>
    str_squish() |>
    tolower() |>
    str_replace_all("[[:punct:][:space:]]*$", "") |> 
    str_replace_all("([a-zA-Z]{4})([0-9]{4})/([0-9]{4})", "\\1\\2 or \\1\\3") |> 
    str_replace("(\\s*[[:punct:]]?)\\s*(?=incompatible)", ". ") |> 
    str_replace_all("of the following courses", "of") |> 
    str_replace_all(
      "\\b([a-z]+) \\(([^)]+)\\)",
      function(m) {
        inner <- tolower(str_match(m, "\\(([^)]+)\\)")[,2])
        if (inner %in% valid_stems) inner else m
      }
    ) |> 
    str_replace_all(
      "permission (of|from) (the )?(course )?(convenor|convener)",
      "permission"
    ) |> 
    str_replace_all(
      fixed,
      "\\band one of:?\\b",
      "and"
    ) |> 
    str_replace_all(
      "\\band/or\\b",
      "or"
    )
  
  fixed <- str_replace_all(
    fixed,
    "(?:[a-z]{4}[0-9]{4},\\s*){2,}[a-z]{4}[0-9]{4}",
    function(match_str) {
      if (is.na(match_str)) return(match_str)
      
      if (str_detect(match_str, "\\b(and|or)\\b")) return(match_str)
      
      codes <- str_extract_all(match_str, "[a-z]{4}[0-9]{4}")[[1]]
      if (length(codes) < 2) return(match_str)
      
      paste0("(", paste(codes, collapse = " or "), ")")
    }
  )
  
  fixed <- str_replace_all(
    fixed,
    # Match full "two of the following courses ..." pattern including course list
    "two of ([a-z]{4}[0-9]{4}(?:[, ]*(?:or|and)?[, ]*[a-z]{4}[0-9]{4})+)",
    function(full_match) {
      # Extract all course codes from the match
      courses <- str_extract_all(full_match, "[a-z]{4}[0-9]{4}")[[1]]
      if (length(courses) < 2) return(full_match)  # Failsafe
      
      # All 2-course combinations
      pairs <- combn(courses, 2, simplify = FALSE)
      exprs <- vapply(pairs, function(x) paste0("(", x[1], " and ", x[2], ")"), character(1))
      
      paste0("(", paste(exprs, collapse = " or "), ")")
    }
  )
  
  # Fix: comma-separated course list followed by AND (flatten to AND)
  fixed <- str_replace_all(
    fixed,
    "((?:[a-z]{4}[0-9]{4},\\s*)+[a-z]{4}[0-9]{4})(,)?\\s+and",
    function(m) {
      # Extract the course codes
      codes <- str_extract_all(m, "[a-z]{4}[0-9]{4}")[[1]]
      if (length(codes) < 2) return(m)
      
      # Return "a and b and c and" (keep the final "and")
      paste(paste(codes, collapse = " and "), "and")
    }
  )
  
  # Nest OR chains right-associatively
  fixed <- str_replace_all(
    fixed,
    "(?:[a-z]{4}[0-9]{4},\\s*)*(?:[a-z]{4}[0-9]{4}\\s*(?:or\\s+)?)+[a-z]{4}[0-9]{4}",
    function(match_str) {
      codes <- str_extract_all(match_str, "[a-z]{4}[0-9]{4}")[[1]]
      if (length(codes) < 2) return(match_str)
      
      # Right-nested OR: a or (b or (c or d))
      Reduce(function(a, b) paste0("(", a, " or ", b, ")"), rev(codes))
    }
  )
  
  # Flatten AND chains (Oxford comma or not)
  fixed <- str_replace_all(
    fixed,
    "(?:[a-z]{4}[0-9]{4},\\s*)*(?:[a-z]{4}[0-9]{4}\\s*(?:and\\s+)?)+[a-z]{4}[0-9]{4}",
    function(match_str) {
      codes <- str_extract_all(match_str, "[a-z]{4}[0-9]{4}")[[1]]
      if (length(codes) < 2) return(match_str)
      
      # Flat AND: a and b and c and d
      paste(codes, collapse = " and ")
    }
  )
  
  # for e.g. 12 units of 1000 level Psychology (PSYC) or Biology (BIOL) courses
  fixed <- str_replace_all(
    fixed,
    "(\\d+) units of (\\d{4}) level ([^\\.,;]*)",
    function(m) {
      match <- str_match(m, "(\\d+) units of (\\d{4}) level ([^\\.,;]*)")
      n_units <- match[2]
      level   <- match[3]
      tail    <- match[4]
      
      # Detect whether it's an AND or OR list (prefer AND if "and" appears)
      is_and <- grepl("\\band\\b", tail)
      connector <- if (is_and) "and" else "or"
      
      # Extract valid stems from tail
      stems <- str_extract_all(tail, "\\b[a-z]{4}\\b")[[1]]
      stems <- stems[stems %in% valid_stems]
      
      if (length(stems) < 2) return(m)  # don’t replace if nothing found
      
      # Construct logical expression
      parts <- vapply(
        stems,
        function(s) paste0(n_units, " units of ", level, " level ", s),
        character(1)
      )
      
      paste0("(", paste(parts, collapse = paste0(" ", connector, " ")), ")")
    }
  )
  
  fixed
}





clean_blurb <- course_info[course_info$code == "STAT3040", "blurb_clean"]

# fix_blurb took 0.020s for STAT3040 (one course)
# 0.046s for PHYS6102 (two courses)
# and 0.007s for MATH1115 (no courses)
# so seems linear with number of courses mentioned
system.time(fix_blurb(blurb))

course_rgx <- "[a-ZA-Z]{4}[0-9]{4}"
fix_blurb(blurb)
course_info <- course_info |>
  rowwise() |>
  mutate(blurb_clean = fix_blurb(blurb)) |>
  ungroup()

course_info |>
  filter(str_detect(tolower(blurb_clean), "biol2202, biol2001, envs1003, psyc2009, envs2002. incompatible: biol2131, biol6004, biol6130")) |>
  select(code, blurb_clean) %>%
  .[1:10, ] |> 
  pull(blurb_clean)

fix_blurb("To enrol in this course you must must be studying a Bachelor of Advanced Computing 4716 or Bachelor of Advanced Computing/Science 4719. You are not able to enrol in this course if you have previously completed COMP4500 or COMP4550 or COMP4810. Students must complete the steps listed on Enrolling in student projects and complete the Student Project Registration Form to enrol.")

fix_blurb("To enrol in this course you must have successfully completed PSYC2001 and two of the following courses PSYC2002, PSYC2007, PSYC2008 or PSYC2009.")

fix_blurb("To enrol in this course you must have completed two of PHYS2013 or PHYS2016 or PHYS2020 and be be currently studying one 3000 level Physics (PHYS) course.")

fix_blurb("To enrol in this course you must have completed BUSN7066 or BUSN8066. This course is incompatible with BUSN3051, BUSN6051 and BUSN7051.")

fix_blurb("To enrol in this course you must have completed GERM1022, GERM1102, or have equivalent level of language proficiency as demonstrated by placement test, or with permission of the convener. You are not able to enrol in this course if you have previously completed any of the following courses: GERM2103, GERM2104 or GERM2102, GERM2105 or GERM3101, GERM3102 or GERM3106, GERM3007 or GERM3008, GERM3103 or GERM3104.")

fix_blurb("To enrol in this course you must have successfully completed BIOL2174 or completed PSYC2007 and 12 units of 1000 level Psychology 
          (PSYC) or Biology (BIOL) courses or high (ANTH). Incompatible with NEUR6102.")

b <- "to enrol in this course you must have completed phys1201, astr2013, and either ((math2405 or math2305)). incompatible with astr6007"



