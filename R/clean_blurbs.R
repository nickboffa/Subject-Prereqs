
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
  map_chr(\(x) tolower(str_remove_all(tail(x, 1), "[()]"))) |>  # last word only, cleaned
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
        last_word <- str_to_lower(str_remove_all(blocks[j], "[()]"))
        
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

phrases <- extract_phrases(blurb)

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
    str_replace_all("[[:punct:][:space:]]*$", "")
  
  fixed
}





clean_blurb <- course_info[course_info$code == "STAT3040", "blurb_clean"]

# fix_blurb took 0.020s for STAT3040 (one course)
# 0.046s for PHYS6102 (two courses)
# and 0.007s for MATH1115 (no courses)
# so seems linear with number of courses mentioned
system.time(fix_blurb(blurb))

fix_blurb(blurb)
course_info <- course_info |>
  rowwise() |>
  mutate(blurb_clean = fix_blurb(blurb)) |>
  ungroup()




