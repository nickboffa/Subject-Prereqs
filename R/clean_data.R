library(shiny)
library(bslib)
library(visNetwork)
library(igraph)
library(graphlayouts)
library(jsonlite)
library(stringr)
library(dplyr)
library(readr)
library(shinyWidgets)
library(tidyr)
library(purrr)

course_page_info <- read_csv("../data/current/scrape_course_info_df.csv")
catalogue_info <- read_csv("../data/current/scrape_catalogue_df.csv")

course_info <- left_join(course_page_info, catalogue_info, by = "code")

all_codes <- unique(course_info$code)

course_info$subject <- substr(course_info$code, 1, 4)

all_sessions <- list(
  Standard = c("First Semester", "Second Semester"),
  `Non-standard` = c("Summer Session", "Autumn Session", "Winter Session", "Spring Session"),
  Quarter = c("Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4")
)

all_sessions_flat <- unlist(all_sessions, use.names = FALSE)

assessment_type <- c("Exam" = "exam", "Test/Quiz" = "test_or_quiz",
                     "Essay/Report" = "essay_or_report",
                     "Participation" = "participation",
                     "Oral Presentation" = "oral_presentation",
                     "Other" = "other")

## FIX WHEN NEXT SCRAPE DATA
not_other_assessments <- assessment_type[assessment_type != "other"]

course_info <- course_info |>
  mutate(other = if_all(all_of(not_other_assessments), ~ .x == FALSE))

# Create binary indicator columns for each session

course_info <- course_info |>
  mutate(session_list = str_split(session, " / ") |> lapply(str_trim))


# tidy up college's and schools
# Get the reference set of all unique school names
all_schools <- unique(course_info$school) |> na.omit()

# Function to intelligently split a string only if both sides are in known values
smart_split_school <- function(school_name, reference_schools) {
  if (is.na(school_name)) return(NA_character_)
  
  parts <- str_split(school_name, "\\s+and\\s+(the\\s+)?")[[1]]
  
  if (length(parts) == 1) return(school_name)
  
  for (i in 1:(length(parts) - 1)) {
    left  <- str_trim(paste(parts[1:i], collapse = " and "))
    right <- str_trim(paste(parts[(i+1):length(parts)], collapse = " and "))
    
    # Only match full names or distinct substrings of long