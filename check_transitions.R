library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

# Fix list columns by unnesting or extracting dates
transitions <- transitions %>%
  mutate(
    start_date = map(start_date, ~ as_date(.x[[1]])),
    end_date = map(end_date, ~ as_date(.x[[1]]))
  ) %>%
  unnest(cols = c(start_date, end_date), keep_empty = TRUE)

# 1. Check column names and data types
cat("Checking table structure (columns and their types):\n")
str(transitions)

# 2. Check for missing values
cat("\nCounting empty (missing) values in each column:\n")
colSums(is.na(transitions))

# 3. Verify next_position is NA for the most recent role per candidate
recent_roles <- transitions %>%
  group_by(candidate_id) %>%
  filter(start_date == max(start_date, na.rm = TRUE)) %>%
  summarise(has_na_next = is.na(next_position))

cat("\nChecking if the latest job for each candidate has no 'next job' (should be empty):\n")
recent_roles %>% 
  filter(!has_na_next) %>%
  print(row.names = FALSE)

# 4. Check chronological order (start_date increasing within candidate_id)
chronology_check <- transitions %>%
  group_by(candidate_id) %>%
  arrange(start_date) %>%
  mutate(is_ordered = start_date <= lead(start_date, default = as_date(Inf))) %>%
  summarise(all_ordered = all(is_ordered, na.rm = TRUE)) %>%
  filter(!all_ordered)

cat("\nChecking if jobs are listed in time order (oldest to newest):\n")
chronology_check %>%
  print(row.names = FALSE)

# 5. Sample transitions for a few candidates (e.g., 3 candidates)
cat("\nShowing job history for 3 random candidates:\n")
sample_candidates <- transitions %>%
  distinct(candidate_id) %>%
  slice_head(n = 3) %>%
  pull(candidate_id)
transitions %>%
  filter(candidate_id %in% sample_candidates) %>%
  arrange(candidate_id, start_date) %>%
  print(n = Inf)

# 6. Summarize transitions per candidate
cat("\nSummary of how many job changes each candidate has:\n")
transitions %>%
  group_by(candidate_id) %>%
  summarise(num_transitions = n()) %>%
  summary()

