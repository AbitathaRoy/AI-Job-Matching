# Load required libraries
# Install these packages only if you haven't already
# install.packages("tidyverse")
# install.packages("markovchain")

library(tidyverse)
library(markovchain)

# --- Data Loading and Initial Cleaning ---

# Load the dataset
resume_data <- read.csv("D:\\AI-Job-Matching\\resume_data.csv", stringsAsFactors = FALSE)
head(resume_data)

# Clean column names (remove special characters like hidden BOM)
names(resume_data) <- str_replace_all(names(resume_data), "[^[:alnum:]_]", "")
names(resume_data)

# --- Date Handling ---

# Clean the dates (function)
clean_date_string <- function(date_str) {
  date_str <- gsub("\\[|\\]", "", date_str)                # Remove [ and ]
  date_str <- gsub("'", "", date_str)                      # Remove single quotes
  date_str <- gsub("None|N/A", "NA", date_str, ignore.case = TRUE)  # Replace with NA
  date_items <- strsplit(date_str, ",\\s*")[[1]]           # Split by comma
  return(date_items)
}
resume_data$start_dates_clean <- lapply(resume_data$start_dates, clean_date_string)
resume_data$start_dates_clean
resume_data$end_dates_clean <- lapply(resume_data$end_dates, clean_date_string)
resume_data$end_dates_clean

# Parse cleaned dates and create date objects (function)
parse_flexible_date <- function(date_str) {
  if (is.na(date_str) || date_str == "NA" || grepl("20XX", date_str)) 
    return(NA)
  
  formats <- c("%b %Y", "%B %Y", "%m/%Y", "%Y")  # Jan 2020, January 2020, 01/2020, 2020
  
  for (fmt in formats) {
    parsed <- as.Date(paste0("01 ", date_str), format = paste0("%d ", fmt))
    if (!is.na(parsed)) 
      return(parsed)
  }
  
  return(NA)  # if all fail
}
resume_data$start_dates_parsed <- lapply(resume_data$start_dates_clean, function(date_vec) {
  lapply(date_vec, parse_flexible_date)
})
resume_data$end_dates_parsed <- lapply(resume_data$end_dates_clean, function(date_vec) {
  lapply(date_vec, parse_flexible_date)
})
resume_data$start_dates_parsed
resume_data$end_dates_parsed
class(resume_data$start_dates_parsed[[1]][[1]])


# Replace NA end dates with the current date
resume_data$end_dates_parsed <- lapply(resume_data$end_dates_parsed, function(date_vec) {
  # Replace NAs with today's date
  date_vec[is.na(date_vec)] <- Sys.Date()
  
  # Convert each element of date_vec to Date
  date_vec <- lapply(date_vec, function(d) as.Date(d))  # Convert each date in the vector
  
  return(date_vec)
})
resume_data$end_dates_parsed

# Drop rows with any invalid (too old) start dates
valid_entries <- sapply(resume_data$start_dates_parsed, function(start_list) {
  all(sapply(start_list, function(d) {
    !is.na(d) && as.numeric(d) > as.numeric(as.Date("1900-01-01"))
  }))
})

resume_data <- resume_data[valid_entries, ]



# Calculate experience in years
safe_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(x, origin = "1970-01-01"))
  if (is.character(x)) return(as.Date(x))
  return(NA)
}

resume_data$experience_years <- mapply(function(start_dates, end_dates) {
  # Safely convert all entries inside each list element
  start_dates <- lapply(start_dates, safe_as_date)
  end_dates <- lapply(end_dates, safe_as_date)
  
  # Unlist and convert to Date again just in case
  start_dates <- as.Date(unlist(start_dates), origin = "1970-01-01")
  end_dates <- as.Date(unlist(end_dates), origin = "1970-01-01")
  
  n <- min(length(start_dates), length(end_dates))
  
  diffs <- difftime(end_dates[1:n], start_dates[1:n], units = "weeks")
  as.numeric(diffs) / 52
}, resume_data$start_dates_parsed, resume_data$end_dates_parsed, SIMPLIFY = FALSE)

# Testing
class(resume_data$start_dates_parsed[[2]])
head(resume_data$start_dates_parsed)
head(Map(function(s, e, x) list(start = s, end = e, experience = x),
         resume_data$start_dates_parsed,
         resume_data$end_dates_parsed,
         resume_data$experience_years), 5)

# Calculating total experience years

resume_data$total_experience_years <- sapply(resume_data$experience_years, function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    return(sum(x, na.rm = TRUE))
  }
})    # In our simplified case, while comparing years and stuff, we will consider the total experience gained till date
      # Not the number of years in xyz role or abc field or things like that


# --- Education Level Mapping ---

# Clean degree column and assign numeric education levels
# resume_data$education_level <- tolower(resume_data$degree_names)
# resume_data$education_numeric <- case_when(
#   str_detect(resume_data$education_level, "phd") ~ 4,
#   str_detect(resume_data$education_level, "master|mba|msc") ~ 3,
#   str_detect(resume_data$education_level, "bachelor|btech|b\\.sc|bca") ~ 2,
#   TRUE ~ 1
# )

head(resume_data$degree_name, 15)

# Converting data to lists
parse_degrees <- function(degree_str) {
  # Remove brackets and split by commas
  degrees <- gsub("\\[|\\]|'", "", degree_str)  # remove [ ] and '
  degrees_split <- strsplit(degrees, ",")[[1]]
  # Trim whitespace
  degrees_trimmed <- trimws(degrees_split)
  return(degrees_trimmed)
}
resume_data$degree_name_parsed <- lapply(resume_data$degree_name, parse_degrees)

head(resume_data$degree_name_parsed, 15)

# Define keyword lists for each academic level
below_class_x <- c("below class x", "none", "n/a", "select one", "skills", "course", "general courses", "seminar", "esol program", "kcpe", "correspondence/distance learning", "attending", "minor", "course revisions")
class_x <- c("class x", "x", "high school diploma", "ged", "grade 12", "school certificate", "kcse")
class_xii <- c("class xii", "xii", "a level", "certification", "certificate", "diploma", "technical diploma", "general diploma", "certificate of completion", "lean certification", "six sigma", "epa certification", "aws brazing", "poke-a-yoke", "neec building", "applied industrial ergonomics", "shipboard firefighting", "waste heat boilers", "class facilitator", "food handler", "project management graduate certificate", "business certification", "certified professional coach", "cacc certification", "pharmacy technician", "acca ii", "testing computer software", "principles of management", "association of business executive", "engineering common core", "engineering mechanical", "engineering electrical", "gas turbine mechanical/electrical", "shipbd gage cal", "machinery control system", "marine maintenance", "waste heat boilers")
ug <- c("b.tech", "b.sc", "b.a", "b.com", "b.e", "b.s", "b.b.a", "b.c.a", "bachelor", "bs", "ba", "be", "bba", "bca", "btech", "associate", "a.a", "a.s", "a.a.s", "honours bachelor", "integrated b.tech", "integrated b.e.", "bsc", "honors", "international business", "business development", "executive management")
pg <- c("m.tech", "m.sc", "m.a", "m.com", "m.b.a", "m.s", "m.e", "m.eng", "master", "msc", "ms", "mba", "mca", "post graduation", "pg diploma", "graduate certificate", "executive mba", "masters", "integrated m.tech", "integrated m.e.", "mbm", "mcom")
phd <- c("phd", "ph.d", "doctor of philosophy", "phd candidate", "d.b.a")

# Function to determine academic level
assign_academic_level <- function(degree_string) {
  # Convert degree string to lowercase for case-insensitive matching
  degree_string <- tolower(degree_string)
  
  # Check lists in descending order to assign highest level
  if (any(sapply(phd, function(x) grepl(x, degree_string)))) {
    return(6)
  } else if (any(sapply(pg, function(x) grepl(x, degree_string)))) {
    return(5)
  } else if (any(sapply(ug, function(x) grepl(x, degree_string)))) {
    return(4)
  } else if (any(sapply(class_xii, function(x) grepl(x, degree_string)))) {
    return(3)
  } else if (any(sapply(class_x, function(x) grepl(x, degree_string)))) {
    return(2)
  } else if (any(sapply(below_class_x, function(x) grepl(x, degree_string)))) {
    return(1)
  } else {
    return(1) # Default to Below Class X for unmatched entries
  }
}

# Apply the function to create the new column
resume_data$academic_level <- sapply(resume_data$degree_name_parsed, function(degree_vec) {
  degree_string <- paste(degree_vec, collapse = " ")
  assign_academic_level(degree_string)
})


head(resume_data$academic_level)

# --- Data Filtering ---

# Drop rows where all experience values are NA and where academic_level is NA
# Step 1: Identify which rows have all NAs in experience_years
# all_exp_na <- sapply(resume_data$experience_years, function(x) all(is.na(x)))
# Step 2: Subset the dataframe manually (using indexing)
# resume_data <- resume_data[!all_exp_na & !is.na(resume_data$academic_level), ]

# Drop rows where even one experience value is NA or where academic level is NA
library(tidyr)
resume_data <- resume_data %>%
  drop_na(experience_years, academic_level)


# Checking if it worked. Should return 0.
# sum(sapply(resume_data$experience_years, function(x) all(is.na(x))))
# sum(is.na(resume_data$academic_level))
sum(is.na(resume_data$experience_years))
sum(is.na(resume_data$academic_level))

# --- Educational Requirements ---

assign_required_academic_level <- function(degree_string) {
  # Convert degree string to lowercase for case-insensitive matching
  degree_string <- tolower(degree_string)
  
  # Check lists in ascending order to assign lowest level
  if (any(sapply(below_class_x, function(x) grepl(x, degree_string)))) {
    return(1)
  } else if (any(sapply(class_x, function(x) grepl(x, degree_string)))) {
    return(2)
  } else if (any(sapply(class_xii, function(x) grepl(x, degree_string)))) {
    return(3)
  } else if (any(sapply(ug, function(x) grepl(x, degree_string)))) {
    return(4)
  } else if (any(sapply(pg, function(x) grepl(x, degree_string)))) {
    return(5)
  } else if (any(sapply(phd, function(x) grepl(x, degree_string)))) {
    return(6)
  } else {
    return(1) # Default to Below Class X for unmatched entries
  }
}
resume_data$required_academic_level <- sapply(resume_data$educationaL_requirements, function(degree_vec) {
  degree_string <- paste(degree_vec, collapse = " ")
  assign_required_academic_level(degree_string)
})

# --- Required Experience Years ---

# Renaming (well, not exactly)
resume_data$required_experience <- resume_data$experiencere_requirement

unique(resume_data$required_experience)

# Parsing and storing required experience as a vector
parse_experience_range <- function(text) {
  text <- tolower(text)
  
  if (grepl("at least", text)) {
    num <- as.numeric(gsub("[^0-9]", "", text))
    return(c(num, 100))
  } else if (grepl("to", text)) {
    bounds <- as.numeric(unlist(regmatches(text, gregexpr("[0-9]+", text))))
    return(c(bounds[1], bounds[2]))
  } else if (grepl("at most", text)) {
    num <- as.numeric(gsub("[^0-9]", "", text))
    return(c(0, num))
  } else {
    return(NA)  # For empty or unrecognized patterns
  }
}
resume_data$required_experience_range <- lapply(resume_data$required_experience, function(exp_str) {
  parse_experience_range(exp_str)
})

# Drop ineligible candidates (based on academic level and experience)

ranges <- resume_data$required_experience_range
experiences <- resume_data$total_experience_years

# Apply logic element-wise
valid_rows <- mapply(function(range, exp) {
  if (is.null(range) || length(range) == 0 || all(is.na(range))) {
    return(TRUE)  # Keep rows with NULL or empty `range` or all NA's
  }
  
  # Ensure that 'range' is a vector with exactly two values
  if (length(range) != 2 || any(is.na(range)) || is.na(exp)) {
    return(FALSE)  # Filter rows with invalid range or experience NA
  }
  
  # Check if the experience falls within the specified range
  exp >= range[1] && exp <= range[2]
}, ranges, experiences)

# Filter the data based on the valid rows and academic level condition
resume_data <- resume_data[valid_rows & resume_data$academic_level >= resume_data$required_academic_level, ]




# --- Naive Hiring Simulation ---

# # Define thresholds for experience and education (using 70th percentile)
# threshold_exp <- quantile(resume_data$experience_years, 0.7, na.rm = TRUE)
# threshold_edu <- quantile(resume_data$academic_level, 0.7, na.rm = TRUE)
# 
# # Simulate hiring outcome based on thresholds
# resume_data$hired <- ifelse(
#   resume_data$experience_years >= threshold_exp & resume_data$academic_level >= threshold_edu,
#   1, 0
# )

# --- Markov Chain Modeling for Career Transitions ---

# Assign candidate IDs
resume_data$candidate_id <- seq_len(nrow(resume_data))

# Parsed job positions held into vectors
parse_positions <- function(pos_string) {
  if (is.na(pos_string)) return(NA)
  
  # Remove square brackets and single/double quotes
  cleaned <- gsub("\\[|\\]|'", "", pos_string)
  
  # Split by comma
  positions <- unlist(strsplit(cleaned, ","))
  
  # Trim whitespace
  positions <- trimws(positions)
  
  return(positions)
}
resume_data$positions_parsed <- lapply(resume_data$positions, parse_positions)


# # Prepare data for Markov chain analysis
# transitions <- resume_data %>%
#   filter(!is.na(job_position_name)) %>%
#   arrange(candidate_id, start_date) %>%
#   group_by(candidate_id) %>%
#   mutate(next_role = lead(job_position_name)) %>%
#   ungroup() %>%
#   filter(!is.na(next_role))
# 
# # Create the transition matrix
# trans_matrix <- table(transitions$job_position_name, transitions$next_role)
# 
# # Create the Markov chain object
# mc <- new("markovchain", transitionMatrix = prop.table(trans_matrix, 1))

library(dplyr)
library(tidyr)
library(purrr)

# Diagnostic: Check vector lengths for mismatches
resume_data <- resume_data %>%
  mutate(
    pos_length = map_int(positions_parsed, length),
    start_length = map_int(start_dates_parsed, length),
    end_length = map_int(end_dates_parsed, length),
    length_mismatch = pos_length != start_length | pos_length != end_length
  )

# Print rows with mismatched lengths for review
mismatch_rows <- resume_data %>%
  filter(length_mismatch) %>%
  select(candidate_id, pos_length, start_length, end_length, positions_parsed, start_dates_parsed, end_dates_parsed)
print("Rows with mismatched vector lengths:")
print(mismatch_rows)

# Function to pad vectors to the same length
pad_vectors <- function(pos, start, end) {
  max_len <- max(length(pos), length(start), length(end))
  list(
    positions = c(pos, rep(NA, max_len - length(pos))),
    start_dates = c(start, rep(NA, max_len - length(start))),
    end_dates = c(end, rep(NA, max_len - length(end)))
  )
}

# Prepare data by padding vectors
resume_data_padded <- resume_data %>%
  mutate(
    padded = pmap(list(positions_parsed, start_dates_parsed, end_dates_parsed), pad_vectors),
    positions_parsed = map(padded, "positions"),
    start_dates_parsed = map(padded, "start_dates"),
    end_dates_parsed = map(padded, "end_dates")
  ) %>%
  select(-padded)

# Create the transitions data frame
transitions <- resume_data_padded %>%
  # Select relevant columns
  select(candidate_id, positions_parsed, start_dates_parsed, end_dates_parsed) %>%
  # Filter out rows with empty positions_parsed
  filter(map_int(positions_parsed, length) > 0) %>%
  # Unnest the vector columns into rows
  unnest(cols = c(positions_parsed, start_dates_parsed, end_dates_parsed)) %>%
  # Group by candidate_id to process each candidate's job history
  group_by(candidate_id) %>%
  # Arrange by reverse index to process from earliest to latest (since vectors are most recent first)
  arrange(desc(row_number()), .by_group = TRUE) %>%
  # Create current_position and next_position
  mutate(
    current_position = positions_parsed,
    next_position = lead(positions_parsed)
  ) %>%
  # Select final columns and rename dates
  select(
    candidate_id,
    current_position,
    next_position,
    start_date = start_dates_parsed,
    end_date = end_dates_parsed
  ) %>%
  ungroup() %>%
  # Remove rows with NA in current_position
  filter(!is.na(current_position))

# -- MARKOV CHAIN --

library(dplyr)
library(markovchain)

# Prepare transitions for Markov chain
# Step 1: Filter out NA and invalid titles
valid_transitions <- transitions %>%
  filter(
    !is.na(current_position) & 
      !is.na(next_position) & 
      !current_position %in% c("N/A", "None") & 
      !next_position %in% c("N/A", "None")
  )

# Step 2: Get common roles (roles in both current and next positions)
common_roles <- intersect(
  unique(valid_transitions$current_position),
  unique(valid_transitions$next_position)
)

# Step 3: Filter transitions to only common roles
valid_transitions <- valid_transitions %>%
  filter(
    current_position %in% common_roles & 
      next_position %in% common_roles
  ) %>%
  select(current_position, next_position)

# Diagnostic: Check number of unique roles
cat("Number of common roles used:", length(common_roles), "\n")
cat("Unique roles in current_position:", length(unique(valid_transitions$current_position)), "\n")
cat("Unique roles in next_position:", length(unique(valid_transitions$next_position)), "\n")

# Step 4 (fixed): Create a complete square transition matrix
all_roles <- union(valid_transitions$current_position, valid_transitions$next_position)

trans_matrix <- table(
  factor(valid_transitions$current_position, levels = all_roles),
  factor(valid_transitions$next_position, levels = all_roles)
)

# Step 5 (force correct class): Convert to numeric matrix row-wise
prob_matrix <- prop.table(trans_matrix, 1)
prob_matrix <- matrix(as.numeric(prob_matrix), 
                      nrow = nrow(trans_matrix),
                      dimnames = dimnames(trans_matrix))
class(prob_matrix)

# Fix numerical imprecision — re-normalize rows strictly
row_sums <- rowSums(prob_matrix)
prob_matrix <- sweep(prob_matrix, 1, row_sums, FUN = "/")

summary(rowSums(prob_matrix))  # Should all be 1

# Replace NaN (from zero rows) with 0
prob_matrix[is.nan(prob_matrix)] <- 0

summary(rowSums(prob_matrix))  # Should all be 1

# Step 6: Now the matrix is square (safe to proceed)
cat("Final square probability matrix dimensions:", dim(prob_matrix), "\n")

# - Fixing Errors with Assumptions -

# Step 7: Identify roles with zero transitions and handle them by assigning self-transition probability

# We will loop through each row of the transition matrix, checking for rows that sum to zero.
# If any row sums to zero (i.e., no one has transitioned out of that role), we will assign the 
# transition probability for the self-transition (i.e., from the role to itself) to 1.

# Loop through each row of the probability matrix
for (i in 1:nrow(prob_matrix)) {
  # Check if the sum of the row is zero (i.e., no one transitioned out of the role)
  if (sum(prob_matrix[i, ]) == 0) {
    # Assign the self-transition probability (current position to itself) to 1
    prob_matrix[i, i] <- 1
  }
}

# Check if the row sums are now all 1, as they should be for a valid Markov Chain transition matrix
cat("Row sums after fixing zero rows:\n")
summary(rowSums(prob_matrix))  # Should all be 1 after this step

# Step 8: Proceed with creating the Markov Chain object
# After ensuring the transition matrix is valid (all rows sum to 1), we can safely create the Markov chain
mc <- new("markovchain", transitionMatrix = prob_matrix)

# Final check
cat("Markov Chain successfully created.\n")
summary(mc)


# Optional: View sample of transition matrix
cat("\nSample of Transition Probability Matrix (first 5 roles):\n")
print(prob_matrix[1:5, 1:5])

# -- Analytics with Markov Chains --

# 1 - The top 10 and bottom 10 lucrative positions -

total_incoming_prob <- colSums(prob_matrix)
lucrative_jobs <- sort(total_incoming_prob, decreasing = TRUE)
cat("Most lucrative jobs:\n")
print(lucrative_jobs[1:10])  # Top 10 lucrative jobs
cat("\nLeast lucrative jobs:\n")
print(lucrative_jobs[(length(lucrative_jobs)-9):length(lucrative_jobs)])  # Bottom 10 lucrative jobs

# 2 - Job Stability - Self Transitions -

# Identify the jobs with the highest and lowest self-transition probabilities
highest_self_transitions <- sort(self_transition_prob, decreasing = TRUE)
lowest_self_transitions <- sort(self_transition_prob, decreasing = FALSE)

head(highest_self_transitions, 60)

# 3 - Transitions between Specific Roles -

# Example: Transition probability from "Senior Accountant" to "Tax Analyst"
transition_prob <- prob_matrix["Senior Accountant", "Tax Analyst"]
print(transition_prob)

# 4 - State Distribution Over Time -

# Calculate steady-state distribution (using the power method or linear algebra)
steady_state <- rep(1, ncol(prob_matrix))  # Initial guess (uniform distribution)
steady_state <- steady_state %*% prob_matrix  # Multiply by transition matrix
steady_state <- steady_state / sum(steady_state)  # Normalize to sum to 1
print(steady_state)

# 5 - Transition Visualisation -

# Heat Map

library(ggplot2)
library(reshape2)

# Melt the matrix for visualization
prob_matrix_melted <- melt(prob_matrix)

# Create the heatmap
ggplot(prob_matrix_melted, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "blue") + 
  theme_minimal() +
  xlab("From Job") + 
  ylab("To Job") +
  ggtitle("Transition Probability Matrix Heatmap")


# Select Top 10 Common Roles (or any number you want)
top_roles <- names(sort(rowSums(prob_matrix), decreasing = TRUE))[1:10]

# Subset your transition matrix
prob_matrix_subset <- prob_matrix[top_roles, top_roles]


# Melt the smaller matrix
prob_matrix_melted <- reshape2::melt(prob_matrix_subset)

# Plot the Heatmap again
library(ggplot2)

ggplot(prob_matrix_melted, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  xlab("From Job") + 
  ylab("To Job") +
  ggtitle("Transition Probability Matrix (Top 10 Roles)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# To detect Promotions :
# Define promotion keywords
promotion_keywords <- c("Senior", "Lead", "Principal", "Manager", "Director", "Head", "Chief", "VP")

# Function to check if a title indicates a promotion
is_promotion <- function(from_title, to_title) {
  from_promotion <- any(str_detect(from_title, promotion_keywords))
  to_promotion <- any(str_detect(to_title, promotion_keywords))
  
  # Promotion occurs when moving from non-promotion to promotion title
  return(!from_promotion & to_promotion)
}

library(dplyr)

# Find Promotions
promotions <- transitions %>%
  filter(!is.na(current_position) & !is.na(next_position)) %>%
  filter(mapply(is_promotion, current_position, next_position))

# View promotions
print(promotions)

promotion_counts <- promotions %>%
  group_by(current_position) %>%
  summarise(Promotions = n()) %>%
  arrange(desc(Promotions))

print(promotion_counts)


# Network Diagram
library(igraph)

# Create an igraph object from the transition matrix
graph <- graph_from_adjacency_matrix(prob_matrix, weighted = TRUE, mode = "directed")

# Plot the network
plot(graph, vertex.size = 5, vertex.label.cex = 0.7, edge.arrow.size = 0.5)

# 6 - Role Transition Clusters -

# Using Hierarchical Clustering

dist_matrix <- dist(prob_matrix)
hc <- hclust(dist_matrix)



# plotting only top 10:
library(igraph)

# Top 10 most common roles
top_roles <- names(sort(rowSums(prob_matrix), decreasing = TRUE))[1:10]

# Filter transitions
filtered_transitions <- transitions %>%
  filter(current_position %in% top_roles & next_position %in% top_roles)

# Create igraph
g <- graph_from_data_frame(filtered_transitions, directed = TRUE)

# Plot
plot(g, vertex.size = 10, vertex.label.cex = 0.8, edge.arrow.size = 0.5,
     main = "Career Transition Network (Top 10 Roles)")


# PLotting only promotions :
# Use promotions data you found earlier
promotion_counts <- promotions %>%
  group_by(current_position, next_position) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

# Take Top 20 most common promotions
top_promotions <- promotion_counts %>%
  slice_head(n = 10)

g <- graph_from_data_frame(top_promotions, directed = TRUE)

plot(g, vertex.size = 15, vertex.label.cex = 0.8, edge.arrow.size = 0.5,
     main = "Promotion Network Only")

# Plot the dendrogram

plot(hc)

# visualising only top roles in dendogram 
# Top 30 roles
top_roles <- names(sort(rowSums(prob_matrix), decreasing = TRUE))[1:30]

# Subset transition matrix
prob_matrix_top <- prob_matrix[top_roles, top_roles]

# Recompute distance and cluster
dist_matrix_top <- dist(prob_matrix_top)
hc_top <- hclust(dist_matrix_top, method = "complete")

# Plot
plot(as.dendrogram(hc_top), main="Top 30 Career Roles")


# 7 - Markov Chain Analysis - Career Transition over n steps

simulate_career <- function(start_role, steps) {
  current_state <- start_role
  for (i in 1:steps) {
    current_state <- sample(1:ncol(prob_matrix), 1, prob = prob_matrix[current_state, ])
    print(rownames(prob_matrix)[current_state])
  }
}

simulate_career("Software Engineer", 5)  # Simulate from "X" for n steps


# --- Z-score Ranking --- [FLAGGED]

library(dplyr)
library(stringr)
library(purrr)

# Define parser for Python-style skill lists
parse_list <- function(skill_str) {
  skill_str %>%
    str_remove_all("\\[|\\]|'") %>%     # remove square brackets and single quotes
    str_split(",\\s*") %>%              # split by comma and optional spaces
    unlist() %>%
    str_trim() %>%
    discard(~ .x == "")                 # remove empty strings
}
resume_data <- resume_data %>%
  mutate(
    skills_parsed = map(skills, parse_list),
    num_skills = map_int(skills_parsed, length),
    composite_score = 0.4 * num_skills +
      0.3 * total_experience_years +
      0.3 * academic_level,
    z_score = as.numeric(scale(composite_score))
  ) %>%
  arrange(desc(z_score))

# Set n for top candidates to display
n <- 50  # Change as needed

# Filter transitions where next_position is NA
filtered_transitions <- transitions %>%
  filter(is.na(next_position))

# Join filtered transitions with resume_data on candidate_id
top_candidates <- resume_data %>%
  filter(candidate_id %in% filtered_transitions$candidate_id) %>%
  left_join(filtered_transitions, by = "candidate_id") %>%  # Join to get current_position
  arrange(desc(z_score)) %>%
  slice_head(n = n) %>%
  select(candidate_id, current_position, z_score)  # Display relevant columns

# Print top candidates
print(top_candidates)
resume_data %>%
  filter(candidate_id <= 50) %>%
  select(candidate_id, num_skills, total_experience_years, academic_level, composite_score)

# --- Estimating Hiring Probability using Bayes Theorem ---

# Define the function to calculate hiring probability
calculate_hiring_probability <- function(experience_years, academic_level, num_skills) {
  p_hired <- 0.4  # Prior probability of getting hired
  
  # Conditional probability of experience given hired
  p_exp_given_hired <- ifelse(experience_years >= 5, 0.8,
                              ifelse(experience_years >= 2, 0.6, 0.3))
  
  # Conditional probability of education given hired
  p_edu_given_hired <- ifelse(academic_level >= 5, 0.9,
                              ifelse(academic_level >= 4, 0.7,
                                     ifelse(academic_level >= 3, 0.5, 0.2)))
  
  # Conditional probability of skills given hired
  p_skills_given_hired <- ifelse(num_skills >= 10, 0.8,
                                 ifelse(num_skills >= 5, 0.6, 0.4))
  
  # Updated Naive Bayes numerator (multiplying all conditionals)
  numerator <- p_exp_given_hired * p_edu_given_hired * p_skills_given_hired * p_hired
  
  # Updated denominator (for normalization)
  denominator <- numerator + ((1 - p_exp_given_hired) * (1 - p_edu_given_hired) * (1 - p_skills_given_hired) * (1 - p_hired))
  
  hiring_probability <- numerator / denominator
  return(hiring_probability)
}

# Now apply it with three arguments
resume_data$hiring_probability <- mapply(
  calculate_hiring_probability,
  resume_data$total_experience_years,
  resume_data$academic_level,
  resume_data$num_skills  # <- add this
)

# Display updated probabilities
print(resume_data[, c("total_experience_years", "academic_level", "num_skills", "hiring_probability")])

# --- Data Export ---

# Export the final ranked data to an RDS file
saveRDS(resume_data, "D:\\AI-Job-Matching\\resume_ranking_data.rds")

