all_degrees <- unique(c(
  unlist(resume_data$degree_name_parsed),
  unlist(resume_data$educationaL_requirements)
))

all_degrees

degree_string <- paste(all_degrees, collapse = ", ")
degree_string



# install.packages("httr2")
# install.packages("jsonlite")  # For easier JSON handling

library(httr2)
library(jsonlite)

# ---- SETUP ----
api_key <- "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"
endpoint <- "https://api.groq.com/openai/v1/chat/completions"


# ---- PROMPT ----
prompt_text <- paste0(
  "You are a data parser. I have a list of educational qualifications in various formats.\n",
  "Please go through them and for each one, return a standardized numeric label for the highest degree level:\n",
  "- 1 = Below X\n",
  "- 2 = X\n",
  "- 3 = XII\n",
  "- 4 = UG\n",
  "- 5 = PG\n",
  "- 6 = PhD\n\n",
  "Return the result as R lists of numbers matching each input line.\n\n",
  "Eg. output:\n",
  "[B.Sc, B.Tech, BBA] = 4\n\n",
  "Give final output as:\n",
  "c(4, 5, 3, 4, ...) — so that I can directly copy-paste into R.\n\n",
  "Here is the list:\n", degree_string
)

# ---- MAKE API REQUEST ----
resp <- request(endpoint) |>
  req_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  ) |>
  req_body_json(list(
    model = "mixtral-8x7b-32768",
    messages = list(
      list(role = "system", content = "You are a helpful assistant."),
      list(role = "user", content = prompt_text)
    ),
    temperature = 0.2
  )) |>
  req_perform()

# Check status code
status_code <- req_status(resp)
if (status_code != 200) {
  stop(paste("Error:", status_code, ":", req_body_text(resp)))
}

# Print response body as text
cat("Response body:\n")
cat(req_body_text(resp))

# Parse and print final result
result <- resp |>
  resp_body_json() |>
  (\(x) x$choices[[1]]$message$content)()

# Print in console, ready to copy-paste
cat("Copy-paste this into your main script:\n\n")
cat(result)