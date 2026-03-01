(resume_data$required_experience_range[[6]])

all_jobs <- unique(c(
  unlist(resume_data$positions_parsed)
))

unique(all_jobs)
