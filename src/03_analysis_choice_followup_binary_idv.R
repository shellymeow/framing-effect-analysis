# -------------------------------------------------------
# This script aims to validate and compare the results of the analysis
# the results of the analysis with the python code
# Logistic regression to test if the frame can predict the choice
# The analysis is performed on one task:
# The follow-up task: "sure" (reference) vs "risky"
# Reference frame is "gain"

# -------------------------------------------------------

# Clear the environment
rm(list = objects())

# ---------------------------
# 1. Install and load necessary packages
# ---------------------------
library(broom)

# ---------------------------
# 2. Load data
# ---------------------------
# Define the current working directory
current_dir <- getwd()  # Get the current working directory
print(current_dir)

# Define paths for data loading
input_path <- file.path(current_dir, "data")
output_path <- file.path(current_dir, "results")

# Define the file names for the CSV files
file_name_1 <- "data_choice_evaluation_strict.csv"
file_name_2 <- "data_choice_evaluation_not_strict.csv"
file_name_3 <- "data_choice_evaluation_super_strict.csv"

# Define full file paths for both CSV files
file_path_1 <- file.path(input_path, file_name_1)
file_path_2 <- file.path(input_path, file_name_2)
file_path_3 <- file.path(input_path, file_name_3)

# Load both CSV files into data frames
df_strict <- read.csv(file_path_1)
df_less_strict <- read.csv(file_path_2)
df_super_strict <- read.csv(file_path_3)
# Check data frames
head(df_strict)

# ---------------------------
# 3. Preprocess data
# ---------------------------
#  STRICT DATASET
# --------------------
# Set the reference frame as "gain"
# Ensure the 'frame' column is a factor
df_strict$frame <- as.factor(df_strict$frame)
df_strict$frame <- relevel(df_strict$frame, ref = "gain")
# Follow-up Task: Set the reference choice as "sure"
df_strict$choice <- factor(
  df_strict$choice,
  levels = c(0, 1), # reference level is 0 (1st argument, sure)
)

#  Less STRICT DATASET
# --------------------
# Set the reference frame as "gain"
# Ensure the 'frame' column is a factor
df_less_strict$frame <- as.factor(df_less_strict$frame)
df_less_strict$frame <- relevel(df_less_strict$frame, ref = "gain")
# Follow-up Task: Set the reference choice as "sure"
df_less_strict$choice <- factor(
  df_less_strict$choice,
  levels = c(0, 1), # reference level is 0 (1st argument, sure)
)

#  SUPER STRICT DATASET
# --------------------
# Set the reference frame as "gain"
# Ensure the 'frame' column is a factor
df_super_strict$frame <- as.factor(df_super_strict$frame)
df_super_strict$frame <- relevel(df_super_strict$frame, ref = "gain")
# Follow-up Task: Set the reference choice as "sure"
df_super_strict$choice <- factor(
  df_super_strict$choice,
  levels = c(0, 1), # reference level is 0 (1st argument, sure)
)

# ----------------------------------
# 4. Fit the logistic regression model
# ----------------------------------
# List of questions to analyze
questions <- c("Disease", "Painting", "Virus")

# Define a function to run logistic regression
run_logistic_regression <- function(data, question, outcome_var, task) {
  # Filter the data based on the task type
  if (task == "follow_up") {
    data <- data[data$follow_up == "True", ]
  } else if (task == "follow-up") {
    data <- data[data$follow_up == "True", ]
  } else {
    stop("Invalid task type. Use 'main' or 'follow-up'.")
  }

  # Filter the data for the specific question
  df_question <- data[data$question == question, ]

  # Dynamically create the formula using the specified outcome variable
  formula <- as.formula(paste(outcome_var, "~ frame"))

  # Fit the logistic regression model
  logistic_model <- glm(formula, data = df_question, family = binomial)

  # Summarize the results with odds ratios and confidence intervals
  results <- tidy(logistic_model, exponentiate = TRUE, conf.int = TRUE)

  # Add columns to indicate the question and task
  results$question <- question
  results$task <- task

  results
}

# ----------------------------------
# 4.1 Follow-up task | Strict dataset
# ----------------------------------
# Run logistic regression for the "follow-up" task
results_strict <- lapply(questions, function(q) {
  run_logistic_regression(
    df_strict, q, outcome_var = "choice", task = "follow-up"
  )
})
# Print the combined results
print(results_strict)

# ----------------------------------
# 4.2 Follow-up task | Less strict dataset
# ----------------------------------
# Run logistic regression for the "follow-up" task
results_less_strict <- lapply(questions, function(q) {
  run_logistic_regression(
    df_less_strict, q, outcome_var = "choice", task = "follow-up"
  )
})

# Print the combined results
print(results_less_strict)

# ----------------------------------
# 4.3 Follow-up task | Super strict dataset
# ----------------------------------
# Run logistic regression for the "follow-up" task
results_super_strict <- lapply(questions, function(q) {
  run_logistic_regression(
    df_super_strict, q, outcome_var = "choice", task = "follow-up"
  )
})

# Print the combined results
print(results_super_strict)

# ----------------------------------
# 5. Save the results
# ----------------------------------
# ----------------------------------
# 5. Save the results
# ----------------------------------
# ----------------------------------
# 5.1  Strict dataset
# ----------------------------------
output_file_path <- file.path(
  output_path,
  "results_choice_follow-up_sure_risky_strict.csv"
)
# Save the results to the specified CSV file
write.csv(results_strict, output_file_path, row.names = FALSE)

# ----------------------------------
# 5.2 Less strict dataset
# ----------------------------------
# Save the results to a CSV file
output_file_path <- file.path(
  output_path,
  "results_choice_follow-up_sure_risky_less_strict.csv"
)
# Save the results to the specified CSV file
write.csv(results_less_strict, output_file_path, row.names = FALSE)

# ----------------------------------
# 5.3 Super strict dataset
# ----------------------------------
# Save the results to a CSV file
output_file_path <- file.path(
  output_path,
  "results_choice_follow-up_sure_risky_super_strict.csv"
)
# Save the results to the specified CSV file
write.csv(results_super_strict, output_file_path, row.names = FALSE)

# ----------------------------------
# 6. Save formatted summaries for each question
# ----------------------------------
format_num <- function(x) {
  sprintf("%.2f", x)
}

format_ci <- function(lower, upper) {
  paste0("[", format_num(lower), ", ", format_num(upper), "]")
}

format_p <- function(p) {
  ifelse(p < 0.001, "<.001", ifelse(p < 0.01, sprintf("%.3f", p), format_num(p)))
}

create_question_summary <- function(data, dataset_label, question_name) {
  df_question <- data[data$follow_up == "True" & data$question == question_name, ]
  logistic_model <- glm(choice ~ frame, data = df_question, family = binomial)
  coef_table <- coef(summary(logistic_model))

  term <- "frameloss"
  estimate <- coef_table[term, "Estimate"]
  std_error <- coef_table[term, "Std. Error"]
  p_value <- coef_table[term, "Pr(>|z|)"]

  ci_lower <- estimate - 1.96 * std_error
  ci_upper <- estimate + 1.96 * std_error
  odds_ratio <- exp(estimate)
  or_ci_lower <- exp(ci_lower)
  or_ci_upper <- exp(ci_upper)

  data.frame(
    Dataset = dataset_label,
    `Choice (vs Sure)` = "Risky (loss vs gain)",
    `Estimate (log-OR)` = format_num(estimate),
    `95% CI (log-OR)` = format_ci(ci_lower, ci_upper),
    OR = format_num(odds_ratio),
    `95% CI (OR)` = format_ci(or_ci_lower, or_ci_upper),
    p = format_p(p_value),
    check.names = FALSE
  )
}

for (question_name in questions) {
  question_summary <- rbind(
    create_question_summary(df_strict, "Strict", question_name),
    create_question_summary(df_less_strict, "Less Strict", question_name),
    create_question_summary(df_super_strict, "Super Strict", question_name)
  )

  write.csv(
    question_summary,
    file.path(
      output_path,
      paste0(
        "results_choice_follow-up_sure_risky_summary_",
        question_name,
        ".csv"
      )
    ),
    row.names = FALSE
  )
}
