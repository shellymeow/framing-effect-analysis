# -------------------------------------------------------
# This script aims to validate and compare the results of the analysis
# the results of the analysis with the python code
# Logistic regression to test if the frame can predict the choice
# The analysis is performed on one task:
# The main task: "non-neutral" (reference) vs "neutral"
# Reference frame is gain

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
output_path <- file.path(current_dir, "stat_results")

# Define the file names for the CSV files
file_name_1 <- "data_choice_evaluation_strict.csv"
file_name_2 <- "data_choice_evaluation_less_strict.csv"
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
# Add a new column "choice_binary" to compare "non-neutral" vs "neutral"
# ---------------------------
# Add a new column "choice_binary" to the data frame
# ps: non-neutral=0, neutral=1

#  STRICT DATASET
# --------------------
df_strict$choice_binary <- ifelse(
  # Set "choice_binary" to 0 if "choice" is in (0, 1), otherwise to 1
  df_strict$choice %in% c(0, 1), 0, 1
)

# Main Task: Set the reference choice as "non-neutral"
df_strict$choice_binary <- factor(
  df_strict$choice_binary,
  levels = c(0, 1), # reference level is 0 (non-neutral, 1st argument)
)
df_strict$frame <- factor(df_strict$frame, levels = c("gain", "loss"))

#  Less STRICT DATASET
# --------------------
df_less_strict$choice_binary <- ifelse(
  # Set "choice_binary" to 0 if "choice" is in (0, 1), otherwise to 1
  df_less_strict$choice %in% c(0, 1), 0, 1
)

# Main Task: Set the reference choice as "non-neutral"
df_less_strict$choice_binary <- factor(
  df_less_strict$choice_binary,
  levels = c(0, 1), # reference level is 0 (non-neutral, 1st argument)
)
df_less_strict$frame <- factor(df_less_strict$frame, levels = c("gain", "loss"))

#  SUPER STRICT DATASET
# --------------------
df_super_strict$choice_binary <- ifelse(
  # Set "choice_binary" to 0 if "choice" is in (0, 1), otherwise to 1
  df_super_strict$choice %in% c(0, 1), 0, 1
)

# Main Task: Set the reference choice as "non-neutral"
df_super_strict$choice_binary <- factor(
  df_super_strict$choice_binary,
  levels = c(0, 1), # reference level is 0 (non-neutral, 1st argument)
)
df_super_strict$frame <- factor(df_super_strict$frame, levels = c("gain", "loss"))

# ----------------------------------
# 4. Fit the logistic regression model
# ----------------------------------
# List of questions to analyze
questions <- c("Disease", "Painting", "Virus")

# Define a function to run logistic regression
run_logistic_regression <- function(data, question, outcome_var, task) {
  # Filter the data based on the task type
  if (task == "main") {
    data <- data[data$main == "True", ]
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

  # Summarize the results with odds ratios
  results <- tidy(logistic_model, exponentiate = TRUE, conf.int = FALSE)

  # Add columns to indicate the question and task
  results$question <- question
  results$task <- task

  results
}

# ----------------------------------
# 4.1 Main task | Strict dataset
# ----------------------------------
# Run logistic regression for the "main" task

# Run logistic regression for the "main" task
main_results_strict <- lapply(questions, function(q) {
  run_logistic_regression(
    df_strict, q, outcome_var = "choice_binary", task = "main"
  )
})

# Print the combined results
print(main_results_strict)


# ----------------------------------
# 4.2 Main task | Less strict dataset
# ----------------------------------
# Run logistic regression for the "main" task
main_results_less_strict <- lapply(questions, function(q) {
  run_logistic_regression(
    df_less_strict, q, outcome_var = "choice_binary", task = "main"
  )
})

# Print the combined results
print(main_results_less_strict)

# ----------------------------------
# 4.3 Main task | Super strict dataset
# ----------------------------------
# Run logistic regression for the "main" task
main_results_super_strict <- lapply(questions, function(q) {
  run_logistic_regression(
    df_super_strict, q, outcome_var = "choice_binary", task = "main"
  )
})

# Print the combined results
print(main_results_super_strict)

all_results_strict <- do.call(rbind, main_results_strict)
all_results_less_strict <- do.call(rbind, main_results_less_strict)
all_results_super_strict <- do.call(rbind, main_results_super_strict)

# ----------------------------------
# 5. Save the results
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
  df_question <- data[data$main == "True" & data$question == question_name, ]
  logistic_model <- glm(choice_binary ~ frame, data = df_question, family = binomial)
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
    Question = question_name,
    Dataset = dataset_label,
    `Choice (vs Non-neutral)` = "Neutral (loss vs gain)",
    `Estimate (log-OR)` = format_num(estimate),
    `95% CI (log-OR)` = format_ci(ci_lower, ci_upper),
    OR = format_num(odds_ratio),
    `95% CI (OR)` = format_ci(or_ci_lower, or_ci_upper),
    p = format_p(p_value),
    check.names = FALSE
  )
}

all_question_summary <- do.call(
  rbind,
  lapply(questions, function(question_name) {
    rbind(
      create_question_summary(df_strict, "Strict", question_name),
      create_question_summary(df_less_strict, "Less Strict", question_name),
      create_question_summary(df_super_strict, "Super Strict", question_name)
    )
  })
)

all_question_summary <- all_question_summary[order(
  all_question_summary$Question,
  match(all_question_summary$Dataset, c("Strict", "Less Strict", "Super Strict"))
), , drop = FALSE]

write.csv(
  all_question_summary,
  file.path(
    output_path,
    "choice_main_binary_neutral_non-neutral_idv_3_questions.csv"
  ),
  row.names = FALSE
)
