# This script aims to compare the choice data by individual question between:
# non-neutral participants (from main task) and
# neutral participants (from follow-up task)
# -------------------------------------------------------
# Clear the environment
rm(list = objects())

# ---------------------------
# 1. Load necessary packages
# ---------------------------
library(emmeans)

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

# ---------------------------
# 3. Preprocess data
# ---------------------------
# Exclude the control question from all datasets
df_strict <- df_strict[df_strict$question != "Unemployment", ]
df_less_strict <- df_less_strict[df_less_strict$question != "Unemployment", ]
df_super_strict <- df_super_strict[df_super_strict$question != "Unemployment", ]

# Compare main-task non-neutral choices with follow-up choices
df_strict$task <- ifelse(df_strict$main == "True", "main", "follow")
df_less_strict$task <- ifelse(df_less_strict$main == "True", "main", "follow")
df_super_strict$task <- ifelse(df_super_strict$main == "True", "main", "follow")

# Keep only sure/risky choices. This removes neutral main-task choices and keeps
# the binary follow-up task on the same 0/1 choice scale.
df_strict_no_neutral <- df_strict[df_strict$choice != 2, ]
df_less_strict_no_neutral <- df_less_strict[df_less_strict$choice != 2, ]
df_super_strict_no_neutral <- df_super_strict[df_super_strict$choice != 2, ]

# ---------------------------
# Extract unique questions from the strict dataset
questions <- unique(df_strict_no_neutral$question)
# ---------------------------
# Factorize the regressors
# Set "gain" as reference
df_strict_no_neutral$frame <- factor(
  df_strict_no_neutral$frame,
  levels = c("gain", "loss")
)
df_less_strict_no_neutral$frame <- factor(
  df_less_strict_no_neutral$frame,
  levels = c("gain", "loss")
)
df_super_strict_no_neutral$frame <- factor(
  df_super_strict_no_neutral$frame,
  levels = c("gain", "loss")
)
# Set "main" as reference
df_strict_no_neutral$task <- factor(
  df_strict_no_neutral$task,
  levels = c("main", "follow")
)
df_less_strict_no_neutral$task <- factor(
  df_less_strict_no_neutral$task,
  levels = c("main", "follow")
)
df_super_strict_no_neutral$task <- factor(
  df_super_strict_no_neutral$task,
  levels = c("main", "follow")
)
# Set "sure" as reference
df_strict_no_neutral$choice <- factor(
  df_strict_no_neutral$choice,
  levels = c(0, 1),
)
df_less_strict_no_neutral$choice <- factor(
  df_less_strict_no_neutral$choice,
  levels = c(0, 1),
)
df_super_strict_no_neutral$choice <- factor(
  df_super_strict_no_neutral$choice,
  levels = c(0, 1),
)
# ----------------------------
# 4. Individual questions
# Function to run logistic regression individually for each question
# and report coefficients with 95% profile-likelihood CIs
run_logistic_regression_ind <- function(df, question) {
  # Filter the data for the specific question
  data <- df[df$question == question, ]

  # Run logistic regression
  model <- glm(
    choice ~ frame * task,
    data = data,
    family = binomial
  )

  # Print model summary
  cat("Model Summary:\n")
  print(summary(model))
  cat("\n")

  # ---- 95% Profile-likelihood CIs for coefficients (log-odds) ----
  # Use stats::confint for profile-likelihood CIs
  coefs <- coef(summary(model))
  est   <- coefs[, "Estimate"]
  se    <- coefs[, "Std. Error"]
  zval  <- coefs[, "z value"]
  pval  <- coefs[, "Pr(>|z|)"]

  # Profile CIs (default 95%); restrict to coefficient names for alignment
  cis_prof <- stats::confint(model, parm = names(est))

  coef_tab <- cbind(
    Estimate     = est,
    "Std. Error" = se,
    "z value"    = zval,
    "Pr(>|z|)"   = pval,
    CI_lower     = cis_prof[, 1],
    CI_upper     = cis_prof[, 2]
  )

  cat("Coefficients with 95% Profile-Likelihood CIs (log-odds):\n")
  print(coef_tab)
  cat("\n")

  # ---- Odds ratios and their 95% CIs (exponentiated log-odds) ----
  or_tab <- cbind(
    Odds_Ratio  = exp(est),
    OR_CI_lower = exp(cis_prof[, 1]),
    OR_CI_upper = exp(cis_prof[, 2])
  )

  cat("Odds Ratios with 95% Profile-Likelihood CIs:\n")
  print(or_tab)
  cat("\n")

  # Estimated marginal means on probability scale
  emm <- emmeans(model, ~ frame * task, type = "response")

  cat("Estimated Marginal Means (on probability scale):\n")
  print(emm)
  cat("\n")

  # Pairwise comparisons across frame within each task (gain vs loss)
  cat("Contrast: Frame effect within each task:\n")
  print(contrast(emm, method = "pairwise", by = "task", adjust = "none"))
  cat("\n")

  # Pairwise comparisons across task within each frame
  cat("Contrast: Task effect within each frame:\n")
  print(contrast(emm, method = "pairwise", by = "frame", adjust = "none"))
  cat("\n")
}



# ---------------------------
# save the summary output to a text file
# Strict dataset
# Define the output file path
output_file_path <- file.path(
  output_path,
  "results_choice_lmm_binary_main_vs_follow_strict_individual_question.txt"
)

# Start redirecting output to the file
sink(output_file_path)

# Loop over each question and write results to the file
for (question in questions) {
  cat("====================================\n")
  cat("Question:", question, "\n")
  cat("Strict dataset:\n")
  run_logistic_regression_ind(df_strict_no_neutral, question)
  cat("\n\n")
}

# Stop redirecting output
sink()


# Less strict dataset
# Define the output file path
output_file_path <- file.path(
  output_path,
  "results_choice_lmm_binary_main_vs_follow_less_strict_individual_question.txt"
)

# Start redirecting output to the file
sink(output_file_path)

# Loop over each question and write results to the file
for (question in questions) {
  cat("====================================\n")
  cat("Question:", question, "\n")
  cat("Less strict dataset:\n")
  run_logistic_regression_ind(df_less_strict_no_neutral, question)
  cat("\n\n")
}

# Stop redirecting output
sink()

#  Super strict dataset
# Define the output file path
output_file_path <- file.path(
  output_path,
  "results_choice_lmm_binary_main_vs_follow_super_strict_individual_question.txt"
)
# Start redirecting output to the file
sink(output_file_path)
# Loop over each question and write results to the file
for (question in questions) {
  cat("====================================\n")
  cat("Question:", question, "\n")
  cat("Super strict dataset:\n")
  run_logistic_regression_ind(df_super_strict_no_neutral, question)
  cat("\n\n")
}

# Stop redirecting output
sink()

# ---------------------------
# 5. Formatted summary table for each question
# ---------------------------
# Each question-specific CSV reports the frame effect, task effect, and their
# interaction across all three data-inclusion thresholds.
format_num <- function(x) {
  sprintf("%.2f", x)
}

format_ci <- function(lower, upper) {
  paste0("[", format_num(lower), ", ", format_num(upper), "]")
}

format_p <- function(p) {
  ifelse(p < 0.001, "<.001", ifelse(p < 0.01, sprintf("%.3f", p), format_num(p)))
}

build_question_term_summary <- function(df, question, dataset_label) {
  data <- df[df$question == question, ]
  model <- glm(choice ~ frame * task, data = data, family = binomial)

  coefs <- coef(summary(model))
  terms <- c("frameloss", "taskfollow", "frameloss:taskfollow")
  estimate <- coefs[terms, "Estimate"]
  std_error <- coefs[terms, "Std. Error"]
  p_value <- coefs[terms, "Pr(>|z|)"]

  ci_lower <- estimate - 1.96 * std_error
  ci_upper <- estimate + 1.96 * std_error
  odds_ratio <- exp(estimate)
  or_ci_lower <- exp(ci_lower)
  or_ci_upper <- exp(ci_upper)

  data.frame(
    Dataset = dataset_label,
    Term = terms,
    `Choice (vs Sure)` = "Risky",
    `Estimate (log-OR)` = format_num(estimate),
    `95% CI (log-OR)` = format_ci(ci_lower, ci_upper),
    OR = format_num(odds_ratio),
    `95% CI (OR)` = format_ci(or_ci_lower, or_ci_upper),
    p = format_p(p_value),
    check.names = FALSE
  )
}

for (question in questions) {
  question_summary <- rbind(
    build_question_term_summary(df_strict_no_neutral, question, "Strict"),
    build_question_term_summary(
      df_less_strict_no_neutral,
      question,
      "Less Strict"
    ),
    build_question_term_summary(
      df_super_strict_no_neutral,
      question,
      "Super Strict"
    )
  )

  write.csv(
    question_summary,
    file.path(
      output_path,
      paste0(
        "results_choice_lmm_binary_main_vs_follow_summary_",
        question,
        ".csv"
      )
    ),
    row.names = FALSE
  )
}
