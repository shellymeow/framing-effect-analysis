# This script aims to test the effect of frame on binary choices
# made in the FOLLOW-UP task (participants who picked "neutral" in the
# main task, then forced into a binary choice on the same scenario):
# 1. Sure (0)
# 2. Risky (1)
# Using two models
# Model 1: Chi-square test
# Model 2: Binary logistic regression
# The script performs the following steps:
# 1. Load necessary packages
# 2. Load data from CSV files
# 3. Preprocess the data to set reference levels for factors
# 4. Strict dataset:
#    - Run a chi-square test
#    - Run a binary logistic regression
# 5. Less strict dataset:
#    - Run a chi-square test
#    - Run a binary logistic regression
# 6. Super strict dataset:
#    - Run a chi-square test
#    - Run a binary logistic regression
# -------------------------------------------------------
# Clear the environment
rm(list = objects())

# ---------------------------
# 1. Install and load necessary packages
# ---------------------------
library(epitools)

# ---------------------------
# 2. Load data
# ---------------------------
# Define the current working directory
current_dir <- getwd()  # Get the current working directory
print(current_dir)

# Define paths for data loading
folder_path <- file.path(current_dir, "data")

# Define the file names for the CSV files
file_name_1 <- "data_choice_evaluation_strict.csv"
file_name_2 <- "data_choice_evaluation_not_strict.csv"
file_name_3 <- "data_choice_evaluation_super_strict.csv"

# Define full file paths for both CSV files
file_path_1 <- file.path(folder_path, file_name_1)
file_path_2 <- file.path(folder_path, file_name_2)
file_path_3 <- file.path(folder_path, file_name_3)

# Load both CSV files into data frames
df_strict <- read.csv(file_path_1)
df_less_strict <- read.csv(file_path_2)
df_super_strict <- read.csv(file_path_3)

# ---------------------------
# 3. Preprocess data
# ---------------------------
# Factorize fixed effects
# -------------------
# Filter out the "Unemployment" question from both data frames
df_strict <- df_strict[df_strict$question != "Unemployment", ]
df_less_strict <- df_less_strict[df_less_strict$question != "Unemployment", ]
df_super_strict <- df_super_strict[df_super_strict$question != "Unemployment", ]

# --------------------
#  STRICT DATASET
# -------------------
# Set "gain" as reference
df_strict$frame <- factor(
  df_strict$frame,
  levels = c("gain", "loss")
)
# Set "Disease" as reference
df_strict$question <- factor(
  df_strict$question,
  levels = c("Disease", "Painting", "Virus") # set "Disease" as reference
)

# Sure as reference; follow-up choice is binary (sure/risky only)
df_strict$choice <- factor(
  df_strict$choice,
  levels = c(0, 1), # reference level is "sure" with value 0
)

# --------------------
#  LESS STRICT DATASET
# -------------------
# Set "gain" as reference
df_less_strict$frame <- factor(
  df_less_strict$frame,
  levels = c("gain", "loss")
)
# Set "Disease" as reference
df_less_strict$question <- factor(
  df_less_strict$question,
  levels = c("Disease", "Painting", "Virus") # set "Disease" as reference
)
# Sure as reference; follow-up choice is binary (sure/risky only)
df_less_strict$choice <- factor(
  df_less_strict$choice,
  levels = c(0, 1), # reference level is "sure" with value 0
)

# -------------------
# SUPER STRICT DATASET
# -------------------
# Preprocess super strict dataset
df_super_strict$frame <- factor(
  df_super_strict$frame,
  levels = c("gain", "loss")
)
df_super_strict$question <- factor(
  df_super_strict$question,
  levels = c("Disease", "Painting", "Virus") # set "Disease" as reference
)
# Sure as reference; follow-up choice is binary (sure/risky only)
df_super_strict$choice <- factor(
  df_super_strict$choice,
  levels = c(0, 1), # reference level is "sure" with value 0
)


# Select follow-up task data (participants who picked "neutral" in the
# main task, then forced into a binary sure/risky choice)
df_strict_followup <- df_strict[df_strict$follow_up == "True", ]
df_less_strict_followup <- df_less_strict[df_less_strict$follow_up == "True", ]
df_super_strict_followup <- df_super_strict[df_super_strict$follow_up == "True", ]

# ---------------------------
# 4. STRICT DATASET
# ---------------------------
# ---------------------------
# 4.1 Run a chi-square test
# ---------------------------
# Overall chi-square test for three quetions

# 1. Create a contingency table
table_frame_choice_strict <- table(
  df_strict_followup$frame,
  df_strict_followup$choice
)

# 2. Run chi-square test and calculate odds ratios
chisq_test_result_strict <- chisq.test(table_frame_choice_strict)


# 3. Save the results to a text file
output_file_path <- file.path(
  folder_path,
  "results_choice_binary_followup_chi_square_test_strict.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)

# Print the summary to the file
cat("\nContingency Table:\n")
print(table_frame_choice_strict)
cat("\nChi-Square Test Results:\n")
print(chisq_test_result_strict)
# Close the sink
sink()

# ---------------------------
# Independent chi-square tests for each question

# Set output file path
output_file_path <- file.path(
  folder_path,
  "results_choice_binary_followup_chi_square_test_by_question_strict.txt"
)

# Open sink to write output to file
sink(output_file_path)

# Get unique questions
questions <- unique(df_strict_followup$question)

# Loop over each question
for (q in questions) {
  cat("===== Question:", q, "=====\n")

  # Subset data for the current question
  df_q <- subset(df_strict_followup, question == q)

  # Create contingency table
  table_q <- table(df_q$frame, df_q$choice)
  print(table_q)

  # Check if the table is valid for chi-square test (no zero rows/cols)
  if (all(dim(table_q) >= 2)) {
    # Run chi-square test
    chi_result <- chisq.test(table_q)
    print(chi_result)
  } else {
    cat("Chi-square test not applicable: table too sparse or not 2D.\n")
  }

  cat("\n\n")
}

# Close sink
sink()


# ---------------------------
# 4.2 Run a binary logistic regression
# ---------------------------
# 4.2.1 Run across all questions
glm_strict <- glm(choice ~ frame, data = df_strict_followup, family = binomial)

# Extract summary
s <- summary(glm_strict)

# Coefficients, standard errors, z- and p-values (glm computes these directly)
coefs_mat <- coef(s)
est <- coefs_mat[, "Estimate"]
ses <- coefs_mat[, "Std. Error"]
z_vals <- coefs_mat[, "z value"]
p_vals <- coefs_mat[, "Pr(>|z|)"]

# Compute odds ratios
odds_ratios <- exp(est)

# Compute 95% confidence intervals
z_crit <- 1.96
ci_lower <- est - z_crit * ses
ci_upper <- est + z_crit * ses

# Compute CI for odds ratios
ci_lower_or <- exp(ci_lower)
ci_upper_or <- exp(ci_upper)

# Build coefficient table
coef_table_strict <- data.frame(
  Estimate = est,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper,
  Std_Error = ses,
  z_value = z_vals,
  p_value = p_vals,
  Odds_Ratio = odds_ratios,
  OR_CI_Lower = ci_lower_or,
  OR_CI_Upper = ci_upper_or
)

# View final table
print(coef_table_strict)

# Save the coefficients table to a text file
output_file_path_coef <- file.path(
  folder_path,
  "results_choice_binary_followup_logistic_coef_strict.txt"
)
sink(output_file_path_coef)
print(coef_table_strict)
sink()

# -----------------------------
# 4.2.2 Run models separately for each question
# Create a list of unique questions
questions <- unique(df_strict_followup$question)
# Initialize an empty list to store results
results_list <- list()
# Loop through each question and run the logistic regression
for (q in questions) {
  # Filter the data for the current question
  df_question <- df_strict_followup[df_strict_followup$question == q, ]

  # Run the logistic regression
  glm_model <- glm(choice ~ frame, data = df_question, family = binomial)

  # Extract summary
  s <- summary(glm_model)

  # Coefficients, standard errors, z- and p-values
  coefs_mat <- coef(s)
  est <- coefs_mat[, "Estimate"]
  ses <- coefs_mat[, "Std. Error"]
  z_vals <- coefs_mat[, "z value"]
  p_vals <- coefs_mat[, "Pr(>|z|)"]

  # Compute odds ratios
  odds_ratios <- exp(est)

  # Compute 95% confidence intervals
  z_crit <- 1.96
  ci_lower <- est - z_crit * ses
  ci_upper <- est + z_crit * ses

  # Compute CI for odds ratios
  ci_lower_or <- exp(ci_lower)
  ci_upper_or <- exp(ci_upper)

  # Build coefficient table
  coef_table_question <- data.frame(
    Estimate = est,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    Std_Error = ses,
    z_value = z_vals,
    p_value = p_vals,
    Odds_Ratio = odds_ratios,
    OR_CI_Lower = ci_lower_or,
    OR_CI_Upper = ci_upper_or
  )

  # Store the results in the list with question name as key
  results_list[[q]] <- coef_table_question

}

# Print the results for each question
for (q in names(results_list)) {
  cat("\nResults for question:", q, "\n")
  print(results_list[[q]])
}

# Save the results for each question to a text file
for (q in names(results_list)) {
  output_file_path_coef_q <- file.path(
    folder_path,
    paste0("results_choice_binary_followup_logistic_coef_strict_", q, ".txt")
  )
  sink(output_file_path_coef_q)
  print(results_list[[q]])
  sink()
}

# -----------------------------
# 5. LESS STRICT DATASET
# -----------------------------
# 5.1 Run a chi-square test
table_frame_choice_less_strict <- table(
  df_less_strict_followup$frame,
  df_less_strict_followup$choice
)
chisq_test_result_less_strict <- chisq.test(
  table_frame_choice_less_strict
)

# Save the results
output_file_path <- file.path(
  folder_path,
  "results_choice_binary_followup_chi_square_test_less_strict.txt"
)
sink(output_file_path)
print(chisq_test_result_less_strict)
sink()

# ---------------------------
# Independent chi-square tests for each question

# Set output file path
output_file_path <- file.path(
  folder_path,
  "results_choice_binary_followup_chi_square_test_by_question_less_strict.txt"
)

# Open sink to write output to file
sink(output_file_path)

# Get unique questions
questions <- unique(df_less_strict_followup$question)

# Loop over each question
for (q in questions) {
  cat("===== Question:", q, "=====\n")

  # Subset data for the current question
  df_q <- subset(df_less_strict_followup, question == q)

  # Create contingency table
  table_q <- table(df_q$frame, df_q$choice)
  print(table_q)

  # Check if the table is valid for chi-square test (no zero rows/cols)
  if (all(dim(table_q) >= 2)) {
    # Run chi-square test
    chi_result <- chisq.test(table_q)
    print(chi_result)
  } else {
    cat("Chi-square test not applicable: table too sparse or not 2D.\n")
  }

  cat("\n\n")
}

# Close sink
sink()

# ---------------------------
# 5.2 Run a binary logistic regression
# ---------------------------
# 5.2.1 Run across all questions
glm_less_strict <- glm(
  choice ~ frame,
  data = df_less_strict_followup,
  family = binomial
)

# Extract summary
s <- summary(glm_less_strict)

# Coefficients, standard errors, z- and p-values
coefs_mat <- coef(s)
est <- coefs_mat[, "Estimate"]
ses <- coefs_mat[, "Std. Error"]
z_vals <- coefs_mat[, "z value"]
p_vals <- coefs_mat[, "Pr(>|z|)"]

# Compute odds ratios
odds_ratios <- exp(est)

# Compute 95% confidence intervals
z_crit <- 1.96
ci_lower <- est - z_crit * ses
ci_upper <- est + z_crit * ses

# Compute CI for odds ratios
ci_lower_or <- exp(ci_lower)
ci_upper_or <- exp(ci_upper)

# Build coefficient table
coef_table_less_strict <- data.frame(
  Estimate = est,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper,
  Std_Error = ses,
  z_value = z_vals,
  p_value = p_vals,
  Odds_Ratio = odds_ratios,
  OR_CI_Lower = ci_lower_or,
  OR_CI_Upper = ci_upper_or
)

# View the final table
print(coef_table_less_strict)

# Save the coefficients table to a text file
output_file_path_coef <- file.path(
  folder_path,
  "results_choice_binary_followup_logistic_coef_less_strict.txt"
)
sink(output_file_path_coef)
print(coef_table_less_strict)
sink()


# -----------------------------
# 5.2.2 Run models separately for each question

# Initialize an empty list to store results
results_list <- list()
# Loop through each question and run the logistic regression
for (q in questions) {
  # Filter the data for the current question
  df_question <- df_less_strict_followup[df_less_strict_followup$question == q, ]

  # Run the logistic regression
  glm_model <- glm(choice ~ frame, data = df_question, family = binomial)

  # Extract summary
  s <- summary(glm_model)

  # Coefficients, standard errors, z- and p-values
  coefs_mat <- coef(s)
  est <- coefs_mat[, "Estimate"]
  ses <- coefs_mat[, "Std. Error"]
  z_vals <- coefs_mat[, "z value"]
  p_vals <- coefs_mat[, "Pr(>|z|)"]

  # Compute odds ratios
  odds_ratios <- exp(est)

  # Compute 95% confidence intervals
  z_crit <- 1.96
  ci_lower <- est - z_crit * ses
  ci_upper <- est + z_crit * ses

  # Compute CI for odds ratios
  ci_lower_or <- exp(ci_lower)
  ci_upper_or <- exp(ci_upper)

  # Build coefficient table
  coef_table_question <- data.frame(
    Estimate = est,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    Std_Error = ses,
    z_value = z_vals,
    p_value = p_vals,
    Odds_Ratio = odds_ratios,
    OR_CI_Lower = ci_lower_or,
    OR_CI_Upper = ci_upper_or
  )

  # Store the results in the list with question name as key
  results_list[[q]] <- coef_table_question

}

# Print the results for each question
for (q in names(results_list)) {
  cat("\nResults for question:", q, "\n")
  print(results_list[[q]])
}

# Save the results for each question to a text file
for (q in names(results_list)) {
  output_file_path_coef_q <- file.path(
    folder_path,
    paste0(
      "results_choice_binary_followup_logistic_coef_less_strict_", q, ".txt"
    )
  )
  sink(output_file_path_coef_q)
  print(results_list[[q]])
  sink()
}

# -----------------------------
# 6. SUPER STRICT DATASET
# -----------------------------
# 6.1 Run a chi-square test
table_frame_choice_super_strict <- table(
  df_super_strict_followup$frame,
  df_super_strict_followup$choice
)
chisq_test_result_super_strict <- chisq.test(
  table_frame_choice_super_strict
)

# Save the results
output_file_path <- file.path(
  folder_path,
  "results_choice_binary_followup_chi_square_test_super_strict.txt"
)
sink(output_file_path)
print(chisq_test_result_super_strict)
sink()

# ---------------------------
# Independent chi-square tests for each question

# Set output file path
output_file_path <- file.path(
  folder_path,
  "results_choice_binary_followup_chi_square_test_by_question_super_strict.txt"
)

# Open sink to write output to file
sink(output_file_path)

# Get unique questions
questions <- unique(df_super_strict_followup$question)

# Loop over each question
for (q in questions) {
  cat("===== Question:", q, "=====\n")

  # Subset data for the current question
  df_q <- subset(df_super_strict_followup, question == q)

  # Create contingency table
  table_q <- table(df_q$frame, df_q$choice)
  print(table_q)

  # Check if the table is valid for chi-square test (no zero rows/cols)
  if (all(dim(table_q) >= 2)) {
    # Run chi-square test
    chi_result <- chisq.test(table_q)
    print(chi_result)
  } else {
    cat("Chi-square test not applicable: table too sparse or not 2D.\n")
  }

  cat("\n\n")
}

# Close sink
sink()

# ---------------------------
# 6.2 Run a binary logistic regression
# ---------------------------
# 6.2.1 Run across all questions
glm_super_strict <- glm(
  choice ~ frame,
  data = df_super_strict_followup,
  family = binomial
)

# Extract summary
s <- summary(glm_super_strict)

# Coefficients, standard errors, z- and p-values
coefs_mat <- coef(s)
est <- coefs_mat[, "Estimate"]
ses <- coefs_mat[, "Std. Error"]
z_vals <- coefs_mat[, "z value"]
p_vals <- coefs_mat[, "Pr(>|z|)"]

# Compute odds ratios
odds_ratios <- exp(est)

# Compute 95% confidence intervals
z_crit <- 1.96
ci_lower <- est - z_crit * ses
ci_upper <- est + z_crit * ses

# Compute CI for odds ratios
ci_lower_or <- exp(ci_lower)
ci_upper_or <- exp(ci_upper)

# Build coefficient table
coef_table_super_strict <- data.frame(
  Estimate = est,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper,
  Std_Error = ses,
  z_value = z_vals,
  p_value = p_vals,
  Odds_Ratio = odds_ratios,
  OR_CI_Lower = ci_lower_or,
  OR_CI_Upper = ci_upper_or
)

# View the final table
print(coef_table_super_strict)

# Save the coefficients table to a text file
output_file_path_coef <- file.path(
  folder_path,
  "results_choice_binary_followup_logistic_coef_super_strict.txt"
)
sink(output_file_path_coef)
print(coef_table_super_strict)
sink()


# -----------------------------
# 6.2.2 Run models separately for each question

# Initialize an empty list to store results
results_list <- list()
# Loop through each question and run the logistic regression
for (q in questions) {
  # Filter the data for the current question
  df_question <- df_super_strict_followup[df_super_strict_followup$question == q, ]

  # Run the logistic regression
  glm_model <- glm(choice ~ frame, data = df_question, family = binomial)

  # Extract summary
  s <- summary(glm_model)

  # Coefficients, standard errors, z- and p-values
  coefs_mat <- coef(s)
  est <- coefs_mat[, "Estimate"]
  ses <- coefs_mat[, "Std. Error"]
  z_vals <- coefs_mat[, "z value"]
  p_vals <- coefs_mat[, "Pr(>|z|)"]

  # Compute odds ratios
  odds_ratios <- exp(est)

  # Compute 95% confidence intervals
  z_crit <- 1.96
  ci_lower <- est - z_crit * ses
  ci_upper <- est + z_crit * ses

  # Compute CI for odds ratios
  ci_lower_or <- exp(ci_lower)
  ci_upper_or <- exp(ci_upper)

  # Build coefficient table
  coef_table_question <- data.frame(
    Estimate = est,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    Std_Error = ses,
    z_value = z_vals,
    p_value = p_vals,
    Odds_Ratio = odds_ratios,
    OR_CI_Lower = ci_lower_or,
    OR_CI_Upper = ci_upper_or
  )

  # Store the results in the list with question name as key
  results_list[[q]] <- coef_table_question

}

# Print the results for each question
for (q in names(results_list)) {
  cat("\nResults for question:", q, "\n")
  print(results_list[[q]])
}

# Save the results for each question to a text file
for (q in names(results_list)) {
  output_file_path_coef_q <- file.path(
    folder_path,
    paste0(
      "results_choice_binary_followup_logistic_coef_super_strict_", q, ".txt"
    )
  )
  sink(output_file_path_coef_q)
  print(results_list[[q]])
  sink()
}
