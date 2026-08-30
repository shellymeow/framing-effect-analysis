# This script aims to compare the evaluation data between:
# non-neutral participants (from main task) and
# neutral participants (from follow-up task)
# -------------------------------------------------------
# Clear the environment
rm(list = objects())

# ---------------------------
# 1. Install and load necessary packages
# ---------------------------
library(lme4)
library(lmerTest)
library(emmeans)
library(magrittr)  # For the pipe operator %>%
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
# Filter out the "Unemployment" question from both data frames
df_strict <- df_strict[df_strict$question != "Unemployment", ]
df_less_strict <- df_less_strict[df_less_strict$question != "Unemployment", ]
df_super_strict <- df_super_strict[df_super_strict$question != "Unemployment", ]

# Add a new column to indicate the task type
df_strict$task <- ifelse(df_strict$main == "True", "main", "follow")
df_less_strict$task <- ifelse(df_less_strict$main == "True", "main", "follow")
df_super_strict$task <- ifelse(df_super_strict$main == "True", "main", "follow")

# Filter out rows with neutral choices in the main task
df_strict_no_neutral <- df_strict[df_strict$choice != 2, ]
df_less_strict_no_neutral <- df_less_strict[df_less_strict$choice != 2, ]
df_super_strict_no_neutral <- df_super_strict[df_super_strict$choice != 2, ]

# ---------------------------
# Extract unique questions from the strict dataset
questions <- df_strict_no_neutral$question %>% unique()
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
# ---------------------------
# 5. Confidence evaluation
# ---------------------------
# Strict dataset
lmm_evalaution_strict <- lmer(
  evaluation ~ frame * choice * task + (1 | participant_id) + (1 | question),
  data = df_strict_no_neutral
)

# Get model summary and save to file
output_file_path <- file.path(
  output_path,
  "results_evaluation_lmm_main_vs_follow_strict.txt"
)
sink(output_file_path)

# Print full model summary
cat("MODEL SUMMARY:\n\n")
print(summary(lmm_evalaution_strict))

# Print confidence intervals using Wald method
cat("\n\n95% CONFIDENCE INTERVALS (WALD METHOD):\n\n")
conf_int <- confint(lmm_evalaution_strict, method = "Wald")
print(conf_int)

sink()

# Save as emmeans format
output_file_path <- file.path(
  output_path,
  "results_evaluation_lmm_main_vs_follow_strict_emmeans.txt"
)
# Open a text file connection
sink(output_file_path)

# --- TASK effect ---
cat("===== TASK effect (task | choice * frame) =====\n\n")
emm_full_task <- emmeans(lmm_evalaution_strict, ~ task | choice * frame)
task_contrast <- contrast(emm_full_task, method = "pairwise")
print(task_contrast)

# --- CHOICE effect ---
cat("\n\n===== CHOICE effect (choice | task * frame) =====\n\n")
emm_full_choice <- emmeans(lmm_evalaution_strict, ~ choice | task * frame)
choice_contrast <- contrast(emm_full_choice, method = "pairwise")
print(choice_contrast)

# --- FRAME effect ---
cat("\n\n===== FRAME effect (frame | task * choice) =====\n\n")
emm_full_frame <- emmeans(lmm_evalaution_strict, ~ frame | task * choice)
frame_contrast <- contrast(emm_full_frame, method = "pairwise")
print(frame_contrast)

# Close the connection
sink()

# ---------------------------
# Less strict dataset
lmm_evalaution_less_strict <- lmer(
  evaluation ~ frame * choice * task + (1 | participant_id) + (1 | question),
  data = df_less_strict_no_neutral
)

# Get model summary and save to file
output_file_path <- file.path(
  output_path,
  "results_evaluation_lmm_main_vs_follow_less_strict.txt"
)
sink(output_file_path)

# Print full model summary
cat("MODEL SUMMARY:\n\n")
print(summary(lmm_evalaution_less_strict))

# Print confidence intervals using Wald method
cat("\n\n95% CONFIDENCE INTERVALS (WALD METHOD):\n\n")
conf_int <- confint(lmm_evalaution_less_strict, method = "Wald")
print(conf_int)

sink()


# Save as emmeans format
output_file_path <- file.path(
  output_path,
  "results_evaluation_lmm_main_vs_follow_less_strict_emmeans.txt"
)
# Open a text file connection
sink(output_file_path)

# --- TASK effect ---
cat("===== TASK effect (task | choice * frame) =====\n\n")
emm_full_task <- emmeans(lmm_evalaution_less_strict, ~ task | choice * frame)
task_contrast <- contrast(emm_full_task, method = "pairwise")
print(task_contrast)

# --- CHOICE effect ---
cat("\n\n===== CHOICE effect (choice | task * frame) =====\n\n")
emm_full_choice <- emmeans(lmm_evalaution_less_strict, ~ choice | task * frame)
choice_contrast <- contrast(emm_full_choice, method = "pairwise")
print(choice_contrast)

# --- FRAME effect ---
cat("\n\n===== FRAME effect (frame | task * choice) =====\n\n")
emm_full_frame <- emmeans(lmm_evalaution_less_strict, ~ frame | task * choice)
frame_contrast <- contrast(emm_full_frame, method = "pairwise")
print(frame_contrast)

# Close the connection
sink()

# ----------------------------
# 5.2 Individual questions
# helper: round only numeric columns in a data.frame/tibble
.round_numeric <- function(df, digits = 3) {
  is_num <- vapply(df, is.numeric, logical(1))
  df[is_num] <- lapply(df[is_num], round, digits = digits)
  df
}

run_lm_ind <- function(df, question) {
  # Filter the data for the specific question and task
  data <- df[df$question == question, ]

  # Run linear regression
  model <- lm(
    evaluation ~ frame * choice * task,
    data = data
  )

  # Extract coefficients
  coefs <- coef(summary(model))
  est   <- round(coefs[, "Estimate"],   3)
  se    <- round(coefs[, "Std. Error"], 3)
  tval  <- round(coefs[, "t value"],    3)
  pval  <- round(coefs[, "Pr(>|t|)"],   3)

  # 95% CIs (using rounded est & se for display consistency)
  ci_lower <- round(est - qt(0.975, df = model$df.residual) * se, 3)
  ci_upper <- round(est + qt(0.975, df = model$df.residual) * se, 3)

  # Residual df (same for all coefficients)
  df_resid <- df.residual(model)

  # Coefficient table
  coef_table <- cbind(
    Estimate    = est,
    "Std. Error"= se,
    "t value"   = tval,
    "Pr(>|t|)"  = pval,
    CI_lower    = ci_lower,
    CI_upper    = ci_upper,
    df_resid    = df_resid
  )

  cat("Coefficients with 95% Confidence Intervals:\n")
  print(coef_table)
  cat("\n")

  # --- TASK effect ---
  cat("===== TASK effect (task | choice * frame) =====\n\n")
  emm_full_task <- emmeans(model, ~ task | choice * frame)
  task_contrast <- summary(contrast(emm_full_task, method = "pairwise"))
  print(.round_numeric(as.data.frame(task_contrast), 3))
  cat("\n\n")

  # --- CHOICE effect ---
  cat("===== CHOICE effect (choice | task * frame) =====\n\n")
  emm_full_choice <- emmeans(model, ~ choice | task * frame)
  choice_contrast <- summary(contrast(emm_full_choice, method = "pairwise"))
  print(.round_numeric(as.data.frame(choice_contrast), 3))
  cat("\n\n")

  # --- FRAME effect ---
  cat("===== FRAME effect (frame | task * choice) =====\n\n")
  emm_full_frame <- emmeans(model, ~ frame | task * choice)
  frame_contrast <- summary(contrast(emm_full_frame, method = "pairwise"))
  print(.round_numeric(as.data.frame(frame_contrast), 3))
  cat("\n\n")
}



# ---------------------------
# save the summary output to a text file

# ---------------------------
# Strict dataset
# Define the output file path
output_file_path <- file.path(
  output_path,
  "results_evaluation_lm_binary_main_vs_follow_strict_ind_question.txt"
)

# Start redirecting output to the file
sink(output_file_path)

# Loop over each question and write results to the file
for (question in questions) {
  cat("====================================\n")
  cat("Question:", question, "\n")
  cat("Strict dataset:\n")
  run_lm_ind(df_strict_no_neutral, question)
  cat("\n\n")
}

# Stop redirecting output
sink()


# ---------------------------
# Less strict dataset
# Define the output file path
output_file_path <- file.path(
  output_path,
  "results_evaluation_lm_binary_main_vs_follow_less_strict_ind_question.txt"
)

# Start redirecting output to the file
sink(output_file_path)

# Loop over each question and write results to the file
for (question in questions) {
  cat("====================================\n")
  cat("Question:", question, "\n")
  cat("Strict dataset:\n")
  run_lm_ind(df_less_strict_no_neutral, question)
  cat("\n\n")
}

# Stop redirecting output
sink()
