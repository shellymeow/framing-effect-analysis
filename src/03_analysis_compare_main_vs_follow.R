# This script aims to compare the choice and evaluation data between:
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
# 4. Choice: Framing effect comparison
# ---------------------------
# 4.1 All questions
# Function
run_glmer_all <- function(data) {
  # Fit model
  fit <- lme4::glmer(
    choice ~ frame * task + (1 | participant_id) + (1 | question),
    data   = data,
    family = binomial
  )

  # Pull pieces from summary
  coefs <- coef(summary(fit))

  # 95% Wald CIs for fixed effects (beta) only
  cis <- confint(fit, method = "Wald", parm = "beta_")

  # Assemble output (same order you used)
  output <- cbind(
    Estimate     = lme4::fixef(fit),
    "CI_lower"   = cis[, 1],
    "CI_upper"   = cis[, 2],
    "Std. Error" = coefs[, "Std. Error"],
    "z value"    = coefs[, "z value"],
    "Pr(>|z|)"   = coefs[, "Pr(>|z|)"]
  )

  output
}

# Run for all datasets
lmm_choice_strict <- run_glmer_all(df_strict_no_neutral)
lmm_choice_less_strict <- run_glmer_all(df_less_strict_no_neutral)
lmm_choice_super_strict <- run_glmer_all(df_super_strict_no_neutral)

# save the summary output to a text file
# Strict dataset
output_file_path <- file.path(
  output_path,
  "results_choice_lmm_binary_main_vs_follow_strict.txt"
)
sink(output_file_path)
print(lmm_choice_strict)
sink()

# Less strict dataset
output_file_path <- file.path(
  output_path,
  "results_choice_lmm_binary_main_vs_follow_less_strict.txt"
)
sink(output_file_path)
print(lmm_choice_less_strict)
sink()

# Super strict dataset
output_file_path <- file.path(
  output_path,
  "results_choice_lmm_binary_main_vs_follow_super_strict.txt"
)
sink(output_file_path)
print(lmm_choice_super_strict)
sink()

# ----------------------------
# 4.2 Individual questions
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
  cat("Strict dataset:\n")
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

# ---------------------------
# 4.3 Summary table: frame/task effects for Disease, across datasets
# ---------------------------
# Log-OR, OR, and their 95% CIs for the frame/task/frame:task terms,
# Disease question only, across all three data-inclusion thresholds.
build_term_summary <- function(df, question, dataset_label) {
  data <- df[df$question == question, ]
  model <- glm(choice ~ frame * task, data = data, family = binomial)

  coefs <- coef(summary(model))
  est   <- coefs[, "Estimate"]
  pval  <- coefs[, "Pr(>|z|)"]
  cis_prof <- stats::confint(model, parm = names(est))

  terms <- setdiff(names(est), "(Intercept)")

  data.frame(
    Dataset = dataset_label,
    Term = terms,
    "Estimate (log-OR)" = round(est[terms], 2),
    "95% CI (log-OR)" = sprintf(
      "[%.2f, %.2f]", cis_prof[terms, 1], cis_prof[terms, 2]
    ),
    OR = round(exp(est[terms]), 2),
    "95% CI (OR)" = sprintf(
      "[%.2f, %.2f]", exp(cis_prof[terms, 1]), exp(cis_prof[terms, 2])
    ),
    p = ifelse(pval[terms] < .001, "<.001", sprintf("%.3f", pval[terms])),
    check.names = FALSE,
    row.names = NULL
  )
}

summary_disease <- rbind(
  build_term_summary(df_strict_no_neutral, "Disease", "Strict"),
  build_term_summary(df_less_strict_no_neutral, "Disease", "Less Strict"),
  build_term_summary(df_super_strict_no_neutral, "Disease", "Super Strict")
)

output_file_path <- file.path(
  output_path,
  "results_choice_lmm_binary_main_vs_follow_summary_Disease.csv"
)
write.csv(summary_disease, output_file_path, row.names = FALSE)

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
