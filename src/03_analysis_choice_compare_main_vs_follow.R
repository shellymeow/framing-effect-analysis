# This script aims to compare the choice data between:
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

# Stop redirecting output
sink()

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
