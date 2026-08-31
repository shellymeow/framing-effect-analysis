# This script tests the effect of frame on trinary choices:
# 1. Sure (0)
# 2. Risky (1)
# 3. Neutral (2)
# The analysis uses multinomial logistic regression and is structured to be
# reproducible across the three inclusion criteria datasets.
#
# The script performs the following steps:
# 1. Load necessary packages
# 2. Load the three CSV datasets
# 3. Preprocess data and set reference levels for factors
# 4. Loop over all datasets and fit the multinomial model by question
# 5. Aggregate the question-level summaries across all datasets into one CSV
# 6. Save the final table ordered by Question, then Dataset
# -------------------------------------------------------
# Clear the environment
rm(list = objects())

# ---------------------------
# 1. Install and load necessary packages
# ---------------------------
library(nnet)

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
# 3. Preprocess data and fit models across all datasets
# ---------------------------
# Filter out the "Unemployment" question and set the reference levels so the
# same preparation pipeline is used for all three datasets.
dataset_labels <- c("Strict", "Less Strict", "Super Strict")
dataset_list <- list(
  "Strict" = df_strict,
  "Less Strict" = df_less_strict,
  "Super Strict" = df_super_strict
)

for (dataset_name in dataset_labels) {
  df <- dataset_list[[dataset_name]]
  df <- df[df$question != "Unemployment", , drop = FALSE]
  df$frame <- factor(df$frame, levels = c("gain", "loss"))
  df$question <- factor(df$question, levels = c("Disease", "Painting", "Virus"))
  df$choice <- factor(df$choice, levels = c(0, 1, 2))
  df_main <- df[df$main == "True", , drop = FALSE]
  dataset_list[[dataset_name]] <- df_main
}

# Select main-task data for each dataset after preprocessing
# This keeps a consistent structure for subsequent analysis.
df_strict_main <- dataset_list[["Strict"]]
df_less_strict_main <- dataset_list[["Less Strict"]]
df_super_strict_main <- dataset_list[["Super Strict"]]

# ---------------------------
# 4. Run multinomial logistic regression across all datasets
# ---------------------------
for (dataset_name in dataset_labels) {
  df_main <- dataset_list[[dataset_name]]
  questions <- levels(df_main$question)
  z_crit <- 1.96

  # Run across all questions
  multinom_all <- multinom(choice ~ frame, data = df_main)
  s <- summary(multinom_all)

  coefs <- s$coefficients
  ses <- s$standard.errors
  z_vals <- coefs / ses
  p_vals <- 2 * (1 - pnorm(abs(z_vals)))
  odds_ratios <- exp(coefs)
  ci_lower <- coefs - z_crit * ses
  ci_upper <- coefs + z_crit * ses
  ci_lower_or <- exp(ci_lower)
  ci_upper_or <- exp(ci_upper)

  coef_table_all <- data.frame(
    Estimate = as.vector(coefs),
    CI_Lower = as.vector(ci_lower),
    CI_Upper = as.vector(ci_upper),
    Std_Error = as.vector(ses),
    z_value = as.vector(z_vals),
    p_value = as.vector(p_vals),
    Odds_Ratio = as.vector(odds_ratios),
    OR_CI_Lower = as.vector(ci_lower_or),
    OR_CI_Upper = as.vector(ci_upper_or)
  )

  rn <- expand.grid(
    Level = paste0(seq_len(nrow(coefs))),
    Variable = colnames(coefs)
  )
  rownames(coef_table_all) <- paste(rn$Level, rn$Variable, sep = ":")

  print(coef_table_all)

  # Run the model separately for each question
  results_list <- list()
  for (q in questions) {
    df_question <- df_main[df_main$question == q, , drop = FALSE]
    multinom_model <- multinom(choice ~ frame, data = df_question)
    s_q <- summary(multinom_model)

    coefs_q <- s_q$coefficients
    ses_q <- s_q$standard.errors
    z_vals_q <- coefs_q / ses_q
    p_vals_q <- 2 * (1 - pnorm(abs(z_vals_q)))
    odds_ratios_q <- exp(coefs_q)
    ci_lower_q <- coefs_q - z_crit * ses_q
    ci_upper_q <- coefs_q + z_crit * ses_q
    ci_lower_or_q <- exp(ci_lower_q)
    ci_upper_or_q <- exp(ci_upper_q)

    coef_table_question <- data.frame(
      Estimate = as.vector(coefs_q),
      CI_Lower = as.vector(ci_lower_q),
      CI_Upper = as.vector(ci_upper_q),
      Std_Error = as.vector(ses_q),
      z_value = as.vector(z_vals_q),
      p_value = as.vector(p_vals_q),
      Odds_Ratio = as.vector(odds_ratios_q),
      OR_CI_Lower = as.vector(ci_lower_or_q),
      OR_CI_Upper = as.vector(ci_upper_or_q)
    )

    rn_q <- expand.grid(
      Level = paste0(seq_len(nrow(coefs_q))),
      Variable = colnames(coefs_q)
    )
    rownames(coef_table_question) <- paste(rn_q$Level, rn_q$Variable, sep = ":")

    results_list[[q]] <- coef_table_question
    cat("\nResults for question:", q, "in", dataset_name, "\n")
    print(coef_table_question)
  }
}

# -----------------------------
# 7. Reproducible final summary across datasets and questions
# -----------------------------
format_num <- function(x) {
  sprintf("%.2f", x)
}

format_ci <- function(lower, upper) {
  paste0("[", format_num(lower), ", ", format_num(upper), "]")
}

format_p <- function(p) {
  ifelse(p < 0.001, "<.001", ifelse(p < 0.01, sprintf("%.3f", p), format_num(p)))
}

create_question_multinom_summary <- function(df, dataset_label, question_name) {
  df_question <- df[df$question == question_name, ]
  multinom_model <- multinom(choice ~ frame, data = df_question, trace = FALSE)
  model_summary <- summary(multinom_model)

  coefs <- model_summary$coefficients
  ses <- model_summary$standard.errors
  frame_term <- "frameloss"

  est <- coefs[, frame_term]
  se <- ses[, frame_term]
  z_vals <- est / se
  p_vals <- 2 * (1 - pnorm(abs(z_vals)))

  z_crit <- 1.96
  ci_lower <- est - z_crit * se
  ci_upper <- est + z_crit * se
  odds_ratios <- exp(est)
  ci_lower_or <- exp(ci_lower)
  ci_upper_or <- exp(ci_upper)

  choice_labels <- c(
    "1" = "Risky (loss vs gain)",
    "2" = "Neutral (loss vs gain)"
  )

  data.frame(
    Question = question_name,
    Dataset = dataset_label,
    `Choice (vs Sure)` = unname(choice_labels[rownames(coefs)]),
    `Estimate (log-OR)` = format_num(est),
    `95% CI (log-OR)` = format_ci(ci_lower, ci_upper),
    OR = format_num(odds_ratios),
    `95% CI (OR)` = format_ci(ci_lower_or, ci_upper_or),
    p = format_p(p_vals),
    check.names = FALSE
  )
}

questions_to_summarize <- c("Disease", "Painting", "Virus")
dataset_list <- list(
  "Strict" = df_strict_main,
  "Less Strict" = df_less_strict_main,
  "Super Strict" = df_super_strict_main
)
dataset_order <- c("Strict", "Less Strict", "Super Strict")

all_question_summary <- do.call(
  rbind,
  lapply(questions_to_summarize, function(question_name) {
    do.call(
      rbind,
      lapply(dataset_order, function(dataset_label) {
        create_question_multinom_summary(
          dataset_list[[dataset_label]],
          dataset_label,
          question_name
        )
      })
    )
  })
)

all_question_summary <- all_question_summary[order(
  all_question_summary$Question,
  match(all_question_summary$Dataset, dataset_order)
), , drop = FALSE]

write.csv(
  all_question_summary,
  file.path(
    output_path,
    "choice_main_trinary_multinom_idv_3_questions.csv"
  ),
  row.names = FALSE
)
