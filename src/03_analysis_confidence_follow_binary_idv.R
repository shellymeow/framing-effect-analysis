# -------------------------------------------------------
# This script aims to run linear regression models
# Goal: Analyze the effect of frame, choice, and their iteraction
#       on the confidence ratings for INDIVIDUAL framing question

# Participant: People with neutral options
# Task: follow-up
# Reference frame: "gain"
# -------------------------------------------------------

# Clear the environment
rm(list = objects())

# ---------------------------
# 1. Install and load necessary packages
# ---------------------------
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

# Filter out the "Unemployment" question from both data frames
df_strict <- df_strict[df_strict$question != "Unemployment", ]
df_less_strict <- df_less_strict[df_less_strict$question != "Unemployment", ]
df_super_strict <- df_super_strict[df_super_strict$question != "Unemployment", ]

questions <- df_strict$question %>% unique()

# ---------------------------
# 3. Preprocess data
# ---------------------------
# Transform the "confidence_rating" column to an ordered factor
# Transform the "frame" and "choice" columns to factors
# Set the reference level for "frame" and "choice"

# STRICT DATASET
# --------------------
# Factorize independent variables
df_strict$frame <- factor(
  df_strict$frame,
  levels = c("gain", "loss") # gain as reference
)
df_strict$question <- factor(
  df_strict$question,
  levels = c("Disease", "Painting", "Virus")  # Disease as reference
)
df_strict$choice <- factor(
  df_strict$choice,
  levels = c(0, 1, 2), # neutral as reference
)

df_strict_follow <- df_strict[df_strict$"follow_up" == "True", ]

# LESS STRICT DATASET
# --------------------
# Factorize independent variables
df_less_strict$frame <- factor(
  df_less_strict$frame,
  levels = c("gain", "loss") # gain as reference
)
df_less_strict$question <- factor(
  df_less_strict$question,
  levels = c("Disease", "Painting", "Virus")  # Disease as reference
)
df_less_strict$choice <- factor(
  df_less_strict$choice,
  levels = c(0, 1, 2), # neutral as reference
)
df_less_strict_follow <- df_less_strict[df_less_strict$"follow_up" == "True", ]

# SUPER STRICT DATASET
# --------------------
# Factorize independent variables
df_super_strict$frame <- factor(
  df_super_strict$frame,
  levels = c("gain", "loss") # gain as reference
)
df_super_strict$question <- factor(
  df_super_strict$question,
  levels = c("Disease", "Painting", "Virus")  # Disease as reference
)
df_super_strict$choice <- factor(
  df_super_strict$choice,
  levels = c(0, 1, 2), # neutral as reference
)
df_super_strict_follow <- df_super_strict[
  df_super_strict$"follow_up" == "True",
]
# ---------------------------
# 4. Run linear mixed models
# ---------------------------
# --------------------
# Function
# --------------------
run_lm_ind <- function(df, question) {
  # Filter the data for the specific question
  data <- df[df$question == question, ]

  # Fit the linear model (task removed)
  model <- lm(
    evaluation ~ frame * choice,
    data = data
  )

  # Print model summary with df information
  cat("Model Summary:\n")
  model_summary <- summary(model)
  print(model_summary)
  cat("\nResidual degrees of freedom:", model_summary$df[2], "\n")
  
  # Print 95% confidence intervals for coefficients
  cat("95% Confidence Intervals for Coefficients:\n")
  conf_intervals <- confint(model, level = 0.95)
  print(conf_intervals)
  cat("\n")

  # --- CHOICE effect ---
  cat("===== CHOICE effect (choice | frame) =====\n\n")
  emm_choice <- emmeans(model, ~ choice | frame)
  choice_contrast <- contrast(emm_choice, method = "pairwise")
  print(choice_contrast)
  cat("\n\n")

  # --- FRAME effect ---
  cat("===== FRAME effect (frame | choice) =====\n\n")
  emm_frame <- emmeans(model, ~ frame | choice)
  frame_contrast <- contrast(emm_frame, method = "pairwise")
  print(frame_contrast)
  cat("\n\n")
}


# ---------------------------
# 1. Strict dataset
# Define the output file path
output_file_path <- file.path(
  output_path,
  "confidence_follow_strict_idv_lm.txt"
)

# Start redirecting output to the file
sink(output_file_path)

# Loop over each question and write results to the file
for (question in questions) {
  cat("====================================\n")
  cat("Question:", question, "\n")
  cat("Strict dataset:\n")
  run_lm_ind(df_strict_follow, question)
  cat("\n\n")
}

# Stop redirecting output
sink()


# ---------------------------
# 2. Less strict dataset
# Define the output file path
output_file_path <- file.path(
  output_path,
  "confidence_follow_less_strict_idv_lm.txt"
)

# Start redirecting output to the file
sink(output_file_path)

# Loop over each question and write results to the file
for (question in questions) {
  cat("====================================\n")
  cat("Question:", question, "\n")
  cat("Less strict dataset:\n")
  run_lm_ind(df_less_strict_follow, question)
  cat("\n\n")
}

# Stop redirecting output
sink()

# ---------------------------
# 3. Super strict dataset
# Define the output file path
output_file_path <- file.path(
  output_path,
  "confidence_follow_super_strict_idv_lm.txt"
)
# Start redirecting output to the file
sink(output_file_path)
# Loop over each question and write results to the file
for (question in questions) {
  cat("====================================\n")
  cat("Question:", question, "\n")
  cat("Super strict dataset:\n")
  run_lm_ind(df_super_strict_follow, question)
  cat("\n\n")
}
# Stop redirecting output
sink()

# ---------------------------
# 5. Question-specific coefficient comparison across datasets
# ---------------------------
# Extract coefficient estimates, 95% CIs, and p-values from a fitted model
# into a tidy data frame tagged with the dataset it came from.
classify_significance <- function(p_value) {
  cut(
    p_value,
    breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
    labels = c("<0.001", "<0.01", "<0.05", "ns"),
    right = FALSE
  ) %>% as.character()
}

extract_coef_table <- function(model, dataset_label) {
  model_summary <- summary(model)
  coefs <- model_summary$coefficients
  ci <- confint(model, level = 0.95)
  p_value <- coefs[, "Pr(>|t|)"]

  data.frame(
    dataset = dataset_label,
    term = rownames(coefs),
    estimate = round(coefs[, "Estimate"], 2),
    ci_lower = round(ci[, 1], 2),
    ci_upper = round(ci[, 2], 2),
    p_value = round(p_value, 2),
    significance = classify_significance(p_value),
    row.names = NULL
  )
}

fit_question_model <- function(df, question_name) {
  lm(
    evaluation ~ frame * choice,
    data = df[df$question == question_name, ]
  )
}

build_question_comparison <- function(question_name) {
  model_strict <- fit_question_model(df_strict_follow, question_name)
  model_less_strict <- fit_question_model(df_less_strict_follow, question_name)
  model_super_strict <- fit_question_model(df_super_strict_follow, question_name)

  rbind(
    extract_coef_table(model_strict, "Strict"),
    extract_coef_table(model_less_strict, "Less strict"),
    extract_coef_table(model_super_strict, "Super strict")
  )
}

for (question_name in questions) {
  question_comparison <- build_question_comparison(question_name)

  write.csv(
    question_comparison,
    file.path(
      output_path,
      paste0(
        "confidence_follow_",
        tolower(question_name),
        "_sum_3_datasets_lm.csv"
      )
    ),
    row.names = FALSE
  )
}
