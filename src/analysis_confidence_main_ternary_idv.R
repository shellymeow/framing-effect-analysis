# -------------------------------------------------------
# This script aims to run linear regression models
# Goal: Analyze the effect of frame, choice, and their iteraction
#       on the confidence ratings for INDIVIDUAL framing question
# Task:
# 1. The main task: "sure" vs "risky" vs "neutral"
# Reference frame is "neutral"
# -------------------------------------------------------

# Clear the environment
rm(list = objects())

# ---------------------------
# 1. Install and load necessary packages
# ---------------------------
library(lmerTest)
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
  levels = c(2, 0, 1), # neutral as reference
)

df_strict_main <- df_strict[df_strict$"main" == "True", ]

# LESS STRICT DATASET
# --------------------
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
  levels = c(2, 0, 1), # neutral as reference
)
df_less_strict_main <- df_less_strict[df_less_strict$"main" == "True", ]

# SUPER STRICT DATASET
# --------------------
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
  levels = c(2, 0, 1), # neutral as reference
)
df_super_strict_main <- df_super_strict[df_super_strict$"main" == "True", ]

# ---------------------------
# 4. Question-specific coefficient comparison across datasets
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
  model_strict <- fit_question_model(df_strict_main, question_name)
  model_less_strict <- fit_question_model(df_less_strict_main, question_name)
  model_super_strict <- fit_question_model(df_super_strict_main, question_name)

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
        "confidence_main_",
        tolower(question_name),
        "_sum_3_datasets_lm.csv"
      )
    ),
    row.names = FALSE
  )
}
