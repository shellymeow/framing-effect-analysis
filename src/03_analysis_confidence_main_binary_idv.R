# -------------------------------------------------------
# This script aims to run an linear regression analysis
# Goal: to analyze the effect of frame, choice, and their iteraction
#       on the confidence ratings
# only non-neutral choices in the ternary choice task
# Reference frame is "gain"
# Reference choice is "sure"
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
folder_path <- file.path(current_dir, "data", "data_processed")

# Define the file names for the CSV files
file_name_1 <- "data_choice_evaluation_strict.csv"
file_name_2 <- "data_choice_evaluation_not_strict.csv"

# Define full file paths for both CSV files
file_path_1 <- file.path(folder_path, file_name_1)
file_path_2 <- file.path(folder_path, file_name_2)

# Load both CSV files into data frames
df_strict <- read.csv(file_path_1)
df_less_strict <- read.csv(file_path_2)

# Filter out the "Unemployment" question from both data frames
df_strict <- df_strict[df_strict$question != "Unemployment", ]
df_less_strict <- df_less_strict[df_less_strict$question != "Unemployment", ]

# Filter out neutral choices (2) from both data frames
df_strict_main <- subset(df_strict, choice != 2 & main == "True")
df_less_strict_main <- subset(df_less_strict, choice != 2 & main == "True")

# ---------------------------
# 3. Preprocess data
# ---------------------------
# Transform the "confidence_rating" column to an ordered factor
# Transform the "frame" and "choice" columns to factors
# Set the reference level for "frame" and "choice"

# STRICT DATASET
# --------------------
# Factorize independent variables
df_strict_main$frame <- factor(
  df_strict_main$frame,
  levels = c("gain", "loss") # gain as reference
)
df_strict_main$choice <- factor(
  df_strict_main$choice,
  levels = c(0, 1), # neutral as reference
)

# LESS STRICT DATASET
# --------------------
# Factorize independent variables
df_less_strict_main$frame <- factor(
  df_less_strict_main$frame,
  levels = c("gain", "loss") # gain as reference
)
df_less_strict_main$choice <- factor(
  df_less_strict_main$choice,
  levels = c(0, 1), # sure as reference
)

# ---------------------------
# 4. Run linear regression analysis
# ---------------------------
# Define the questions to analyze
questions <- df_strict$question %>% unique()

# Define a function to run the analysis for each question
analyze_evaluation_ind <- function(df, question) {
  # Filter data for the specific question
  data <- df[df$question == question, ] # nolint
  # Run the model
  model <- lm(
    evaluation ~ frame * choice,
    data = data,
  )

  # Print model summary
  cat("Model Summary:\n")
  print(summary(model))
  cat("\n")

  # Estimated marginal means
  emm <- emmeans(model, ~ frame * choice, type = "response")

  # Print estimated marginal means
  cat("Estimated Marginal Means (on probability scale):\n")
  print(emm)
  cat("\n")

  # Pairwise comparisons across frame within each task (e.g., gain vs loss)
  cat("Contrast: Frame effect within each choice:\n")
  print(contrast(emm, method = "pairwise", by = "choice", adjust = "none"))
  cat("\n")

  # Pairwise comparisons across task within each frame
  cat("Contrast: Choice effect within each frame:\n")
  print(contrast(emm, method = "pairwise", by = "frame", adjust = "none"))
  cat("\n")
}

# ---------------------------
# save the summary output to a text file
# Strict dataset
# ---------------------------
# Define the output file path
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lm_binary_main_strict_ind_question.txt"
)

# -------------------------------
# Ternary task: Analyze each question separately
# -------------------------------
# Start redirecting output to the file
sink(output_file_path)

# Loop over each question and write results to the file
for (question in questions) {
  cat("====================================\n")
  cat("Question:", question, "\n")
  cat("Strict dataset:\n")
  analyze_evaluation_ind(df_strict_main, question)
  cat("\n\n")
}

# Stop redirecting output
sink()



# Less strict dataset
# ---------------------------
# Define the output file path
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lm_binary_main_less_strict_ind_question.txt"
)

# -------------------------------
# Ternary task: Analyze each question separately
# -------------------------------
# Start redirecting output to the file
sink(output_file_path)

# Loop over each question and write results to the file
for (question in questions) {
  cat("====================================\n")
  cat("Question:", question, "\n")
  cat("Strict dataset:\n")
  analyze_evaluation_ind(df_less_strict_main, question)
  cat("\n\n")
}

# Stop redirecting output
sink()