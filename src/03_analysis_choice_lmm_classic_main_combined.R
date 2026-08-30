# This script aims to test classic framing effect in the main task,
# the effect of frame on binary choices:
# 1. Sure (0)
# 2. risky (1)

# Models: Two linear mixed models
# 1. question as a fixed effect
# 2. question as a random effect
# The script performs the following steps:
# 1. Load necessary packages
# 2. Load data from CSV files
# 3. Preprocess the data to set reference levels for factors
# 4. Strict dataset:
#    - Run models
# 5. Less strict dataset:
#    - Run models
# -------------------------------------------------------
rm(list = objects())
# ---------------------------
# Load required packages
# ---------------------------
library(lme4)
library(dplyr)

# ---------------------------
# Define function to run GLMM and save results
# ---------------------------
run_glmm_and_save <- function(formula, data, file_prefix, output_path) {
  # Step 1: Fit the model
  model <- glmer(formula, data = data, family = binomial)

  # Step 2: Extract coefficient table
  coef_table <- coef(summary(model))

  # Step 3: Get confidence intervals using Wald method
  # Using try() in case the calculation fails
  conf_int <- try(confint(model, method = "Wald"), silent = TRUE)

  # Step 4: Create results dataframe
  results_df <- as.data.frame(coef_table)
  results_df$OR <- exp(results_df$Estimate)  # Calculate odds ratios

  # Handle confidence intervals based on success/failure
  if (!inherits(conf_int, "try-error")) {
    # Optional improvement for ensuring matching order
    matching_rows <- match(rownames(coef_table), rownames(conf_int))
    conf_int <- conf_int[matching_rows[!is.na(matching_rows)], , drop = FALSE]

    # Check if conf_int still has the right dimensions after filtering
    if (is.matrix(conf_int) && ncol(conf_int) >= 2) {
      # Add raw confidence intervals for coefficients
      results_df$Estimate_CI_lower <- conf_int[, 1]
      results_df$Estimate_CI_upper <- conf_int[, 2]
      
      # Calculate confidence intervals for odds ratios
      results_df$OR_CI_lower <- exp(conf_int[, 1])
      results_df$OR_CI_upper <- exp(conf_int[, 2])
    } else {
      # If filtered conf_int doesn't have expected structure
      warning("Confidence interval structure is not as expected after filtering")
      results_df$Estimate_CI_lower <- NA
      results_df$Estimate_CI_upper <- NA
      results_df$OR_CI_lower <- NA
      results_df$OR_CI_upper <- NA
    }
  } else {
    # If confidence interval calculation failed, set all CIs to NA
    results_df$Estimate_CI_lower <- NA
    results_df$Estimate_CI_upper <- NA
    results_df$OR_CI_lower <- NA
    results_df$OR_CI_upper <- NA
  }
  
  # Create a formatted CI column for easier reading
  results_df$Estimate_CI <- with(results_df, 
                                paste0("[", round(Estimate_CI_lower, 3), ", ", 
                                      round(Estimate_CI_upper, 3), "]"))
  results_df$OR_CI <- with(results_df, 
                          paste0("[", round(OR_CI_lower, 3), ", ", 
                                round(OR_CI_upper, 3), "]"))
  
  # Step 5: Save results to file
  file_path <- file.path(output_path, paste0(file_prefix, ".txt"))
  sink(file_path)
  cat("=== Model Summary ===\n")
  print(summary(model))
  cat("\n=== Coefficients with 95% Confidence Intervals ===\n")
  
  # Create a nicer formatted output table
  output_table <- data.frame(
    Term = rownames(results_df),
    Estimate = round(results_df$Estimate, 3),
    CI_95 = results_df$Estimate_CI,
    Std_Error = round(results_df$`Std. Error`, 3),
    z_value = round(results_df$`z value`, 3),
    p_value = round(results_df$`Pr(>|z|)`, 4),
    Odds_Ratio = round(results_df$OR, 3),
    OR_CI_95 = results_df$OR_CI
  )
  
  print(output_table)
  sink()

  # Step 6: Return the model
  model
}

# ---------------------------
# 1. Clear environment and load data
# ---------------------------

current_dir <- getwd()
folder_path <- file.path(current_dir, "data")
output_path <- file.path(current_dir, "results")

file_path_1 <- file.path(folder_path, "data_choice_evaluation_strict.csv")
file_path_2 <- file.path(folder_path, "data_choice_evaluation_not_strict.csv")
file_path_3 <- file.path(folder_path, "data_choice_evaluation_super_strict.csv")
df_strict <- read.csv(file_path_1)
df_less_strict <- read.csv(file_path_2)
df_super_strict <- read.csv(file_path_3)

# ---------------------------
# 2. Preprocess data function
# ---------------------------
prepare_data <- function(df) {
  df <- df[df$question != "Unemployment" & df$choice != 2 & df$main == "True", ]
  df$frame <- factor(df$frame, levels = c("gain", "loss"))
  df$choice <- factor(df$choice, levels = c(0, 1)) 
  df$question <- factor(df$question, levels = c("Disease", "Painting", "Virus"))
  df
}

df_strict_cls <- prepare_data(df_strict)
df_less_strict_cls <- prepare_data(df_less_strict)
df_super_strict_cls <- prepare_data(df_super_strict)

# ---------------------------
# 3. Run models and save output
# ---------------------------

# STRICT
run_glmm_and_save(
  formula = choice ~ frame * question + (1 | participant_id),
  data = df_strict_cls,
  file_prefix = "results_choice_glmm_qfix_MainClassic_BinaryChoice_StrictData",
  output_path = output_path
)

run_glmm_and_save(
  formula = choice ~ frame + (1 | question) + (1 | participant_id),
  data = df_strict_cls,
  file_prefix = "results_choice_glmm_qrandom_MainClassic_BinaryChoice_StrictData", 
  output_path = output_path
)

# LESS STRICT
run_glmm_and_save(
  formula = choice ~ frame * question + (1 | participant_id),
  data = df_less_strict_cls,
  file_prefix = "results_choice_glmm_qfix_MainClassic_BinaryChoice_LessStrictData", 
  output_path = output_path
)

run_glmm_and_save(
  formula = choice ~ frame + (1 | question) + (1 | participant_id),
  data = df_less_strict_cls,
  file_prefix = "results_choice_glmm_qrandom_MainClassic_BinaryChoice_LessStrictData", 
  output_path = output_path
)

# SUPER STRICT
run_glmm_and_save(
  formula = choice ~ frame * question + (1 | participant_id),
  data = df_super_strict_cls,
  file_prefix = "results_choice_glmm_qfix_MainClassic_BinaryChoice_SuperStrictData",
  output_path = output_path
)

run_glmm_and_save(
  formula = choice ~ frame + (1 | question) + (1 | participant_id),
  data = df_super_strict_cls,
  file_prefix = "results_choice_glmm_qrandom_MainClassic_BinaryChoice_SuperStrictData",
  output_path = output_path
)
