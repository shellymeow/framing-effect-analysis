# -------------------------------------------------------
# This script aims to run two linear mixed models on choice data
# focusing on global frame effect, accounting for scenario variability
# 2 Models as described below:
# 1. Model 1: question as a random effect
# 2. Model 2: question as a fixed effect

# The analysis is performed on two datasets:
# 1. Strict dataset: "data_choice_evaluation_strict.csv"
# 2. Less strict dataset: "data_choice_evaluation_not_strict.csv"
# 3. Super strict dataset: "data_choice_evaluation_super_strict.csv"

# The analysis is performed on two tasks:
# 1. The main task: "non-neutral" (reference) vs "neutral"
# 2. The follow-up task: "sure" (reference) vs "risky"
# The reference frame is "gain"
# The reference question is "Disease"
# The script includes the following steps:
# 1. Load necessary packages
# 2. Load data
# 3. Preprocess data
# 4. Define the model
# 5. Run mixed models
# 6. Save model summaries to text files
# -------------------------------------------------------
# -------------------------------------------------------

# Clear the environment
rm(list = objects())

# ---------------------------
# 1. Install and load necessary packages
# ---------------------------
library(lme4)

# ---------------------------
# 2. Load data
# ---------------------------
# Define the current working directory
current_dir <- getwd()  # Get the current working directory
print(current_dir)

# Define paths for data loading
folder_path <- file.path(current_dir, "data")
output_path <- file.path(current_dir, "results")

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

# Filter out the "Unemployment" question from both data frames
df_strict <- df_strict[df_strict$question != "Unemployment", ]
df_less_strict <- df_less_strict[df_less_strict$question != "Unemployment", ]
df_super_strict <- df_super_strict[df_super_strict$question != "Unemployment", ]

# ---------------------------
# 3. Preprocess data
# ---------------------------
# Add a new column "choice_binary" to the data frame
# Factorize fixed effects
# ps: non-neutral=0, neutral=1

#  STRICT DATASET
# --------------------
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
# Group sure and risky choices
df_strict$choice_binary <- ifelse(
  # Set "choice_binary" to 0 if "choice" is in (0, 1), otherwise to 1
  df_strict$choice %in% c(0, 1), 0, 1
)

# Main Task
# Non-neutral as reference
df_strict$choice_binary <- factor(
  df_strict$choice_binary,
  levels = c(0, 1), # reference level is 0 (non-neutral, 1st argument)
)

# Follow-up Task
# Sure as reference
df_strict$choice <- factor(
  df_strict$choice,
  levels = c(0, 1, 2), # reference level is 0 (1st argument, sure)
)

#  less STRICT DATASET
# --------------------
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
# Group sure and risky choices
df_less_strict$choice_binary <- ifelse(
  # Set "choice_binary" to 0 if "choice" is in (0, 1), otherwise to 1
  df_less_strict$choice %in% c(0, 1), 0, 1
)

# Main Task
# Non-neutral as reference
df_less_strict$choice_binary <- factor(
  df_less_strict$choice_binary,
  levels = c(0, 1), # reference level is 0 (non-neutral, 1st argument)
)

# Follow-up Task
# Sure as reference
df_less_strict$choice <- factor(
  df_less_strict$choice,
  levels = c(0, 1, 2), # reference level is 0 (1st argument, sure)
)

#  Super STRICT DATASET
# --------------------
# Set "gain" as reference
df_super_strict$frame <- factor(
  df_super_strict$frame,
  levels = c("gain", "loss")
)
# Set "Disease" as reference
df_super_strict$question <- factor(
  df_super_strict$question,
  levels = c("Disease", "Painting", "Virus") # set "Disease" as reference
)
# Group sure and risky choices
df_super_strict$choice_binary <- ifelse(
  # Set "choice_binary" to 0 if "choice" is in (0, 1), otherwise to 1
  df_super_strict$choice %in% c(0, 1), 0, 1
)

# Main Task
# Non-neutral as reference
df_super_strict$choice_binary <- factor(
  df_super_strict$choice_binary,
  levels = c(0, 1), # reference level is 0 (non-neutral, 1st argument)
)

# Follow-up Task
# Sure as reference
df_super_strict$choice <- factor(
  df_super_strict$choice,
  levels = c(0, 1, 2), # reference level is 0 (1st argument, sure)
)

# ---------------------------
# 4. Define the model
# ---------------------------
# MODEL 1: question as a random effect
run_mixed_model_qrandom <- function(df, task) {
  # Validate task
  if (!task %in% c("main", "follow_up")) {
    stop("Invalid task specified. Use 'main' or 'follow_up'.")
  }

  # Filter data based on task
  data_in <- subset(df, get(task) == "True")

  # Choose dependent variable
  dep_var <- if (task == "main") "choice_binary" else "choice"

  # Fit the model
  model <- glmer(
    reformulate("frame + (1 | question) + (1 | participant_id)", response = dep_var), # nolint
    data = data_in,
    family = binomial
  )

  # Extract coefficients
  coefs <- as.data.frame(coef(summary(model)))

  # Calculate odds ratios and CIs
  coefs$OR <- exp(coefs$Estimate)
  ci <- confint(
    model,
    parm = "beta_", # "beta_" restricts to fixed effects
    method = "Wald",
  )  # "beta_" restricts to fixed effects

  # Combine into final results table
  results <- cbind(
    Term = rownames(coefs),
    Estimate = coefs$Estimate,
    Estimate_CI_lower = ci[, 1],        # Raw coefficient CI lower bound
    Estimate_CI_upper = ci[, 2],        # Raw coefficient CI upper bound
    Estimate_CI = paste0("[", round(ci[, 1], 3), ", ", round(ci[, 2], 3), "]"), # Formatted CI
    Std_Error = coefs$`Std. Error`,
    z_value = coefs$`z value`,
    p_value = coefs$`Pr(>|z|)`,
    OR = coefs$OR,
    OR_CI_lower = exp(ci[, 1]),         # OR CI lower bound
    OR_CI_upper = exp(ci[, 2]),         # OR CI upper bound
    OR_CI = paste0("[", round(exp(ci[, 1]), 3), ", ", round(exp(ci[, 2]), 3), "]") # Formatted OR CI
  )

  rownames(results) <- NULL  # Clean row names

  # Return results implicitly
  list(
    model = model,
    results_table = results
  )
}
# ---------------------------
# MODEL 2: question as a fixed effect
run_mixed_model_qfix <- function(df, task) {
  # Validate task
  if (!task %in% c("main", "follow_up")) {
    stop("Invalid task specified. Use 'main' or 'follow_up'.")
  }

  # Filter data based on task
  data_in <- subset(df, get(task) == "True")

  # Choose dependent variable
  dep_var <- if (task == "main") "choice_binary" else "choice"

  # Fit the model
  model <- glmer(
    reformulate("frame * question + (1 | participant_id)", response = dep_var), # nolint
    data = data_in,
    family = binomial
  )

  # Extract coefficients
  coefs <- as.data.frame(coef(summary(model)))

  # Calculate odds ratios and CIs
  coefs$OR <- exp(coefs$Estimate)
  ci <- confint(
    model,
    parm = "beta_", # "beta_" restricts to fixed effects
    method = "Wald",
  )  # "beta_" restricts to fixed effects

  # Combine into final results table
  results <- cbind(
    Term = rownames(coefs),
    Estimate = coefs$Estimate,
    Estimate_CI_lower = ci[, 1],        # Raw coefficient CI lower bound
    Estimate_CI_upper = ci[, 2],        # Raw coefficient CI upper bound
    Estimate_CI = paste0("[", round(ci[, 1], 3), ", ", round(ci[, 2], 3), "]"), # Formatted CI
    Std_Error = coefs$`Std. Error`,
    z_value = coefs$`z value`,
    p_value = coefs$`Pr(>|z|)`,
    OR = coefs$OR,
    OR_CI_lower = exp(ci[, 1]),         # OR CI lower bound
    OR_CI_upper = exp(ci[, 2]),         # OR CI upper bound
    OR_CI = paste0("[", round(exp(ci[, 1]), 3), ", ", round(exp(ci[, 2]), 3), "]") # Formatted OR CI
  )

  rownames(results) <- NULL  # Clean row names

  # Return results implicitly
  list(
    model = model,
    results_table = results
  )
}
# ---------------------------
# 4. Run mixed models
# ---------------------------
# Main Task: "non-neutral" (reference) vs "neutral"
# Follow-up Task: "sure" (reference) vs "risky"
# Reference frame is "gain"
# --------------------
# 1. Strict dataset
# --------------------
# a. Main Task
# --------------------

# --------------------
# Run the Model 1
# --------------------
results <- run_mixed_model_qrandom(df_strict, task = "main")
# Print the summary of the model
print(results)

# Save model summary to a txt file
output_file_path <- file.path(
  output_path,
  "results_choice_MixedModel_qrandom_MainTask_BinaryChoice_StrictData.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)
# Print the summary to the file
print(results)
# Close the sink
sink()

# --------------------
# Run the Model 2
# --------------------
results <- run_mixed_model_qfix(df_strict, task = "main")
# Print the summary of the model
print(results)

# Save model summary to a txt file
output_file_path <- file.path(
  output_path,
  "results_choice_MixedModel_qfix_MainTask_BinaryChoice_StrictData.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)
# Print the summary to the file
print(results)
# Close the sink
sink()

# ---------------------
# b. Follow Task
# ---------------------
# Run the Model 1
results <- run_mixed_model_qrandom(df_strict, task = "follow_up")
# Print the summary of the model
print(results)

# Save model summary to a txt file
output_file_path <- file.path(
  output_path,
  "results_choice_MixedModel_qrandom_FollowTask_BinaryChoice_StrictData.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)
# Print the summary to the file
print(results)
# Close the sink
sink()

# ---------------------
# Run the Model 2
# --------------------
results <- run_mixed_model_qfix(df_strict, task = "follow_up")
# Print the summary of the model
print(results)

# Save model summary to a txt file
output_file_path <- file.path(
  output_path,
  "results_choice_MixedModel_qfix_FollowTask_BinaryChoice_StrictData.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)
# Print the summary to the file
print(results)
# Close the sink
sink()
# --------------------
# 2. Less strict dataset
# --------------------
# a. Main Task
# --------------------
# Run the model 1
# --------------------
results <- run_mixed_model_qrandom(df_less_strict, task = "main")
# Print the summary of the model
print(results)

# Save model summary to a txt file
output_file_path <- file.path(
  output_path,
  "results_choice_MixedModel_qrandom_MainTask_BinaryChoice_LessStrictData.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)
# Print the summary to the file
print(results)
# Close the sink
sink()

# --------------------
# Run the model 2
# --------------------
results <- run_mixed_model_qfix(df_less_strict, task = "main")
# Print the summary of the model
print(results)

# Save model summary to a txt file
output_file_path <- file.path(
  output_path,
  "results_choice_MixedModel_qfix_MainTask_BinaryChoice_LessStrictData.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)
# Print the summary to the file
print(results)
# Close the sink
sink()

# ---------------------
# b. Follow Task
# ---------------------

# --------------------
# Run the Model 1
# --------------------
results <- run_mixed_model_qrandom(df_less_strict, task = "follow_up")
# Print the summary of the model
print(results)

# Save model summary to a txt file
output_file_path <- file.path(
  output_path,
  "results_choice_MixedModel_qrandom_FollowTask_BinaryChoice_LessStrictData.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)
# Print the summary to the file
print(results)
# Close the sink
sink()

# ---------------------
# Run the Model 2
# --------------------
results <- run_mixed_model_qfix(df_less_strict, task = "follow_up")
# Print the summary of the model
print(results)

# Save model summary to a txt file
output_file_path <- file.path(
  output_path,
  "results_choice_MixedModel_qfix_FollowTask_BinaryChoice_LessStrictData.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)
# Print the summary to the file
print(results)
# Close the sink
sink()


# ---------------------------
# 5. Model comparison
# ---------------------------
# Strict dataset
df_strict_main <- df_strict[df_strict$main == "True", ]
m1 <- glmer(
  choice_binary ~ frame + question + (1 | participant_id),
  data = df_strict_main,
  family = binomial,
)
m2 <- glmer(
  choice_binary ~ frame * question + (1 | participant_id),
  data = df_strict_main,
  family = binomial,
)
anova(m1, m2)
# Visualize the model
library(emmeans)
emm <- emmeans(m2, ~ frame * question)
plot(emm)

# ---------------------------
# Less strict dataset
df_less_strict_main <- df_less_strict[df_less_strict$main == "True", ]
m11 <- glmer(
  choice_binary ~ frame + question + (1 | participant_id),
  data = df_less_strict_main,
  family = binomial,
)
m22 <- glmer(
  choice_binary ~ frame * question + (1 | participant_id),
  data = df_less_strict_main,
  family = binomial,
)
anova(m11, m22)
# Visualize the model
library(emmeans)
emm <- emmeans(m22, ~ frame * question)
plot(emm)



# --------------------
# 3. Super strict dataset
# --------------------
# a. Main Task
# --------------------
# Run the model 1
# --------------------
results <- run_mixed_model_qrandom(df_super_strict, task = "main")
# Print the summary of the model
print(results)

# Save model summary to a txt file
output_file_path <- file.path(
  output_path,
  "results_choice_MixedModel_qrandom_MainTask_BinaryChoice_SuperStrictData.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)
# Print the summary to the file
print(results)
# Close the sink
sink()

# --------------------
# b. Follow-up Task
# --------------------
# Run the model 1
# --------------------
results <- run_mixed_model_qrandom(df_super_strict, task = "follow_up")
# Print the summary of the model
print(results)

# Save model summary to a txt file
output_file_path <- file.path(
  output_path,
  "results_choice_MixedModel_qrandom_FollowTask_BinaryChoice_SuperStrictData.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)
# Print the summary to the file
print(results)
# Close the sink
sink()

# --------------------
# Run the model 2
# --------------------
results <- run_mixed_model_qfix(df_super_strict, task = "follow_up")
# Print the summary of the model
print(results)

# Save model summary to a txt file
output_file_path <- file.path(
  output_path,
  "results_choice_MixedModel_qfix_FollowTask_BinaryChoice_SuperStrictData.txt"
)
# Open sink to write console output to a .txt file
sink(output_file_path)
# Print the summary to the file
print(results)
# Close the sink
sink()
