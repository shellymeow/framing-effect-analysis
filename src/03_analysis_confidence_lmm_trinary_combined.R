# -------------------------------------------------------
# This script aims to run an linear mixed effect model
# Goal: to analyze the effect of frame, choice, and their iteraction
#       on the confidence ratings
# Two tasks:
# 1. The main task: "sure" vs "risky" vs "neutral" (reference)
# 2. The follow-up task: "sure" (reference) vs "risky"
# Reference frame is "gain"
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
folder_path <- file.path(current_dir, "data")

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
  levels = c(2, 0, 1), # neutral as reference
)

# ---------------------------
# 4. Run linear mixed models
# ---------------------------
# --------------------
# A. Strict dataset
# --------------------
# 1. Main Task
data_strict_main <- df_strict[df_strict$main == "True", ] # Filter for main task

# ----------------------
# Model 1: Question as a random effect
lmm_strict_main_qr <- lmer(
  evaluation ~ frame * choice + (1 | question) + (1 | participant_id),
  data = data_strict_main,
)
# 1. Print the summary of the model
model_summary <- summary(lmm_strict_main_qr)
print(model_summary)

# Calculate 95% confidence intervals for fixed effects
conf_int <- confint(lmm_strict_main_qr, method = "Wald", level = 0.95)
# Extract only the fixed effects (excluding random effects)
fixed_effects_ci <- conf_int[grep("(Intercept)|frame|choice", rownames(conf_int)),]

# Combine model summary coefficients with confidence intervals
coef_summary <- data.frame(
  Estimate = fixef(lmm_strict_main_qr),
  Std.Error = model_summary$coefficients[,"Std. Error"],
  t.value = model_summary$coefficients[,"t value"],
  p.value = model_summary$coefficients[,"Pr(>|t|)"],
  CI_lower = fixed_effects_ci[,1],
  CI_upper = fixed_effects_ci[,2]
)

# Display the combined summary
print("Fixed Effects with Standard Errors and 95% Confidence Intervals:")
print(coef_summary)
# Save the results as a CSV file
# Define the output file path
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_main_qrandom.txt"
)
# Save the summary of the model to a CSV file
# Create a text file with the full model summary
sink(output_file_path)
print(coef_summary)
sink()
#  ----------------------
# 2. Perform Type III ANOVA to assess overall effects of predictors
anova_results <- anova(lmm_strict_main_qr)

# Save to CSV
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_main_qrandom_anova.csv"
)
write.csv(
  as.data.frame(anova_results),
  file = output_file_path,
  row.names = TRUE
)



# Get estimated marginal means for all combinations of frame and choice
emm <- emmeans(lmm_strict_main_qr, ~ frame * choice)
# View estimated marginal means
print(emm)
# Pairwise comparisons within 'frame' (pairs(): default adjustment is Tukey)
frame_pairs <- pairs(emm, by = "frame") 
# Pairwise comparisons within 'choice'
choice_pairs <- pairs(emm, by = "choice")

# Set output file path
output_file_path_txt <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_main_qrandom_pairwise.txt"
)

# Open file connection
sink(output_file_path_txt)

# Write frame comparisons
cat("===== FRAME COMPARISONS (within each frame) =====\n\n")
print(frame_pairs)

cat("\n\n===== CHOICE COMPARISONS (within each choice) =====\n\n")
print(choice_pairs)

# Close file connection
sink()

# ----------------------
# Model 2: Question as a fixed effect
lmm_strict_main_qf <- lmer(
  evaluation ~ frame * choice + question + (1 | participant_id),
  data = data_strict_main,
)
# Print the summary of the model
summary(lmm_strict_main_qf)

# Save the results as a CSV file
# Define the output file path
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_main_qfix.txt"
)
# Save the summary of the model to a CSV file
# Create a text file with the full model summary
sink(output_file_path)
print(summary(lmm_strict_main_qf))
sink()

#  ----------------------
# 2. Perform Type III ANOVA to assess overall effects of predictors
anova_results <- anova(lmm_strict_main_qf)
print(anova_results)
# Save to CSV
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_main_qfix_anova.csv"
)
write.csv(
  as.data.frame(anova_results),
  file = output_file_path,
  row.names = TRUE
)

# Get estimated marginal means for all combinations of frame and choice
emm <- emmeans(lmm_strict_main_qr, ~ frame * choice)
# View estimated marginal means
print(emm)
# Pairwise comparisons within 'frame'
frame_pairs <- pairs(emm, by = "frame")
# Pairwise comparisons within 'choice'
choice_pairs <- pairs(emm, by = "choice")
# Inspect with 'question'
emm_question <- emmeans(lmm_strict_main_qf, ~ question)

# Convert to data frames
frame_pairs_df <- as.data.frame(frame_pairs)
choice_pairs_df <- as.data.frame(choice_pairs)

print(frame_pairs_df)
print(choice_pairs_df)
summary(emm_question)
pairs(emm_question)
# Save the pairwise comparisons to CSV files
output_file_path_frame <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_main_qfix_frame_pairs.csv"
)
output_file_path_choice <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_main_qfix_choice_pairs.csv"
)
output_file_path_question <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_main_qfix_question_pairs.csv"
)
write.csv(
  frame_pairs_df,
  file = output_file_path_frame,
  row.names = FALSE
)
write.csv(
  choice_pairs_df,
  file = output_file_path_choice,
  row.names = FALSE
)
write.csv(
  as.data.frame(pairs(emm_question)),
  file = output_file_path_question,
  row.names = FALSE
)

# Compare the two models using ANOVA
model_camparison_main <- anova(lmm_strict_main_qf, lmm_strict_main_qr)
# save the results of the comparison
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_main_qfix_vs_qrandom.txt"
)
# Create a text file with the full model comparison summary
sink(output_file_path)
print(model_camparison_main)
sink()
# --------------------
# 2 Follow-up Task
# --------------------
data_strict_follow <- df_strict[df_strict$follow_up == "True", ] # Filter for follow task # nolint
# ----------------------
# Model 1: Question as a random effect
# Fit the model
lmm_strict_follow_qr <- lmer(
  evaluation ~ frame * choice + (1 | question) + (1 | participant_id),
  data = data_strict_follow,
)

# Extract model summary
model_summary <- summary(lmm_strict_follow_qr)
print(model_summary)

# Calculate 95% confidence intervals using Wald method
conf_int <- confint(lmm_strict_follow_qr, method = "Wald", level = 0.95)

# Extract only the fixed effects confidence intervals (excluding random effects)
fixed_effects_ci <- conf_int[grep("(Intercept)|frame|choice", rownames(conf_int)),]

# Create a comprehensive summary dataframe with estimates, SEs, t-values, p-values, and CIs
coef_summary <- data.frame(
  Estimate = fixef(lmm_strict_follow_qr),
  Std.Error = model_summary$coefficients[,"Std. Error"],
  t.value = model_summary$coefficients[,"t value"],
  p.value = model_summary$coefficients[,"Pr(>|t|)"],
  CI_lower = fixed_effects_ci[,1],
  CI_upper = fixed_effects_ci[,2],
  CI_formatted = paste0("[", round(fixed_effects_ci[,1], 3), ", ", round(fixed_effects_ci[,2], 3), "]")
)

# Save the results to a file
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_follow_qrandom.txt"
)

# Create a text file with both the model summary and the confidence intervals
sink(output_file_path)
cat("=== MODEL SUMMARY ===\n\n")
print(model_summary)

cat("\n\n=== FIXED EFFECTS WITH 95% CONFIDENCE INTERVALS ===\n\n")
print(coef_summary)
sink()

# Return the comprehensive summary for further use
# You could assign this to a variable if needed in later code
coef_summary


# Save as emmeans
# Get EMMs for frame × choice combinations
emm <- emmeans(lmm_strict_follow_qr, ~ frame * choice)

# Pairwise comparisons within 'frame' (compare choices within each frame)
frame_pairs <- pairs(emm, by = "frame")

# Pairwise comparisons within 'choice' (compare frames within each choice)
choice_pairs <- pairs(emm, by = "choice")

# Set output file path
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_follow_qrandom_pairs.txt"
)

# Write to .txt file
sink(output_file_path)

cat("===== ESTIMATED MARGINAL MEANS (frame × choice) =====\n\n")
print(emm)

cat("\n\n===== PAIRWISE COMPARISONS BY FRAME =====\n\n")
print(frame_pairs)

cat("\n\n===== PAIRWISE COMPARISONS BY CHOICE =====\n\n")
print(choice_pairs)

# Close the file
sink()


#  ----------------------
# 2. Perform Type III ANOVA to assess overall effects of predictors
anova_results <- anova(lmm_strict_follow_qr)
anova(lmm_strict_follow_qr)
# Save to CSV
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_follow_qrandom_anova.csv"
)
write.csv(
  as.data.frame(anova_results),
  file = output_file_path,
  row.names = TRUE
)

# Get estimated marginal means for all combinations of frame and choice
emm <- emmeans(lmm_strict_follow_qr, ~ frame * choice)
# View estimated marginal means
print(emm)
# Pairwise comparisons within 'frame'
frame_pairs <- pairs(emm, by = "frame")
# Pairwise comparisons within 'choice'
choice_pairs <- pairs(emm, by = "choice")

# Print the pairwise comparisons
print(frame_pairs)
print(choice_pairs)

# Convert to data frames
frame_pairs_df <- as.data.frame(frame_pairs)
choice_pairs_df <- as.data.frame(choice_pairs)

# Save the pairwise comparisons to CSV files
output_file_path_frame <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_follow_qrandom_frame_pairs.csv"
)
output_file_path_choice <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_follow_qrandom_choice_pairs.csv"
)
write.csv(
  frame_pairs_df,
  file = output_file_path_frame,
  row.names = FALSE
)
write.csv(
  choice_pairs_df,
  file = output_file_path_choice,
  row.names = FALSE
)

# ----------------------
# Model 2: Question as a fixed effect
lmm_strict_follow_qf <- lmer(
  evaluation ~ frame * choice + question + (1 | participant_id),
  data = data_strict_follow,
)
# Print the summary of the model
summary(lmm_strict_follow_qf)

# Save the results as a CSV file
# Define the output file path
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_follow_qfix.txt"
)
# Save the summary of the model to a CSV file
# Create a text file with the full model summary
sink(output_file_path)
print(summary(lmm_strict_follow_qf))
sink()

#  ----------------------
# 2. Perform Type III ANOVA to assess overall effects of predictors
anova_results <- anova(lmm_strict_follow_qf)
print(anova_results)
# Save to CSV
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_follow_qfix_anova.csv"
)
write.csv(
  as.data.frame(anova_results),
  file = output_file_path,
  row.names = TRUE
)

# Get estimated marginal means for all combinations of frame and choice
emm <- emmeans(lmm_strict_follow_qr, ~ frame * choice)
# View estimated marginal means
print(emm)
# Pairwise comparisons within 'frame'
frame_pairs <- pairs(emm, by = "frame")
# Pairwise comparisons within 'choice'
choice_pairs <- pairs(emm, by = "choice")
# Inspect with 'question'
emm_question <- emmeans(lmm_strict_follow_qf, ~ question)

# Convert to data frames
frame_pairs_df <- as.data.frame(frame_pairs)
choice_pairs_df <- as.data.frame(choice_pairs)

print(frame_pairs_df)
print(choice_pairs_df)
summary(emm_question)
pairs(emm_question)
# Save the pairwise comparisons to CSV files
output_file_path_frame <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_follow_qfix_frame_pairs.csv"
)
output_file_path_choice <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_follow_qfix_choice_pairs.csv"
)
output_file_path_question <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_follow_qfix_question_pairs.csv"
)
write.csv(
  frame_pairs_df,
  file = output_file_path_frame,
  row.names = FALSE
)
write.csv(
  choice_pairs_df,
  file = output_file_path_choice,
  row.names = FALSE
)
write.csv(
  as.data.frame(pairs(emm_question)),
  file = output_file_path_question,
  row.names = FALSE
)

# Compare the two models using ANOVA
model_camparison_follow <- anova(lmm_strict_follow_qf, lmm_strict_follow_qr)
print(model_camparison_follow)
# save the results of the comparison
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_strict_follow_qfix_vs_qrandom.txt"
)
# Create a text file with the full model comparison summary
sink(output_file_path)
print(model_camparison_follow)
sink()


# --------------------
# B. Less strict dataset
# --------------------
# 1. Main Task
data_less_strict_main <- df_less_strict[
  df_less_strict$main == "True",
] # Filter for main task

# ----------------------
# Model 1: Question as a random effect
lmm_less_strict_main_qr <- lmer(
  evaluation ~ frame * choice + (1 | question) + (1 | participant_id),
  data = data_less_strict_main,
)
# 1. Print the summary of the model
model_summary <- summary(lmm_less_strict_main_qr)
print(model_summary)

# Calculate 95% confidence intervals for fixed effects
conf_int <- confint(lmm_less_strict_main_qr, method = "Wald", level = 0.95)
# Extract only the fixed effects (excluding random effects)
fixed_effects_ci <- conf_int[grep("(Intercept)|frame|choice", rownames(conf_int)),]

# Combine model summary coefficients with confidence intervals
coef_summary <- data.frame(
  Estimate = fixef(lmm_strict_main_qr),
  Std.Error = model_summary$coefficients[,"Std. Error"],
  t.value = model_summary$coefficients[,"t value"],
  p.value = model_summary$coefficients[,"Pr(>|t|)"],
  CI_lower = fixed_effects_ci[,1],
  CI_upper = fixed_effects_ci[,2]
)

# Display the combined summary
print("Fixed Effects with Standard Errors and 95% Confidence Intervals:")
print(coef_summary)
# Save the results as a CSV file
# Define the output file path
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_main_qrandom.txt"
)
# Save the summary of the model to a CSV file
# Create a text file with the full model summary
sink(output_file_path)
print(coef_summary)
sink()
#  ----------------------
# 2. Perform Type III ANOVA to assess overall effects of predictors
anova_results <- anova(lmm_less_strict_main_qr)

# Save to CSV
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_main_qrandom_anova.csv"
)
write.csv(
  as.data.frame(anova_results),
  file = output_file_path,
  row.names = TRUE
)



# Get estimated marginal means for all combinations of frame and choice
emm <- emmeans(lmm_less_strict_main_qr, ~ frame * choice)
# View estimated marginal means
print(emm)
# Pairwise comparisons within 'frame'
frame_pairs <- pairs(emm, by = "frame")
# Pairwise comparisons within 'choice'
choice_pairs <- pairs(emm, by = "choice")

# Set output file path
output_file_path_txt <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_main_qrandom_pairwise.txt"
)

# Open file connection
sink(output_file_path_txt)

# Write frame comparisons
cat("===== FRAME COMPARISONS (within each frame) =====\n\n")
print(frame_pairs)

cat("\n\n===== CHOICE COMPARISONS (within each choice) =====\n\n")
print(choice_pairs)

# Close file connection
sink()

# ----------------------
# Model 2: Question as a fixed effect
lmm_less_strict_main_qf <- lmer(
  evaluation ~ frame * choice + question + (1 | participant_id),
  data = data_less_strict_main,
)
# Print the summary of the model
summary(lmm_less_strict_main_qf)

# Save the results as a CSV file
# Define the output file path
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_main_qfix.txt"
)
# Save the summary of the model to a CSV file
# Create a text file with the full model summary
sink(output_file_path)
print(summary(lmm_less_strict_main_qf))
sink()

#  ----------------------
# 2. Perform Type III ANOVA to assess overall effects of predictors
anova_results <- anova(lmm_less_strict_main_qf)
print(anova_results)
# Save to CSV
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_main_qfix_anova.csv"
)
write.csv(
  as.data.frame(anova_results),
  file = output_file_path,
  row.names = TRUE
)

# Get estimated marginal means for all combinations of frame and choice
emm <- emmeans(lmm_less_strict_main_qr, ~ frame * choice)
# View estimated marginal means
print(emm)
# Pairwise comparisons within 'frame'
frame_pairs <- pairs(emm, by = "frame")
# Pairwise comparisons within 'choice'
choice_pairs <- pairs(emm, by = "choice")
# Inspect with 'question'
emm_question <- emmeans(lmm_less_strict_main_qf, ~ question)

# Convert to data frames
frame_pairs_df <- as.data.frame(frame_pairs)
choice_pairs_df <- as.data.frame(choice_pairs)

print(frame_pairs_df)
print(choice_pairs_df)
summary(emm_question)
pairs(emm_question)
# Save the pairwise comparisons to CSV files
output_file_path_frame <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_main_qfix_frame_pairs.csv"
)
output_file_path_choice <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_main_qfix_choice_pairs.csv"
)
output_file_path_question <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_main_qfix_question_pairs.csv"
)
write.csv(
  frame_pairs_df,
  file = output_file_path_frame,
  row.names = FALSE
)
write.csv(
  choice_pairs_df,
  file = output_file_path_choice,
  row.names = FALSE
)
write.csv(
  as.data.frame(pairs(emm_question)),
  file = output_file_path_question,
  row.names = FALSE
)

# Compare the two models using ANOVA
model_camparison_main <- anova(lmm_less_strict_main_qf, lmm_less_strict_main_qr)
# save the results of the comparison
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_main_qfix_vs_qrandom.txt"
)
# Create a text file with the full model comparison summary
sink(output_file_path)
print(model_camparison_main)
sink()
# --------------------
# 2 Follow-up Task
# --------------------
data_less_strict_follow <- df_less_strict[df_less_strict$follow_up == "True", ] # Filter for follow task # nolint
# ----------------------
# Model 1: Question as a random effect
lmm_less_strict_follow_qr <- lmer(
  evaluation ~ frame * choice + (1 | question) + (1 | participant_id),
  data = data_less_strict_follow,
)
# Extract model summary
model_summary <- summary(lmm_less_strict_follow_qr)
print(model_summary)

# Calculate 95% confidence intervals using Wald method
conf_int <- confint(lmm_less_strict_follow_qr, method = "Wald", level = 0.95)

# Extract only the fixed effects confidence intervals (excluding random effects)
fixed_effects_ci <- conf_int[grep("(Intercept)|frame|choice", rownames(conf_int)),]

# Create a comprehensive summary dataframe
coef_summary <- data.frame(
  Estimate = fixef(lmm_strict_follow_qr),
  Std.Error = model_summary$coefficients[,"Std. Error"],
  t.value = model_summary$coefficients[,"t value"],
  p.value = model_summary$coefficients[,"Pr(>|t|)"],
  CI_lower = fixed_effects_ci[,1],
  CI_upper = fixed_effects_ci[,2],
  CI_formatted = paste0(
    "[", 
    round(fixed_effects_ci[,1], 3),
    ", ",
    round(fixed_effects_ci[,2], 3),
    "]")
)

# Save the results to a file
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_follow_qrandom.txt"
)

# Create a text file with both the model summary and the confidence intervals
sink(output_file_path)
cat("=== MODEL SUMMARY ===\n\n")
print(model_summary)

cat("\n\n=== FIXED EFFECTS WITH 95% CONFIDENCE INTERVALS ===\n\n")
print(coef_summary)
sink()



# Save as emmeans
# Get EMMs for frame × choice combinations
emm <- emmeans(lmm_less_strict_follow_qr, ~ frame * choice)

# Pairwise comparisons within 'frame' (compare choices within each frame)
frame_pairs <- pairs(emm, by = "frame")

# Pairwise comparisons within 'choice' (compare frames within each choice)
choice_pairs <- pairs(emm, by = "choice")

# Set output file path
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_follow_qrandom_pairs.txt"
)

# Write to .txt file
sink(output_file_path)

cat("===== ESTIMATED MARGINAL MEANS (frame × choice) =====\n\n")
print(emm)

cat("\n\n===== PAIRWISE COMPARISONS BY FRAME =====\n\n")
print(frame_pairs)

cat("\n\n===== PAIRWISE COMPARISONS BY CHOICE =====\n\n")
print(choice_pairs)

# Close the file
sink()


#  ----------------------
# 2. Perform Type III ANOVA to assess overall effects of predictors
anova_results <- anova(lmm_less_strict_follow_qr)
anova(lmm_less_strict_follow_qr)
# Save to CSV
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_follow_qrandom_anova.csv"
)
write.csv(
  as.data.frame(anova_results),
  file = output_file_path,
  row.names = TRUE
)

# Get estimated marginal means for all combinations of frame and choice
emm <- emmeans(lmm_less_strict_follow_qr, ~ frame * choice)
# View estimated marginal means
print(emm)
# Pairwise comparisons within 'frame'
frame_pairs <- pairs(emm, by = "frame")
# Pairwise comparisons within 'choice'
choice_pairs <- pairs(emm, by = "choice")

# Print the pairwise comparisons
print(frame_pairs)
print(choice_pairs)

# Convert to data frames
frame_pairs_df <- as.data.frame(frame_pairs)
choice_pairs_df <- as.data.frame(choice_pairs)

# Save the pairwise comparisons to CSV files
output_file_path_frame <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_follow_qrandom_frame_pairs.csv"
)
output_file_path_choice <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_follow_qrandom_choice_pairs.csv"
)
write.csv(
  frame_pairs_df,
  file = output_file_path_frame,
  row.names = FALSE
)
write.csv(
  choice_pairs_df,
  file = output_file_path_choice,
  row.names = FALSE
)

# ----------------------
# Model 2: Question as a fixed effect
lmm_less_strict_follow_qf <- lmer(
  evaluation ~ frame * choice + question + (1 | participant_id),
  data = data_less_strict_follow,
)
# Print the summary of the model
summary(lmm_less_strict_follow_qf)

# Save the results as a CSV file
# Define the output file path
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_follow_qfix.txt"
)
# Save the summary of the model to a CSV file
# Create a text file with the full model summary
sink(output_file_path)
print(summary(lmm_less_strict_follow_qf))
sink()

#  ----------------------
# 2. Perform Type III ANOVA to assess overall effects of predictors
anova_results <- anova(lmm_less_strict_follow_qf)
print(anova_results)
# Save to CSV
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_follow_qfix_anova.csv"
)
write.csv(
  as.data.frame(anova_results),
  file = output_file_path,
  row.names = TRUE
)

# Get estimated marginal means for all combinations of frame and choice
emm <- emmeans(lmm_less_strict_follow_qr, ~ frame * choice)
# View estimated marginal means
print(emm)
# Pairwise comparisons within 'frame'
frame_pairs <- pairs(emm, by = "frame")
# Pairwise comparisons within 'choice'
choice_pairs <- pairs(emm, by = "choice")
# Inspect with 'question'
emm_question <- emmeans(lmm_strict_follow_qf, ~ question)

# Convert to data frames
frame_pairs_df <- as.data.frame(frame_pairs)
choice_pairs_df <- as.data.frame(choice_pairs)

print(frame_pairs_df)
print(choice_pairs_df)
summary(emm_question)
pairs(emm_question)
# Save the pairwise comparisons to CSV files
output_file_path_frame <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_follow_qfix_frame_pairs.csv"
)
output_file_path_choice <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_follow_qfix_choice_pairs.csv"
)
output_file_path_question <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_follow_qfix_question_pairs.csv"
)
write.csv(
  frame_pairs_df,
  file = output_file_path_frame,
  row.names = FALSE
)
write.csv(
  choice_pairs_df,
  file = output_file_path_choice,
  row.names = FALSE
)
write.csv(
  as.data.frame(pairs(emm_question)),
  file = output_file_path_question,
  row.names = FALSE
)

# Compare the two models using ANOVA
model_camparison_follow <- anova(lmm_strict_follow_qf, lmm_strict_follow_qr)
print(model_camparison_follow)
# save the results of the comparison
output_file_path <- file.path(
  folder_path,
  "results_evaluation_lmm_less_strict_follow_qfix_vs_qrandom.txt"
)
# Create a text file with the full model comparison summary
sink(output_file_path)
print(model_camparison_follow)
sink()