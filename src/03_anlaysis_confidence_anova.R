# -------------------------------------------------------
#  This script is wrong, it is not used anymore.
#  Reminder: acitivate radian before running this script!
# type: `radian` in the terminal
# METHOD : Robust Rank-Based ANOVA
#   Handles ordinal scales
#   Handles non-normal, skewed, and extreme values
#   Robust to unbalanced designs
#   Supports factorial designs
# -------------------------------------------------------

# Clear the workspace
rm(list = objects())

# ---------------------------
# 1. Install and load necessary packages
# ---------------------------
library(rankFD)
library(ggplot2)

# ---------------------------
# 2. Load data
# ---------------------------
# Define the current working directory
current_dir <- getwd()  # Get the current working directory
print(current_dir)

# Define paths for data loading
folder_path <- file.path(current_dir, "data", "data_processed")
# Print the folder path
print(folder_path)


# Define the file names for the CSV files
file_name_1 <- "data_choice_evaluation_strict.csv"
file_name_2 <- "data_choice_evaluation_not_strict.csv"


# Define full file paths for both CSV files
file_path_1 <- file.path(folder_path, file_name_1)
file_path_2 <- file.path(folder_path, file_name_2)
print(file_path_1)

# Load both CSV files into data frames
df_strict <- read.csv(file_path_1)
df_not_strict <- read.csv(file_path_2)

# Check the structure of the data frames
# First few rows (default is 6)
head(df_strict)

# ---------------------------
# Example with data from Disease scenario, main task
# The indpendent variables are
#   1. 'frame' (gain/loss)
#   2. 'choice' (sure/risky/neutral)
# The dependent variable is 'confidence evaluation'
# ---------------------------


# ---------------------------
# METHOD 1: Robust Rank-Based ANOVA

#  Handles ordinal scales
#  Handles non-normal, skewed, and extreme values
#  Robust to unbalanced designs
#  Supports factorial designs
# ---------------------------

# Define a function to perform robust rank-based ANOVA
run_rank_based_anova <- function(df, task, question) {

  # Filter the data based on the task
  if (task == "main") {
    task_filter <- df$main == "True"
  } else if (task == "follow-up") {
    task_filter <- df$follow_up == "True"
  }

  # Filter the data for the specific question and task
  question_filter <- df$question == question
  filtered_df <- df[question_filter & task_filter, ]

  # Convert evaluation to numeric if it's not already
  filtered_df$evaluation <- as.numeric(filtered_df$evaluation)

  # Convert predictors to factors if they aren't already
  filtered_df$frame <- as.factor(filtered_df$frame)
  filtered_df$choice <- as.factor(filtered_df$choice)

  # Run robust rank-based ANOVA
  result <- rankFD(
    evaluation ~ frame * choice,
    data = filtered_df,
    alpha = 0.05,
    round = 3 # Number of decimal places to round
  )

  # Return the result
  result
}

# Define the list of questions
questions <- c("Disease", "Unemployment", "Painting", "Virus")


# -----------------------------
# -----------------------------
#  Strict dataset #
# -----------------------------
# Main Task
# -----------------------------
# Initialize an empty data frame to store results
anova_results_all_main <- data.frame()
descriptive_results_all_main <- data.frame()

# Loop through each question
for (question in questions) {
  # Run the rank-based ANOVA for the current question
  result <- run_rank_based_anova(
    df = df_strict, task = "main", question = question
  )

  # ANOVA-Type Statistic section
  anova_result <- as.data.frame(result$ANOVA.Type.Statistic)
  # Add a column for the question
  anova_result$question <- question
  # Combine the results into the final data frame
  anova_results_all_main <- rbind(anova_results_all_main, anova_result)

  # Descriptive Statistics section
  descriptive_result <- as.data.frame(result$Descriptive)
  # Add a column for the question
  descriptive_result$question <- question
  # Combine the results into the final data frame
  descriptive_results_all_main <- rbind(
    descriptive_results_all_main, descriptive_result
  )
}

# Print the combined results
print(anova_results_all_main)
print(descriptive_results_all_main)

# Save the results to CSV files
write.csv(
  anova_results_all_main,
  file = file.path(
    folder_path,
    "confidence_evaluation_anova_stat_results_strict_main.csv"
  ),
  row.names = TRUE
)

write.csv(
  descriptive_results_all_main,
  file = file.path(
    folder_path,
    "confidence_evaluation_anova_descriptive_results_strict_main.csv"
  ),
  row.names = TRUE
)

# ---------------------------
# Follow-up Task
# ---------------------------

# Initialize an empty data frame to store results
anova_results_all_follow <- data.frame()
descriptive_results_all_follow <- data.frame()
# Loop through each question
for (question in questions) {
  # Run the rank-based ANOVA for the current question
  result <- run_rank_based_anova(
    df = df_strict, task = "follow-up", question = question
  )

  # ANOVA-Type Statistic section
  anova_result <- as.data.frame(result$ANOVA.Type.Statistic)
  # Add a column for the question
  anova_result$question <- question
  # Combine the results into the final data frame
  anova_results_all_follow <- rbind(
    anova_results_all_follow, anova_result
  )

  # Descriptive Statistics section
  descriptive_result <- as.data.frame(result$Descriptive)
  # Add a column for the question
  descriptive_result$question <- question
  # Combine the results into the final data frame
  descriptive_results_all_follow <- rbind(
    descriptive_results_all_follow, descriptive_result
  )
}

# Print the combined results
print(anova_results_all_follow)
print(descriptive_results_all_follow)

# Save the results to CSV files
write.csv(
  anova_results_all_follow,
  file = file.path(
    folder_path,
    "confidence_evaluation_anova_stat_results_strict_follow_up.csv"
  ),
  row.names = TRUE
)
write.csv(
  descriptive_results_all_follow,
  file = file.path(
    folder_path,
    "confidence_evaluation_anova_descriptive_results_strict_follow_up.csv"
  ),
  row.names = TRUE
)



# -----------------------------
#  Not strict dataset #
# -----------------------------
# Main Task
# -----------------------------
# Initialize an empty data frame to store results
anova_results_all_main <- data.frame()
descriptive_results_all_main <- data.frame()

# Loop through each question
for (question in questions) {
  # Run the rank-based ANOVA for the current question
  result <- run_rank_based_anova(
    df = df_not_strict, task = "main", question = question
  )

  # ANOVA-Type Statistic section
  anova_result <- as.data.frame(result$ANOVA.Type.Statistic)
  # Add a column for the question
  anova_result$question <- question
  # Combine the results into the final data frame
  anova_results_all_main <- rbind(anova_results_all_main, anova_result)

  # Descriptive Statistics section
  descriptive_result <- as.data.frame(result$Descriptive)
  # Add a column for the question
  descriptive_result$question <- question
  # Combine the results into the final data frame
  descriptive_results_all_main <- rbind(
    descriptive_results_all_main, descriptive_result
  )
}

# Print the combined results
print(anova_results_all_main)
print(descriptive_results_all_main)

# Save the results to CSV files

write.csv(
  anova_results_all_main,
  file = file.path(
    folder_path,
    "confidence_evaluation_anova_stat_results_not_strict_main.csv"
  ),
  row.names = TRUE
)
write.csv(
  descriptive_results_all_main,
  file = file.path(
    folder_path,
    "confidence_evaluation_anova_descriptive_results_not_strict_main.csv"
  ),
  row.names = TRUE
)

# ---------------------------
# Follow-up Task
# ---------------------------

# Initialize an empty data frame to store results
anova_results_all_follow <- data.frame()
descriptive_results_all_follow <- data.frame()
# Loop through each question
for (question in questions) {
  # Run the rank-based ANOVA for the current question
  result <- run_rank_based_anova(
    df = df_not_strict, task = "follow-up", question = question
  )

  # ANOVA-Type Statistic section
  anova_result <- as.data.frame(result$ANOVA.Type.Statistic)
  # Add a column for the question
  anova_result$question <- question
  # Combine the results into the final data frame
  anova_results_all_follow <- rbind(
    anova_results_all_follow, anova_result
  )

  # Descriptive Statistics section
  descriptive_result <- as.data.frame(result$Descriptive)
  # Add a column for the question
  descriptive_result$question <- question
  # Combine the results into the final data frame
  descriptive_results_all_follow <- rbind(
    descriptive_results_all_follow, descriptive_result
  )
}

# Print the combined results
print(anova_results_all_follow)
print(descriptive_results_all_follow)

# Save the results to CSV files
write.csv(
  anova_results_all_follow,
  file = file.path(
    folder_path,
    "confidence_evaluation_anova_stat_results_not_strict_follow_up.csv"
  ),
  row.names = TRUE
)
write.csv(
  descriptive_results_all_follow,
  file = file.path(
    folder_path,
    "confidence_evaluation_anova_descriptive_results_not_strict_follow_up.csv"
  ),
  row.names = TRUE
)