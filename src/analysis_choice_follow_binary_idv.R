# This script tests the effect of frame on binary follow-up choices.
# It keeps the analysis focused on logistic regression and loops over all
# datasets to reduce repetition and improve readability.

rm(list = objects())

current_dir <- getwd()
folder_path <- file.path(current_dir, "data")
output_path <- file.path(current_dir, "stat_results")
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

dataset_files <- list(
  "Strict" = "data_choice_evaluation_strict.csv",
  "Less Strict" = "data_choice_evaluation_less_strict.csv",
  "Super Strict" = "data_choice_evaluation_super_strict.csv"
)

dataset_order <- c("Strict", "Less Strict", "Super Strict")

significance_label <- function(p_values) {
  ifelse(
    p_values < 0.001,
    "<.001***",
    ifelse(
      p_values < 0.01,
      "<.01**",
      ifelse(p_values < 0.05, "<.05*", "ns")
    )
  )
}

format_p_values <- function(p_values, digits = 2) {
  ifelse(
    is.na(p_values),
    NA,
    formatC(round(p_values, digits), format = "f", digits = digits)
  )
}

format_output_values <- function(df, digits = 2) {
  p_value_columns <- intersect(names(df), c("p.value", "p_value", "Pr(>|z|)", "Pr(>Chisq)"))
  for (column in p_value_columns) {
    df[[column]] <- format_p_values(df[[column]], digits = digits)
  }

  numeric_columns <- vapply(df, is.numeric, logical(1))
  df[numeric_columns] <- lapply(
    df[numeric_columns],
    function(x) {
      ifelse(is.na(x), NA, formatC(x, format = "f", digits = digits))
    }
  )

  if ("p_value" %in% names(df) && "significance" %in% names(df)) {
    df <- df[, c(
      setdiff(names(df), c("p_value", "significance")),
      "p_value",
      "significance"
    )]
  }

  df
}

build_coef_table <- function(model) {
  s <- summary(model)
  coefs_mat <- coef(s)

  est <- coefs_mat[, "Estimate"]
  ses <- coefs_mat[, "Std. Error"]
  z_vals <- coefs_mat[, "z value"]
  p_vals <- coefs_mat[, "Pr(>|z|)"]

  odds_ratios <- exp(est)
  z_crit <- 1.96
  ci_lower <- est - z_crit * ses
  ci_upper <- est + z_crit * ses

  data.frame(
    Estimate = est,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    Std_Error = ses,
    z_value = z_vals,
    p_value = p_vals,
    significance = significance_label(p_vals),
    Odds_Ratio = odds_ratios,
    OR_CI_Lower = exp(ci_lower),
    OR_CI_Upper = exp(ci_upper),
    stringsAsFactors = FALSE
  )
}

preprocess_followup_data <- function(df) {
  df <- df[df$question != "Unemployment", , drop = FALSE]

  df$frame <- factor(df$frame, levels = c("gain", "loss"))
  df$question <- factor(df$question, levels = c("Disease", "Painting", "Virus"))
  df$choice <- factor(df$choice, levels = c(0, 1))

  df[df$follow_up == "True", , drop = FALSE]
}

all_question_summary <- list()

for (dataset_name in names(dataset_files)) {
  df_raw <- read.csv(file.path(folder_path, dataset_files[[dataset_name]]))
  df_followup <- preprocess_followup_data(df_raw)

  questions <- unique(df_followup$question)

  for (q in questions) {
    df_question <- df_followup[df_followup$question == q, , drop = FALSE]
    glm_model <- glm(choice ~ frame, data = df_question, family = binomial)

    coef_table <- build_coef_table(glm_model)
    coef_table$Question <- q
    coef_table$Dataset <- dataset_name

    all_question_summary[[length(all_question_summary) + 1]] <- coef_table
  }
}

summary_df <- do.call(rbind, all_question_summary)
summary_df <- summary_df[order(
  summary_df$Question,
  match(summary_df$Dataset, dataset_order)
), , drop = FALSE]

summary_df <- summary_df[, c(
  "Question",
  "Dataset",
  setdiff(names(summary_df), c("Question", "Dataset"))
)]

summary_df <- summary_df[, c(
  setdiff(names(summary_df), c("p_value", "significance")),
  "p_value",
  "significance"
)]

write.csv(
  format_output_values(summary_df),
  file.path(output_path, "choice_follow_binary_idv_3_questions.csv"),
  row.names = FALSE
)
