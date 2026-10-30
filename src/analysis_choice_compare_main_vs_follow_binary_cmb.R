# -------------------------------------------------------
# Compare non-neutral choices between main and follow-up tasks
# across all questions.
#
# Outcome: binary choice, "sure" (0, reference) vs "risky" (1)
# Predictors: frame, task, and their interaction
# Reference frame: "gain"
# Reference task: "main"
# -------------------------------------------------------

rm(list = objects())

# ---------------------------
# 1. Load packages
# ---------------------------
library(lme4)

# ---------------------------
# 2. Paths and dataset definitions
# ---------------------------
current_dir <- getwd()
print(current_dir)

input_path <- file.path(current_dir, "data")
output_path <- file.path(current_dir, "stat_results")
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

dataset_specs <- list(
  list(
    label = "Strict",
    slug = "strict",
    file = "data_choice_evaluation_strict.csv"
  ),
  list(
    label = "Less strict",
    slug = "less_strict",
    file = "data_choice_evaluation_less_strict.csv"
  ),
  list(
    label = "Super strict",
    slug = "super_strict",
    file = "data_choice_evaluation_super_strict.csv"
  )
)

# ---------------------------
# 3. Helper functions
# ---------------------------
prepare_data <- function(file_name) {
  df <- read.csv(file.path(input_path, file_name))
  df <- df[df$question != "Unemployment", ]
  df <- df[df$main == "True" | df$follow_up == "True", ]

  df$task <- ifelse(df$main == "True", "main", "follow")
  df <- df[df$choice != 2, ]

  df$frame <- factor(df$frame, levels = c("gain", "loss"))
  df$task <- factor(df$task, levels = c("main", "follow"))
  df$choice <- factor(df$choice, levels = c(0, 1))
  df$question <- factor(
    df$question,
    levels = c("Disease", "Painting", "Virus")
  )

  df
}

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
    ifelse(
      p_values < 0.005,
      formatC(p_values, format = "e", digits = digits),
      formatC(round(p_values, digits), format = "f", digits = digits)
    )
  )
}

format_output_values <- function(df, digits = 2) {
  p_value_columns <- intersect(names(df), c("p.value", "Pr(>|z|)"))
  for (column in p_value_columns) {
    df[[column]] <- format_p_values(df[[column]], digits = digits)
  }

  numeric_columns <- vapply(df, is.numeric, logical(1))
  df[numeric_columns] <- lapply(
    df[numeric_columns],
    formatC,
    format = "f",
    digits = digits
  )
  df
}

run_glmer_all <- function(dataset_spec) {
  message("\n===== Running ", dataset_spec$label, " dataset =====")

  data <- prepare_data(dataset_spec$file)
  fit <- glmer(
    choice ~ frame * task + question + (1 | participant_id),
    data = data,
    family = binomial
  )

  coefs <- coef(summary(fit))
  fixed_terms <- names(fixef(fit))
  cis <- confint(fit, method = "Wald", parm = "beta_")
  p_values <- coefs[fixed_terms, "Pr(>|z|)"]

  summary_df <- data.frame(
    dataset = dataset_spec$label,
    dataset_slug = dataset_spec$slug,
    term = fixed_terms,
    Estimate = fixef(fit),
    CI_lower = cis[fixed_terms, 1],
    CI_upper = cis[fixed_terms, 2],
    p.value = p_values,
    significance = significance_label(p_values),
    Std.Error = coefs[fixed_terms, "Std. Error"],
    z.value = coefs[fixed_terms, "z value"],
    row.names = NULL
  )

  invisible(list(model = fit, summary = summary_df))
}

save_combined_summary <- function(results) {
  combined_summary <- do.call(
    rbind,
    lapply(results, `[[`, "summary")
  )

  write.csv(
    format_output_values(combined_summary),
    file = file.path(
      output_path,
      "choice_main_vs_follow_binary_cmb_lmm_qfix_3_datasets.csv"
    ),
    row.names = FALSE
  )
}

# ---------------------------
# 4. Run models for all datasets
# ---------------------------
results <- lapply(dataset_specs, run_glmer_all)
names(results) <- vapply(dataset_specs, `[[`, character(1), "slug")

# ---------------------------
# 5. Save combined summary across datasets
# ---------------------------
save_combined_summary(results)
