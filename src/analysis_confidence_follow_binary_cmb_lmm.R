# -------------------------------------------------------
# Linear mixed-effect models for follow-up-task confidence ratings
#
# Goal: analyze the effect of frame, choice, and their interaction
#       on confidence ratings.
#
# Task: follow-up task, "sure" (reference) vs "risky"
# Reference frame: "gain"
# Reference question: "Disease"
# -------------------------------------------------------

rm(list = objects())

# ---------------------------
# 1. Load packages
# ---------------------------
library(lmerTest)

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

  df$frame <- factor(df$frame, levels = c("gain", "loss"))
  df$question <- factor(
    df$question,
    levels = c("Disease", "Painting", "Virus")
  )

  df
}

fixed_effect_summary <- function(model) {
  model_summary <- summary(model)
  fixed_terms <- names(fixef(model))
  conf_int <- confint(model, parm = fixed_terms, method = "Wald", level = 0.95)
  p_values <- model_summary$coefficients[fixed_terms, "Pr(>|t|)"]

  data.frame(
    term = fixed_terms,
    Estimate = fixef(model),
    Std.Error = model_summary$coefficients[fixed_terms, "Std. Error"],
    t.value = model_summary$coefficients[fixed_terms, "t value"],
    p.value = p_values,
    significance = significance_label(p_values),
    CI_lower = conf_int[fixed_terms, 1],
    CI_upper = conf_int[fixed_terms, 2],
    row.names = NULL
  )
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
  p_value_columns <- intersect(names(df), c("p.value", "Pr(>F)", "Pr(>Chisq)"))
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

run_follow_lmm <- function(dataset_spec) {
  message("\n===== Running ", dataset_spec$label, " dataset =====")

  df <- prepare_data(dataset_spec$file)
  data_follow <- df[df$follow_up == "True", ]
  data_follow$choice <- factor(data_follow$choice, levels = c(0, 1))

  qrandom_model <- lmer(
    evaluation ~ frame * choice + (1 | question) + (1 | participant_id),
    data = data_follow
  )
  print(summary(qrandom_model))

  qfix_model <- lmer(
    evaluation ~ frame * choice + question + (1 | participant_id),
    data = data_follow
  )
  print(summary(qfix_model))

  invisible(list(qrandom = qrandom_model, qfix = qfix_model))
}

summarize_fixed_effects_across_datasets <- function(models, dataset_specs, model_type) {
  do.call(
    rbind,
    Map(
      function(model_set, dataset_spec) {
        summary_df <- fixed_effect_summary(model_set[[model_type]])
        summary_df$dataset <- dataset_spec$label
        summary_df$dataset_slug <- dataset_spec$slug
        summary_df$model <- model_type
        summary_df[, c(
          "dataset",
          "dataset_slug",
          "model",
          "term",
          "Estimate",
          "CI_lower",
          "CI_upper",
          "Std.Error",
          "t.value",
          "p.value",
          "significance"
        )]
      },
      models,
      dataset_specs
    )
  )
}

summarize_anova_across_datasets <- function(models, dataset_specs, model_type) {
  do.call(
    rbind,
    Map(
      function(model_set, dataset_spec) {
        anova_df <- as.data.frame(anova(model_set[[model_type]]))
        anova_df$term <- row.names(anova_df)
        anova_df$dataset <- dataset_spec$label
        anova_df$dataset_slug <- dataset_spec$slug
        anova_df$model <- model_type
        row.names(anova_df) <- NULL
        anova_df[, c(
          "dataset",
          "dataset_slug",
          "model",
          "term",
          setdiff(
            names(anova_df),
            c("dataset", "dataset_slug", "model", "term")
          )
        )]
      },
      models,
      dataset_specs
    )
  )
}

summarize_model_comparisons_across_datasets <- function(models, dataset_specs) {
  do.call(
    rbind,
    Map(
      function(model_set, dataset_spec) {
        comparison_df <- as.data.frame(anova(model_set$qfix, model_set$qrandom))
        comparison_df$model_name <- row.names(comparison_df)
        comparison_df$dataset <- dataset_spec$label
        comparison_df$dataset_slug <- dataset_spec$slug
        row.names(comparison_df) <- NULL
        comparison_df[, c(
          "dataset",
          "dataset_slug",
          "model_name",
          setdiff(
            names(comparison_df),
            c("dataset", "dataset_slug", "model_name")
          )
        )]
      },
      models,
      dataset_specs
    )
  )
}

save_combined_summaries <- function(models, dataset_specs) {
  qrandom_coef_summary <- summarize_fixed_effects_across_datasets(
    models,
    dataset_specs,
    "qrandom"
  )
  qfix_coef_summary <- summarize_fixed_effects_across_datasets(
    models,
    dataset_specs,
    "qfix"
  )
  qrandom_anova_summary <- summarize_anova_across_datasets(
    models,
    dataset_specs,
    "qrandom"
  )
  qfix_anova_summary <- summarize_anova_across_datasets(
    models,
    dataset_specs,
    "qfix"
  )
  model_comparison_summary <- summarize_model_comparisons_across_datasets(
    models,
    dataset_specs
  )

  write.csv(
    format_output_values(qrandom_coef_summary),
    file = file.path(output_path, "confidence_follow_cmb_qrandom_lmm_3_datasets.csv"),
    row.names = FALSE
  )
  write.csv(
    format_output_values(qfix_coef_summary),
    file = file.path(output_path, "confidence_follow_cmb_qfix_lmm_3_datasets.csv"),
    row.names = FALSE
  )
  write.csv(
    format_output_values(qrandom_anova_summary),
    file = file.path(output_path, "confidence_follow_cmb_qrandom_anova_3_datasets.csv"),
    row.names = FALSE
  )
  write.csv(
    format_output_values(qfix_anova_summary),
    file = file.path(output_path, "confidence_follow_cmb_qfix_anova_3_datasets.csv"),
    row.names = FALSE
  )
  write.csv(
    format_output_values(model_comparison_summary),
    file = file.path(
      output_path,
      "confidence_follow_cmb_qfix_vs_qrandom_3_datasets.csv"
    ),
    row.names = FALSE
  )
}

# ---------------------------
# 4. Run models for all datasets
# ---------------------------
models <- lapply(dataset_specs, run_follow_lmm)
names(models) <- vapply(dataset_specs, `[[`, character(1), "slug")

# ---------------------------
# 5. Save combined summaries across datasets
# ---------------------------
save_combined_summaries(models, dataset_specs)
