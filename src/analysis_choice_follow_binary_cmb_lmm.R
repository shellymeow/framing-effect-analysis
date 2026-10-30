# -------------------------------------------------------
# Binary choice GLMMs for the follow-up task across all questions.
#
# Outcome: "sure" (0, reference) vs "risky" (1)
# Reference frame: "gain"
# -------------------------------------------------------

rm(list = objects())

library(lme4)

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

prepare_follow_data <- function(file_name) {
  df <- read.csv(file.path(input_path, file_name))
  df <- df[df$question != "Unemployment", ]
  df <- df[df$follow_up == "True", ]

  df$frame <- factor(df$frame, levels = c("gain", "loss"))
  df$question <- factor(
    df$question,
    levels = c("Disease", "Painting", "Virus")
  )
  df$choice <- factor(df$choice, levels = c(0, 1))

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
  p_value_columns <- intersect(names(df), c("p.value", "Pr(>|z|)", "Pr(>Chisq)"))
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

summarize_glmer <- function(model, dataset_spec, model_type) {
  coefs <- coef(summary(model))
  fixed_terms <- names(fixef(model))
  cis <- confint(model, method = "Wald", parm = "beta_")
  p_values <- coefs[fixed_terms, "Pr(>|z|)"]

  data.frame(
    dataset = dataset_spec$label,
    dataset_slug = dataset_spec$slug,
    model = model_type,
    term = fixed_terms,
    Estimate = fixef(model),
    CI_lower = cis[fixed_terms, 1],
    CI_upper = cis[fixed_terms, 2],
    Std.Error = coefs[fixed_terms, "Std. Error"],
    z.value = coefs[fixed_terms, "z value"],
    p.value = p_values,
    significance = significance_label(p_values),
    OR = exp(fixef(model)),
    OR_CI_lower = exp(cis[fixed_terms, 1]),
    OR_CI_upper = exp(cis[fixed_terms, 2]),
    row.names = NULL
  )
}

run_follow_models <- function(dataset_spec) {
  message("\n===== Running ", dataset_spec$label, " dataset =====")

  data_follow <- prepare_follow_data(dataset_spec$file)

  qrandom_model <- glmer(
    choice ~ frame + (1 | question) + (1 | participant_id),
    data = data_follow,
    family = binomial
  )

  list(
    qrandom = qrandom_model,
    qrandom_summary = summarize_glmer(qrandom_model, dataset_spec, "qrandom")
  )
}

results <- lapply(dataset_specs, run_follow_models)
names(results) <- vapply(dataset_specs, `[[`, character(1), "slug")

qrandom_summary <- do.call(rbind, lapply(results, `[[`, "qrandom_summary"))

write.csv(
  format_output_values(qrandom_summary),
  file = file.path(output_path, "choice_follow_binary_cmb_qrandom_lmm_3_datasets.csv"),
  row.names = FALSE
)
