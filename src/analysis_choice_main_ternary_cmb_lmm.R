# -------------------------------------------------------
# Multinomial choice models for the main task across all questions.
#
# Outcome reference: "sure" (0)
# Compared outcomes: "risky" (1) and "neutral" (2)
# Reference frame: "gain"
# Reference question: "Disease"
# -------------------------------------------------------

rm(list = objects())

library(nnet)

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

choice_labels <- c(
  "1" = "Risky vs Sure",
  "2" = "Neutral vs Sure"
)

prepare_main_data <- function(file_name) {
  df <- read.csv(file.path(input_path, file_name))
  df <- df[df$question != "Unemployment", ]
  df <- df[df$main == "True", ]

  df$frame <- factor(df$frame, levels = c("gain", "loss"))
  df$question <- factor(
    df$question,
    levels = c("Disease", "Painting", "Virus")
  )
  df$choice <- factor(df$choice, levels = c(0, 1, 2))
  df$choice <- relevel(df$choice, ref = "0")

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
  p_value_columns <- intersect(names(df), c("p.value", "Pr(>Chisq)"))
  for (column in p_value_columns) {
    df[[column]] <- format_p_values(df[[column]], digits = digits)
  }

  numeric_columns <- vapply(df, is.numeric, logical(1))
  df[numeric_columns] <- lapply(df[numeric_columns], function(values) {
    ifelse(
      is.na(values),
      NA,
      formatC(values, format = "f", digits = digits)
    )
  })
  df
}

summarize_multinom <- function(model, dataset_spec, model_type) {
  model_summary <- summary(model)
  estimates <- model_summary$coefficients
  std_errors <- model_summary$standard.errors

  if (is.null(dim(estimates))) {
    estimates <- matrix(estimates, nrow = 1)
    std_errors <- matrix(std_errors, nrow = 1)
  }

  z_values <- estimates / std_errors
  p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
  ci_lower <- estimates - 1.96 * std_errors
  ci_upper <- estimates + 1.96 * std_errors

  do.call(
    rbind,
    lapply(
      seq_len(nrow(estimates)),
      function(row_index) {
        outcome <- row.names(estimates)[row_index]

        data.frame(
          dataset = dataset_spec$label,
          dataset_slug = dataset_spec$slug,
          model = model_type,
          outcome = outcome,
          choice_comparison = unname(choice_labels[outcome]),
          term = colnames(estimates),
          Estimate = as.numeric(estimates[row_index, ]),
          CI_lower = as.numeric(ci_lower[row_index, ]),
          CI_upper = as.numeric(ci_upper[row_index, ]),
          Std.Error = as.numeric(std_errors[row_index, ]),
          z.value = as.numeric(z_values[row_index, ]),
          p.value = as.numeric(p_values[row_index, ]),
          significance = significance_label(as.numeric(p_values[row_index, ])),
          OR = exp(as.numeric(estimates[row_index, ])),
          OR_CI_lower = exp(as.numeric(ci_lower[row_index, ])),
          OR_CI_upper = exp(as.numeric(ci_upper[row_index, ])),
          row.names = NULL
        )
      }
    )
  )
}

compare_multinom_models <- function(no_interaction_model, interaction_model, dataset_spec) {
  no_interaction_loglik <- logLik(no_interaction_model)
  interaction_loglik <- logLik(interaction_model)

  chisq <- 2 * (
    as.numeric(interaction_loglik) - as.numeric(no_interaction_loglik)
  )
  df_diff <- attr(interaction_loglik, "df") - attr(no_interaction_loglik, "df")
  p_value <- pchisq(chisq, df = df_diff, lower.tail = FALSE)

  data.frame(
    dataset = dataset_spec$label,
    dataset_slug = dataset_spec$slug,
    model_name = c("multinom_no_interaction", "multinom_interaction"),
    npar = c(attr(no_interaction_loglik, "df"), attr(interaction_loglik, "df")),
    AIC = c(AIC(no_interaction_model), AIC(interaction_model)),
    BIC = c(BIC(no_interaction_model), BIC(interaction_model)),
    logLik = c(as.numeric(no_interaction_loglik), as.numeric(interaction_loglik)),
    Chisq = c(NA, chisq),
    Df = c(NA, df_diff),
    `Pr(>Chisq)` = c(NA, p_value),
    significance = c(NA, significance_label(p_value)),
    check.names = FALSE
  )
}

run_main_models <- function(dataset_spec) {
  message("\n===== Running ", dataset_spec$label, " dataset =====")

  data_main <- prepare_main_data(dataset_spec$file)

  multinom_model_no_interaction <- multinom(
    choice ~ frame + question,
    data = data_main,
    trace = FALSE
  )

  multinom_model_interaction <- multinom(
    choice ~ frame * question,
    data = data_main,
    trace = FALSE
  )

  list(
    no_interaction = multinom_model_no_interaction,
    interaction = multinom_model_interaction,
    no_interaction_summary = summarize_multinom(
      multinom_model_no_interaction,
      dataset_spec,
      "multinom_no_interaction"
    ),
    interaction_summary = summarize_multinom(
      multinom_model_interaction,
      dataset_spec,
      "multinom_interaction"
    ),
    model_comparison = compare_multinom_models(
      multinom_model_no_interaction,
      multinom_model_interaction,
      dataset_spec
    )
  )
}

results <- lapply(dataset_specs, run_main_models)
names(results) <- vapply(dataset_specs, `[[`, character(1), "slug")

no_interaction_summary <- do.call(
  rbind,
  lapply(results, `[[`, "no_interaction_summary")
)
interaction_summary <- do.call(
  rbind,
  lapply(results, `[[`, "interaction_summary")
)
model_comparison <- do.call(
  rbind,
  lapply(results, `[[`, "model_comparison")
)

write.csv(
  format_output_values(no_interaction_summary),
  file = file.path(
    output_path,
    "choice_main_ternary_cmb_multinom_no_interaction_3_datasets.csv"
  ),
  row.names = FALSE
)
write.csv(
  format_output_values(interaction_summary),
  file = file.path(
    output_path,
    "choice_main_ternary_cmb_multinom_interaction_3_datasets.csv"
  ),
  row.names = FALSE
)
write.csv(
  format_output_values(model_comparison),
  file = file.path(
    output_path,
    "choice_main_ternary_cmb_multinom_no_interaction_vs_interaction_3_datasets.csv"
  ),
  row.names = FALSE
)
