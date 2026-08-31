# Framing Effect Analysis

## Overview
This project analyzes how framing influences decision-making in the main task and follow-up task. The data set contains several versions of the same choice task with different strictness thresholds, and the analysis compares behavior across those datasets.

The project focuses on:
- binary and trinary choice outcomes
- main-task versus follow-up-task comparisons
- individual-question analyses
- combined analyses across datasets
- confidence ratings and choice patterns

## Repository structure
- [data](data): raw experiment data files for the different dataset versions
- [src](src): analysis scripts in R
- [figs](figs): generated figures and visualizations
- [stat_results](stat_results): summary tables and model-output CSV files
- [main.py](main.py): minimal Python entry point
- [pyproject.toml](pyproject.toml): Python project configuration
- [README.md](README.md): project documentation

## Current analysis scripts
The R analysis scripts in [src](src) cover the main analyses used in this project:

- choice analyses
  - [src/analysis_choice_main_binary_neutral_non-neutral_cmb_lmm.R](src/analysis_choice_main_binary_neutral_non-neutral_cmb_lmm.R)
  - [src/analysis_choice_main_binary_neutral_non-neutral_idv.R](src/analysis_choice_main_binary_neutral_non-neutral_idv.R)
  - [src/analysis_choice_main_trinary_idv.R](src/analysis_choice_main_trinary_idv.R)
  - [src/analysis_choice_follow_binary_idv.R](src/analysis_choice_follow_binary_idv.R)
  - [src/analysis_choice_follow_binary_cmb_lmm.R](src/analysis_choice_follow_binary_cmb_lmm.R)
  - [src/analysis_choice_compare_main_vs_follow_binary_idv.R](src/analysis_choice_compare_main_vs_follow_binary_idv.R)
  - [src/analysis_choice_compare_main_vs_follow_binary_cmb.R](src/analysis_choice_compare_main_vs_follow_binary_cmb.R)
  - [src/analysis_choice_main_ternary_cmb_lmm.R](src/analysis_choice_main_ternary_cmb_lmm.R)

- confidence analyses
  - [src/analysis_confidence_main_ternary_idv.R](src/analysis_confidence_main_ternary_idv.R)
  - [src/analysis_confidence_main_ternary_cmb_lmm.R](src/analysis_confidence_main_ternary_cmb_lmm.R)
  - [src/analysis_confidence_follow_binary_idv.R](src/analysis_confidence_follow_binary_idv.R)
  - [src/analysis_confidence_follow_binary_cmb_lmm.R](src/analysis_confidence_follow_binary_cmb_lmm.R)

- exploratory or visualization notebooks
  - [src/descriptitve_stat.ipynb](src/descriptitve_stat.ipynb)
  - [src/visualization_choice.ipynb](src/visualization_choice.ipynb)
  - [src/visualization_confidence.ipynb](src/visualization_confidence.ipynb)

## Datasets
The project includes three dataset variants:
- Strict
- Less strict
- Super strict

These correspond to the CSV files in [data](data), including the main choice dataset files and follow-up task data.

## Outputs
Results are written to [stat_results](stat_results), with CSV summaries for model coefficients, confidence intervals, p-values, and significance labels. Figures are stored in [figs](figs).

## Analysis conventions
Across the project, the outputs generally follow a consistent structure:
- dataset tag
- question label where relevant
- term or coefficient name
- estimate
- confidence interval bounds
- p-value
- significance label
- ordering by question and dataset where requested

## Environment
Python dependencies are managed in [pyproject.toml](pyproject.toml). The R analysis scripts rely on packages such as:
- lme4
- lmerTest
- emmeans
- nnet
- statsmodels for related workflow support

## Notes
This repository is organized around reproducible analysis scripts and summary CSV outputs rather than long-form text reports. The active output workflow is centered on the generated files in [stat_results](stat_results), which are the main artifacts for downstream interpretation and reporting.

