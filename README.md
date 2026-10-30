# Framing Effect Analysis

Analysis of how gain/loss framing affects choices and confidence in a main task and a follow-up task, across three datasets with different inclusion criteria (less strict, strict, super strict).

## Structure
- [data](data): one CSV per dataset
- [src](src): R analysis scripts and Jupyter notebooks
  - `analysis_choice_*`: choice models (per question, and combined across questions with question as a random effect)
  - `analysis_confidence_*`: confidence models (same structure)
  - `*.ipynb`: descriptive statistics and visualizations
- [stat_results](stat_results): model output tables
- [figs](figs): figures

## Data
Columns: `participant_id`, `frame` (gain/loss), `question`, `main`, `follow_up`, `choice` (0 = sure, 1 = risky, 2 = neutral; main task only) and `evaluation` (confidence, 0–100).

## Setup
Run the R scripts from the repository root. They use lme4, lmerTest, emmeans, nnet, broom and magrittr. For the notebooks:
```bash
uv sync
```
