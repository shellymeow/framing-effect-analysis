# Framing Effect Analysis

Analysis of how gain/loss framing affects choices and confidence in a main task and a follow-up task, across three datasets with different inclusion criteria (less strict, strict, super strict).

## Data
Each CSV in [data](data) has the columns `participant_id`, `frame` (gain/loss), `question`, `main`, `follow_up`, `choice` (0 = sure, 1 = risky, 2 = neutral; main task only) and `evaluation` (confidence, 0–100).

## Setup
```bash
uv sync
```
