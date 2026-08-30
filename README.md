# Behavioral Data Analysis - Framing Effect on Neutral Decision-Making

## Overview  
This repository contains data analysis scripts and resources for a behavioral experiment investigating how framing (gain vs. loss) affects neutral decision-making. The project explores whether introducing a neutral option (in addition to standard risky/sure choices) mitigates classic framing biases in human decision-making.


## Repository Structure
```
├── data/ # Raw and cleaned behavioral data
├── src/
│ ├── data_preprocess.ipynb # Scripts for cleaning and preparing the data
│ ├── analysis_classic_framing_effect.ipynb # Classic framing effect analysis
│ └── EDA.ipynb # Exploratory data analysis
├── results/ # Output figures, tables, and summary files
├── README.md # This file
```

## Experimental Design  
Participants will first encounter classic framing questions and choose among three options: sure, risky, and neutral. Those who select the neutral option will then be presented with the same scenario without the neutral option, requiring them to make a forced binary choice. In our experiment design, there are four test questions, and we mainly focus on the Asian Disease scenario, which will be presented first.


## Data Description
Sample Size (from src/03_analysis_choice_idv.ipynb): N_SuperStrict = 320, N_Strict = 457, N_LessStrict = 630

## Analysis Goals
- Preprocess and clean behavioral data  
- Explore response distributions 
- Quantify framing effects with and without neutral options  
- Compare behavior across gain and loss frames  
- Generate summary figures and statistics for publication

## Requirements 
This project uses both Python and R. You’ll need environments for both languages set up to run all scripts.

- Python 3.9.2: `pandas`, `numpy`, `matplotlib`, `seaborn`, `scikit-learn`, `statsmodels`, `jupyter`

- R4.2: `lme4`, `lmerTest`, `emmeans`, `nnet`

