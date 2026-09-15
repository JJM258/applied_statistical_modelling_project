# Applied Statistical Modelling Project

## Overview

A statistical analysis pipeline built in R, exploring an Amazon e-commerce dataset. This project covers the full analysis workflow, from data ingestion, cleaning, sampling, descriptive statistics, and inferential hypothesis testing to data visualisation.

## Project Structure
amazon_ecommerce_1M.csv - raw dataset (tracked via Git LFS) Sourced from: https://www.kaggle.com/datasets/sharmajicoder/amazon-e-commerce
Stat_Project.R - full R script covering data ingestion, cleaning, sampling, descriptive statistics, inferential tests (Pearson correlation, Chi-squared, One-way ANOVA, Binomial Logistic Regression), and data visualisation
README.md

## Technological Requirements
R (with the following packages: tidyverse, caret, ploty, )
RStudio (recommended)
Git LFS (required to clone the dataset file)

## How to Run
Ensure Git LFS is installed locally (git lfs install) before cloning, so the dataset downloads correctly rather than as a pointer file
Clone this repository:
  git clone https://github.com/JJM258/applied_statistical_modelling_project.git
Open Stat_Project.R in RStudio
Install required packages if not already installed:
r
  install.packages("tidyverse")
  install.packages("caret")
Ensure amazon_ecommerce_1M.csv is in the same working directory as the script (use getwd() to check, setwd() to change if needed)
Run the script in order, from data ingestion through to data visualisation

## Author

JJM258
