
# Obesity Level Predictive Modeling

## Table of Contents
1. [Overview](#overview)
2. [About the Dataset](#about-the-dataset)
3. [Research Question](#research-question)
4. [Results](#results)
5. [Methodology](#methodology)
6. [Limitations](#limitations)
7. [Future Work](#future-work)
8. [References](#references)
9. [Contributors](#contributors)

## Overview
This project investigates how synthetic data generation techniques (SMOTE and Weka) inflate ROC-AUC performance in obesity level classification. The study compares classifier performance between two datasets: one containing real data and another containing both real and synthetic data.

## About the Dataset
The following paragraph describes the nature of the dataset:

> "This dataset include data for the estimation of obesity levels in individuals from the countries of Mexico, Peru and Colombia, based on their eating habits and physical condition. The data contains 17 attributes and 2111 records, the records are labeled with the class variable NObesity (Obesity Level), that allows classification of the data using the values of Insufficient Weight, Normal Weight, Overweight Level I, Overweight Level II, Obesity Type I, Obesity Type II and Obesity Type III. 77% of the data was generated synthetically using the Weka tool and the SMOTE filter, 23% of the data was collected directly from users through a web platform.


*The link to the dataset can be found [here](https://archive.ics.uci.edu/dataset/544/estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition).*

## Research Question

> Does the inclusion of synthetic data lead to inflated ROC-AUC scores compared to classifier models trained and evaluated on non-synthetic (real) data?

### Variables Used

Our **dependent variable** for this study is the respondent's obesity level: **NObeyesdad.**

For **independent variables**, a combination of numeric and categorical (in the form of survery responses) data were used, such as:
- Age
- Height
- Weight
- Number of meals
- If the respondent's family suffers or has suffered from being overweight
- If the respondent eats food between meals
- If the respondent drinks alcohol
- etc...

All variables except the dependent variable (NObeyesdad) were used in prediction.

## Results

The dataset containing real data produced lower ROC-AUC scores than the dataset containing real and synthetic data. This was verified using three classifier models:

### Real Data Only

| Model # | Model Name | 10 fold cross validation result (ROC AUC)| Prediction Accuracy on Test Set (ROC AUC) |
|--------------|--------------|------------|-------------|
| Model 1 | Base decision tree | 0.8848726 | 0.9388945 |
| Model 2 | K-NN w/ tuned hyperparameters | 0.6937371 | 0.6443231 |
| Model 3 | Gradient boosted tree w/ tuned hyperparameters | 0.9296265 | 0.9523720 |

### Real and Synthetic Data

| Model # | Model Name | 10 fold cross validation result (ROC AUC)| Prediction Accuracy on Test Set (ROC AUC)|
|--------------|--------------|------------|-------------|
| Model 1 | Base decision tree | 0.7596 | 0.97776858 |
| Model 2 | K-NN w/ tuned hyperparameters | 0.9416791 | 0.8855494 |
| Model 3 | Gradient boosted tree w/ tuned hyperparameters | 0.9988123 | 0.9989265 |
---
## Methodology

### 1. Data Preparation

The dataset was imported as a .csv file from the [UC Irvine Machine Learning Repository](https://archive.ics.uci.edu/dataset/544/estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition) and read into R as a DataFrame object. To determine which records contained synthetic data, we proposed the following hypothesis: number columns (e.g., Age, FCVC, NCP, CH20, FAF, and TUE) that contained decimal point values were synthetic, while those that were whole numbers were real records. If a record contained at least one decimal point value in the aforementioned columns, they were saved to a separate DataFrame titled "syn_df." All real records were saved into a DataFrame titled "normal_df."

*See detailed data preparation steps in section "Load in and separate normal and synthetic data" of "main.md" in this repository.*
  
### 2. EDA (Exploratory Data Analysis)

Three histograms were produced to analyze the "NObeyesdad" distributions between the dataset containing normal/real records, the dataset containing only synthetic records, and the dataset containing both.

Normal/real records displayed an uneven distribution, with most records being in the "Normal Weight" category. The datasets containing synthetic data, including the one with both normal and synthetic records, had a much more even distribution, validating the hypothesis that records with decimal point values were synthetic.

*See visualizations in section "EDA" of "main.md" in this repository.*

### 3. Rounding Logic
Because the dataset containing only synthetic data did not include any records in the "Normal Weight" category, it was excluded from testing; instead, we opted to test the differences between the dataset containing normal/real records and normal + synthetic records.

Before modeling, the following rounding logic was applied:

| Variable | Rounding Logic |
|--------------|--------------|
| Age | 0 decimals, age in years, whole number only |
| Height | 2 decimals height in meters, 2 decimal points for cm precision |
| Weight | 1 decimal weight in kg, 1 decimal point is sufficient |
| FCVC | 0 decimals scale 1-3, whole number only (categorical) - recoded to 0/1/2 for consistency |
| NCP | 0 decimals number of meals, whole number only |
|CH20| 0 decimal water intake scale 1-3, whole number only (in liters) - recoded to 0/1/2 for consistency |
|FAF| 0 decimal physical activity scale 0-3 (in days) |
|TUE| 0 decimals tech use scale 0-2, whole number only |

*See detailed rounding logic steps in section "Cleaning Data: Rounding" of "main.md" in this repository.*


### 4. Splitting the Data
Both datasets, one containing only real records and the other containing real + synthetic records, were split into training and testing sets and stratified based on "NObeyesdad," or the obesity levels. The training sets were further divided into folds for cross-validation and resampling. Four customer metrics were recorded for modeling: accuracy, ROC AUC, sensitivity, and specificity.

*See detailed train/test splits in section "Splitting Data" of "main.md" in this repository.*

### 5. Modeling
To classify obesity levels, three models were used: a base decision tree, a gradient-boosted tree with tuned hyperparameters, and K-Nearest Neighbors. These models were also used to compare model performance and accuracy between the two datasets. As shown in the "Results" section, the dataset containing real and synthetic data performed much better than the dataset containing only real data, suggesting that imputation of synthetic data may artificially inflate accuracy scores.

*See detailed modeling starting from section "Model 1 - Base Decision Tree: Real Data (normal_df)" until the end of "main.md" in this repository.*

## Limitations
This study presents some limitations. First, ROC-AUC scores alone may not prove that synthetic data generation techniques inflate model classification performance. Secondly, dataset-specific findings may not generalize, and the hypothesis that decimal-point values represent artificial values may not be accurate. Lastly, results may differ when using other models, such as logistic regression or SVM models.

---

  

## Future Work
- Evaluate additional synthetic generation methods.
- Test across multiple domains/models.

---

## References

```bibtex
@article{ref1,
title={Estimation of Obesity Levels Based On Eating Habits and Physical Condition [Dataset]},
author={UCI Machine Learning Repository},
year={2019},
doi={https://doi.org/10.24432/C5H31Z}
}

@article{ref2,
title={Dataset for estimation of obesity levels based on eating habits and physical condition in individuals from Colombia, Peru and Mexico},
author={Palechor and Manotas},
journal={Data in Brief},
year={2019},
volume={25},
doi={https://doi.org/10.1016/j.dib.2019.104344}
}
```
---

## Contributors

| Name | Role |
|--------|-------|
|Zachary Cannon|Model building|
| Makayla Harvey | Data prep |
|Giancarlo Pantano|Data Prep|
|Kaylen Ton|Model building|
|Megan Xiao|Project Management|

