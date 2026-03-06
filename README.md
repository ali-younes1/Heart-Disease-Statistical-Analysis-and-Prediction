# 🩺 Heart Disease Statistical Analysis and Prediction

Statistical analysis and predictive modeling on a cardiovascular dataset (HeartDisease.csv) using **R**.  
This project covers data cleaning, missing-value imputation, exploratory analysis, hypothesis testing, and multiple models to study and predict **heart disease**.

## Project Context
Cardiovascular diseases are a leading cause of mortality worldwide.  
The goal here is to analyze a medical dataset with multiple clinical features and build models that help explain and predict the presence of heart disease.

## Dataset
- Target (qualitative): **HeartDisease** (1 = heart disease, 0 = normal)
- Target (quantitative): **Cholesterol**

Main variables include:
- Age, Sex
- ChestPainType (TA / ATA / NAP / ASY)
- RestingBloodPressure
- Cholesterol
- FastingBloodSugar
- RestingECG
- MaxHeartRate
- ExerciseAngina
- Oldpeak
- STSlope
- HeartDisease



## What’s Inside

### 1) Data Preparation & Quality Checks
- Fixes inconsistent types (numeric vs categorical)
- Removes malformed row(s)
- Detects outliers using boxplots
- Visualizes missing values
- Imputes missing data:
  - **Median** for numeric variables (robust to outliers)
  - **Mode** for categorical variables
- Removes empty-string entries in key categorical columns
- Standardizes / scales numeric variables (Min-Max style scaling)

### 2) Exploratory Data Analysis (EDA)
**Univariate analysis**
- Frequency tables + bar plots for categorical variables
- Distribution and normality checks for numerical variables (e.g., Shapiro-Wilk)

**Bivariate analysis**
- Relationships between variables using plots and statistical tests
- Focus on the association between **HeartDisease** and explanatory variables
- Example: Chi-square test for associations between categorical variables (e.g., HeartDisease vs Sex)

### 3) Regression (Cholesterol as a quantitative target)
- Simple and multiple linear regression to study links between **Cholesterol** and other predictors
- Model interpretation and diagnostics

### 4) ANOVA (when relevant)
- Comparison of groups across selected variables

### 5) Logistic Regression (Predicting Heart Disease)
- Builds a logistic regression model for **HeartDisease**
- Identifies significant predictors (reported in the presentation), including:
  - Age, Sex, ChestPainType
  - MaxHeartRate, ExerciseAngina, STSlope
  - Oldpeak
- Interprets coefficients in terms of log-odds

### 6) Linear Discriminant Analysis (LDA / ADL)
- Applies **LDA** to discriminate between patients with and without heart disease
- Includes references and methodological overview in the slides
