# Diabetes Prediction Project
## Introduction
This project focuses on predicting diabetes risk based on various health-related predictors using machine learning techniques. The key objectives include:

Comprehensive Descriptive Analysis: Uncover patterns and relationships within the dataset, revealing insights into the factors associated with diabetes.
Development of Predictive Models: Construct baseline logistic regression models and explore two additional machine learning models suitable for classification tasks.
Rigorous Model Comparison: Evaluate performance metrics to identify the most accurate and reliable model for diabetes prediction.
Best Model Application: Utilize the selected model to predict diabetes risk in new patients, empowering healthcare professionals with a valuable decision-making tool.
Dataset
The dataset used for this project is the "Diabetes Prediction Dataset" obtained from Kaggle. It contains features such as age, gender, BMI, hypertension, heart disease, smoking history, HbA1c, and blood glucose level, with the target variable being diabetes status (positive or negative).

Dataset source: Kaggle - Diabetes Prediction Dataset

## Approach
The approach involves exploratory data analysis (EDA) to visualize data distributions, identify correlations, and address missing values and anomalies. Model development includes implementing logistic regression as a baseline and experimenting with other machine learning models (Random Forest) . Model evaluation focuses on accuracy, precision, and recall, leading to the selection of the best-performing model for diabetes prediction.

## Random Forest Development
### Approach
Utilized Random Forest algorithm for robust predictive modeling.
Balanced classes through upsampling to address class imbalance.
### Methodology Overview
Data Sampling: Sampled a subset of the training dataset.
Initial Model Building: Constructed Random Forest models with varying tree counts.
Optimization Strategies: Explored optimal mtry value for minimizing error rate.
### Conclusion
Model 3 with optimal mtry of 5 exhibited the lowest error rate among the models evaluated.
## Logistic Regression Model
### Approach
Developed logistic regression model to predict diabetes likelihood.
Balanced classes through upsampling to address class imbalance.
### Model Performance
McFadden's R²: Approximately 0.645, signifying a good fit.
Accuracy: Achieved 86.84% on the test set.
Sensitivity & Specificity: Sensitivity (98.94%) is high, while Specificity (38.61%) is comparatively lower.
### Conclusion
Logistic regression model offers good predictive power but requires further refinement for improved specificity.
Comparative Analysis
Random Forest: Shows a lower overall error rate (6.95%) but higher class errors for "0" predictions compared to Logistic Regression.
Logistic Regression: Demonstrates a good fit (McFadden's R² of 0.6447), indicating a strong association between predictors and the target variable.
## Overall Conclusion
Both Random Forest and Logistic Regression models showcase strengths, and the choice between them depends on specific requirements regarding accuracy or interpretability. Further improvements and model comparisons could be explored in future iterations of the project.

## Recommendations and Further Steps
Refinement of models to reduce false positives and enhance specificity.
Exploration of additional model types (e.g., stepwise and backward models) for comparison.
Continuous monitoring and updating of models to ensure relevance and accuracy in diabetes prediction.
