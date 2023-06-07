# Survival-data-analysis

## Introduction
This project aimed to analyze the Randomised International Stroke Trial Dataset to investigate the survival time of patients treated with aspirin. The dataset consisted of 19,435 patients with acute ischemic stroke who were randomized to different combinations of aspirin and heparin treatments. The analysis focused on examining the effect of aspirin on time to death, time to death due to specific causes, and the association between covariates such as sex, age, systolic blood pressure, and stroke type with time to death.

## Methodology
The methodology involved extracting seven variables from the dataset and removing missing values. The effect of aspirin on the outcome variable was assessed using a log-rank test and Cox proportional hazards models. Model building was conducted using Cox proportional hazards models, considering variables such as age, sex, systolic blood pressure, and stroke type. The final model revealed that age and stroke type had a significant impact on the risk of death, while the effect of aspirin treatment was not statistically significant.

## Results and Discussion
The results showed that aspirin therapy had little effect on reducing mortality in patients with acute ischemic stroke. The survival curves varied between genders, with males having a longer survival period than females. However, the difference was not statistically significant. The analysis also examined the effect of aspirin on specific causes of death, such as pulmonary embolism. The results showed no significant difference in death rates due to pulmonary embolism or other causes between the treatment groups.

## Conclusion 
Overall, the analysis of the Randomised International Stroke Trial Dataset found that aspirin treatment had minimal impact on the survival time of patients with acute ischemic stroke. The study highlighted the importance of age and stroke type in predicting the risk of death.
