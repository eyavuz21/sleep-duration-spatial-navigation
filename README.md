# Sleep Duration & Virtual Spatial Navigation (n=766)

[![Paper](https://img.shields.io/badge/Journal-Sci._Reports-green)](https://doi.org/10.1038/s41598-024-54715-x)

This repository contains the analysis code for our study investigating how various sleep-related variables independently associate with wayfinding performance, and the role gender plays in these associations.

## 🔬 Key Findings
- **Gender-Specific Association:** Shorter self-reported sleep duration is significantly associated with worse wayfinding performance in **men only**.
- **Sensitivity Analysis:** The significant association in men disappears when "non-typical" sleepers (<6h or >9h) are removed, suggesting the effect may be driven by extreme sleep durations.
- **Other Variables:** Sleep quality, daytime sleepiness, and nap frequency showed no significant independent associations with performance.

## 📁 Files
- `Sleeppaper.R`: Main statistical pipeline including stratified regressions and sensitivity tests.
- `Sleeppaper.ipynb`: Exploratory analysis and gender-comparative visualizations.
- 
## 🛠 Skills Demonstrated
- **Multivariate Regression:** Using Multiple Linear Regression to isolate independent predictors from a range of sleep metrics.
- **Subgroup Analysis:** Implementing gender-stratified models to identify demographic-specific effects.
- **Data Filtering & Robustness:** Conducting sensitivity analyses by removing outliers (non-typical sleepers) to test the stability of results.
- **Large-N Behavioral Analytics:** Analyzing a US-based cohort of 766 participants using the Sea Hero Quest platform.

