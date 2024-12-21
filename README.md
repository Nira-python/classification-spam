# Predictive Modeling of Spam Email Detection
Classification of Emails: Binary response variable if Email is spam or not

## Data Source:
Data obtained from Kaggle:
https://www.kaggle.com/datasets/somesh24/spambase?resource=download

## Repository Structure
* `data/`: two datasets
    * spambase_csv.csv : original dataset downloaded from Kaggle
    * binary_spam.csv : same as original except with one-hot-encoded response variable `spam`
* `docs/`: R files on model fitting (stepwise linear regression, logistic regression, KNN, Elastic Net, etc) and Exploratory Data Analysis
* `notebooks/`: Python Jupyter Notebooks on preprocessing data, correlation matrix, PCA, logistic regression
* `paper_presentation/`: Paper and Presentation of this project
