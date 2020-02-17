# bayesian-sales-in-stormy-weather
Repository containing the R code related to our project for the Bayesian Statistics course @ Polimi

Instructions:

1. Clone this repo to your desktop

2. Get the data from the official kaggle competition (https://www.kaggle.com/c/walmart-recruiting-sales-in-stormy-weather/data) and paste it into the Data folder

3. Run the scripts in the folder "01. Data Preparation & Preprocessing" that will create the cleaned dataset in the folder "Dataset_pronti". All scripts are made tu run correctly when the working directory is set in their location, so set it there before running any script.

Content of every folder:

"01. Data Preparation & Preprocessing": Scripts to prepare the dataset to be analyzed.

"02. Descriptive Analysis": Scriptis to visually see some features of the dataset.

"03. Univariates models": Models that analyze time series by themselves, without taking into account interactions, as seen in Presetation 2. Those include:
- Poisson & ZIP
- Gaussian & Gamma
- BSTS
- A first attempt to buid an AR model in JAGS (was developed more in depth in the multivariate part)

"04. Multivariate models": In the folder you find scripts numbered from 01 to 09 to put the dataset in a format that is instrumental for multivariate analysis and two model folders:
- MBSTS which proves that the BSTS package beta version for Multivariate models is not mature enough for real use.
- VARX with JAGS which contains the multivariate autoregressive models with external covariates we have built. A more accurate description of what is inside can be found in the "Description.txt" file inside that folder.





