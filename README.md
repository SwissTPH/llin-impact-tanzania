# llin-impact-tanzania
R scripts for retrospective impact evaluation of LLINs using routine surveillance data and interrupted time series analysis. This repository contains the R code used to reproduce the analyses presented in the manuscript:  
> **Using Routine Health Facility Data to Evaluate the Retrospective Impact of Long-Lasting Insecticidal Nets in Mainland Tanzania**

## Overview

This study retrospectively evaluates the epidemiological impact of the 2020 long-lasting insecticidal net (LLIN) mass distribution campaign in mainland Tanzania using routine health facility surveillance data.

The analysis applies interrupted time series methods using dynamic regression models with ARIMA errors to estimate the counterfactual malaria incidence that would have been expected in the absence of the campaign. The difference between the observed and predicted incidence is used to estimate the number and proportion of malaria cases/1,000 population averted.

Analyses were conducted independently for each council using routinely collected surveillance data together with environmental and health system covariates.

## Data

The analyses use multiple data sources including:

- Routine malaria surveillance data from the Tanzania DHIS2/Health Management Information System (HMIS)
- Council-level ITN access estimates (https://pubmed.ncbi.nlm.nih.gov/36604693/)
- CHIRPS rainfall data
- MODIS NDVI
- Landsat-derived land surface temperature
- Health facility reporting completeness indicator

Data from routine HMIS/DHIS2 are not publicly available and were obtained with request from the National Malaria Control Programme of mainland Tanzania. Restrictions apply to the availability of these data and permission can be obtained with reasonable request from the Ministry of Health of mainland Tanzania. Therefore, this repository does not contain the raw surveillance data.

## Analysis workflow

The analytical workflow consists of the following steps:

1. Data cleaning and quality assessment
2. Aggregation of health facility data to council level
3. Assembly of environmental and health system covariates
4. Selection of lagged covariates using cross-correlation analysis
5. ARIMA model fitting for the pre-intervention period using auto.arima function
6. Checking the model orders and residuals and updating the orders where necessary
7. ARIMA model fitting using revised orders
8. Counterfactual forecasting assuming no LLIN campaign
10. Estimation of:
   - percentage of malaria cases/1000 population averted
   - absolute reduction in malaria incidence/1000 population
11. Generation of manuscript figures and tables

## Statistical methods

For each council, dynamic regression models with seasonal ARIMA errors were fitted to monthly malaria incidence.

The models account for:

- temporal autocorrelation
- seasonality
- long-term trends
- environmental covariates
- reporting indicators
- LLIN access

Model adequacy was assessed using:

- residual diagnostics
- ACF/PACF plots
- Ljung–Box tests

## Software

Analyses were performed in:

- R (version 4.5.1)

Main packages include:

- forecast
- tidyverse
- data.table
- lubridate
- ggplot2
- sf
- terra

## Reproducibility

Because the surveillance data cannot be shared publicly, this repository provides:

- all analysis scripts
- documentation of the analytical workflow
- code used to generate the manuscript figures and tables

Users with access to the original data should be able to reproduce all analyses by following the workflow described above.

## Citation

If you use this code, please cite:

> Thawer SG, Golumbeanu M, Gitanya MP, et al. *Using Routine Health Facility Data to Evaluate the Retrospective Impact of Long-Lasting Insecticidal Nets in Mainland Tanzania.* (Manuscript under review.)

## Funding

This work was supported by the Swiss Tropical and Public Health Institute and the Gates Foundation (Investment ID INV-068864). The funders had no role in study design, data collection and analysis, decision to publish, or manuscript preparation.

## Contact

For questions regarding the analysis or repository, please contact:


**Sumaiyya G. Thawer**  
Swiss Tropical and Public Health Institute, Allschwil, Switzerland

or
 
**Monica Golumbeanu**  
Swiss Tropical and Public Health Institute, Allschwil, Switzerland
