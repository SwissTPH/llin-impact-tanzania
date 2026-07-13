############################################################################
# Fit ARIMA Model for various covariate combination and save best model 
############################################################################

packages= c("zoo","forecast", "leaps", "dplyr", "xlsx", "openxlsx", "readxl", "tidyverse", "ggplot2", "cowplot", "tidyr", "reshape","lubridate")

pacman::p_load(packages, character.only = T)


# Import dataset
manual_order <- read_excel("C:\\Users\\thawsu\\Swiss Tropical and Public Health Institute, Swiss TPH\\AIM - AIM Drive\\Country work\\Tanzania\\Tanzania 2026\\ITN impact evaluation\\9. Manuscripts\\data_codes\\Input_data\\manual_orders.xlsx")

input_data <- read_excel("C:\\Users\\thawsu\\Swiss Tropical and Public Health Institute, Swiss TPH\\AIM - AIM Drive\\Country work\\Tanzania\\Tanzania 2026\\ITN impact evaluation\\9. Manuscripts\\data_codes\\Input_data\\impact_analysis_inputdata.xlsx")


#function to fit model with manual orders--> adjusted orders from auto.arima through inspection of ACF and PACF plots
fit_auto_arima_with_exog = function(ts_var, exog_vars, merged_df) {
  xreg = as.matrix(merged_df[exog_vars])
  model = Arima(ts_var, 
                xreg = xreg, 
                method ="ML", 
                seasonal = c(P,D,Q),
                order =c(p,d,q))
  
  return(list(model = model, exog_vars = exog_vars))
}


District_list = unique(input_data$District)


for(district_idx in seq_along(District_list)) {
district_idx=1 #running code for one district Bahi
result_folder = "C:\\Users\\thawsu\\Swiss Tropical and Public Health Institute, Swiss TPH\\AIM - AIM Drive\\Country work\\Tanzania\\Tanzania 2026\\ITN impact evaluation\\9. Manuscripts\\data_codes\\Results"
result_file_aic <- paste0(result_folder, "\\AICc\\", District_list[district_idx], "_AIC.csv")
result_file_model_ITN = paste0(result_folder, "/Model/", District_list[district_idx], "_best_model_with_ITN.rds")

# Subset the data for the current distrcit
data = input_data %>% dplyr::filter( District == District_list[district_idx] )

#specif the manual orders to be used for model fit--> This step comes after using orders from auto.arima function
orders = manual_order %>% dplyr::filter( District == District_list[district_idx] )

p=orders$newp
d=orders$newd
q=orders$newq
P=orders$newP
D=orders$newD
Q=orders$newQ

# data processing
data$date<-as.Date(data$date)

data<-data[order(data$date),]

#create lags
data <- data%>% group_by(District)%>%
  mutate(
    NDVI_lag1 = lag(NDVI, 1),
    NDVI_lag2 = lag(NDVI, 2),
    NDVI_lag3 = lag(NDVI, 3),
    NDVI_lag4 = lag(NDVI, 4),
    NDVI_lag5 = lag(NDVI, 5),
    Temperature_lag1 = lag(Temperature, 1),
    Temperature_lag2 = lag(Temperature, 2),
    Temperature_lag3 = lag(Temperature, 3),
    Temperature_lag4 = lag(Temperature, 4),
    Temperature_lag5 = lag(Temperature, 5),
    Rainfall_lag1 = lag(Rainfall, 1),
    Rainfall_lag2 = lag(Rainfall, 2),
    Rainfall_lag3 = lag(Rainfall, 3),
    Rainfall_lag4 = lag(Rainfall, 4),
    Rainfall_lag5 = lag(Rainfall, 5)
    
  )

#define the periods
start_date = min(data$date)
startY <- as.numeric(format(start_date,'%Y'))
startM <- as.numeric(format(start_date," %m"))
end_date = max(data$date)
endY <- as.numeric(format(end_date,'%Y'))
endM <- as.numeric(format(end_date," %m"))
deployment_date=data[1,4]
deployment_date=deployment_date$deployment_date
deploymentM <-  as.numeric(format(deployment_date," %m"))
deploymentY <-  as.numeric(format(deployment_date,'%Y'))
datebeforedeployment <- as.Date(as.yearmon(deployment_date) - 1/12)
monthbeforedeployment <-  as.numeric(format(datebeforedeployment," %m"))
yearbeforedeployment <-  as.numeric(format(datebeforedeployment,'%Y'))

print("Create the time series for the incidence for the pre-intervention period")
ts_incidence = ts(log(data$incidence), start=c(startY,startM), 
                  end=c(yearbeforedeployment, monthbeforedeployment), frequency = 12)
ts_RR_month = ts(log(data$RR_month), start=c(startY,startM), 
                 end=c(yearbeforedeployment, monthbeforedeployment), frequency = 12)
ts_ITN_access = ts(log(data$ITNaccess_campaign), start=c(startY,startM), 
                   end=c(yearbeforedeployment, monthbeforedeployment), frequency = 12)

# Initialize the covariate matrix

merged_covariates = cbind(ts_incidence,ts_ITN_access,ts_RR_month)

colnames_cov = c("Incidence", "ITN_access","RR_month")


print("Select the best lag for each covariate for the pre-intervention period")

#NDVI
ts_covariate_NDVI = ts(log(data$NDVI), start = c(startY, startM), 
                       end = c(yearbeforedeployment, monthbeforedeployment), 
                       frequency = 12)

# Calculate cross-correlations 
cross_corr = ccf(ts_incidence, ts_covariate_NDVI, lag.max =3, plot = FALSE)
positive_lags <- cross_corr$lag >= 0

positive_cross_correlation <- cross_corr$acf[positive_lags]

positive_lag_values <- cross_corr$lag[positive_lags]
max_positive_correlation <- max(positive_cross_correlation)

lag_with_max_positive_correlation <- positive_lag_values[which.max(positive_cross_correlation)]
max_lag_idx = which.max(abs(cross_corr$acf[positive_lags]))

max_lag_idx = which.max(abs(  positive_cross_correlation ))

#Construct the lagged covariate name
if (max_lag_idx > 1) {
  covariate_name_NDVI = paste0("NDVI", "_lag", max_lag_idx-1)
  ts_covariate_NDVIlag = ts(log(data[, covariate_name_NDVI]), start = c(startY, startM), 
                            end = c(yearbeforedeployment, monthbeforedeployment), frequency = 12)
} else {
  covariate_name_NDVI = "NDVI"
  ts_covariate_NDVIlag = ts(log(data$NDVI), start = c(startY, startM), 
                            end = c(yearbeforedeployment, monthbeforedeployment), frequency = 12)
}

#Temperature
ts_covariate_Temperature = ts(log(data$Temperature), start = c(startY, startM), 
                              end = c(yearbeforedeployment, monthbeforedeployment), 
                              frequency = 12)  
# Calculate cross-correlations 
cross_corr = ccf(ts_incidence, ts_covariate_Temperature, lag.max =3, plot = FALSE)
positive_lags <- cross_corr$lag >= 0

positive_cross_correlation <- cross_corr$acf[positive_lags]

# positive_lag_values <- cross_corr$lag[positive_lags]
max_positive_correlation <- max(positive_cross_correlation)

max_lag_idx = which.max(abs(  positive_cross_correlation ))

# Construct the lagged covariate name
if (max_lag_idx > 1) {
  covariate_name_Temperature = paste0("Temperature", "_lag", max_lag_idx-1)
  ts_covariate_Temperaturelag = ts(log(data[, covariate_name_Temperature]), start = c(startY, startM), 
                                   end = c(yearbeforedeployment, monthbeforedeployment), frequency = 12)
} else {
  covariate_name_Temperature = "Temperature"
  ts_covariate_Temperaturelag = ts(log(data$Temperature), start = c(startY, startM), 
                                   end = c(yearbeforedeployment, monthbeforedeployment), frequency = 12)
}


#Rainfall

ts_covariate_Rainfall = ts(log(data$Rainfall), start = c(startY, startM), 
                           end = c(yearbeforedeployment, monthbeforedeployment), 
                           frequency = 12)

# Calculate cross-correlations 
cross_corr = ccf(ts_incidence, ts_covariate_Rainfall, lag.max =3, plot = FALSE)
positive_lags <- cross_corr$lag >= 0

positive_cross_correlation <- cross_corr$acf[positive_lags]

# positive_lag_values <- cross_corr$lag[positive_lags]
max_positive_correlation <- max(positive_cross_correlation)

max_lag_idx = which.max(abs(  positive_cross_correlation ))

# Construct the lagged covariate name
if (max_lag_idx > 1) {
  covariate_name_Rainfall = paste0("Rainfall", "_lag", max_lag_idx-1)
  ts_covariate_Rainfalllag = ts(log(data[, covariate_name_Rainfall]), start = c(startY, startM), 
                                end = c(yearbeforedeployment, monthbeforedeployment), frequency = 12)
} else {
  covariate_name_Rainfall = "Rainfall"
  ts_covariate_Rainfalllag = ts(log(data$Rainfall), start = c(startY, startM), 
                                end = c(yearbeforedeployment, monthbeforedeployment), frequency = 12)
}

# Create the merged covariates matrix
merged_covariates = cbind(merged_covariates, ts_covariate_NDVIlag,ts_covariate_Temperaturelag,ts_covariate_Rainfalllag)
colnames_cov = c(colnames_cov, covariate_name_NDVI,covariate_name_Temperature,covariate_name_Rainfall)

colnames(merged_covariates) = colnames_cov

# Just checking the series
autoplot(merged_covariates)

exogenous_vars = colnames_cov[2:length(colnames_cov)]
merged_covariates = transform(as.data.frame(merged_covariates))

# Generate all possible combinations of exogenous variables
exog_combinations = lapply(1:length(exogenous_vars), function(i) combn(exogenous_vars, i, simplify = FALSE))
exog_combinations = unlist(exog_combinations, recursive = FALSE)

# Initialize a dataframe to store AIC values for the current district
aic_df = data.frame(Model_Index = integer(), AICc_Value = numeric(), 
                    AIC_Value = numeric(), Exogenous_Variables = character(), 
                    District = character(), stringsAsFactors = FALSE)

# Fit auto.arima models for each combination of exogenous variables
model = list()
for (j in seq_along(exog_combinations)) { #seq_along(exog_combinations)
  # Fit the model
  print(paste("Fit model for combination", j))
  model[[j]] = fit_auto_arima_with_exog(ts_incidence, 
                                        exog_combinations[[j]],
                                        merged_covariates)
  
  aic_value = model[[j]]$model$aic
  
  aicc_value = (model[[j]]$model$aicc)
  
  # Get the combination of exogenous variables for the current model
  exog_vars = paste(model[[j]]$exog_vars, collapse = ", ")
  
  # Append the AIC value and combination of exogenous variables to the dataframe
  aic_df = rbind(aic_df, data.frame(Model_Index = j, AIC_Value = aic_value, 
                                    AICc_Value = aicc_value, 
                                    Exogenous_Variables = exog_vars, 
                                    District=District_list[district_idx]))
  #if you want to save all model combinations for further analysis
  #output_folder="C:\\Users\\thawsu\\Swiss Tropical and Public Health Institute, Swiss TPH\\AIM - AIM Drive\\Country work\\Tanzania\\Tanzania 2026\\ITN impact evaluation\\9. Manuscripts\\data_codes\\Results\\Model_combinations"
  #model_filename <- file.path(output_folder, paste0("model_", j, District_list[district_idx], ".rds"))
  #saveRDS(model[[j]], model_filename)

}

# Write data frame with AIC values for the fitted models for the given district to file
write.csv(aic_df, result_file_aic)

#Save best model forcing ITN access as covariate based on lowest AICc in rds
aic_df_with_ITN = aic_df %>% filter(grepl("ITN_access", Exogenous_Variables))

aic_df_with_ITN$Exogenous_Variables<-as.character(aic_df_with_ITN$Exogenous_Variables)

best_combination_with_ITN = aic_df_with_ITN %>%
  filter(AICc_Value == min(AICc_Value)) %>%
  pull(Exogenous_Variables) %>%
  strsplit(",")

best_combination_with_ITN = unlist(best_combination_with_ITN, recursive = FALSE)
best_combination_with_ITN = gsub("\\s+", "", best_combination_with_ITN)
xreg_with_ITN = as.matrix(merged_covariates[best_combination_with_ITN])
best_model_with_ITN = Arima(ts_incidence, 
                            xreg = xreg_with_ITN, 
                            method ="ML", 
                            seasonal = c(P,D,Q),
                            order =c(p,d,q))
saveRDS(best_model_with_ITN, file = result_file_model_ITN)
}

