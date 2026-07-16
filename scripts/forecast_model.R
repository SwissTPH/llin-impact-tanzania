###########################################################
#Forecast counterfactual for selected models
#############################################################

packages= c("tseries","ggpubr","zoo","forecast", "leaps", "dplyr", "xlsx", "openxlsx", "readxl", "tidyverse", "ggplot2", "cowplot", "tidyr", "reshape","lubridate","vroom","reshape2")

pacman::p_load(packages, character.only = T)

#Import data and best model
input_data <- read_excel("filepath\\impact_analysis_inputdata.xlsx")

result_folder = "filepath\\Results"

# Function to import the model for a specific district name
import_model_by_district <- function(district_name) {
  # Construct the file path for the RDS file based on the district name
  file_path <- paste0("filepath\\Model\\", District_list[district_idx], "_best_model_with_ITN.rds")
   
  # Load the model from the RDS file
  model <- readRDS(file_path)
  
  # Return the loaded model
  return(model)
}

#---------------------------------------
#forecast
#---------------------------------------
District_list = unique(input_data$District)

coeff_list <- list()
merged_list <- list()

# Significance level
alpha <- 0.05 

for(district_idx in seq_along(District_list)) {

  # Result file
  result_file = paste0(result_folder,"\\Forecast_result\\", District_list[district_idx], "_forecast.csv")
  
  best_model_with_ITN <- import_model_by_district(District_list[district_idx])
  
  #Residual plots
  setwd("filepath\\Results\\residual_plots")
  png(paste0(District_list[district_idx],"residuals_combined.png"))
  checkresiduals(best_model_with_ITN)
  dev.off()
  
  # Extract coefficients and their standard errors directly from the model
  coefficients <- coef(best_model_with_ITN)
  std_errors <-  sqrt(diag(vcov(best_model_with_ITN))) 
  
  # Compute the t-statistic
  t_stats <- coefficients / std_errors
  
  # Define the degrees of freedom
  n <- length(best_model_with_ITN$residuals)  # Number of observations
  p <- length(coefficients)     # Number of parameters (coefficients)
  df <- n - p                   # Degrees of freedom
  
  # Compute the p-values for two-tailed test
  p_values <- 2 * (1 - pt(abs(t_stats), df))
  
  
  # Extract sigma2 (residual variance)
  sigma2 <-best_model_with_ITN$sigma2
  
  # Determine significance (p-value < alpha)
  significance <- p_values < alpha
  
  # Extract (p, d, q) orders
  p <- best_model_with_ITN$arma[1]
  q <- best_model_with_ITN$arma[2]
  P <- best_model_with_ITN$arma[3]
  Q <- best_model_with_ITN$arma[4]
  d <- best_model_with_ITN$arma[6]
  D <- best_model_with_ITN$arma[7]
  
  
  #Ljung-box test for residual autocorrelation
  lb_test <- Box.test(best_model_with_ITN$residuals, lag = min(10, length(best_model_with_ITN$residuals)/5), type = "Ljung-Box")
  
  # Extract p-value
  Ljungpvalue<- lb_test$p.value

  #data processing
  data = input_data %>% dplyr::filter(District == District_list[district_idx])
  
  data$date<-as.Date(data$date)
  
  data$deployment_date<-as.Date(data$deployment_date)
  
  data<-data[order(data$date),]
  
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
  #define the period
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
  
  datebeforedeployment =  deployment_date %m+% months(-1)
  monthbeforedeployment <-  as.numeric(format(datebeforedeployment," %m"))
  yearbeforedeployment <-  as.numeric(format(datebeforedeployment,'%Y'))
  
  twelvedatepostintervention =  deployment_date %m+% months(+11)
  twelvedatepostinterventionY<-as.numeric(format(twelvedatepostintervention,'%Y'))
  twelvedatepostinterventionM<-as.numeric(format(twelvedatepostintervention," %m"))
  
  #rename columns in lagged_df to match covariate names saved in model
  data<-data %>% 
    dplyr::rename("ITN_access"="ITNaccess_hillsteady" ) #for forecasting counterfactual, LLIN access was fixed at its pre-intervention level using a hill decay function

  #convert to time series object
  ts_incidence = ts(log(data$incidence), start=c(startY,startM), 
                end=c(yearbeforedeployment, monthbeforedeployment), frequency = 12)
  
  autoplot(ts_incidence)

  #decompose the data
  mergedcomponents <- decompose(ts_incidence,filter = rep(1/12, 12))
  trend = mergedcomponents$trend
  
  setwd("filepath\\Results\\Decomposition_plots")
  png(paste0(District_list[district_idx],".png"))
  plot(mergedcomponents)+title((main = paste(District_list[district_idx],"(",p,d,q,")","(",P,D,Q,")")))
  dev.off()
  
  #Save acf and pacf plots
  if (d > 0) {
    ts_to_plot <- diff(ts_incidence, differences = d)
  } else {
    ts_to_plot <- ts_incidence
  }
  
  # Generate ACF and PACF plots
  acf_plot <- ggAcf(ts_to_plot) + ggtitle(paste("ACF for", District_list[district_idx]))
  pacf_plot <- ggPacf(ts_to_plot) + ggtitle(paste("PACF for", District_list[district_idx]))
  
  setwd("filepath\\Results\\ACF_PACF_plots")
  
  # Combine and save plots
  combined_plot <- ggarrange(acf_plot, pacf_plot, ncol = 2, nrow = 1)
  ggsave(
    filename = paste0("ACF_PACF_", District_list[district_idx], ".png"),
    plot = combined_plot,
   width = 12, height = 6)
  
  #Dicker fuller test for stationarity
  adf_result <- adf.test(ts_incidence)
  
  # Extract the p-value
  Dickerpvalue <- adf_result$p.value
  
  # Create a data frame for the current model's coefficients, standard errors, t-stats, p-values, and sigma2
  model_df <- data.frame(
    District = paste0(District_list[district_idx]),
    term = names(coefficients),
    estimate = coefficients,
    std_error = std_errors,
    t_stat = t_stats,
    p_value = p_values,
    significant = significance,
    sigma2 = sigma2,
    p=p,
    d=d,
    q=q,
    P=P,
    D=D,
    Q=Q,
    Dickerfullertest=Dickerpvalue,
    Ljungboxtst=Ljungpvalue
  )
  
  # Append to the results list
  coeff_list[[District_list[district_idx]]] <- model_df
  
  #create matrix of covariates
  #Get the column names of the covariates from the saved model
  covariate_names <- colnames(best_model_with_ITN$xreg)
  
  #Select the columns from the dataframe
  # xreg = (data[covariate_names])
  data <- data[, c( "date", "incidence", covariate_names)]
  
  #log transform the data
  data[, -1] <- log(data[, -1])
  
  ts_data <- ts(data[, -1], start = c(startY, startM), frequency = 12)
  ts_subset <- window(ts_data, start = c(deploymentY,deploymentM), end=c(twelvedatepostinterventionY, twelvedatepostinterventionM), frequency = 12)
  ts_autoplot <- window(ts_data, start = c(startY, startM), end=c(twelvedatepostinterventionY, twelvedatepostinterventionM), frequency = 12)
  
  # Checking the series
  autoplot(ts_autoplot)
  
  # Create the plot using autoplot
  date <- as.Date(deployment_date)
  
  # Extract the year and month components
  year <- as.numeric(format(date, "%Y"))
  month <- as.numeric(format(date, "%m"))
  
  # Calculate the month fraction
  month_fraction <- (month - 1) / 12  # Subtract 1 to start from 0 for January
  
  # Combine the year and month fractions
  decimal_date <- year + month_fraction
  
  plot<-forecast::autoplot(ts_autoplot )  +
    ggtitle(District_list[district_idx])+
    geom_vline(xintercept = decimal_date, linetype = "dashed", color = "black")
  
  setwd("filepath\\Results\\Covariate_autoplots")
  print(plot)
  ggsave((paste0( District_list[district_idx], "_autoplot",".png")), width=10, height=7, dpi=300)
 
  #transform to dataframe
  ts_subset_df = transform(as.data.frame(ts_subset))  
  xreg = as.matrix(ts_subset_df[covariate_names])
  
  #Forecast
  fc <- forecast(best_model_with_ITN, xreg = xreg , h = 12, level=c(95))
  autoplot(fc)
  fitted <- ts(as.numeric(fc$mean), start=c(deploymentY,deploymentM), frequency=12)
  fc.ts <- ts(as.numeric(fc$mean), start=c(deploymentY,deploymentM), frequency=12)
  Lowpi <- ts(as.numeric(fc$upper), start=c(deploymentY,deploymentM), frequency=12)
  Highpi <- ts(as.numeric(fc$lower), start=c(deploymentY,deploymentM), frequency=12)
  
  ts_data.forecast <- ts.union( fitted, fc.ts,Lowpi, Highpi)
  
  ts_data.forecast_df <- transform(as.data.frame(ts_data.forecast))
  
  #data format to merge with observed values
  #add date column
  h=length(seq(from=deployment_date, length.out = 12, by='month')) 
  date = seq(from =  deployment_date, length.out = h, by = 'month')
  ts_data.forecast_df <-cbind(ts_data.forecast_df ,date)
  
  ts_data.forecast_df['Lowci'] <- NA
  ts_data.forecast_df['Highci'] <- NA
  col_order <- c("date", "fitted", "fc.ts","Lowci", "Highci", "Lowpi","Highpi")
  ts_data.forecast_df<- ts_data.forecast_df[, col_order]
  
  #merge with fitted values of best_model
  fitted_all_df = cbind(
    c(fitted(best_model_with_ITN)),
    c(fitted(best_model_with_ITN) - (2*(best_model_with_ITN)$sigma2)),
    c(fitted(best_model_with_ITN) + (2*(best_model_with_ITN)$sigma2)))
  
  fitted_all_df<- transform(as.data.frame(fitted_all_df))
  
  colnames(fitted_all_df) = c("fitted", "Lowci", "Highci")
  
  date = seq(from = start_date, to = as.Date(datebeforedeployment), by = 'month')
  
  fitted_all_df<-cbind(fitted_all_df,date)
  fitted_all_df['Lowpi'] <- NA
  fitted_all_df['Highpi'] <- NA
  fitted_all_df['fc.ts'] <- NA
  col_order <- c("date", "fitted","fc.ts", "Lowci", "Highci", "Lowpi","Highpi")
  fitted_all_df<-  fitted_all_df[, col_order]
  
  fitted_all_df<-rbind(fitted_all_df,ts_data.forecast_df)
  
  #exponentiate
  fitted_all_df$expfitted<-c(exp(fitted_all_df$fitted))
  fitted_all_df$expLowci<-c(exp(fitted_all_df$fitted-(2*(best_model_with_ITN)$sigma2)))
  fitted_all_df$expHighci<-c(exp(fitted_all_df$fitted+(2*(best_model_with_ITN)$sigma2)))
  
  # Combine with observed data
  #to reintroduce the rows that were previously deleted
  data = input_data %>% dplyr::filter( District == District_list[district_idx] )
  
  observed<-data[c("District","date","incidence")]
  
  h=length(seq(from=deployment_date, length.out = 12, by='month')) 
  Date1 = seq(from =  as.Date(deployment_date), length.out = h, by = 'month')
  Date2 = seq(from = start_date, to=as.Date(datebeforedeployment), by = 'month')
  
  Date1 <- data.frame(Column1 = Date1)
  Date2 <- data.frame(Column1 = Date2)
  appended_dates <- rbind(Date1, Date2)
  colnames(appended_dates) = c("date")
  
  
  observed <-  appended_dates %>% 
    mutate(code = as.character(date)) %>% 
    left_join(  observed, by="date")
  
  merged_df <-observed %>% 
    mutate(code = as.character(date)) %>% 
    left_join(fitted_all_df, by="date")
  
  merged_df$Log_incidence<-c(log(merged_df$incidence))
  
  merged_df$date<-as.Date(merged_df$date)
  merged_df<- merged_df[order( merged_df$date),]
  merged_df$expLowpi<-c(exp(merged_df$Lowpi))
  merged_df$expHighpi<-c(exp(merged_df$Highpi))
  merged_df$expfc.ts<-c(exp(merged_df$fc.ts))
  merged_df$expLowci<-c(exp(merged_df$Lowci))
  merged_df$expHighci<-c(exp(merged_df$Highci))
  
  merged_df$expLowci<-c(exp(merged_df$fitted-(2*(best_model_with_ITN)$sigma2)))
  merged_df$expHighci<-c(exp(merged_df$fitted+(2*(best_model_with_ITN)$sigma2)))
  
  #compute absdiff and % of cases averted (ratio)
  merged_df <- merged_df %>%
    mutate(diff= expfc.ts - incidence)
  
  #remove NA values so as to get incidence post intervention values to compute abs diff
  merged_df2 <- merged_df[!is.na(merged_df$expfc.ts),]
  
  merged_df2$expfc.ts_lowci<-c(exp(merged_df2$fitted-(2*(best_model_with_ITN)$sigma2)))
  merged_df2$expfc.ts_highci<-c(exp(merged_df2$fitted+(2*(best_model_with_ITN)$sigma2)))
  
  merged_df2 <- merged_df2 %>%
    mutate(diff_low= expfc.ts_lowci - incidence, diff_high=expfc.ts_highci-incidence)
  
  #merge back to original dataset
  merged_df$concatenate<-paste(merged_df$District,merged_df$date)
  merged_df2$concatenate<-paste(merged_df2$District, merged_df2$date)
  
  merged_df <-merged_df %>% 
    mutate(code = as.character(concatenate)) %>% 
    left_join( merged_df2, by="concatenate")
  
  #select required columns and rename
  merged_df<-dplyr::select(
    merged_df, District.x, date.x, incidence.x,fitted.x, fc.ts.x,expfitted.x,expLowci.x, expHighci.x,incidence.y,expfc.ts.x, diff.x , diff_low, diff_high,expfc.ts_highci,expfc.ts_lowci)
  
  merged_df<-merged_df %>% 
    dplyr::rename("diff" = "diff.x", "District" = "District.x", 
                  "date" = "date.x", "expfc.ts" = "expfc.ts.x",
                  "incidence" = "incidence.x", "incidence_observedp.i" = "incidence.y",
                  "fitted" = "fitted.x", "fc.ts" = "fc.ts.x", 
                  "expfitted" = "expfitted.x", "expLowci" = "expLowci.x",
                  "expHighci" = "expHighci.x")
  
  #Confidence interval for forecast values
  #apply the sigma from model to the forecast values to get the confidence bounds
  merged_df <-merged_df%>%
    group_by(District)%>%
    mutate(S1=sum(diff,na.rm=TRUE), 
           S2=sum(expfc.ts,na.rm=TRUE),
           S3=sum(incidence_observedp.i, na.rm=TRUE),
           S1_low=sum(diff_low,na.rm=TRUE),
           S1_high=sum(diff_high,na.rm=TRUE),
           S2_low=sum(expfc.ts_lowci,na.rm=TRUE),
           S2_high=sum(expfc.ts_highci,na.rm=TRUE))
  
  merged_df$ratio<-c(merged_df$S1/merged_df$S2)
  merged_df$ratio_low<-c(merged_df$S1_low/merged_df$S2_low)
  merged_df$ratio_high<-c(merged_df$S1_high/merged_df$S2_high)

  merged_df$absdiff<-c(merged_df$S2-merged_df$S3)
  merged_df$absdiff_low<-c(merged_df$S2_low-merged_df$S3)
  merged_df$absdiff_high<-c(merged_df$S2_high-merged_df$S3)
  
  merged_df$District <- District_list[district_idx]
  merged_list[[District_list[district_idx]]] <- merged_df
  
  write.csv(merged_df, result_file)
  
  # %of cases averted/1000
  ratio=merged_df[1,23]
  ratio=ratio$ratio
  
  #absolute differnce in cases/1000 averted
  absdiff=merged_df[1,26]
  absdiff= absdiff$ absdiff
  
  setwd("filepath\\Results\\Forecast_plots")
  
  plot <- ggplot(merged_df, aes(x = date)) +
    
    # Counterfactual CI
    geom_ribbon(
      aes(ymin = expLowci,
          ymax = expHighci),
      fill = "grey50",
      alpha = 0.3
    ) +
    
    # Observed incidence
    geom_line(
      aes(y = incidence,
          colour = "Observed"),
      linewidth = 1.8        
    ) +
    
    geom_point(
      aes( y=incidence),
      color="#1F77B4",
      size=4) +
    
    # Fitted model
    geom_line(
      aes(y = expfitted,
          colour = "Model fitted"),
      linewidth = 1.8,       
      linetype = "solid"
    ) +
    
    geom_point(
      aes( y=expfitted),
      color="#D62728",
      size=4)+
  
    # Counterfactual
    geom_line(
      aes(y = expfc.ts,
          colour = "Counterfactual"),
      linewidth = 1.8,      
      linetype = "solid"
    ) +
      geom_point(
        aes( y=expfc.ts),
        color="#2CA02C",
        size=4)+
      
    # Intervention date
    geom_vline(
      xintercept = as.Date(deployment_date),
      linetype = "longdash",
      linewidth = 1.0,       
      colour = "black"
    ) +
  
    
    scale_colour_manual(
      values = c(
        "Observed" = "#1F77B4",
        "Model fitted" = "#D62728",
        "Counterfactual" = "#2CA02C"
      )
    ) +
    
    labs(
      title = District_list[district_idx],
      subtitle = paste0(
        "% cases averted:",
        round(ratio * 100, 1),
        "%",
        ";",
        " Absolute difference:",
        round(absdiff, 1)
      ),
      x = "Year",
      y = "Malaria incidence per 1,000 population",
      colour = NULL
    ) +
    
    theme_classic(base_size = 20) +   
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 26            
      ),
      plot.subtitle = element_text(
        size = 24         
      ),
      axis.title.y = element_text(
        face = "bold",
        size = 22           
      ),
      axis.text = element_text(
        size = 22          
      ),
      legend.position = "bottom",
      legend.text = element_text(size = 18),   
      legend.key.size = unit(1.5, "cm"),     
      legend.spacing.x = unit(0.5, "cm")        
    )
  
  print(plot)
  
  ggsave((paste0( District_list[district_idx], "_Forecast",".png")), width=10, height=7, dpi=300)

}


all_merged_df <- dplyr::bind_rows(merged_list)
coeff_df <- do.call(rbind, coeff_list)

#-----------------------------------------------------
# Produce plots per region (Supplementary Figure S4)
#-----------------------------------------------------

#merge to region name
names_correspond <- read_excel("filepath\\Input_data\\names_correspond.xlsx")

all_merged_df   <-all_merged_df   %>%
  left_join(names_correspond, by = "District")

district_summary <- all_merged_df %>%
  group_by(District) %>%
  summarise(
    cases_averted_pct = first(ratio) * 100
  )

all_merged_df <- all_merged_df %>%
  left_join(district_summary, by = "District")

all_merged_df <- all_merged_df %>%
  mutate(District_label = paste0(District, "\n", "% Cases averted:", round(cases_averted_pct, 1), "\n", "Absolute difference:", round(absdiff, 1)))

Region_list <- unique(all_merged_df $Region)

for(region_idx in seq_along(Region_list)) {
  
  region_data <- all_merged_df %>%
    filter(Region == Region_list[region_idx])
  
  plot <- ggplot(region_data, aes(x = date)) +
    
    geom_ribbon(
      aes(ymin = expLowci,
          ymax = expHighci),
      fill = "grey50",
      alpha = 0.3
    ) +
    
    geom_line(
      aes(y = incidence,
          colour = "Observed"),
      linewidth = 1.8       
    ) +
    
    geom_point(
      aes( y=incidence),
      color="#1F77B4",
      size=3)+
    
    geom_line(
      aes(y = expfitted,
          colour = "Model fitted"),
      linewidth = 1.8       
    ) +
    geom_point(
      aes( y=expfitted),
      color="#D62728",
      size=3)+
    
    geom_line(
      aes(y = expfc.ts,
          colour = "Counterfactual"),
      linewidth = 1.8    
    ) +
    geom_point(
      aes( y=expfc.ts),
      color="#2CA02C",
      size=3)+
    
    geom_vline(
      xintercept = as.Date(deployment_date),
      linetype = "longdash",
      linewidth = 1.0       
    ) +
    
    facet_wrap(~ District_label, scales = "free_y", ncol = 3) +
    
    scale_colour_manual(
      values = c(
        "Observed" = "#1F77B4",
        "Model fitted" = "#D62728",
        "Counterfactual" = "#2CA02C"
      )
    ) +
    
    labs(
      title = Region_list[region_idx],
      x = "Year",
      y = "Malaria incidence per 1,000 population",
      colour = NULL
    ) +
    
    theme_classic(base_size = 20) +  
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 22,           
        hjust = 0.5
      ),
      strip.text = element_text(
        face = "bold",
        size = 18             
      ),
      axis.title.y = element_text(
        face = "bold",
        size = 20         
      ),
      axis.text = element_text(
        size = 16             
      ),
      legend.position = "bottom",
      legend.text = element_text(size = 18),    
      legend.key.size = unit(1.5, "cm"),         
      legend.spacing.x = unit(0.5, "cm")         
    )
  
  print(plot)
  
  ggsave(
    paste0(Region_list[region_idx], "_Forecast_Facets.png"),
    plot = plot,
    width = 16,    
    height = 12,   
    dpi = 300
  )
}

#---------------------------------------
# Covariate matrix heat map (Figure 4)
#---------------------------------------

# Get exogenous variables names
setwd("filepath\\Results\\AICc")
AIC_files <- list.files(path = "filepath\\Results\\AICc")

#merge the AIC files for all districts
df.list <- lapply(AIC_files, function(filename) {
  print(paste("Merging",filename,sep=" "))
  read_csv(filename)
})

AIC_results<- as.data.frame(do.call("rbind", df.list))

aic_df_with_ITN = AIC_results %>% filter(grepl("ITN_access", Exogenous_Variables))

AICc_min<-aic_df_with_ITN %>%
  group_by( District)%>%
  summarize(
    AICc_min = min(AICc_Value),
    AICc = which.min(AICc_Value),
    Exogenous_Variables =  Exogenous_Variables[AICc],
    Model_Index =  Model_Index [AICc])

words <- paste(c("District", "Council"), collapse = "|")
AICc_min$District2<-trimws(gsub(words, "\\1", AICc_min$District))

# Split the Exogenous_variable column into a list of covariates
AICc_min$Exogenous_list <- strsplit(AICc_min$Exogenous_Variables, ", ")

# Get unique covariate names
covariates <- unique(unlist(AICc_min$Exogenous_list))

# Create a matrix indicating the presence of each covariate for each area
cov_matrix <- sapply(covariates, function(cov) {
  ifelse(sapply(AICc_min$Exogenous_list, function(x) cov %in% x), 1, 0)
})

# Convert the matrix to a dataframe
cov_df <- as.data.frame(cov_matrix)

# Set row names to Areas
rownames(cov_df) <- AICc_min$District2

# Set column names to Covariate names
colnames(cov_df) <- covariates

melted_cov_df <- melt(cov_df, varnames = c("Area", "Covariate"))

# Add 'Area' column to the melted dataframe
melted_cov_df$District <- rownames(cov_df)

#arrange alphabetically
melted_cov_df$variable <- factor(melted_cov_df$variable, levels = sort(levels(factor(melted_cov_df$variable))))

melted_cov_df$variable <- gsub("RR_month", "Reporting rate", melted_cov_df$variable)

p <- ggplot(data = melted_cov_df,
            aes(x = District, y = variable, fill = as.factor(value))) +
  
  geom_tile(
    color     = "grey60",    
    linewidth = 0.5
  ) +
  

  scale_fill_manual(
    values = c("0" = "white", "1" = "black"),
    labels = c("0" = "No",    "1" = "Yes"),
    name   = NULL
  ) +
  
  # Districts ordered by covariate completeness
  scale_x_discrete(limits = melted_cov_df %>%
                     group_by(District) %>%
                     summarise(n = sum(value, na.rm = TRUE)) %>%
                     arrange(desc(n)) %>%
                     pull(District)) +
  
  labs(
    x       = "Councils",
    y       = "Covariates"
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    # ── District labels ───────────────────────────────────────────────────────
    axis.text.x = element_text(
      angle  = 45,
      hjust  = 1,
      vjust  = 1,
      size   = 8,
      colour = "black",
      face   = "bold"
    ),
    
    # ── Covariate labels ──────────────────────────────────────────────────────
    axis.text.y = element_text(
      size   = 9,
      colour = "black",
      face   = "bold",
      hjust  = 1
    ),
    
    # ── Legend ────────────────────────────────────────────────────────────────
    legend.position  = "top",
    legend.direction = "horizontal",
    legend.key.size  = unit(0.45, "cm"),
    legend.key.width = unit(0.65, "cm"),
    legend.text      = element_text(size  = 10,
                                    face  = "bold",
                                    colour = "black"),
    legend.key       = element_rect(colour = "grey60",   
                                    linewidth = 0.5),
    legend.margin    = margin(b = 4),
    legend.spacing.x = unit(0.4, "cm"),
    
    panel.grid   = element_blank(),
    
    panel.border = element_rect(colour    = "black",
                                fill      = NA,
                                linewidth = 0.8),
    
    axis.ticks       = element_line(colour    = "black",
                                    linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    
    plot.caption = element_text(size   = 8,
                                colour = "black",
                                face   = "italic",
                                hjust  = 0,
                                margin = margin(t = 6)),
    
    plot.margin = margin(6, 12, 6, 6)
  )

print(p)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave(
  "covariate_heatmap.png",
  plot   = p,
  width  = 24,
  height = 10,
  units  = "cm"
)


#------------------------------------------------------------------------------
# Bar plot of impact indicators by strata with confidence interval (Figure 6)
#------------------------------------------------------------------------------
setwd("filepath\\Results")

Strata_2020<- read_excel("filepath\\Input_data\\Strata_2020.xlsx")

Forecast_files <- list.files(
  path = "filepath\\Results\\Forecast_result",
  pattern = "_forecast\\.csv$",
  full.names = TRUE
)

df.list <- lapply(Forecast_files, function(filename) {
  print(paste("Merging",filename,sep=" "))
  read_csv(filename)
})

Forecast_results<- as.data.frame(do.call("rbind", df.list))

Forecast_results2<-Forecast_results %>%
  group_by(Region,District)%>%
  summarise(ratio=mean(ratio,na.rm=TRUE ),S2=mean(S2,na.rm=TRUE ), S1=mean(S1,na.rm=TRUE ), ratio_low=mean(ratio_low,na.rm=TRUE ),ratio_high=mean(ratio_high,na.rm=TRUE ),S1_low=mean(S1_low,na.rm=TRUE ), S1_high=mean(S1_high,na.rm=TRUE ),S2_low= mean(S2_low,na.rm=TRUE ),S2_high=mean(S2_high,na.rm=TRUE ),absdiff=mean(absdiff,na.rm=TRUE ),absdiff_low=mean(absdiff_low,na.rm=TRUE ),absdiff_high=mean(absdiff_high,na.rm=TRUE ))

words <- paste(c("District", "Council"), collapse = "|")
Forecast_results2 $District2<-trimws(gsub(words, "\\1", Forecast_results2$District))

#attach risk strata
Forecast_results2  <- Forecast_results2   %>% 
  mutate(code = as.character(District)) %>% 
  left_join(Strata_2020, by="District")

Forecast_results2 <-Forecast_results2  %>% 
  dplyr::rename("Region" = "Region.x")

group.colors <- c(high = "red", low = "lightgreen", moderate ="orange", 'Very low'= "darkgreen")

#% cases averted
p <- ggplot(Forecast_results2) +
  geom_col(
    mapping = aes(x = reorder(District2, -ratio*100),
                  y = ratio*100,
                  fill = Strata_2020),
    position = position_dodge(width = 0.9)   # dodge grouped bars
  ) +
  geom_errorbar(
    mapping = aes(x = reorder(District2, -ratio*100),
                  ymin = ratio_low*100,
                  ymax = ratio_high*100,
                  group = Strata_2020),            
    position = position_dodge(width = 0.9),
    width = 0.2,
    color = "black"
  ) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = element_blank(),
       y = "% of cases averted",
       caption = "") +
  scale_fill_manual(
    values = group.colors,
    breaks = c("Very low", "low", "moderate", "high"),
    labels = c("Very low", "low", "moderate", "high")
  ) +
  coord_cartesian(ylim = c(-50, 100)) +      # cut scale
  theme(
    axis.title = element_text(size = 14, face = "bold", colour = "black"),
    axis.text = element_text(size = 14, face = "bold", colour = "black"),
    strip.text = element_text(size = 14, face = "bold", colour = "black"),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_line(colour = "grey90", size = 0.2),
    panel.grid.minor = element_line(colour = "grey98", size = 0.5)
  )

print(p)
ggsave((paste0("casesaverted", ".png")), width=10, height=7, dpi=300)

#absolute difference in incidence

p <- ggplot(Forecast_results2) +
  geom_col(
    mapping = aes(x = reorder(District2, -absdiff),
                  y = absdiff,
                  fill = Strata_2020),
    position = position_dodge(width = 0.9)   
  ) +
  geom_errorbar(
    mapping = aes(x = reorder(District2, -absdiff),
                  ymin = absdiff_low,
                  ymax = absdiff_high,
                  group = Strata_2020),           
    position = position_dodge(width = 0.9),
    width = 0.2,
    color = "black"
  ) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = element_blank(),
       y = "Absolute difference",
       caption = "") +
  scale_fill_manual(
    values = group.colors,
    breaks = c("Very low", "low", "moderate", "high"),
    labels = c("Very low", "low", "moderate", "high")
  ) +
  coord_cartesian(ylim = c(-40, 200)) +      #cut scale
  theme(
    axis.title = element_text(size = 14, face = "bold", colour = "black"),
    axis.text = element_text(size = 14, face = "bold", colour = "black"),
    strip.text = element_text(size = 14, face = "bold", colour = "black"),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_line(colour = "grey90", size = 0.2),
    panel.grid.minor = element_line(colour = "grey98", size = 0.5)
  )

print(p)
ggsave((paste0("absolutediff", ".png")), width=10, height=7, dpi=300)
