#######################################
#plots for figures in manuscript
#######################################
#Download packages
packages_needed = c("anomalize","tibbletime","ggpubr","tseries","forecast", "leaps", "dplyr","gsubfn", "xlsx", "openxlsx", "readxl", "tidyverse", "ggplot2", "cowplot", "tidyr", "reshape")
lapply(packages_needed, require, character.only = TRUE)

# ITN access variability
input_data <- read_excel("C:\\Users\\thawsu\\switchdrive\\Institution\\Netcell Project\\Consultancy Swiss TPH\\Manuscripts\\Retrospectiveimpact_TZ\\Codes\\impact_analysis_inputdata.xlsx")

deployment_ITN <- input_data %>%
  filter(!is.na(deployment_date),
         date == deployment_date) %>%
  group_by(District) %>%
  slice(1) %>%
  ungroup()

Admin2 <- st_read(file.path("C:\\Users\\thawsu\\Swiss Tropical and Public Health Institute, Swiss TPH\\AIM - AIM Drive\\Country work\\Tanzania\\Tanzania 2026\\ITN impact evaluation\\9. Manuscripts\\data_codes\\Input_data\\Shapefile\\District edited Jan 2021.shp"))

district_ITN_map <- Admin2 %>%
  left_join(deployment_ITN, by = c("DHIS2_Dist" = "District"))

ggplot(district_ITN_map) +
  geom_sf(aes(fill = ITNaccess_campaign), color = "black", size = 0.15) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "white",
    name = "LLIN access (%)"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )


#Supplementary Figure S1
# Import data 

result_folder <- "C:\\Users\\thawsu\\switchdrive\\Institution\\Netcell Project\\Consultancy Swiss TPH\\Tanzania\\Impact analysis\\ARIMA ITS\\Scicore run\\Model_Fit\\Results\\Forecast_manualorders\\decomposed_plots"


Region_list <- unique(input_data$Region)

for(region_idx in seq_along(Region_list)) {
  
  region_data <- input_data %>%
    filter(Region == Region_list[region_idx])
  
  District_list <- unique(region_data$District)
  
  # Collect decomposition components for all districts in this region
  decomp_list <- list()
  
  for(district_idx in seq_along(District_list)) {
    
    data <- region_data %>%
      filter(District == District_list[district_idx])
    
    data$date <- as.Date(data$date)
    data <- data[order(data$date), ]
    
    data <- data %>%
      group_by(District) %>%
      mutate(
        NDVI_lag1 = lag(NDVI, 1), NDVI_lag2 = lag(NDVI, 2),
        NDVI_lag3 = lag(NDVI, 3), NDVI_lag4 = lag(NDVI, 4),
        NDVI_lag5 = lag(NDVI, 5),
        Temperature_lag1 = lag(Temperature, 1), Temperature_lag2 = lag(Temperature, 2),
        Temperature_lag3 = lag(Temperature, 3), Temperature_lag4 = lag(Temperature, 4),
        Temperature_lag5 = lag(Temperature, 5),
        Rainfall_lag1 = lag(Rainfall, 1), Rainfall_lag2 = lag(Rainfall, 2),
        Rainfall_lag3 = lag(Rainfall, 3), Rainfall_lag4 = lag(Rainfall, 4),
        Rainfall_lag5 = lag(Rainfall, 5)
      )
    
    start_date    <- min(data$date)
    startY        <- as.numeric(format(start_date, '%Y'))
    startM        <- as.numeric(format(start_date, '%m'))
    
    deployment_date      <- data$deployment_date[1]
    datebeforedeployment <- deployment_date %m+% months(-1)
    monthbeforedeployment <- as.numeric(format(datebeforedeployment, '%m'))
    yearbeforedeployment  <- as.numeric(format(datebeforedeployment, '%Y'))
    
    data <- data %>%
      dplyr::rename(
        "ITN_access" = "ITNaccess_TVCA_1_hillsteady"
      )
    
    ts_incidence <- ts(log(data$incidence),
                       start = c(startY, startM),
                       end   = c(yearbeforedeployment, monthbeforedeployment),
                       frequency = 12)
    
    mergedcomponents <- decompose(ts_incidence, filter = rep(1/12, 12))
    
    # Extract components into a tidy dataframe
    dates <- seq(as.Date(paste0(startY, "-", startM, "-01")),
                 by = "month",
                 length.out = length(mergedcomponents$x))
    
    decomp_df <- data.frame(
      date     = dates,
      District = District_list[district_idx],
      observed = as.numeric(mergedcomponents$x),
      trend    = as.numeric(mergedcomponents$trend),
      seasonal = as.numeric(mergedcomponents$seasonal),
      random   = as.numeric(mergedcomponents$random)
    )
    
    decomp_list[[district_idx]] <- decomp_df
  }
  
  # Combine all districts for this region
  region_decomp <- bind_rows(decomp_list) %>%
    pivot_longer(cols = c(observed, trend, seasonal, random),
                 names_to = "component",
                 values_to = "value") %>%
    mutate(component = factor(component,
                              levels = c("observed", "trend", "seasonal", "random"),
                              labels = c("Observed", "Trend", "Seasonal", "Random")))
  
  # Plot faceted by district, with component as colour/panel
  plot <- ggplot(region_decomp, aes(x = date, y = value, colour = component)) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    facet_grid(component ~ District, scales = "free_y") +   # rows = component, cols = district
    scale_colour_manual(values = c(
      "Observed" = "#1F77B4",
      "Trend"    = "#D62728",
      "Seasonal" = "#2CA02C",
      "Random"   = "#FF7F0E"
    )) +
    labs(
      title  = Region_list[region_idx],
      x      = "Year",
      y      = "log(Incidence)",
      colour = NULL
    ) +
    theme_classic(base_size = 20) +
    theme(
      plot.title    = element_text(face = "bold", size = 22, hjust = 0.5),
      strip.text    = element_text(face = "bold", size = 16),
      axis.title.y  = element_text(face = "bold", size = 20),
      axis.text     = element_text(size = 14),
      axis.text.x   = element_text(angle = 45, hjust = 1),
      legend.position  = "none",   # legend redundant since component is in strip labels
      panel.background = element_rect(fill = "white", colour = "black"),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      strip.background = element_rect(fill = "grey95", colour = "black")
    )
  
  print(plot)
  
  ggsave(
    filename = paste0(result_folder, "/", Region_list[region_idx], "_decomposition_facets.png"),
    plot     = plot,
    width    = 5 * length(District_list),   # scales width with number of districts
    height   = 12,
    dpi      = 300,
    limitsize = FALSE 
  )
}


#Figure 3
input_data2<- input_data %>% 
  tibbletime::as_tbl_time(index = date)

input_data2<-input_data2[order(input_data2$date),]


input_data2_STL_decompose<-input_data2 %>%
  group_by(Region, District)%>%
  time_decompose(incidence, method = "stl", trend="12 months")


plot_df <- input_data2_STL_decompose %>%
  ungroup() %>%
  as.data.frame()


#define intervention period
intervention_dates <- data.frame(
  Region = c(
    "Dodoma Region",
    "Iringa Region",
    "Kilimanjaro Region",
    "Manyara Region",
    "Mbeya Region",
    "Njombe Region",
    "Rukwa Region",
    "Singida Region",
    "Songwe Region",
    "Tanga Region"
  ),
  intervention_date = as.Date(c(
    "2020-10-01",
    "2020-09-01",
    "2020-11-01",
    "2020-11-01",
    "2020-07-01",
    "2020-09-01",
    "2020-09-01",
    "2020-10-01",
    "2020-08-01",
    "2020-08-01"
  ))
)


intervention_dates <- intervention_dates %>%
  mutate(
    xmin = intervention_date,
    xmax = intervention_date + 31
  )


# Plot
p <- ggplot(
  plot_df,
  aes(x = date, y = trend)
) +
  
  #Intervention period
  geom_rect(
    data = intervention_dates,
    inherit.aes = FALSE,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#D55E00",
    alpha = 0.2
  ) +
  
  
  
  geom_line(
    aes(group = District, colour = Region),
    linewidth = 0.6
  ) +
  
  facet_wrap(
    ~Region,
    scales = "free_y",
    ncol = 4  ) +
  
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  
  # scale_colour_brewer(palette = "Dark2") +
  # scale_colour_viridis_d(option = "D")+
  scale_colour_hue()+
  
  labs(
    x = "Year",
    y = "Incidence trend"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    
    strip.background = element_rect(
      fill = "grey95",
      colour = "black"
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_line(
      colour = "grey90",
      linewidth = 0.3
    ),
    
    panel.grid.major.y = element_line(
      colour = "grey92",
      linewidth = 0.3
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text = element_text(
      colour = "black"
    ),
    
    panel.spacing = unit(0.8, "lines")
  )

p


ggsave(
  filename = "incidence_trends.png",
  plot = p,
  width = 12,
  height = 8,
  units = "in",
  dpi = 600
)


#Figure 1a map
packages_needed <- c("sf","tmap","rgdal","merTools","lme4","dplyr","gsubfn", "xlsx", "openxlsx", "readxl", "tidyverse", "cowplot", "grid", "gridExtra", "RColorBrewer", "shapefiles", "raster", "ggplot2", "cowplot", "tidyr", "ggrepel", "ggExtra", "ggridges", "ggthemes","ggpubr","reshape","viridis", "scales","RColorBrewer","ggsci","reshape","stringr","ggeffects","effects","nlme","sjPlot")
lapply(packages_needed, require, character.only = TRUE)


ITN_TZ<-read_excel("C:\\Users\\thawsu\\Swiss Tropical and Public Health Institute, Swiss TPH\\AIM - AIM Drive\\Country work\\Tanzania\\Tanzania 2026\\ITN impact evaluation\\9. Manuscripts\\data_codes\\Input_data\\ITN_year_TZ_map.xlsx")

Names_correspondence<-read_excel("C:\\Users\\thawsu\\Swiss Tropical and Public Health Institute, Swiss TPH\\AIM - AIM Drive\\Country work\\Tanzania\\Tanzania 2026\\ITN impact evaluation\\9. Manuscripts\\data_codes\\Input_data\\names_correspondence_forITN_TZ.xlsx")

#Join names correaspondence to dataset
ITN_TZ <- ITN_TZ %>% 
  mutate(code = as.character(council)) %>% 
  left_join(Names_correspondence, by="council")

#remove unnecessary columns
ITN_TZ <- ITN_TZ %>% dplyr::select(-c(6,7))

#rename column
names(ITN_TZ)[names(ITN_TZ) == "region.x"] <- "Region"


#Campaign
Campaign<-ITN_TZ%>%filter(Servicedeliverymechanism=="Campaign")

#make it into wide format
ITN_TZ_wide_Campaign <- spread(Campaign, year, Nets)

#relace NA with no
ITN_TZ_wide_Campaign[is.na(ITN_TZ_wide_Campaign)] <- "No Campaign"

# Replace String with Another Stirng
ITN_TZ_wide_Campaign[ITN_TZ_wide_Campaign == 'Yes'] <- 'Campaign'

#merge to shapefile
merged_Campaign <- merge(
  Admin2,
  ITN_TZ_wide_Campaign,
  by.x = "DHIS2_Dist",
  by.y = "Council_shapefile",
  all.x = TRUE,
  duplicateGeoms = TRUE
)



Campaign.map <- tm_shape(merged_Campaign) +
  tm_fill(
    "2020",
    fill.scale = tm_scale(values = c("darkgreen", "grey")),
    fill.legend = tm_legend_hide()
  ) +
  tm_borders() +
  tm_add_legend(
    type = "polygons",
    labels = "Campaign councils",
    fill = "darkgreen",
    col = NA
  ) +
  tm_layout(
    legend.position = tm_pos_in("left", "bottom"),
    legend.frame = FALSE,
    legend.bg = FALSE,
    frame = FALSE,
    legend.text.size = 1.2
  )

Campaign.map

setwd("C:\\Users\\thawsu\\Swiss Tropical and Public Health Institute, Swiss TPH\\AIM - AIM Drive\\Country work\\Tanzania\\Tanzania 2026\\ITN impact evaluation\\9. Manuscripts\\data_codes\\Results")
tmap_save(Campaign.map, "Campaign.map.jpg")


#Figure 1b approach
observed<-read_excel("C:\\Users\\thawsu\\Swiss Tropical and Public Health Institute, Swiss TPH\\AIM - AIM Drive\\Country work\\Tanzania\\Tanzania 2026\\ITN impact evaluation\\9. Manuscripts\\data_codes\\Input_data\\observed.xlsx")

pre_data <- observed %>% filter(date <= as.Date(deployment_date))
post_data <- observed %>% filter(date >= as.Date(deployment_date))
observed$date<-as.Date(observed$date)

plot <- ggplot() +
  # Pre-intervention model fitted values (only pre period)
  geom_line(data = pre_data, aes(x=date, y=fitted_autoarima), color="#D62728", size=1) +
  # geom_point(aes(x=date, y=(fitted_autoarima)),color="lightblue4",size=4) +
  # Post-intervention model fitted values (only post period, if needed)
  # geom_line(data = post_data, aes(x=date, y=fitted_autoarima), color="blue", size=1) +
  
  # Observed incidence (all periods)
  geom_point(data = observed, aes(x=date, y=exp(Incidence)), color="#1F77B4", size=4) +
  geom_line(data = post_data, aes(x=date, y=exp(Incidence)),color="#1F77B4", size=1)+
  
  # ITNhigh line across all dates (or you can also split)
  # geom_line(data = observed, aes(x=date, y=exp(ITNhigh)/scale), color="darkseagreen4", size=1) +
  #geom_point(data = observed, aes(x=date, y=exp(ITNhigh)/scale), color="darkseagreen4", size=2) +
  # geom_line(data = observed,aes(x=date, y=exp(ITNlow)/scale),color="darkseagreen4", size=1)+
  # geom_point(data = observed, aes(x=date, y=exp(ITNlow)/scale),color="darkseagreen4",size=2) +
  geom_ribbon(data = post_data, 
              aes(x = date, 
                  ymin = pmin(manual2, exp(Incidence)), 
                  ymax = pmax(manual2, exp(Incidence))), 
              fill = "#FF9999", alpha = 0.4) +
  # Ribbon across all dates
  geom_ribbon(data = pre_data, aes(x=date, ymin=manual_lowci, ymax=manual_highci), fill="grey50", alpha=0.5) +
  
  #counterfactual
  geom_line(data = post_data, aes(x=date, y=(manual2)),color="#2CA02C", size=1)+
  geom_point(data = post_data,aes(x=date, y=(manual2)),color="#2CA02C",size=4) +
  geom_ribbon(data = post_data, aes(x=date,,ymin=manual_low_lowci2, ymax=manual_low_highci2),fill="#2CA02C", alpha=0.5)+
  #
  # Intervention line
  geom_vline(xintercept = as.Date(deployment_date), linetype=4) +
  
  labs(x="Year", y="Incidence") +
  
  # scale_y_continuous(name='Incidence', sec.axis = sec_axis(~.*scale, name="ITN access")) +
  
  theme(
    legend.title = element_text(size = 8, face="bold", colour="black"),
    legend.text = element_text(size = 8, face="bold", colour="black"),
    axis.title=element_text(size=24, face="bold", colour="black"),
    axis.text = element_text(size=24, face="bold", colour="black"),
    strip.text = element_text(size = 15, face="bold", colour="black"),
    legend.position="bottom",
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_line(colour = "grey90", size = 0.2),
    panel.grid.minor = element_line(colour = "grey98", size = 0.5),
    plot.title = element_text(face="bold")
  )+
  scale_x_date(limits = c(min(observed$date), as.Date("2021-12-31")))+
  scale_y_continuous(
    name='Incidence',
    limits = c(0, 5)                   # Set y-axis from 0 to 4.5
    #sec.axis = sec_axis(~.*scale, name="ITN access")
  ) 

print(plot)
ggsave((paste0("plotformanuscript_counterfatual", ".png")), width=9, height=7, dpi=300)

