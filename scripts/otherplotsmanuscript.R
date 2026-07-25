#######################################
#plots for figures in manuscript
#######################################
#Download packages
packages_needed = c("anomalize","tibbletime","ggpubr","tseries","forecast", "leaps", "dplyr","gsubfn", "xlsx", "openxlsx", "readxl", "tidyverse", "ggplot2", "cowplot", "tidyr", "reshape")
lapply(packages_needed, require, character.only = TRUE)

#--------------------------------------
# ITN access variability (Figure 1a)
#--------------------------------------
input_data <- read_excel("filepath\\impact_analysis_inputdata.xlsx")

#get the itn access values following campaign
deployment_ITN <- input_data %>%
  filter(!is.na(deployment_date),
         date == deployment_date) %>%
  group_by(District) %>%
  slice(1) %>%
  ungroup()

#import shapefile
Admin2 <- st_read(file.path("filepath\\Input_data\\Shapefile\\District edited Jan 2021.shp"))

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

#--------------------------------------
# Supplementary Figure S1
#--------------------------------------
result_folder <- "filepath\\decomposed_plots"

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

    #process data
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

#--------------------------------------
#Figure 3: incidence trends
#--------------------------------------
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

#--------------------------------------
#Figure 1b analytical approach
#--------------------------------------
observed<-read_excel("filepath\\Input_data\\observed.xlsx")


deployment_date <- "2020-07-01"
cutoff_date <- as.Date("2021-12-31")
annotation_x <- cutoff_date + 100  

observed$date <- as.Date(observed$date)
pre_data  <- observed %>% filter(date <= as.Date(deployment_date))
post_data <- observed %>% filter(date >= as.Date(deployment_date) & date <= cutoff_date)

plot <- ggplot() +
  geom_line(data = pre_data, aes(x=date, y=fitted_autoarima, color="Model fitted"), size=1) +
  
  geom_point(data = observed %>% filter(date <= cutoff_date), aes(x=date, y=exp(Incidence), color="Observed"), size=4) +
  geom_line(data = post_data, aes(x=date, y=exp(Incidence), color="Observed"), size=1)+
  
  geom_ribbon(data = post_data, 
              aes(x = date, 
                  ymin = pmin(manual2, exp(Incidence)), 
                  ymax = pmax(manual2, exp(Incidence))), 
              fill = "#FF9999", alpha = 0.4) +
  geom_ribbon(data = pre_data, aes(x=date, ymin=manual_lowci, ymax=manual_highci), fill="grey50", alpha=0.5) +
  
  geom_line(data = post_data, aes(x=date, y=(manual2), color="Counterfactual"), size=1)+
  geom_point(data = post_data,aes(x=date, y=(manual2), color="Counterfactual"), size=4) +
  geom_ribbon(data = post_data, aes(x=date,,ymin=manual_low_lowci2, ymax=manual_low_highci2),fill="#2CA02C", alpha=0.5)+
  
  geom_vline(xintercept = as.Date(deployment_date), linetype=4) +
  
  labs(x="Year", y="Incidence", color = NULL) +
  
  scale_color_manual(
    values = c("Model fitted" = "#D62728", "Observed" = "#1F77B4", "Counterfactual" = "#2CA02C"),
    breaks = c("Counterfactual", "Model fitted", "Observed")
  ) +
  
  theme(
    legend.title = element_text(size = 8, face="bold", colour="black"),
    legend.text = element_text(size = 14, face="bold", colour="black"),
    axis.title=element_text(size=24, face="bold", colour="black"),
    axis.text = element_text(size=24, face="bold", colour="black"),
    strip.text = element_text(size = 15, face="bold", colour="black"),
    legend.position="bottom",
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_line(colour = "grey90", size = 0.2),
    panel.grid.minor = element_line(colour = "grey98", size = 0.5),
    plot.title = element_text(face="bold"),
    plot.margin = margin(t = 20, r = 220, b = 10, l = 10)  # <-- widened further for margin text
  )+
  guides(color = guide_legend(override.aes = list(size = 2, linetype = 1, shape = NA))) +
  scale_y_continuous(
    name='Incidence',
    limits = c(0, 5.5)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  coord_cartesian(xlim = c(min(observed$date), cutoff_date), clip = "off") +
  
  # ---- ANNOTATIONS ----

annotate("text", x = as.Date(deployment_date) - 365*3.1, y = 5.5, 
         label = "Pre-intervention", size = 6, fontface = "bold", color = "black", hjust = 0.5) +
  annotate("text", x = as.Date(deployment_date) + 365*0.7, y = 5.5, 
           label = "Post-intervention", size = 6, fontface = "bold", color = "black", hjust = 0.5) +
  
  annotate("text", x = as.Date(deployment_date) - 365*1.2, y = 4.9, 
           label = "Data points the model is fitted to", size = 5.5, color = "#1F77B4", hjust = 0.5,fontface = "bold") +
  annotate("curve", x = as.Date(deployment_date) - 365*1.4, y = 4.7, 
           xend = as.Date(deployment_date) - 365*2, yend = 2.95,
           curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "#1F77B4") +
  annotate("curve", x = as.Date(deployment_date) - 365*1.0, y = 4.7, 
           xend = as.Date(deployment_date) - 90, yend = 3.9,
           curvature = 0.2, arrow = arrow(length = unit(0.2, "cm")), color = "#1F77B4") +
  
  # right-side annotations 
  annotate("text", x = annotation_x, y = 3.6, 
           label = "Forecasted counterfactual\nsimulated by model:\nWith low coverage\nof LLIN", 
           size = 5.5, color = "#1F77B4", hjust = 0,fontface = "bold") +
  annotate("curve", x = annotation_x - 5, y = 3.5, 
           xend = as.Date(deployment_date) + 365*0.5, yend = 3.6,
           curvature = -0.3, arrow = arrow(length = unit(0.2, "cm")), color = "#1F77B4") +
  
  annotate("text", x = annotation_x, y = 2.2, 
           label = "Epidemiological\nimpact due to LLINs", 
           size = 5.5, color = "#1F77B4", hjust = 0,fontface = "bold") +
  annotate("curve", x = annotation_x - 5, y = 2.2, 
           xend = as.Date(deployment_date) + 365*0.8, yend = 1.6,
           curvature = -0.3, arrow = arrow(length = unit(0.5, "cm")), color = "#1F77B4") +
  
  annotate("text", x = annotation_x, y = 0.4, 
           label = "Observed data in DHIS2:\nWith current\ncoverage of LLIN", 
           size = 5.5, color = "#1F77B4", hjust = 0,fontface = "bold") +
  annotate("curve", x = annotation_x - 5, y = 0.5, 
           xend = as.Date(deployment_date) + 365*1.2, yend = 0.2,
           curvature = -0.3, arrow = arrow(length = unit(0.2, "cm")), color = "#1F77B4")

print(plot)

ggsave(
  "plotformanuscript_counterfactual.tiff",
  plot = plot,
  width = 12,  
  height = 7,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

#-------------------------------
#Figure 2
#-------------------------------
out_file      <- "Fig 2.tif"
img_width_in  <- 6.5
img_res       <- 300   # dpi

# ---- Box content: header (bold) lines + stat (regular) lines, per box ----
boxes <- list(
  list(header = c("Councils receiving LLIN campaign (2020)"),
       stats  = c("N_councils =50", "N_facilities = 2,493", "N_reports= 179,484")),
  list(header = c("Facilities performing mRDT testing"),
       stats  = c("N_councils =50", "N_facilities = 2,493", "N_reports= 139,739")),
  list(header = c("Included after reporting completeness threshold (\u226575%) per year & across analysis period"),
       stats  = c("N_councils =49", "N_facilities = 1,603", "N_reports= 105,649")),
  list(header = c("Excluded facilities with >3 consecutive months of missing reports"),
       stats  = c("N_councils =49", "N_facilities = 1,502", "N_reports= 104,487")),
  list(header = c("Excluded facility reports having extreme outliers"),
       stats  = c("N_councils =49", "N_facilities = 1,502", "N_reports= 99,912")),
  list(header = c("Excluded district with data from only one facility"),
       stats  = c("N_councils =48", "N_facilities = 1,501", "N_reports= 99, 857"))
)
n_boxes <- length(boxes)

# ---- Style ----
cex_txt   <- 0.95
margin_lr <- 0.35   # in
margin_tb <- 0.30   # in
box_w_in  <- img_width_in - 2 * margin_lr
gap_in    <- 0.40   # vertical space between boxes (room for the arrow)
line_h_in <- 0.26   # vertical space allotted per text line
pad_in    <- 0.16   # padding above/below the text block inside each box

x_left  <- margin_lr
x_right <- margin_lr + box_w_in
x_mid   <- (x_left + x_right) / 2
text_width_avail_in <- box_w_in * 0.92

# ---- Open a throwaway device purely to measure text in real inches ----
# (cairo_pdf is Unicode-safe, unlike the default pdf() device, so the
#  '\u2265' symbol in box 3 measures correctly)
cairo_pdf(tempfile(fileext = ".pdf"))
par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

wrap_header <- function(txt, width_avail_in, cex) {
  if (strwidth(txt, units = "inches", cex = cex, font = 2) <= width_avail_in) {
    return(txt)
  }
  words <- strsplit(txt, " ")[[1]]
  lines <- c(); cur <- ""
  for (w in words) {
    trial <- if (cur == "") w else paste(cur, w)
    if (strwidth(trial, units = "inches", cex = cex, font = 2) <= width_avail_in) {
      cur <- trial
    } else {
      lines <- c(lines, cur); cur <- w
    }
  }
  c(lines, cur)
}

box_lines  <- list()
box_h_in   <- numeric(n_boxes)

for (i in seq_len(n_boxes)) {
  hdr_wrapped <- unlist(lapply(boxes[[i]]$header, wrap_header,
                               width_avail_in = text_width_avail_in, cex = cex_txt))
  all_lines <- c(hdr_wrapped, boxes[[i]]$stats)
  is_bold   <- c(rep(TRUE, length(hdr_wrapped)), rep(FALSE, length(boxes[[i]]$stats)))
  box_lines[[i]] <- list(text = all_lines, bold = is_bold)
  box_h_in[i]    <- length(all_lines) * line_h_in + 2 * pad_in
}
dev.off()  # close the measuring device

# ---- Compute vertical position (in inches, measured from the TOP) of every box ----
top_of_box    <- numeric(n_boxes)
bottom_of_box <- numeric(n_boxes)
top_of_box[1] <- margin_tb
bottom_of_box[1] <- top_of_box[1] + box_h_in[1]
for (i in 2:n_boxes) {
  top_of_box[i]    <- bottom_of_box[i - 1] + gap_in
  bottom_of_box[i] <- top_of_box[i] + box_h_in[i]
}

img_height_in <- bottom_of_box[n_boxes] + margin_tb

# ---- Open the real TIFF device sized exactly to fit the content ----
tiff(filename = out_file,
     width = img_width_in, height = img_height_in,
     units = "in", res = img_res, compression = "lzw")

par(mar = c(0, 0, 0, 0))
plot.new()
# y-axis: 0 at TOP of image, growing downward, in inches -> flip for plot.window
plot.window(xlim = c(0, img_width_in), ylim = c(img_height_in, 0), xaxs = "i", yaxs = "i")
rect(0, 0, img_width_in, img_height_in, col = "white", border = NA)

for (i in seq_len(n_boxes)) {
  
  if (i > 1) {
    arrows(x0 = x_mid, y0 = bottom_of_box[i - 1],
           x1 = x_mid, y1 = top_of_box[i],
           length = 0.09, angle = 25, lwd = 1.6, col = "black")
  }
  
  rect(x_left, top_of_box[i], x_right, bottom_of_box[i],
       col = "white", border = "black", lwd = 1.3)
  
  lines_i <- box_lines[[i]]$text
  bold_i  <- box_lines[[i]]$bold
  n_lines <- length(lines_i)
  
  y_text_top <- top_of_box[i] + pad_in + line_h_in * 0.65
  
  for (j in seq_len(n_lines)) {
    y_pos <- y_text_top + (j - 1) * line_h_in
    text(x_mid, y_pos, labels = lines_i[j],
         cex = cex_txt,
         font = if (bold_i[j]) 2 else 1,
         family = "sans")
  }
}

dev.off()
cat("Saved:", normalizePath(out_file), " | dimensions (in):", img_width_in, "x", img_height_in, "\n")
