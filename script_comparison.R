rm(list = ls()) 

#Plotting libs
library(ggplot2)
library(ggthemes)
#Used to transform dataframes befor plotting
library(reshape)

#function to compute rMSSD
hrv_features_rmssd <- function(values)
{
  valuesDiff <- diff(values)
  return (sqrt(mean(valuesDiff^2, na.rm = TRUE)))
}

hrv_features_hr <- function(values)
{
  return (60000/mean(values))
}

output_to_pdf = TRUE
#EDIT based on your machine settings
files_path_root <- paste("~/Dropbox/R workspace/github/zoom_hrv/", sep = "")
files_path_data <- paste(files_path_root, "data/", sep = "")
files_path <- paste(files_path_root, "figures/", sep = "")
setwd(files_path_root)

source(paste(files_path_root, "multiplot.R", sep = ""))

measurements <- c("001_01", "001_02", "001_03", "001_04", "001_05", "001_06", "001_07", "001_08", "001_09")
df_rr <- data.frame()
for(index_measurement in 1:length(measurements))
{
  curr_measurement <- measurements[index_measurement]
  
  #Load reference (Polar H7 data)
  rr_h7 = read.csv(paste(files_path_data, curr_measurement, "/rr_h7.csv", sep = ""), header=TRUE)
  names(rr_h7) <- c("date", "rr", "since_start")
  rr_h7$since_start <- rr_h7$since_start / 1000 #convert to seconds 
  rr_h7[, "sensor"] <- "Chest strap (Polar H7)"
  
  #Load HRV4Training data (collected with the same app)
  rr_hrv4training = read.csv(paste(files_path_data, curr_measurement, "/hrv4training/rr.csv", sep = ""), header=TRUE)
  names(rr_hrv4training) <- c("date", "rr", "since_start", "window", "lap")
  rr_hrv4training <- rr_hrv4training[, c(1:3)] #drop extra columns, not present for HRV Logger data collected for other sensors
  rr_hrv4training$since_start <- rr_hrv4training$since_start / 1000 #convert to seconds
  rr_hrv4training[, "sensor"] <- "HRV4Training (camera)"
  
  #Load other sensors
  rr_zoom = read.csv(paste(files_path_data, curr_measurement, "/zoom/rr.csv", sep = ""), header=TRUE)
  names(rr_zoom) <- c("date", "rr", "since_start", "window", "lap")
  rr_zoom <- rr_zoom[, c(1:3)] 
  rr_zoom$since_start <- rr_zoom$since_start / 1000 
  rr_zoom$rr <- rr_zoom$rr / 1.024
  rr_zoom[, "sensor"] <- "Zoom HRV"
  
  #add also corrected rr
  rr_zoom_corrected <- rr_zoom
  threshold_rr <- 0.2
  rr_zoom_corrected <- rr_zoom_corrected[abs(diff(rr_zoom_corrected$rr)/rr_zoom_corrected$rr[2:nrow(rr_zoom_corrected)]) <= threshold_rr &
                   abs(diff(rr_zoom_corrected$rr)/rr_zoom_corrected$rr[1:(nrow(rr_zoom_corrected)-1)]) <= threshold_rr, ]
  rr_zoom_corrected[, "sensor"] <- "Zoom HRV (corrected)"
  
  #Create data frame to plot using ggplot
  df_rr_curr_subj <- rbind(rr_h7, rr_hrv4training, rr_zoom, rr_zoom_corrected)
  df_rr_curr_subj[, "measurement_ID"] <- curr_measurement
  df_rr <- rbind(df_rr, df_rr_curr_subj)
}

#for each recording, remove the initial part (stabilization)
df_rr <- df_rr[df_rr$since_start > 15, ]
df_rr <- df_rr[df_rr$since_start < 170, ] 

#Segment windows for HRV computation and plotting (1 minute), force max to 3 as the zoom HRV sensor stops after 3 minutes
max_window <- 3
df_rr[, "window_min"] <- NA
for(index_window_min in 1:max_window)
{
  df_rr[df_rr$since_start >= (60*(index_window_min-1)) & 
          df_rr$since_start < (60*index_window_min), "window_min"] <- index_window_min
}
#remove excluded windows (plotting reasons)
df_rr <- df_rr[!is.na(df_rr$window_min), ]

#Compute features over segmented windows
df_features <- data.frame()
df_features_all <- data.frame()
for(index_measurement in 1:length(measurements))
{
  curr_measurement <- measurements[index_measurement]
  curr_measurement_data <- df_rr[df_rr$measurement_ID == curr_measurement, ]
  
  #Reference feature
  curr_window_h7 <- curr_measurement_data[curr_measurement_data$sensor == "Chest strap (Polar H7)", "rr"]
  rMSSD_h7 <- round(hrv_features_rmssd(curr_window_h7), 1)
  hr_h7 <- round(hrv_features_hr(curr_window_h7), 1)
  
  #HRV4Training
  curr_window_hrv4t <- curr_measurement_data[curr_measurement_data$sensor == "HRV4Training (camera)", "rr"]
  rMSSD_hrv4training <- round(hrv_features_rmssd(curr_window_hrv4t), 1)
  hr_hrv4training <- round(hrv_features_hr(curr_window_hrv4t), 1)
  
  #Other sensors
  curr_window_zoom <- curr_measurement_data[curr_measurement_data$sensor == "Zoom HRV", "rr"]
  rMSSD_zoom <- round(hrv_features_rmssd(curr_window_zoom), 1)
  hr_zoom <- round(hrv_features_hr(curr_window_zoom), 1)
  
  curr_window_zoom_corrected <- curr_measurement_data[curr_measurement_data$sensor == "Zoom HRV (corrected)", "rr"]
  rMSSD_zoom_corrected <- round(hrv_features_rmssd(curr_window_zoom_corrected), 1)
  hr_zoom_corrected <- round(hrv_features_hr(curr_window_zoom_corrected), 1)
  
  curr_features_all <- data.frame(rMSSD_h7, rMSSD_hrv4training, rMSSD_zoom, rMSSD_zoom_corrected,
                              hr_h7, hr_hrv4training, hr_zoom, hr_zoom_corrected)
  names(curr_features_all) <- c("rMSSD_Polar_h7", "rMSSD_HRV4Training", "rMSSD_Zoom_HRV", "rMSSD_Zoom_HRV_corrected", 
                            "HR_Polar_h7", "HR_HRV4Training", "HR_Zoom_HRV", "HR_Zoom_HRV_corrected")
  curr_features_all[, "measurement_ID"] <- curr_measurement 
  df_features_all <- rbind(df_features_all, curr_features_all)
  
  #same but by window
  for(index_window_min in 1:max_window)
  {
    #Reference feature
    curr_window_h7 <- curr_measurement_data[!is.na(curr_measurement_data$window_min) & 
                                          curr_measurement_data$window_min == index_window_min &
                                          curr_measurement_data$sensor == "Chest strap (Polar H7)", "rr"]
    rMSSD_h7 <- round(hrv_features_rmssd(curr_window_h7), 1)
    hr_h7 <- round(hrv_features_hr(curr_window_h7), 1)
    
    #HRV4Training
    curr_window_hrv4t <- curr_measurement_data[!is.na(curr_measurement_data$window_min) & 
                                             curr_measurement_data$window_min == index_window_min &
                                             curr_measurement_data$sensor == "HRV4Training (camera)", "rr"]
    rMSSD_hrv4training <- round(hrv_features_rmssd(curr_window_hrv4t), 1)
    hr_hrv4training <- round(hrv_features_hr(curr_window_hrv4t), 1)
    
    #Other sensors
    curr_window_zoom <- curr_measurement_data[!is.na(curr_measurement_data$window_min) & 
                                            curr_measurement_data$window_min == index_window_min &
                                            curr_measurement_data$sensor == "Zoom HRV", "rr"]
    rMSSD_zoom <- round(hrv_features_rmssd(curr_window_zoom), 1)
    hr_zoom <- round(hrv_features_hr(curr_window_zoom), 1)
    
    curr_window_zoom_corrected <- curr_measurement_data[!is.na(curr_measurement_data$window_min) & 
                                                curr_measurement_data$window_min == index_window_min &
                                                curr_measurement_data$sensor == "Zoom HRV (corrected)", "rr"]
    rMSSD_zoom_corrected <- round(hrv_features_rmssd(curr_window_zoom_corrected), 1)
    hr_zoom_corrected <- round(hrv_features_hr(curr_window_zoom_corrected), 1)
    
    curr_features <- data.frame(rMSSD_h7, rMSSD_hrv4training, rMSSD_zoom, rMSSD_zoom_corrected,
                                hr_h7, hr_hrv4training, hr_zoom, hr_zoom_corrected)
    names(curr_features) <- c("rMSSD_Polar_h7", "rMSSD_HRV4Training", "rMSSD_Zoom_HRV", "rMSSD_Zoom_HRV_corrected", 
                              "HR_Polar_h7", "HR_HRV4Training", "HR_Zoom_HRV", "HR_Zoom_HRV_corrected")
    curr_features[, "window"] <- index_window_min 
    curr_features[, "measurement_ID"] <- curr_measurement 
    df_features <- rbind(df_features, curr_features)
  }
}
head(df_features)

#Plot data, RR intervals first (synch is not perfect but signals overlap decently, won't be shifting or aligning them any further)
for(index_measurement in 1:length(measurements))
{
  curr_measurement <- measurements[index_measurement]
  curr_measurement_data <- df_rr[df_rr$measurement_ID == curr_measurement, ]
  curr_features <- df_features_all[df_features_all$measurement_ID == curr_measurement, ]
  #rr intervals 
  p1 <- ggplot(curr_measurement_data, aes(since_start, rr, col = sensor)) +
    geom_line(size = 2) +
    facet_wrap(sensor~window_min, scale = "free_x", ncol = 3) +
    ggtitle(paste("rMSSD -",#Subject", curr_measurement, 
                  'Polar H7:', curr_features$rMSSD_Polar_h7, 'ms -',
                  'HRV4Training:', curr_features$rMSSD_HRV4Training, 'ms -',
                  'Zoom HRV:', curr_features$rMSSD_Zoom_HRV, 'ms -',
                  'Zoom HRV (corrected):', curr_features$rMSSD_Zoom_HRV_corrected, 'ms')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
    xlab("Timestamp") +
    ylab("RR interval (ms)") +
    theme_minimal() + 
    scale_colour_ptol() +
    scale_fill_ptol() +
    theme(legend.position="none") 
  
  if(output_to_pdf)
  {
    pdf(paste(files_path,"fig_rr_", curr_measurement, "_byminute.pdf", sep=""), width=10, height=12)
  }
  multiplot(p1)
  if(output_to_pdf)
  {
    dev.off()
  }
  #all together
  p1 <- ggplot(curr_measurement_data, aes(since_start, rr, col = sensor)) +
    geom_line(size = 2) +
    facet_wrap(~sensor, scale = "free_x", ncol = 1) +
    ggtitle(paste("rMSSD -",#Subject", curr_measurement, 
                  'Polar H7:', curr_features$rMSSD_Polar_h7, 'ms -',
                  'HRV4Training:', curr_features$rMSSD_HRV4Training, 'ms -',
                  'Zoom HRV:', curr_features$rMSSD_Zoom_HRV, 'ms -',
                  'Zoom HRV (corrected):', curr_features$rMSSD_Zoom_HRV_corrected, 'ms')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
    xlab("Timestamp") +
    ylab("RR interval (ms)") +
    theme_minimal() + 
    scale_colour_ptol() +
    scale_fill_ptol() +
    theme(legend.position="none") 
  
  if(output_to_pdf)
  {
    pdf(paste(files_path,"fig_rr_", curr_measurement, ".pdf", sep=""), width=10, height=12)
  }
  multiplot(p1)
  if(output_to_pdf)
  {
    dev.off()
  }
}

#Plot features (rMSSD) for all sensors and measurements (one boxplot per person)
df_rmssd <- melt(df_features[, c("rMSSD_Polar_h7", "rMSSD_HRV4Training", "rMSSD_Zoom_HRV", "rMSSD_Zoom_HRV_corrected", "window", "measurement_ID")], id = c("window", "measurement_ID"))
names(df_rmssd)[3:4] <- c("Sensor", "rMSSD")
p1 <- ggplot(df_rmssd, aes(Sensor, rMSSD, fill = Sensor)) +
  geom_boxplot() +
  facet_wrap(~measurement_ID) +
  ggtitle("Comparison (rMSSD in ms)") +
  xlab("Time window") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol() +
  #scale_fill_manual(labels = c("Polar H7", "HRV4Training", "Zoom HRV", "Zoom HRV (corrected)")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle=30, hjust=1))

if(output_to_pdf)
{
  pdf(paste(files_path,"fig_rmssd_grouped.pdf", sep=""), width=10, height=10)
}
multiplot(p1)
if(output_to_pdf)
{
  dev.off()
}

#same for heart rate
df_hr <- melt(df_features[, c("HR_Polar_h7", "HR_HRV4Training", "HR_Zoom_HRV", "HR_Zoom_HRV_corrected","window", "measurement_ID")], id = c("window", "measurement_ID"))
names(df_hr)[3:4] <- c("Sensor", "HR")
p1 <- ggplot(df_hr, aes(Sensor, HR, fill = Sensor)) +
  geom_boxplot() +
  facet_wrap(~measurement_ID) +
  ggtitle("Comparison (HR in bpm)") +
  xlab("Time window") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol() +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle=30, hjust=1))

if(output_to_pdf)
{
  pdf(paste(files_path,"fig_hr_grouped.pdf", sep=""), width=10, height=10)
}
multiplot(p1)
if(output_to_pdf)
{
  dev.off()
}


### BLAND ALTMAN
min_rmssd <- min(df_features[, "rMSSD_Polar_h7"], df_features[, "rMSSD_HRV4Training"], df_features[, "rMSSD_Zoom_HRV"])
min_rmssd <- min_rmssd - 0.05*min_rmssd

max_rmssd <- max(df_features[, "rMSSD_Polar_h7"], df_features[, "rMSSD_HRV4Training"], df_features[, "rMSSD_Zoom_HRV"])
max_rmssd <- max_rmssd + 0.05*max_rmssd

#rMSSD
data_figure1 <- data.frame(residuals = resid(lm(rMSSD_Polar_h7 ~ rMSSD_HRV4Training, data = df_features)),
                           fitted = (lm(rMSSD_Polar_h7 ~ rMSSD_HRV4Training, data = df_features))$fitted, 
                           reference = df_features$rMSSD_Polar_h7,
                           mean_value = ((lm(rMSSD_Polar_h7 ~ rMSSD_HRV4Training, data = df_features))$fitted+df_features$rMSSD_Polar_h7)/2 )
sizeText <- 5 
text_R2 <- round(summary(lm(rMSSD_Polar_h7 ~ rMSSD_HRV4Training, data = df_features))$adj.r.squared, 2)
p1a <- ggplot(data_figure1, aes(fitted, reference)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
  xlab("HRV4Training") +
  ylab("Polar H7") +
  scale_x_continuous(limits = c(min_rmssd, max_rmssd)) +
  scale_y_continuous(limits = c(min_rmssd, max_rmssd)) +
  annotate("text", label = paste("R2=",text_R2,sep=""), x = min_rmssd, y = max_rmssd, size = sizeText, hjust = 0) +
  ggtitle("Polar H7 vs HRV4Training - rMSSD") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol() 
p1b <- ggplot(data_figure1, aes(mean_value, residuals)) +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
  xlab("Mean, (Reference + Fitted)/2") +
  ylab("Residuals") +
  geom_hline(aes(yintercept = sd(data_figure1$residuals)*1.96),linetype="dashed") + 
  geom_hline(aes(yintercept = -sd(data_figure1$residuals)*1.96),linetype="dashed") +
  geom_hline(aes(yintercept = mean(data_figure1$residuals))) +
  scale_y_continuous(limits = c(-max_rmssd/2, max_rmssd/2)) +
  ggtitle("Bland-Altman - Polar H7 vs HRV4Training - rMSSD") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol()
data_figure1 <- data.frame(residuals = resid(lm(rMSSD_Polar_h7 ~ rMSSD_Zoom_HRV, data = df_features)),
                           fitted = (lm(rMSSD_Polar_h7 ~ rMSSD_Zoom_HRV, data = df_features))$fitted, 
                           reference = df_features$rMSSD_Polar_h7,
                           #Gender = as.factor(dataPerSubjectLab$currgender),
                           mean_value = ((lm(rMSSD_Polar_h7 ~ rMSSD_Zoom_HRV, data = df_features))$fitted+df_features$rMSSD_Polar_h7)/2 )
sizeText <- 5 
text_R2 <- round(summary(lm(rMSSD_Polar_h7 ~ rMSSD_Zoom_HRV, data = df_features))$adj.r.squared, 2)
p2a <- ggplot(data_figure1, aes(fitted, reference)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
  xlab("Zoom HRV") +
  ylab("Polar H7") +
  scale_x_continuous(limits = c(min_rmssd, max_rmssd)) +
  scale_y_continuous(limits = c(min_rmssd, max_rmssd)) +
  annotate("text", label = paste("R2=",text_R2,sep=""), x = min_rmssd, y = max_rmssd, size = sizeText, hjust = 0) +
  ggtitle("Polar H7 vs Zoom HRV - rMSSD") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol() 
p2b <- ggplot(data_figure1, aes(mean_value, residuals)) +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
  xlab("Mean, (Reference + Fitted)/2") +
  ylab("Residuals") +
  geom_hline(aes(yintercept = sd(data_figure1$residuals)*1.96),linetype="dashed") + 
  geom_hline(aes(yintercept = -sd(data_figure1$residuals)*1.96),linetype="dashed") +
  geom_hline(aes(yintercept = mean(data_figure1$residuals))) +
  scale_y_continuous(limits = c(-max_rmssd/2, max_rmssd/2)) +
  ggtitle("Bland-Altman - Polar H7 vs Zoom HRV - rMSSD") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol()
#corrected
data_figure1 <- data.frame(residuals = resid(lm(rMSSD_Polar_h7 ~ rMSSD_Zoom_HRV_corrected, data = df_features)),
                           fitted = (lm(rMSSD_Polar_h7 ~ rMSSD_Zoom_HRV_corrected, data = df_features))$fitted, 
                           reference = df_features$rMSSD_Polar_h7,
                           #Gender = as.factor(dataPerSubjectLab$currgender),
                           mean_value = ((lm(rMSSD_Polar_h7 ~ rMSSD_Zoom_HRV_corrected, data = df_features))$fitted+df_features$rMSSD_Polar_h7)/2 )
sizeText <- 5 
text_R2 <- round(summary(lm(rMSSD_Polar_h7 ~ rMSSD_Zoom_HRV_corrected, data = df_features))$adj.r.squared, 2)
p3a <- ggplot(data_figure1, aes(fitted, reference)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
  xlab("Zoom HRV (corrected)") +
  ylab("Polar H7") +
  scale_x_continuous(limits = c(min_rmssd, max_rmssd)) +
  scale_y_continuous(limits = c(min_rmssd, max_rmssd)) +
  annotate("text", label = paste("R2=",text_R2,sep=""), x = min_rmssd, y = max_rmssd, size = sizeText, hjust = 0) +
  ggtitle("Polar H7 vs Zoom HRV (corrected) - rMSSD") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol() 
p3b <- ggplot(data_figure1, aes(mean_value, residuals)) +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
  xlab("Mean, (Reference + Fitted)/2") +
  ylab("Residuals") +
  geom_hline(aes(yintercept = sd(data_figure1$residuals)*1.96),linetype="dashed") + 
  geom_hline(aes(yintercept = -sd(data_figure1$residuals)*1.96),linetype="dashed") +
  geom_hline(aes(yintercept = mean(data_figure1$residuals))) +
  scale_y_continuous(limits = c(-max_rmssd/2, max_rmssd/2)) +
  ggtitle("Bland-Altman - Polar H7 vs Zoom HRV (corrected) - rMSSD") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol()
if(output_to_pdf)
{
  pdf(paste(files_path,"fig_BlandAltman_rMSSD.pdf", sep=""), width=12, height=18)
}
multiplot(p1a, p2a, p3a, p1b, p2b, p3b, cols = 2)
if(output_to_pdf)
{
  dev.off()
}



#HR
min_rmssd <- min(df_features[, "HR_Polar_h7"], df_features[, "HR_HRV4Training"], df_features[, "HR_Zoom_HRV"])
min_rmssd <- min_rmssd - 0.05*min_rmssd

max_rmssd <- max(df_features[, "HR_Polar_h7"], df_features[, "HR_HRV4Training"], df_features[, "HR_Zoom_HRV"])
max_rmssd <- max_rmssd + 0.05*max_rmssd

data_figure1 <- data.frame(residuals = resid(lm(HR_Polar_h7 ~ HR_HRV4Training, data = df_features)),
                           fitted = (lm(HR_Polar_h7 ~ HR_HRV4Training, data = df_features))$fitted, 
                           reference = df_features$HR_Polar_h7,
                           #Gender = as.factor(dataPerSubjectLab$currgender),
                           mean_value = ((lm(HR_Polar_h7 ~ HR_HRV4Training, data = df_features))$fitted+df_features$HR_Polar_h7)/2 )
sizeText <- 5 
text_R2 <- round(summary(lm(HR_Polar_h7 ~ HR_HRV4Training, data = df_features))$adj.r.squared, 2)
p1a <- ggplot(data_figure1, aes(fitted, reference)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
  xlab("HRV4Training") +
  ylab("Polar H7") +
  scale_x_continuous(limits = c(min_rmssd, max_rmssd)) +
  scale_y_continuous(limits = c(min_rmssd, max_rmssd)) +
  annotate("text", label = paste("R2=",text_R2,sep=""), x = min_rmssd, y = max_rmssd, size = sizeText, hjust = 0) +
  ggtitle("Polar H7 vs HRV4Training - Heart rate") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol()  
p1b <- ggplot(data_figure1, aes(mean_value, residuals)) +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
  xlab("Mean, (Reference + Fitted)/2") +
  ylab("Residuals") +
  geom_hline(aes(yintercept = sd(data_figure1$residuals)*1.96),linetype="dashed") + 
  geom_hline(aes(yintercept = -sd(data_figure1$residuals)*1.96),linetype="dashed") +
  geom_hline(aes(yintercept = mean(data_figure1$residuals))) +
  scale_y_continuous(limits = c(-max_rmssd/2, max_rmssd/2)) +
  ggtitle("Bland-Altman - Polar H7 vs HRV4Training - Heart rate") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol() 
data_figure1 <- data.frame(residuals = resid(lm(HR_Polar_h7 ~ HR_Zoom_HRV, data = df_features)),
                           fitted = (lm(HR_Polar_h7 ~ HR_Zoom_HRV, data = df_features))$fitted, 
                           reference = df_features$HR_Polar_h7,
                           #Gender = as.factor(dataPerSubjectLab$currgender),
                           mean_value = ((lm(HR_Polar_h7 ~ HR_Zoom_HRV, data = df_features))$fitted+df_features$HR_Polar_h7)/2 )
sizeText <- 5 
text_R2 <- round(summary(lm(HR_Polar_h7 ~ HR_Zoom_HRV, data = df_features))$adj.r.squared, 2)
p2a <- ggplot(data_figure1, aes(fitted, reference)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
  xlab("Zoom HRV") +
  ylab("Polar H7") +
  scale_x_continuous(limits = c(min_rmssd, max_rmssd)) +
  scale_y_continuous(limits = c(min_rmssd, max_rmssd)) +
  annotate("text", label = paste("R2=",text_R2,sep=""), x = min_rmssd, y = max_rmssd, size = sizeText, hjust = 0) +
  ggtitle("Polar H7 vs Zoom HRV - Heart rate") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol() 
p2b <- ggplot(data_figure1, aes(mean_value, residuals)) +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))  +
  xlab("Mean, (Reference + Fitted)/2") +
  ylab("Residuals") +
  geom_hline(aes(yintercept = sd(data_figure1$residuals)*1.96),linetype="dashed") + 
  geom_hline(aes(yintercept = -sd(data_figure1$residuals)*1.96),linetype="dashed") +
  geom_hline(aes(yintercept = mean(data_figure1$residuals))) +
  scale_y_continuous(limits = c(-max_rmssd/2, max_rmssd/2)) +
  ggtitle("Bland-Altman - Polar H7 vs Zoom HRV - Heart rate") +
  theme_minimal() + 
  scale_colour_ptol() +
  scale_fill_ptol() 
if(output_to_pdf)
{
  pdf(paste(files_path,"fig_BlandAltman_HR.pdf", sep=""), width=12, height=12)
}
multiplot(p1a, p2a, p1b, p2b, cols = 2)
if(output_to_pdf)
{
  dev.off()
}

