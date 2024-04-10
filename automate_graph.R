library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
setwd("~/Documents/GRA/PSInet-QC/")

main_dir = "processed_data/"

csv_files <- list.files(path = main_dir, full.names = FALSE)
csv_files <- lapply(csv_files, function(filename) sub("_sheet.*", "", filename))
csv_files <- unique(csv_files)

error_files <<- c()

tryCatch({
  for(csv in csv_files){
    graph_dir = paste0("graphs/",csv,"/")
    dir.create(graph_dir)
    
    filepath = paste0(main_dir,csv,"_sheet7.csv")
    sheet1_path = paste0(main_dir,csv,"_sheet1.csv")
    sheet1 = read.csv(sheet1_path)
    if(file.exists(filepath)){
      df = read.csv(filepath)
      
      if(!all(is.na(df$date))){
        df$date = as.Date(as.character(df$date),format="%Y%m%d")
        
        if(!is.na(df$time[1]) && str_detect(df$time[1], "^[A-Za-z]+$")){
          df$time_of_day = ifelse(is.na(df$time),"NA",df$time)
        }else{
          df$time_of_day = ifelse(is.na(df$time), "NA", 
                                  ifelse(as.POSIXct(df$time, format="%H:%M:%S") >= as.POSIXct("08:00:00", format="%H:%M:%S"), 
                                         "midday", "predawn"))
        }
        
        df$range_threshold <- ifelse(df$water_potential_mean > 0 | df$water_potential_mean < -15, "Outlier", "Within Range")
        p = ggplot(df, aes(x = date, y = water_potential_mean, color=time_of_day, shape = range_threshold)) +
          geom_point() +
          facet_wrap(~ canopy_position, ncol = 1) +
          labs(title = paste0("Water Potential (email: ",sheet1$email[1], ")"),
               x = "Date",
               y = "Water Potential Mean") 
        ggsave(paste0(graph_dir,"wp_data.png"), plot = p, width = 15, height = 8)
      }
    }
    
    #--------------------------------------------------------------------------------------------------------------------
    filepath = paste0(main_dir,csv,"_sheet10.csv")
    if(file.exists(filepath)){
      df = read.csv(filepath)
      
      if(all(is.na(df$date))){
        next
      }
      df$date = as.Date(as.character(df$date),format="%Y%m%d")
      df$time = ifelse(is.na(df$time), "00:00:00", df$time)
      df$datetime = paste0("2024-",format(df$date,"%m-%d "), df$time)
      df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S")
      df$year = factor(format(df$date, "%Y"))
      
      ranges <- list(
        precipitation_mm = c(0, 250),
        relative_humidity_percent = c(10, 100),
        vapor_pressure_deficit_k_pa = c(0, 10),
        air_temperature_c = c(-30, 50),
        photosynthetically_active_radiation_ppfd_mumol_photon_m_2_s_1 = c(0, 2400),
        incident_shortwave_radiation = c(0, 1362),
        net_radiation_m_s_1 = c(-300, 2000),
        windspeed_m_s = c(0, 45)
      )
      
      for (col_name in names(ranges)) {
        df[[paste0(col_name, "_status")]] <- ifelse(df[[col_name]] >= ranges[[col_name]][1] & df[[col_name]] <= ranges[[col_name]][2], "in-range", "outlier")
      }
      
      for (col_name in names(ranges)) {
        p <- ggplot(df, aes_string(x = "datetime", y = col_name)) +
          geom_line() +
          facet_wrap(~ year, ncol = 1) +
          labs(title = paste0(col_name,"(email: ",sheet1$email[1], ")")) +
          geom_point(data = subset(df, df[[paste0(col_name, "_status")]] == "outlier"), color = "red", size = 2)
        
        ggsave(paste0(graph_dir,col_name, ".png"), plot = p, width = 15, height = 8)
      }
    }

  }
}, error = function(e) {
  error_files <<- c(error_files, e$message, csv)  
  print(e)
})
