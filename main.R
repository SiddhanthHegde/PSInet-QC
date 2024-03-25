library(readxl)
library(tidyverse)
library(openxlsx)
library(ggplot2)

parent_dir = "template_submissions/"

filename = "Gharun_PSInetData.xlsx"

file_path = paste0(parent_dir,filename)

template = lapply(2:11,
                  FUN = function(x) readxl::read_xlsx(file_path, sheet = x))

# data desc
data_desc = template[[2]]

# Water potential data
wp_data <- template[[7]]
wp_data <- wp_data[-1,] #remove first row

wp_data$`Water potential mean` = as.numeric(wp_data$`Water potential mean`)
# wp_data$Date <- as.Date(as.numeric(wp_data$Date), origin = "1899-12-30")
wp_data$Date <- as.Date(as.character(wp_data$Date), format = "%Y%m%d")