library(readxl)
library(tidyverse)
library(openxlsx)
library(ggplot2)

parent_dir = "template_submissions/"

filename = "Matouskova_PSInetData.xlsx"

file_path = paste0(parent_dir,filename)

template = lapply(2:11,
                  FUN = function(x) readxl::read_xlsx(file_path, sheet = x))

# data desc
data_desc = template[[2]]

# Water potential data
wp_data <- template[[7]]
wp_data <- wp_data[-1,]

# wp_data$Date <- as.Date(as.numeric(wp_data$Date), origin = "1899-12-30")
wp_data$Date <- as.Date(as.character(wp_data$Date), format = "%Y%m%d")
wp_data$`Water potential mean` = as.numeric(wp_data$`Water potential mean`)

wp_data$facet_group <- ifelse(wp_data$Date < as.Date("2017-01-01"), "Before", "After")
# wp_data$Time <- as.character(round(as.numeric(wp_data$Time), 2))
unique(wp_data$Time)
ts = ggplot(wp_data, aes(x = Date, y = `Water potential mean`, color=Time, group=Time)) +
  geom_point() +
  labs(x = "Date", y = "Water Potential", title = filename) 
ts
ggsave(paste0("graphs/",substr(filename, 1, nchar(filename) - 5),"wp_time.png"),ts, width = 10, height = 5)
