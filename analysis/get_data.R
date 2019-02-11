## load libraries  ----
library(tidyverse)


## get data ----
# Update csv file with name of input data table, remove non-data rows, save it as RDS file
data <- read_csv(paste0(getwd(),"/app/data/Provinces_5_year_agesex_population_totals.csv"), skip = 4)
data <- data %>% filter(!is.na(data$ID))
saveRDS(data, "app/data/data.rds")


