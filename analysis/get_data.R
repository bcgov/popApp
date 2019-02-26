## load libraries  ----
# install.packages("here")
library(here)
library(tidyverse)


## OLD: get data ----
# Update csv file with name of input data table, remove non-data rows, save it as RDS file
  # data <- read_csv(paste0(getwd(),"/app/data/Provinces_5_year_agesex_population_totals.csv"), skip = 4)
  # data <- data %>% filter(!is.na(data$ID))
  # saveRDS(data, "app/data/data.rds")
  # rm(data)

# # Pre-specify column types
# col_specs <- cols(.default = col_integer(), Region.Type = col_character())
# 
# # Get sub-provincial estimates data, by single-year intervals
# data1 <- readr::read_csv(here("app","data","FromR1.csv"), col_names = TRUE, col_specs)
# 
# # prefer column names with A in front of age
# A_cols <- paste0("A", colnames(data1)[which(colnames(data1) == "0"):which(colnames(data1) == "89")])
# colnames(data1)[which(colnames(data1) == "0"):which(colnames(data1) == "89")] <- A_cols
# 
# # re-name and re-arrange columns
# data1 <- data1 %>% 
#   select(TYPE = Region.Type, TYPEID = Region, Year, Gender = gender, 
#          A0:A89, A90 = `-90`, TOTAL = `-999`)
# 
# # Get sub-provincial estimates data, by 5-year intervals
# data5 <- readr::read_csv(here("app","data","FromR5.csv"), col_names = TRUE, col_specs)
# 
# # Save as RDS files
# saveRDS(data1, "app/data/data1.rds")
# saveRDS(data5, "app/data/data5.rds")
# rm(col_specs)


## get data ----
# Get sub-provincial estimates data, by single-year intervals
data.pretty.1 <- function(df) {
  # pre-specify column types
  col_specs <- cols(.default = col_integer(), Region.Type = col_character())
  
  # read in data
  df <- readr::read_csv(here("app","data","FromR1.csv"), col_names = TRUE, col_specs)
  
  # prefer column names with A in front of age
  A_cols <- paste0("A", colnames(df)[which(colnames(df) == "0"):which(colnames(df) == "89")])
  colnames(df)[which(colnames(df) == "0"):which(colnames(df) == "89")] <- A_cols
  
  # re-name and re-arrange columns
  df <- df %>% mutate(Gender = 
                        case_when(gender == 1 ~ "M",
                                  gender == 2 ~ "F",
                                  gender == 3 ~ "T")) %>%
    select(Region, Region.Type, Year, Gender, 
           A0:A89, A90 = `-90`, TOTAL = `-999`)
  
  # save as RDS
  saveRDS(df, paste0("app/data/data1.rds"))
  
  # return df
  df
}
data1 <- data.pretty.1(data1)

# Get sub-provincial estimates data, by 5-year intervals
data.pretty.5 <- function(df) {
  # pre-specify column types
  col_specs <- cols(.default = col_integer(), Region.Type = col_character())
  
  # read in data
  df <- readr::read_csv(here("app","data","FromR5.csv"), col_names = TRUE, col_specs)
  
  # prefer column names with A in front of age
  A_cols <- c("TOTAL","A90PL","A85_89","A80_84","A75_79","A70_74","A65_69","A60_64","A55_59","A50_54",
              "A45_49","A40_44","A35_39","A30_34","A25_29","A20_24","A15_19","A10_14","A5_9","A1_4","LT1")
  colnames(df)[which(colnames(df) == "-999"):which(colnames(df) == "0")] <- A_cols

  # re-name and re-arrange columns
  df <- df %>% mutate(Gender = 
                        case_when(gender == 1 ~ "M",
                                  gender == 2 ~ "F",
                                  gender == 3 ~ "T"),
                      # FromR5.csv column "-4" includes LT1, so must remove it from 1-4 group
                      A1_4 = A1_4 - LT1) %>%
    select(Region, Region.Type, Year, Gender, "LT1":"A90PL", TOTAL)
  
  # save as RDS
  saveRDS(df, paste0("app/data/data5.rds"))
  
  # return df
  df
}
data5 <- data.pretty.5(data5)

