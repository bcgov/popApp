## load libraries  ----
# install.packages("here")
library(here)
library(tidyverse)


## get data ----
# Function to get sub-provincial estimates data
data.pretty <- function(df, age_var) {

  # pre-specify column types
  col_specs <- cols(.default = col_integer(), Region.Type = col_character())
  
  # read in data
  df <- readr::read_csv(here("app","data",paste0("FromR",age_var,".csv")), col_names = TRUE, col_specs)
  
  # use column names with A in front of age
  if(age_var == 1){

    # for single-year intervals
    A_cols <- paste0("A", colnames(df)[which(colnames(df) == "0"):which(colnames(df) == "89")])
    colnames(df)[which(colnames(df) == "0"):which(colnames(df) == "89")] <- A_cols

  } else if(age_var == 5){
    
    # for 5-year intervals
    A_cols <- c("A85_89","A80_84","A75_79","A70_74","A65_69","A60_64","A55_59","A50_54",
                "A45_49","A40_44","A35_39","A30_34","A25_29","A20_24","A15_19","A10_14",
                "A5_9","A1_4","LT1")
    colnames(df)[which(colnames(df) == "-89"):which(colnames(df) == "0")] <- A_cols
    #colnames(df)[which(colnames(df) == "0"):which(colnames(df) == "-89")] <- rev(A_cols)
    
    # FromR5.csv column "-4" includes LT1 (age=0), so must remove it from A1-4 group
    df <- df %>% mutate(A1_4 = A1_4 - LT1)
    
    # correct the reversed order of age groups
    df <- df %>% select(Year:"-90", 
             colnames(df)[which(colnames(df) == "LT1"):which(colnames(df) == "A85_89")], 
             gender)
  }
  
  # rename TOTAL and 90+ cols
  colnames(df)[which(colnames(df) == "-999")] <- "TOTAL"
  colnames(df)[which(colnames(df) == "-90")] <- "A90+"
  
  # create character Gender var and re-arrange columns
  df <- df %>% mutate(Gender = 
                        case_when(gender == 1 ~ "M",
                                  gender == 2 ~ "F",
                                  gender == 3 ~ "T")) %>%
    select(Region, Region.Type, Year, Gender, 
           colnames(df)[(which(colnames(df) == "A90+")+1):(which(colnames(df) == "gender")-1)],
           -gender, `A90+`, TOTAL)
  
  # open lookup and join in Region.Names
  lookup <- readr::read_csv(here("app","data","lookup.csv"), col_names = TRUE, col_types = "dcc")
  df <- left_join(df, lookup, by = c("Region", "Region.Type")) %>%
    select(Region, Region.Name = NAME, everything())
  
  # replace Region.Type abbreviations with names
  df <- df %>% mutate(Region.Type = case_when(
    Region.Type == "CF" ~ "Children and Family Development",
    Region.Type == "DR" ~ "Development Region",
    Region.Type == "HA" ~ "Local Health Area",
    Region.Type == "HY" ~ "Health Authority",
    Region.Type == "HS" ~ "Health Service Delivery Area",
    Region.Type == "PS" ~ "College Region",
    Region.Type == "RD" ~ "Regional District",
    Region.Type == "SD" ~ "School District",
    Region.Type == "SR" ~ "Special Regions (CMAs and Vancouver Island)",
    TRUE ~ as.character(Region.Type)
    )
  )
  
  df <- df %>% filter(!is.na(Region.Name))
  
  # save as RDS
  saveRDS(df, paste0("app/data/data",age_var,".rds"))
  
  # return df
  df
}

data1 <- data.pretty(data1, 1)  # by single-year intervals
data5 <- data.pretty(data5, 5)  # by 5-year intervals

