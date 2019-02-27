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
  df <- readr::read_csv(here("analysis","inputs",paste0("FromR",age_var,".csv")), col_names = TRUE, col_specs)
  
  # fix age column names and order
  if(age_var == 5){
    
    # for 5-year intervals
    A_cols <- c("85-89","80-84","75-79","70-74","65-69","60-64","55-59","50-54",
                "45-49","40-44","35-39","30-34","25-29","20-24","15-19","10-14",
                "5-9","1-4","<1")
    colnames(df)[which(colnames(df) == "-89"):which(colnames(df) == "0")] <- A_cols
    
    # FromR5.csv column "-4" includes LT1 (<1 = age=0), so must remove it from 1-4 group
    df <- df %>% mutate(`1-4` = `1-4` - `<1`)
    
    # correct the reversed order of age groups
    df <- df %>% select(Year:"-90", 
             colnames(df)[which(colnames(df) == "<1"):which(colnames(df) == "85-89")], 
             gender)
  }
  
  # rename Total and 90+ cols
  colnames(df)[which(colnames(df) == "-999")] <- "Total"
  colnames(df)[which(colnames(df) == "-90")] <- "90+"
  
  # create character Gender var and re-arrange columns
  df <- df %>% mutate(Gender = 
                        case_when(gender == 1 ~ "M",
                                  gender == 2 ~ "F",
                                  gender == 3 ~ "T")) %>%
    select(Region, Region.Type, Year, Gender, 
           colnames(df)[(which(colnames(df) == "90+")+1):(which(colnames(df) == "gender")-1)],
           -gender, `90+`, Total)
  
  # open lookup and join in Region.Names
  lookup <- readr::read_csv(here("analysis","inputs","lookup.csv"), col_names = TRUE, col_types = "dcc")
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

