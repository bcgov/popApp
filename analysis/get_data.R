options(java.parameters = "-Xmx8g" )  ## run BEFORE loading any libraries else can't open huge .xlsx file

### load libraries  ----
if (!require('here')) install.packages('here')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('xlsx')) install.packages('xlsx')

library(here)
library(tidyverse)  ## for: dplyr, readr, ...
library(xlsx)


### get data ----
## Function to get sub-provincial estimates data
data.pretty <- function(base_folder, file_name, file_type, mysheet, age_var, data_cols, final_cols) {

  ## A. if csv file_type, else xlsx file_type
  if(file_type == "csv"){
    
    ## pre-specify column types
    #col_specs <- cols(.default = col_integer(), col_RegionType = col_character())
    
    ## read in csv data
    df <- readr::read_csv(file = paste0(base_folder, file_name, age_var, ".csv"))   #,col_names = TRUE, col_specs)
    
  } else {
    
    ## read in xlsx data
    df <- xlsx::read.xlsx2(file = paste0(base_folder, file_name, age_var, ".xlsx"),
                           sheetName = mysheet, stringsAsFactors = FALSE) %>%
      mutate_at(vars(-c(data_cols[1])), as.numeric)  ## else everything comes in as character class
  }
  
  ## B. change colnames if needed
  if(data_cols[1] != final_cols[1]){ df <- rename(df, !!final_cols[1] := !!data_cols[1]) }  ## "Region.Type"
  if(data_cols[2] != final_cols[2]){ df <- rename(df, !!final_cols[2] := !!data_cols[2]) }  ## "Region"
  if(data_cols[3] != final_cols[3]){ df <- rename(df, !!final_cols[3] := !!data_cols[3]) }  ## Year
  if(data_cols[4] != "gender")     { df <- rename(df, gender = !!data_cols[4]) }  ## will capitalize later
  if(data_cols[5] != final_cols[5]){ df <- rename(df, !!final_cols[5] := !!data_cols[5]) }  ## Total

  
  ## fix age column names
  if(age_var == 1){
    ## remove leading "A" from column names
    df <- df %>% 
      rename_at(vars(starts_with("A")), ~ str_replace(.x, pattern = "A", replacement = "")) %>%
      rename(`90+` = `90PL`, `0` = `LT1`)
  }
  
  if(age_var == 5){
    ## remove leading "A" from column names, and replace "_" with "-"
    df <- df %>%
      rename_at(vars(starts_with("A")), ~ str_replace(.x, pattern = "A", replacement = "")) %>%
      rename_at(vars(contains("_")), ~ str_replace(.x, pattern = "_", replacement = "-")) %>%
      rename(`0` = `LT1`, `90+` = `90PL`)
  }

  ## create character Gender var and re-arrange columns
  df <- df %>% mutate(Gender =
                        case_when(gender == 1 ~ "M",
                                  gender == 2 ~ "F",
                                  gender == 3 ~ "T")) %>%
     select(Region, Region.Type, Year, Gender, 
            colnames(df)[(which(colnames(df) == "gender")+1):(which(colnames(df) == "90+"))],
            -gender, Total)
  
  ## if analysis/inputs/lookup.csv does not exist, make it (no longer working b/c data changed)
  if(!exists(here("analysis", "inputs", "lookup.csv"))){
    ## requires: "analysis/inputs/REGNAMES_from_Access.csv"
    ## made manually by opening Database work/WorkingFile.accdb and copying REGNAMES into Excel
    source(here("analysis", "make_lookup.R"))
  }

  ## open lookup and join in Region.Names
  lookup <- readr::read_csv(here("analysis", "inputs", "lookup.csv"),
                            col_names = TRUE, col_types = "dcc")
  df <- left_join(df, lookup, by = c("Region", "Region.Type")) %>%
    select(Region, Region.Name = NAME, everything())

  ## replace Region.Type abbreviations with names
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
	Region.Type == "CH" ~ "Community Health Service Area",
    TRUE ~ as.character(Region.Type)
    )
  )

  df <- df %>% filter(!is.na(Region.Name))

  ## save as RDS
  saveRDS(df, paste0("app/data/data", age_var, ".rds"))

  ## return df
  df
}

## data1 for single-yr intervals, data5 for 5-yr intervals
data1 <- data.pretty(base_folder, file_name, file_type, mysheet, age_var <- 1, data_cols, final_cols)
#data5 <- data.pretty(base_folder, file_name, file_type, mysheet, age_var <- 5, data_cols, final_cols)


## clean up ----
rm(data.pretty, data1, data5)