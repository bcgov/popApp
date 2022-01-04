## load libraries
if (!require('here')) install.packages('here')
if (!require('tidyverse')) install.packages('tidyverse')

library(tidyverse)
library(here)


## if df doesn't exist in global environment, get it
if(!("df" %in% ls(.GlobalEnv))){
  
  ## if csv file_type, else xlsx file_type
  if(file_type == "csv"){
    
    ## read in csv data
    df <- readr::read_csv(file = paste0(base_folder, file_name, age_var, ".csv"))
    
  } else {
    
    ## read in xlsx data
    df <- openxlsx::readWorkbook(xlsxFile = paste0(base_folder, file_name, age_var, ".xlsx")) %>%
      mutate_at(vars(-c(data_cols[1])), as.numeric)  ## else everything comes in as character class
  }
  
  ## change Region.Type and Region colnames if needed
  if(data_cols[1] != final_cols[1]){ df <- rename(df, !!final_cols[1] := !!data_cols[1]) }  ## "Region.Type"
  if(data_cols[2] != final_cols[2]){ df <- rename(df, !!final_cols[2] := !!data_cols[2]) }  ## "Region"

}


## change Region.Type to names
lookup <- df %>% 
  group_by(Region, Region.Type) %>% 
  tally() %>% 
  select(-n)

# ## check (on 2017 data)
# a <- lookup %>% group_by(Region.Type) %>% tally()
# sum(a$n)  ## 280, but Access database has only 255 - the difference is in SR - only 7 in Access
# rm(a)

## read in REGNAMES_from_Access.csv to match up
## REGNAMES_from_Access.csv was created by opening Database work/WorkingFile.accdb and copying REGNAMES into Excel
# REGNAMES <- read_csv(here("analysis", "inputs", "REGNAMES_from_Access.csv"))
REGNAMES <- read_csv(here("analysis", "inputs", "REGNAMES.csv"))

## add Region.Names
lookup <- left_join(lookup, REGNAMES, by = c("Region", "Region.Type"))

## save as csv
write_csv(lookup, here("analysis", "inputs", "lookup.csv"))

## clean-up
rm(lookup, REGNAMES)

