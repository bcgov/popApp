## load libraries
library(tidyverse)
library(here)

## pre-specify column types
col_specs <- cols(.default = col_integer(), Region.Type = col_character())

## read in data
lookup <- readr::read_csv(here("analysis", "inputs", paste0("FromR1.csv")), col_names = TRUE, col_specs)

## change Region.Type to names
lookup <- lookup %>% 
  group_by(Region, Region.Type) %>% 
  tally() %>% 
  select(-n)

# ## check
# a <- lookup %>% group_by(Region.Type) %>% tally()
# sum(a$n)  ## 280, but Access database has only 255 - the difference is in SR - only 7 in Access
# rm(a)

## read in REGNAMES_from_Access.csv to match up
## REGNAMES_from_Access.csv was created by opening Database work/WorkingFile.accdb and copying REGNAMES into Excel
REGNAMES <- read_csv(here("analysis", "inputs", "REGNAMES_from_Access.csv"))

## add Region.Names
lookup <- left_join(lookup, REGNAMES, by = c("Region" = "TYPEID", "Region.Type" = "TYPE"))
## NAs are due to fact that REGNAMES has names for only 7 in SR Region.Type

## save as csv
write_csv(lookup, here("analysis", "inputs", "lookup.csv"))

## clean-up
rm(col_specs, lookup, REGNAMES)

