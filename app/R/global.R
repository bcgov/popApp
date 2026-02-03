# Copyright 2023 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


# load libraries  ----
## installs any missing packages this script uses
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('shiny')) install.packages('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard')
if (!require('rsconnect')) install.packages('rsconnect')
if (!require('DT')) install.packages('DT')
#if (!require('GAlogger')) devtools::install_github("bnosac/GAlogger")
if (!require('markdown')) install.packages('markdown')
if (!require('scales')) install.packages('scales')
if (!require('bcdata')) install.packages('bcdata')
if (!require('bcsapps')) remotes::install_github('bcgov/bcsapps')

## metadata for app ----
switch_wording <- "Estimates above, Projections below"  ## text in Years selection AFTER switch-year

## Google Analytics ----
# GAlogger::ga_set_tracking_id("UA-150850915-1")
# GAlogger::ga_set_approval(consent = TRUE)
# GAlogger::ga_collect_pageview(page = "/popApp")

## read data ----

## get data from bcdc
# resource_codes <-  bcdata::bcdc_tidy_resources("86839277-986a-4a29-9f70-fa9b1166f6cb") %>% pull(id)
# 
# data1 <- map_dfr(
#  resource_codes,
#  bcdata::bcdc_get_data,
#  record = "86839277-986a-4a29-9f70-fa9b1166f6cb",
#  show_col_types = FALSE
# )
data1 <- readRDS("data/data1.rds")  ## by single-year intervals

## initial selection box values
## c(Region.Type, Region.Name, Year, Gender)
initVals <- list("Local Health Area", "British Columbia", max(data1$Year), list("M", "F", "T"))

## weighted median function ----
median_pop <- function(a, p, t){
  #Consumes the vectors of the same length that represent the age, the 
  #population by age, and the total population. It returns the median as 
  #computed by the ministry of health
  
  #Computing percentiles
  perc<-cumsum({{p}})/{{t}}
  #Computing share of the population in an age bracket
  share <- {{p}}/{{t}}
  #Computing percentile up to the previous age bracket
  previous<- perc-share
  #Computing the median
  median <- case_when(
    perc == 0.5 ~ {{a}}+1,
    previous < 0.5 & perc > 0.5 ~ {{a}}+(0.5-previous)/share,
    TRUE ~ 0
  )
  #Since the result above may return more than one median different from zero 
  #due to populations of size zero, we pick the median with the first non zero
  #size population (the max).
  return(janitor::round_half_up(max(median),1))
}
