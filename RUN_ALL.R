options(java.parameters = "-Xmx8g" )  ## run BEFORE loading any libraries else can't open huge .xlsx file
### 1. load libraries ----

## installs any missing packages this script uses
if (!require('here')) install.packages('here')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('rsconnect')) install.packages('rsconnect')

library(here)
library(tidyverse)
library(rsconnect)  ## to connect to shiny.io dashboard to deploy app


### 2. set values ----

## A. Do you need to update the underlying data?
update_data = TRUE     ## set to TRUE if you need to update the underlying data, otherwise set to FALSE

## update folder path below ** keep slashes as is to work in R **
#base_folder <- "//SFP.IDIR.BCGOV/S152/S52004/PEOPLEPROJECTIONS/P19/ACCESSDATABASE/WEB/Estimate 2018-19/"
base_folder <- here("analysis", "inputs", paste0("/"))

## variables needed to read in Excel/csv data, if updating data
file_name <- "Combined"   ## file name prefix ("1" or "5" are added to file_name in function later)
#file_type <- "xlsx"      ## "xlsx" or "csv"
file_type <- "csv"        ## "xlsx" or "csv"
mysheet <- "DATA"         ## name of sheet with data
col_RegionType <- "TYPE"  ## case-sensitive name of Region Type column
col_Region <- "TYPEID"    ## case-sensitive name of Region number column
col_Year <- "YR"          ## case-sensitive name of Year column
col_Gender <- "GENDERID"  ## case-sensitive name of Gender ID column
col_Total <- "TOTAL"      ## case-sensitive name of Total column


### 3. data ----
if(update_data == TRUE) {

  ## don't change order of data_cols or final_cols
  data_cols <- c(col_RegionType, col_Region, col_Year, col_Gender, col_Total)
  final_cols<- c("Region.Type", "Region", "Year", "Gender", "Total")
  
  ## requires: csv or xlsx files in base_folder
  ## assumes: 5 columns (Type, Region, Year, Gender, Total), age columns ("A90PL" & "LT1" in 5-yr)
  ## will make lookup.csv if it doesn't exist
  source(here("analysis", "get_data.R"))
  
}


### 4. deploy app ----
## You need an admin access to the bcstats shiny.io account. Martin can grant you access.
## Once you have access to shiny.io dashboard, you need to deploy the app:
## Type deployApp() in console of app.R. If you get an error, you may need to set the app title:
## In app.R, click the publish button (blue icon), and choose "Publish Application".
## Type in a title (must be at least 4 characters). Publish.

rsconnect::deployApp(appName = "popApp", appId = 958258)  ## command to deploy app to shiny.io dashboard; account: bcstats

### DONE ----
