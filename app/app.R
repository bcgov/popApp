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

# This is a Shiny web application. You can run the application locally by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#   http://shiny.rstudio.com/
#   http://rstudio.github.io/shinydashboard/get_started.html
# 
# To deploy an update:
#   1. update code and data
#   2. load library(rsconnect)
#   3. set working directory to app.R directory (setwd("I:/PEOPLEPROJECTIONS/00 - R_code/shiny_apps/Production/popApp/app"))
#      (Or, in Files pane, in app folder, click on "More", then "Set As Working Directory")
#   4. deployApp(appName = "popApp", appId = 958258)
#      (Or, click on shiny icon (blue eye) at top right of Script window, "Manage Accounts" to log into shinyapps.io)
# 
# https://bcstats.shinyapps.io/popProjApp/
#
# Revision notes:
# 2022-12-01 MK: 1) Changed region type select box to size=11 to fit TEA that was added; 2) Added alpha-sort on region type names
# 2022-04-03 MK: Added note to bottom app page explaining that CHSA numbering has been updated.
# 2024-07-26 MK: 1) Tweaked code to add header information to the downloaded CSV output;
#                2) Tweaked code to include Estimate/Projection (Type) variable in the output.
#                3) Tweaked code to sort region selections by region ID numerically.
# 2025-01-29 JH  1) Changed code to rename Age group columns in case custom age groups overlapped.
# 2026-01-15 JP  1) Changed code to add notes that reference Statistics Canada's estimates in notes and Methods section.
#                2) Added parameters to facilate the update


#Parameters---
date_STATCAN_CD <- "January 16, 2025" #Date when STATCAN's CD population estimates used on PEOPLE were published.

date_STATCAN_HSDA <- "February 19, 2025" #Date when STATCAN's HSDA population estimates used on PEOPLE were published.

PEOPLE_Update_Date <- "December 19, 2025" #Date when the latest version of PEOPLE is/was published

## Define ui layout ----
# UI demonstrating column layouts
ui <- fluidPage(title = "BC Population Estimates & Projections",
  theme = "bootstrap.css",
  HTML("<html lang='en'>"),
  fluidRow(
    ## appname = title that will appear in the header
    ## header ----
    bcsapps::bcsHeaderUI(id = 'header', appname = "Population Estimates & Projections for British Columbia"),
    
    htmltools::HTML("<!-- Google tag (gtag.js) -->
<script async src='https://www.googletagmanager.com/gtag/js?id=G-904KHMXRJB'></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'G-904KHMXRJB');
</script>"),

    column(width = 12,
           style = "margin-top:100px",
           
           ## creating tabs here
           tabsetPanel(
             id = "tabs",
             
            ## Main tab ----
            tabPanel(title = "Main",
                     tags$head(tags$style(type='text/css', ".nav-tabs {font-size: 20px} ")),
                     ### Sidebar ----        
                     sidebarLayout(
                       sidebarPanel(style = "background-color:#F2F2F2;",
                                    tags$fieldset(
                                      tags$legend(h3("Step 1: Select data")),
                                      HTML("Use the Ctrl or Shift key to select multiple entries."),
                                      br(),br(),
                                      selectInput(inputId = "Region.Type",
                                                  label = h4("Select a region type"),
                                                  choices = sort(unique(data1$Region.Type)),  #MK: added alpha-sort on region types
                                                  selected = initVals[[1]],         ## default selection: "Local Health Area", 
                                                  selectize = FALSE, size = 12    ## forces all 12 options to be shown at once (not drop-down); MK: changed size from 10 to 11 to include TEA
                                      ),
                                      uiOutput("Region.Name"),
                                      uiOutput("Switch.Year"),
                                      br(),
                                      uiOutput("Year"),
                                      uiOutput("Gender")
                                    ),
                                    br(),
                                    tags$fieldset(
                                      HTML(paste0("Produced by BC Stats ", "<br>", 
                                                  "Data available on the <a href = 'https://catalogue.data.gov.bc.ca/dataset/86839277-986a-4a29-9f70-fa9b1166f6cb' target = '_blank'>BC Data Catalogue</a>"))
                                    )
                       ),  ## end of sidebarPanel
                       ### Main panel ----
                       mainPanel(
                         #### Statistic selection ----
                         tags$fieldset(style = "margin-top:20px;",
                                       tags$legend(h3("Step 2: Choose statistic")),
                                       column(width = 12,
                                              column(width = 12, ## second column to match alignment of age selection  
                                                     tags$fieldset(tags$legend(h4("Select which statistic to display")),
                                                                   radioButtons(inputId = "Statistic_Var",
                                                                                label = NULL,
                                                                                choices = c("Count", "Proportion", "Average Age", "Median Age", "Change in Total Population"),
                                                                                inline = TRUE,
                                                                                selected = "Count"))))
                         ),   ## end of tags$fieldset (Choose Statistic)
                         #### Age selection ----
                         ## Conditional panels: only show if "custom" age type is selected
                         # https://shiny.rstudio.com/reference/shiny/1.0.5/conditionalPanel.html
                          conditionalPanel(condition = "input.Statistic_Var == 'Count' | input.Statistic_Var == 'Proportion'",
                                          tags$fieldset(style = "margin-top:20px;",
                                                        tags$legend(h3("Step 3: Select age format")),
                                                        column(width = 12,
                                                               column(width = 12,
                                                                      tags$fieldset(tags$legend(h4("Select type of age group")),
                                                                                    radioButtons(inputId = "Age_Type",
                                                                                                 label = NULL,
                                                                                                 choices = c("Totals", "Single Year Age Groups", "5-year Age Groups", 
                                                                                                             "Custom Age Groups" = "custom"),
                                                                                                 selected = "Totals",
                                                                                                 inline = TRUE))
                                                                      ),  ## end of column
                                                               column(width = 3, 
                                                                      conditionalPanel(
                                                                        condition = "input.Age_Type == 'custom'",
                                                                        tags$fieldset(tags$legend(h4("Custom age groups example"))),
                                                                        tableOutput(outputId = "example_table"))
                                                                      ),  ## end of column
                                                               column(width = 6,
                                                                      conditionalPanel(
                                                                        condition = "input.Age_Type == 'custom'",
                                                                        tags$fieldset(tags$legend(h4("Custom age groups")),
                                                                                      br(),
                                                                                      column(width = 6,
                                                                                             numericInput(inputId = "start1", 
                                                                                                          label = "From - 1st",
                                                                                                          value = NA, 
                                                                                                          #min = 0, max = 99
                                                                                                          )),
                                                                                      column(width = 6,
                                                                                             numericInput(inputId = "end1", 
                                                                                                          label = "To - 1st", 
                                                                                                          value = NA, 
                                                                                                          #min = 0, max = 99
                                                                                                          )), 
                                                                                      column(width = 6,
                                                                                             numericInput(inputId = "start2", 
                                                                                                          label = "From - 2nd",
                                                                                                          value = NA, 
                                                                                                          #min = 0, max = 99
                                                                                                          )), 
                                                                                      column(width = 6,
                                                                                             numericInput(inputId = "end2", 
                                                                                                          label = "To - 2nd", 
                                                                                                          value = NA, 
                                                                                                          #min = 0, max = 99
                                                                                                          )), 
                                                                                      column(width = 6,
                                                                                             numericInput(inputId = "start3", 
                                                                                                          label = "From - 3rd", 
                                                                                                          value = NA, 
                                                                                                          #min = 0, max = 99
                                                                                                          )), 
                                                                                      column(width = 6,
                                                                                             numericInput(inputId = "end3", 
                                                                                                          label = "To - 3rd", 
                                                                                                          value = NA, 
                                                                                                          #min = 0, max = 99
                                                                                                          )), 
                                                                                      column(width = 6,
                                                                                             numericInput(inputId = "start4", 
                                                                                                          label = "From - 4th", 
                                                                                                          value = NA, 
                                                                                                          #min = 0, max = 99
                                                                                                          )),
                                                                                      column(width = 6,
                                                                                             numericInput(inputId = "end4", 
                                                                                                          label = "To - 4th", 
                                                                                                          value = NA, 
                                                                                                          #min = 0, max = 99
                                                                                                          )),
                                                                                      column(width = 6,
                                                                                             numericInput(inputId = "start5", 
                                                                                                          label = "From - 5th", 
                                                                                                          value = NA, 
                                                                                                          #min = 0, max = 99
                                                                                                          )),
                                                                                      column(width = 6,
                                                                                             numericInput(inputId = "end5", 
                                                                                                          label = "To - 5th", 
                                                                                                          value = NA, 
                                                                                                          #min = 0, max = 99
                                                                                                          ))
                                                                                      
                                                                                      )  ## end of tags$fieldset
                                                                        )  ## end of conditionalPanel
                                                                      )
                                                               )
                                                        )),  ## end of tags$fieldset (Age selection)
                         #### Proportion by gender or age ----
                         conditionalPanel(
                           condition = "input.Statistic_Var == 'Proportion'",
                           tags$fieldset(style = "margin-top:20px;",
                                         tags$legend(h3("Step 4: Choose Denominator")),
                                         column(width = 12,
                                                column(width = 12, ## second column to match alignment of age selection  
                                                       tags$fieldset(tags$legend(h4("Select Distribution across Age or Gender")),
                                                                     radioButtons(inputId = "Proportion_Var",
                                                                                  label = NULL,
                                                                                  choices = c("Age", "Gender"),
                                                                                  inline = TRUE,
                                                                                  selected = "Age"))))
                                         )),   ## end of tags$fieldset (proportion by gender of age)
                         #### Customize layout (count) ----
                         conditionalPanel(
                           condition = "input.Statistic_Var == 'Count' | input.Statistic_Var == 'Proportion'",
                           tags$fieldset(style = "margin-top:20px;",
                                         tags$legend(h3(textOutput("layout_step"))),
                                         column(width = 12,
                                                column(width = 12, ## second column to match alignment of age selection  
                                                       tags$fieldset(tags$legend(h4("Select variable to display as columns")),
                                                                     radioButtons(inputId = "Column_Var",
                                                                                  label = NULL,
                                                                                  choices = c("Age", "Gender", "Year", "None"),
                                                                                  inline = TRUE,
                                                                                  selected = "Age"))))
                                         )),   ## end of tags$fieldset (Customize layout (count))
                         #### Actions and table ----
                         br(),
                         tags$fieldset(
                           hr(),
                           column(width=12,
                                  actionButton(inputId = "goButton", label = "Generate output"),
                                  actionButton(inputId = "resetButton", label = "Reset selection"),
                                  downloadButton(outputId = "downloadData", label = "Download data as csv")
                                  )
                           ),
                         br(),br(),
                         DTOutput("default_table"),  ## only shows until "Generate Output" is clicked (and again on reset)
                         DTOutput("table"),
                         br(),
                         ## end of Actions and table
                         #### Notes ----
                         tags$fieldset(
                         tags$legend(h3("Notes")),
                         uiOutput("notes")
                         )  ## end of tags$fieldset (Notes)
                         )  ## end of mainPanel
                       )  ## end of sidbarLayout
                     ),  ## end of tabPanel "Main"
                        ## Methods tab ----
            tabPanel(title = "Methods",
                     column(width = 12,
                            style = "margin-top:25px",
                            tags$fieldset(
                              tags$legend(h3("Population Information")),
                              includeMarkdown("Methods.md")
                              )
                            )
                     ) ## end of tabPanel "Methods"
            )  ## end of tabsetPanel
           ), ## end of column
    ## footer ----
    bcsapps::bcsFooterUI(id = 'footer')
    )  ## end of fluidrow
  )

## Define server logic ----
server <- function(input, output, session) {
  
  bcsapps::bcsHeaderServer(id = 'header', links = TRUE)
  bcsapps::bcsFooterServer(id = 'footer')

  ## Sidebar ----
  
  ### Region.Name ----
  ## defaults: selectInput(inputId, label, choices, selected = NULL, multiple = FALSE,
  ##                       selectize = TRUE, width = NULL, size = NULL)
  ## size = how many items to show in box, requires selectize = FALSE
  ## select Region(s) within selected Region.Type, multiples OK
  output$Region.Name <- renderUI({

    #!!! MK: tweaking code to sort by Region ID numerically
    unique_regions <- data1 %>% 
      filter(Region.Type == input$Region.Type) %>% 
      select(Region,Region.Name) %>% unique() %>%
      mutate(Region_sort = as.numeric(Region)) %>%
      arrange(Region_sort)
    unique_num <- unique_regions$Region
    unique_name <- unique_regions$Region.Name
#    unique_num <- unique(data1$Region[data1$Region.Type == input$Region.Type])
#    unique_name <- unique(data1$Region.Name[data1$Region.Type == input$Region.Type])
    #!!! end of code tweaking
    
    display_name <- as.list(paste0(unique_num, " - ", unique_name))
    
    choices_list <- as.list(unique_name)
    names(choices_list) <- display_name
    
    
    selectInput(inputId = "Region.Name",
                label = h4("Select region(s)"),
                choices = choices_list,
                selected = initVals[[2]],
                multiple = TRUE,
                selectize = FALSE, size = 7)
  })

  ### switch_year ----
  switch_year <- reactive({
    data1 %>%
      filter(Region.Type == input$Region.Type) %>%
      filter(Type == "Estimate") %>%
      summarize(switch_year = max(Year)) %>%
      pull(switch_year)
  })
  
  ### Proj_Years ----
  Proj_Years <- reactive({
    data1 %>%
    filter(Region.Type == input$Region.Type,
           Type == "Projection") %>%
    summarize(min = min(Year),
              max = max(Year),
              Proj_Years = paste(min, max, sep = "-")) %>%
    pull(Proj_Years)
  })
  
  ### Switch.Year text ----
  output$Switch.Year <- renderUI({
    
    HTML(paste0("Estimate and projection figures can be updated
                                                  independently at different times of the year.",
                "<br><br>",
                "<strong>Estimates:</strong> Years ", switch_year(),
                " and earlier", "<br>",
                "<strong>Projections:</strong> Years ", Proj_Years()
    ))
    
  })
  
  ### Year ----
  ## select Year(s), multiples OK
  # HTML(paste0("<strong>",switch_wording,"</strong>"))
  output$Year <- renderUI({
    
    years <- data1 %>%
      filter(Region.Type == input$Region.Type) %>%
      distinct(Year)
    
    years_fmtd <- c(min(years$Year):switch_year(), paste("--",switch_wording,"--"), (switch_year() + 1):max(years$Year))
    
    selectInput(inputId = "Year",
                label = h4("Select year(s)"),
                choices = years_fmtd, #unique(data1$Year),
                selected = initVals[[3]],
                multiple = TRUE,
                selectize = FALSE, size = 7)
  })

  ### Gender ----
  ## select Sex(es), multiples OK
  output$Gender <- renderUI({
    selectInput(inputId = "Gender",
                label = h4("Select gender(s)"),
                choices = c("Males" = "M", "Females" = "F", "Totals" = "T"),
                selected = initVals[[4]], ## default selection: all ("M"ales, "F"emales, "T"otals)
                multiple = TRUE,
                selectize = FALSE, 
                size = 3) ## QQ: Is 4 a minimum? It's ignoring size=3
  })
  

  ## Main panel ----
  
  ### Custom age example ----
  ## example table for custom age groups (as text to keep decimals out)
  output$example_table <- renderTable({
    matrix(data = c("15", "24",  "25", "54",  "55", "64",  "65", "74"),
           nrow = 4, ncol = 2, byrow = TRUE, dimnames = list(c(1:4), c("From", "To")))
  })
  
  ### Customize layout header ----
  output$layout_step <- renderText({
    
    text <- case_when(
      input$Statistic_Var == "Count" ~ "Step 4: Customize Layout",
      input$Statistic_Var == "Proportion" ~ "Step 5: Customize Layout",
      TRUE ~ ""
    )
    
  })


  ### initial table with default selections ----
  
  ## initVals <- c(Region.Type, Region.Name, Year, Gender)
  data_init <- function(data1, initVals) {
    
    data1[data1$Region.Type == initVals[[1]], ] %>%
      filter(Region.Name == initVals[[2]]) %>%
      filter(Year == initVals[[3]]) %>%
      filter(Gender %in% initVals[[4]]) %>%
      #!!! MK: keeping Type variable
      select(Region, !!initVals[[1]] := Region.Name, Year, Type, Gender, Total) %>%
#      select(Region, !!initVals[[1]] := Region.Name, Year, Gender, Total) %>%
      mutate(Total = format(Total, big.mark = ","))
  }
  
  # https://stackoverflow.com/questions/54393592/hide-plot-when-action-button-or-slider-changes-in-r-shiny
  ## initial setting to show the table
  showDefaultTable <- reactiveVal(TRUE)
  
  ## make default_table with data_init()
  output$default_table <- DT::renderDataTable(datatable({
    
    ## show table only initially (before "Generate Output" button is clicked)
    if(showDefaultTable()) {
      data_init(data1, initVals)
    } else {
      NULL
    }
  },
  filter = "none",
  ## table options: https://shiny.rstudio.com/articles/datatables.html
  options = list(
    pageLength = 10,       ## show only X rows/page; https://datatables.net/reference/option/pageLength
    lengthMenu = c(10, 20, 25, 50), ## choices of pageLength to display
    scrollX = TRUE,        ## allows horizontal scrolling; https://datatables.net/reference/option/scrollX
    dom ="ltpi")
  )
  )
  
  ## note: showDefaultTable changes to FALSE whenever goButton is clicked in data_df
  
  ### reactive resetButton send analytics when reset ----
  observeEvent(input$resetButton, {
    
    # ga_collect_event(event_category = "resetButton", 
    #                  event_label = "Reset", 
    #                  event_action = "Reset application")
    
    ## just reload the session
    session$reload()

  })

  ### reactive send analytics when download ----
  rv <- reactiveValues(download_flag = 0)
  
  observeEvent(rv$download_flag, {
    
    # ga_collect_event(event_category = "downloadButton", 
    #                  event_label = paste0("Download: ", input$Age_Type, ", ", input$Region.Type), 
    #                  event_action = "Download data")
    
  }, ignoreInit = TRUE)
  
  ### reactive send analytics when query table ----
  observeEvent(input$goButton, {
    
    # ga_collect_event(event_category = "goButton", 
    #                  event_label = paste0("Query: ", input$Age_Type, ", ", input$Region.Type), 
    #                  event_action = "Generate data")
    
  })
  
  
  ### reactive data table ----
  ## create reactive values for input data to create table and download data
  data_df <- eventReactive(input$goButton, {
    
    ## with input$goButton in eventReactive(), nothing will happen until button clicked
    
    showDefaultTable(FALSE)  ## now hide initial default table
    
    #### A. Filter regions ----
    Reg.Type <- input$Region.Type  ## to be able to use as dynamic name in select
    
    output <- data1 %>%
      filter(Region.Type == Reg.Type) %>%
      filter(Region.Name %in% input$Region.Name) %>%
      ## remove empty Age columns (some Regions go to 90+ some to 100+)
      janitor::remove_empty(which = c("rows", "cols")) %>%
      #!!! MK: keeping Estimate/Projection variable (Type) for later
      select(Region, !!rlang::sym(Reg.Type) := Region.Name, everything(), -Region.Type) %>%
      pivot_longer(-c(Region, !!rlang::sym(Reg.Type), Year, Type, Gender), names_to = "Age", values_to = "Pop")
#      select(Region, !!rlang::sym(Reg.Type) := Region.Name, everything(), -Region.Type, -Type) %>%
#      pivot_longer(-c(Region, !!rlang::sym(Reg.Type), Year, Gender), names_to = "Age", values_to = "Pop")
    
    
      #!!!MK: adding sort by region ID numerically
      output <- output %>% 
        mutate(Region_sort = as.numeric(Region)) %>%
        arrange(Region_sort) %>%
        select(-Region_sort)
    
    ### B. Create age groups ----
    ## depending on age group type chosen
    if(input$Statistic_Var %in% c("Count", "Proportion")) {
      
      #### Total ----
      if(input$Age_Type == "Totals") {
        output <- output %>% filter(Age == "Total")
      }
      
      #### Single year ----
      if(input$Age_Type == "Single Year Age Groups") {
        output <- output ## redundant but keeping for clarity
      }
      
      #### Five year ----
      if(input$Age_Type == "5-year Age Groups") {
        output <- output %>%
          mutate(
            ## retain Age names for Total and ##+ (e.g., 90+ or 100+)
            Age_num = ifelse(Age == "Total" | str_detect(Age, "\\+"), NA, Age),
            ## make remaining age values numeric
            Age_num = as.numeric(Age_num),
            ## create age groups
            ## if Age_num is NA -- either Age = Total or ##+ -- use Age label
            ## manually define LT1, 1-4 and 5-9 as they don't fit the normal format
            ## label Age values divisible by 5 as Age to Age+4 (e.g., "10 to 14")
            ## leave remainder as NA and use fill to fill in the labels with the previous non-NA value
                 Age_grp = case_when(is.na(Age_num) ~ Age,
                                     Age_num == 0 ~ "LT1",
                                     between(Age_num, 1,4) ~ " 1 to 4",
                                     between(Age_num, 5,9) ~ " 5 to 9",
                                     Age_num %% 5 == 0 ~ paste(Age_num, "to", Age_num + 4))) %>%
          fill(Age_grp) %>%
          ## make factor to retain order
          mutate(Age = fct_inorder(Age_grp)) %>%
          #!!!MK: keeping Type variable
          group_by(Region, !!rlang::sym(Reg.Type), Year, Type, Gender, Age) %>%
#          group_by(Region, !!rlang::sym(Reg.Type), Year, Gender, Age) %>%
          summarize(Pop = sum(Pop), .groups = "drop")
        
      }
      
      #### Custom ----
      if(input$Age_Type == "custom") {
        
        ## a. create data frame of custom age groups user typed in
        custom_ages <- data.frame(S = c(input$start1, input$start2, input$start3, input$start4, input$start5),
                                  E = c(input$end1, input$end2, input$end3, input$end4, input$end5),
                                  stringsAsFactors = FALSE)
        
        ## b. if input (start or end) is decimal,  drop it from calculations.
        ## if input (start or end) is < 0 or > max ingle age, replace with min/max in df and label.
        ## if startX > endX, calculation will be correct, and use min and max in labelling for right order
        max_single_age <- output %>% 
          filter(str_detect(Age, "\\+")) %>% 
          distinct(Age) %>% 
          transmute(Age = str_remove(Age, "\\+") %>% as.numeric() %>% sum(-1)) %>%
          pull()
        
        custom_ages <- custom_ages %>%
          mutate_at(c("S", "E"), ~ case_when(str_detect(.x, pattern = "\\.") ~ NA,  ## anything with decimal will be dropped
                                             .x < 0 ~ 0,    ## any whole # < 0 will be re-set to 0
                                             .x > max_single_age ~ max_single_age,  ## any whole # > max_single_age will be re-set to max_single_age (+ group defined below)
                                             TRUE ~ .x))
        
        ## c. create age groups
        output <- output %>%
          mutate( ## retain Age name for Total, remove + from plus group (e.g., 90+)
            Age_num = ifelse(Age == "Total", NA, str_remove_all(Age, "\\+")),
            ## make age values numeric
            Age_num = as.numeric(Age_num),
            ## create age groups
            Age_grp = case_when(Age == "Total" ~ "Total",
                                Age_num < min(custom_ages$S, custom_ages$E, na.rm = TRUE) ~ paste0("LT", min(custom_ages$S, custom_ages$E, na.rm = TRUE)),
                                between(Age_num, 
                                        min(custom_ages$S[1], custom_ages$E[1]),
                                        max(custom_ages$S[1], custom_ages$E[1])) ~ paste(min(custom_ages$S[1], custom_ages$E[1]), "to",
                                                                                         max(custom_ages$S[1], custom_ages$E[1])),
                                between(Age_num, 
                                        min(custom_ages$S[2], custom_ages$E[2]),
                                        max(custom_ages$S[2], custom_ages$E[2])) ~ paste(min(custom_ages$S[2], custom_ages$E[2]), "to",
                                                                                         max(custom_ages$S[2], custom_ages$E[2])),
                                between(Age_num, 
                                        min(custom_ages$S[3], custom_ages$E[3]),
                                        max(custom_ages$S[3], custom_ages$E[3])) ~ paste(min(custom_ages$S[3], custom_ages$E[3]), "to",
                                                                                         max(custom_ages$S[3], custom_ages$E[3])),
                                between(Age_num, 
                                        min(custom_ages$S[4], custom_ages$E[4]),
                                        max(custom_ages$S[4], custom_ages$E[4])) ~ paste(min(custom_ages$S[4], custom_ages$E[4]), "to",
                                                                                         max(custom_ages$S[4], custom_ages$E[4])),
                                between(Age_num, 
                                        min(custom_ages$S[5], custom_ages$E[5]),
                                        max(custom_ages$S[5], custom_ages$E[5])) ~ paste(min(custom_ages$S[5], custom_ages$E[5]), "to",
                                                                                         max(custom_ages$S[5], custom_ages$E[5])),
                                Age_num > max(custom_ages$S, custom_ages$E, na.rm = TRUE) ~ paste0(max(custom_ages$S, custom_ages$E, na.rm = TRUE) +1, "+")
                                
                                
            ))
        
        ## JH (2025-01-29): rename Age_grp in case there is overlap in any starting and ending ages
        tmp_get_names <- output %>% filter(Age_grp != "Total") %>% pull(Age_grp) %>% unique()
        for(i in seq_along(tmp_get_names)) {
          tmp_ages <- output %>% filter(Age_grp == tmp_get_names[i]) %>% pull(Age_num) %>% unique()
          tmp_min <- min(tmp_ages, na.rm = TRUE)
          tmp_max <- max(tmp_ages, na.rm = TRUE)
          if(tmp_min == 0) {
            new_name <- paste0("LT", tmp_max+1)
          } else if(tmp_max > max(custom_ages$S, custom_ages$E, na.rm = TRUE)) {
            new_name <- paste0(tmp_min, "+")
          } else {
            new_name <- paste0(tmp_min, " to ", tmp_max)
          }
          output <- output %>% mutate(Age_grp = case_when(Age_grp == tmp_get_names[i] ~ new_name, TRUE ~ Age_grp))
          rm(tmp_ages, tmp_min, tmp_max, new_name)
        }; rm(i, tmp_get_names)
        
        output <- output %>%
          mutate(Age = fct_inorder(Age_grp)) %>%
        #!!! MK: keeping Type variable
          group_by(Region, !!rlang::sym(Reg.Type), Year, Type, Gender, Age) %>%
#          group_by(Region, !!rlang::sym(Reg.Type), Year, Gender, Age) %>%
          summarize(Pop = sum(Pop), .groups = "drop") 
      
      }
    }

    ### C. Statistic ----
    
    ## C.1 Count (default)
    # if(input$Statistic_Var == "Count") {
    #   output <- output %>%
    #     filter(Gender %in% input$Gender) %>%
    #     filter(Year %in% input$Year)
    # }
    
    
    ## C.2 Proportions
    if(input$Statistic_Var == "Proportion") {
      
      ifelse(input$Proportion_Var == "Gender",
           ## by gender
           output <- output %>%
             pivot_wider(names_from = "Gender", values_from = "Pop") %>%
             mutate(across(c(M,F,T), ~scales::label_percent(accuracy = 0.1)(janitor::round_half_up(.x/T, digits = 3)))) %>%
             pivot_longer(c(M,F,T), names_to = "Gender", values_to = "Pop"),
           ## by age
           output <- output %>%
             pivot_wider(names_from = "Age", values_from = "Pop") %>%
             #!!! MK: keeping Type variable
             mutate(across(-c(Region, !!rlang::sym(Reg.Type), Year, Type, Gender), ~scales::label_percent(accuracy = 0.1)(janitor::round_half_up(.x/Total, digits = 3)))) %>%
             pivot_longer(-c(Region, !!rlang::sym(Reg.Type), Year, Type, Gender), names_to = "Age", values_to = "Pop")
#             mutate(across(-c(Region, !!rlang::sym(Reg.Type), Year, Gender), ~scales::label_percent(accuracy = 0.1)(janitor::round_half_up(.x/Total, digits = 3)))) %>%
#             pivot_longer(-c(Region, !!rlang::sym(Reg.Type), Year, Gender), names_to = "Age", values_to = "Pop")
           )
    }
    
    ## C.3 Average Age
    if(input$Statistic_Var == "Average Age") {
      
      output <- output %>% ## include all ages in average calculation
        mutate(Total = ifelse(Age == "Total", Pop, NA),
               Age_num = ifelse(Age == "Total", NA, str_remove_all(Age, "\\+")),
               ## make remaining age values numeric
               Age_num = as.numeric(Age_num)) %>%
        fill(Total) %>%
        filter(!is.na(Age_num)) %>%
        #!!! MK: keeping Type variable
        group_by(Region, !!rlang::sym(Reg.Type), Year, Type, Gender, Total) %>%
#        group_by(Region, !!rlang::sym(Reg.Type), Year, Gender, Total) %>%
        summarize(`Average Age` = janitor::round_half_up(weighted.mean(x = Age_num, w = Pop), digits = 1), .groups = "drop")
    }
    
    
    ## C.4 Median Age
    if(input$Statistic_Var == "Median Age") {
      
      output <- output %>% ## include all ages in average calculation
        mutate(Total = ifelse(Age == "Total", Pop, NA),
               Age_num = ifelse(Age == "Total", NA, str_remove_all(Age, "\\+")),
               ## make remaining age values numeric
               Age_num = as.numeric(Age_num)) %>%
        fill(Total) %>%
        filter(!is.na(Age_num)) %>%
        #!!! MK: keeping Type variable
        group_by(Region, !!rlang::sym(Reg.Type), Year, Type, Gender, Total) %>%
#        group_by(Region, !!rlang::sym(Reg.Type), Year, Gender, Total) %>%
        summarize(`Median Age` = median_pop(Age_num, Pop, Total), .groups = "drop")
      
      
    }
    
    ## C.5 Growth
    if(input$Statistic_Var == "Change in Total Population") {
      
      output <- output %>%
        filter(Age == "Total") %>%
        select(-Age) %>%
        #!!! MK: grouping by Type messes up calculation so leaving code here as-is
        group_by(Region, !!rlang::sym(Reg.Type), Gender) %>%
        mutate(`Year-Over-Year Change` = janitor::round_half_up(Pop/lag(Pop) - 1, digits = 3),
               Year = paste(lag(Year), Year, sep = "-")) %>%
        ungroup() %>%
        filter(!is.na(`Year-Over-Year Change`) & Gender %in% input$Gender) %>%
        mutate(`Year-Over-Year Change` = scales::label_percent(accuracy = 0.1)(`Year-Over-Year Change`)) 
      
    }
    
    ### D. filter year/gender, format numeric ----
    output <- output %>%
      filter(Gender %in% input$Gender) %>%
      ## for growth: 2022-2023 or other statistics: 2023
      filter(str_sub(Year, start = -4) %in% input$Year) %>%
      #!!! MK: keeping Type variable
      mutate(across(names(output)[!(names(output) %in% c("Region", Reg.Type, "Year", "Type", "Gender", "Age"))], format, big.mark = ","))
#      mutate(across(names(output)[!(names(output) %in% c("Region", Reg.Type, "Year", "Gender", "Age"))], format, big.mark = ","))    
    
    ### E. customize layout----
    if(input$Statistic_Var %in% c("Count", "Proportion") & input$Column_Var != "None") {
        #!!! MK: if Year column is selected, include Type in pivot wider
        if (input$Column_Var == "Year") {
          output <- output %>% pivot_wider(names_from = c("Type",input$Column_Var), names_expand = F, values_from = "Pop")
        } else {
          output <- output %>% pivot_wider(names_from = input$Column_Var, values_from = "Pop")
        }
#      output <- output %>%
#        pivot_wider(names_from = input$Column_Var, values_from = "Pop")
    }

    output
  })

  ### render table ----
  output$table <- DT::renderDataTable(datatable({
    
      ## call function to create specified data table
      data_df()
      
    },
    filter = "none",
    ## table options: https://shiny.rstudio.com/articles/datatables.html
    options = list(
      pageLength = 10,       ## show only X rows/page; https://datatables.net/reference/option/pageLength
      lengthMenu = c(10, 20, 25, 50), ## choices of pageLength to display
      scrollX = TRUE,        ## allows horizontal scrolling; https://datatables.net/reference/option/scrollX
      dom ="ltpi"
    )
  )
  )
  
  ### reactive download ----
  output$downloadData <- downloadHandler(
    
    filename = function() {
      c("Population_Projections.csv")
    },

    content = function(file) {
      
      #!!! MK: Tweaking code to add header to downloaded output
      df <- data_df()
      col_names <- names(df)
      
      #Reformatting year selection to only show contiguous ranges
      yrs <- input$Year
      yrs <- yrs[!grepl("Estimates", yrs)] %>% as.numeric()  #remove est/proj divider text
      start = c(1, which(diff(yrs) != 1 & diff(yrs) != 0) + 1)  #start of each contiguous sub-range
      end = c(start - 1, length(yrs)) #end of each contiguous sub-range
      yr_ranges <- data.frame(start=yrs[start],end=yrs[end])
      yrs <- apply(yr_ranges,1,function(x) if (x[1]!=x[2]) {paste0(x[1],"-",x[2])} else {x[1]}) %>% str_flatten(",")
#      yrs <- input$Year %>% str_flatten(.," ") %>% str_remove("-- Estimates above, Projections below --")
      
      #Building text block for header
      text <- paste0("
P.E.O.P.L.E. Population Estimates and Projections
Author: BC Stats
Release date: ", PEOPLE_Update_Date,"
Geographic level: ",input$Region.Type,"
Years: ", yrs,"
-------------------------------------------------
")
      #Wrangling the df output so that header text and column names appear before the data values
      header <- read.table(text=text,header=F,sep="\n")
      header <- cbind(header, matrix("",nrow(header),ncol(df)-1) %>% as.data.frame())
      names(header) <- col_names
      df <- rbind(header,col_names,df) %>% unname()
      
      write.csv(df, file, row.names = F, na = "")
#      write.csv(data_df(), file, row.names = FALSE, na = "")  ## col.names = FALSE, append = TRUE,
      #!!! MK: End of code tweak
      
      rv$download_flag <- rv$download_flag + 1
    }
  )
  
  output$downloadTranslation <- downloadHandler(
    
    filename = function() {
      c("lha_translation.csv")
    },
    
    content = function(file) {
      file.copy("data/lha_translation.csv", file)
    }
  )
  
  ### notes ----
  output$notes <- renderUI({
    
    notes <- c()
    
    if(input$Statistic_Var %in% c("Average Age", "Median Age")) {
      max_age <- data1 %>%
        filter(Region.Type == input$Region.Type) %>%
        ## remove empty Age columns (some Regions go to 90+ some to 100+)
        janitor::remove_empty(which = c("rows", "cols")) %>%
        select(contains("+")) %>% 
        names() %>% 
        str_remove("\\+") %>%
        as.numeric()
      
      notes <- notes %>% append(paste("<li>For", 
                                      str_to_lower(input$Statistic_Var),
                                      "calculations, persons",
                                      max_age +1,
                                      "years and older are considered to be",
                                      max_age,
                                      "years old.</li>"))
      
    }
    
    notes <- notes %>% append(
      paste0("<li>This version of P.E.O.P.L.E. was published on <b>",
             PEOPLE_Update_Date, "</b>",
             " and uses as reference Statistics Canada's population estimates for ",
             "<b><a href = 'https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710015201'>Census Divisions</a></b>",
             " published on <b>", date_STATCAN_CD, "</b> and for ", 
             "<b><a href = 'https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710015701'>Health Service Delivery Areas</a></b>",
             " published on <b>", date_STATCAN_HSDA,"</b>.</li>")
    )
    
    notes <- notes %>% append(
      "<li>All figures are as of July 1 and are adjusted for census net undercoverage 
      (including adjustment for incompletely enumerated Indian Reserves).</li>"
    )
    
    notes <- notes %>% append(
      "<li>As of April 2023, Community Health Service Area (CHSA) numbering has
      been updated to reflect the latest version of the boundaries released by 
      the Ministry of Health.</li>"
    )
    
    notes <- notes %>% append(
      paste0("<li>As of January 2020, Local Health Area (LHA) numbering has been 
              updated to reflect the latest version of the boundaries released by the 
              Ministry of Health. Translation between old and new LHA identifiers 
              can be downloaded <b>",
             downloadLink(outputId = "downloadTranslation", label = "here"),
             "</b>.</li>"
            )
    )
    
    notes <- notes %>% append(
      "<li>Wondering about the location of a particular region or its boundaries? Check out the 
      <b><a href = 'https://www2.gov.bc.ca/gov/content/data/geographic-data-services/land-use/administrative-boundaries'>Administrative Boundaries</a></b> 
      page for more information.</li>",
    )
    
    notes <- notes %>% append(
      "<li>Don't see what you need? See our Custom Population Products <b>
      <a href='https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats/custom-products-services/custom-population-products'>page</a>
      </b> for more information.</li>"
    )
    
    HTML(paste0("<ul>", paste(notes, collapse = ""), "</ul><br>"))
    
  })
  

}

## Knit together ui and server ----
shinyApp(ui = ui, server = server)
