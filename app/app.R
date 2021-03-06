# This is a Shiny web application. You can run the application locally by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#   http://shiny.rstudio.com/
#   http://rstudio.github.io/shinydashboard/get_started.html
#
# To deploy an update:
#   1. update code and data, and !! likely dataVersion !!
#   2. load library(rsconnect)
#   3. set working directory to app.R directory (setwd("I:/PEOPLEPROJECTIONS/00 - R_code/shiny_apps/Production/popApp/app"))
#   4. deployApp(appName = "popApp", appId = 958258)
# 
# https://bcstats.shinyapps.io/popApp/

## metadata for app ----
dataVersion <- "Estimates 2020"

## load libraries ----
## installs any missing packages this script uses
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('shiny')) install.packages('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard')
if (!require('rsconnect')) install.packages('rsconnect')
if (!require('DT')) install.packages('DT')
if (!require('GAlogger')) devtools::install_github("bnosac/GAlogger")
if (!require('markdown')) install.packages('markdown')

## Google Analytics ----
ga_set_tracking_id("UA-150850915-1")
ga_set_approval(consent = TRUE)
ga_collect_pageview(page = "/popApp")

## read data ----
data1 <- readRDS("data/data1.rds")  ## by single-year intervals

initVals <- c("Local Health Area", "British Columbia", max(data1$Year), "M", "F", "T") ## c(Region.Type, Region.Name, Year, Gender)

## Define ui layout ----
# UI demonstrating column layouts
ui <- fluidPage(title = "BC Population Estimates",
  theme = "bootstrap.css",
  HTML("<html lang='en'>"),
  fluidRow(
    column(width = 12, 
           style = "background-color:#003366; border-bottom:2px solid #fcba19; position:fixed; z-index:10000",
           tags$header(class="header", style="padding:0 0px 0 0px; display:flex; height:80px; width:100%;",
             tags$div(class="banner", style="display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px",
               a(href="https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
                 img(src = "bcstats_logo_rev.png", title = "BC Stats", height = "80px", alt = "British Columbia - BC Stats"),
                 onclick="gtag"
               ),
               h1("British Columbia - Population Estimates", style="font-weight:400; color:white; margin: 5px 5px 0 18px;")
             )
           )
    ),
    column(width = 12,
           style = "margin-top:100px",
           
           ## creating tabs here
           tabsetPanel(
             id = "tabs",
             
             ## Main tab ----
             tabPanel(title = "Main",
                      tags$head(tags$style(type='text/css', ".nav-tabs {font-size: 20px} ")),
                      
                      sidebarLayout(
                        sidebarPanel(style = "background-color:#F2F2F2;",
                                     tags$fieldset(
                                       tags$legend(h3("Step 1: Select data")),
                                       HTML("Use the Ctrl or Shift key to select multiple entries."),
                                       br(),br(),
                                       uiOutput("Region.Type"),
                                       uiOutput("Region.Name"),
                                       uiOutput("Year"),
                                       uiOutput("Gender")
                                     ),
                                     br(),
                                     tags$fieldset(
                                       HTML(paste0("Produced by BC Stats ", "<br>", "Data version: ", 
                                                   dataVersion))
                                     )
                        ),  ## end of sidebarPanel
                        
                        mainPanel(
                          ## Age selection ----
                          tags$fieldset(style = "margin-top:20px;",
                                        tags$legend(h3("Step 2: Select age format")),
                                        column(width = 12,
                                               column(width = 3,
                                                      tags$fieldset(tags$legend(h4("Select type of age group")),
                                                                    uiOutput("Age_Type"))
                                               ),  ## end of column
                                               ## Conditional panels: only show if "custom" age type is selected
                                               # https://shiny.rstudio.com/reference/shiny/1.0.5/conditionalPanel.html
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
                                                                                          min = 0, max = 99)),
                                                                      column(width = 6,
                                                                             numericInput(inputId = "end1", 
                                                                                          label = "To - 1st", 
                                                                                          value = NA, 
                                                                                          min = 0, max = 99)), 
                                                                      column(width = 6,
                                                                             numericInput(inputId = "start2", 
                                                                                          label = "From - 2nd", 
                                                                                          value = NA, 
                                                                                          min = 0, max = 99)), 
                                                                      column(width = 6,
                                                                             numericInput(inputId = "end2", 
                                                                                          label = "To - 2nd", 
                                                                                          value = NA, 
                                                                                          min = 0, max = 99)), 
                                                                      column(width = 6,
                                                                             numericInput(inputId = "start3", 
                                                                                          label = "From - 3rd", 
                                                                                          value = NA, 
                                                                                          min = 0, max = 99)), 
                                                                      column(width = 6,
                                                                             numericInput(inputId = "end3", 
                                                                                          label = "To - 3rd", 
                                                                                          value = NA, 
                                                                                          min = 0, max = 99)), 
                                                                      column(width = 6,
                                                                             numericInput(inputId = "start4", 
                                                                                          label = "From - 4th", 
                                                                                          value = NA, 
                                                                                          min = 0, max = 99)),
                                                                      column(width = 6,
                                                                             numericInput(inputId = "end4", 
                                                                                          label = "To - 4th", 
                                                                                          value = NA, 
                                                                                          min = 0, max = 99))
                                                        )  ## end of tags$fieldset
                                                      )  ## end of conditionalPanel
                                               )
                                        )
                          ),  ## end of tags$fieldset (Age selection)
                          ## Actions and table ----
                          br(),
                          tags$fieldset(
                            tags$legend(h3("Step 3: Action")),
                            column(width = 12,
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
                          
                          ## Notes ----
                          tags$fieldset(
                            tags$legend(h3("Notes")),
                            HTML(paste0("<ul><li>All figures are as of July 1 and are adjusted for 
                                                        census net undercoverage (including adjustment for 
                                                        incompletely enumerated Indian Reserves).</li>",
                                        "<li>As of January 2020, Local Health Area (LHA) numbering 
                                                        has been updated to reflect the latest version of 
                                                        the boundaries released by the Ministry of Health. 
                                                        Translation between old and new LHA identifiers 
                                                        can be downloaded <b>", 
                                        downloadLink(outputId = "downloadTranslation", label = "here"),
                                        "</b>.</li>",
                                        "<li>Don't see what you need? See our Custom 
                                                        Population Products <b>
                                                        <a href='https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats/custom-products-services/custom-population-products'>page</a>
                                                        </b> for more information.</li>","</ul><br>"))
                          )  ## end of tags$fieldset (Notes)
                          ## ----
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
    column(width = 12,
           style = "background-color:#003366; border-top:2px solid #fcba19;",
           tags$footer(class="footer",
                       tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                        tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                        tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                        tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                        tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                        tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                        tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                )
                       )
           )
    )  ## end of column "footer"
    ## ----
  )  ## end of fluidrow
)

## Define server logic ----
server <- function(input, output, session) {

  ## selections ----
  ## defaults: selectInput(inputId, label, choices, selected = NULL, multiple = FALSE,
  ##                       selectize = TRUE, width = NULL, size = NULL)
  ## size = how many items to show in box, requires selectize = FALSE

  ## select Region.Type, just one
  output$Region.Type <- renderUI({
    selectInput(inputId = "Region.Type",
                label = h4("Select a region type"),
                choices = unique(data1$Region.Type),
                selected = initVals[1],         ## default selection: "Local Health Area"
                selectize = FALSE, size = 10    ## forces all 10 options to be shown at once (not drop-down)
                )
  })

  ## select Region(s) within selected Region.Type, multiples OK
  output$Region.Name <- renderUI({
    selectInput(inputId = "Region.Name",
                label = h4("Select region(s)"),
                choices = NULL,
                multiple = TRUE,
                selectize = FALSE, size = 7)
  })

  ## update Region.Name choices based on selected Region.Type
  observeEvent(input$Region.Type,{
    
    unique_num <- unique(data1$Region[data1$Region.Type == input$Region.Type])
    unique_name <- unique(data1$Region.Name[data1$Region.Type == input$Region.Type])
    display_name <- as.list(paste0(unique_num, " - ", unique_name))
    
    choices_list <- as.list(unique_name)
    names(choices_list) <- display_name
    
    updateSelectInput(session,
                      inputId = "Region.Name",
                      choices = choices_list,
                      selected = initVals[2]  ## default selection: "British Columbia"
                      )
  })

  ## select Year(s), multiples OK
  output$Year <- renderUI({
    selectInput(inputId = "Year",
                label = h4("Select year(s)"),
                choices = unique(data1$Year),
                multiple = TRUE,
                selectize = FALSE, size = 7)
  })
  
  ## update Year(s) choices based on selected Region.Type
  observeEvent(input$Region.Type,{
    
    unique_year <- #sort(
      unique((data1 %>% filter(Region.Type == input$Region.Type))$Year) 
                        #, decreasing = TRUE)
    
    updateSelectInput(session,
                      inputId = "Year",
                      choices = unique_year,
                      selected = initVals[3]  ## default selection: max year
                      )
  })

  ## select Sex(es), multiples OK
  output$Gender <- renderUI({
    selectInput(inputId = "Gender",
                label = h4("Select gender(s)"),
                choices = c("Males" = "M", "Females" = "F", "Totals" = "T"),
                selected = initVals[-(1:3)],  ## default selection: all ("M"ales, "F"emales, "T"otals)
                multiple = TRUE,
                selectize = FALSE, size = 3) ## QQ: Is 4 a minimum? It's ignoring size=3
  })

  ## select type of age group, just one
  output$Age_Type <- renderUI({
    radioButtons(inputId = "Age_Type",
                 label = NULL,
                 choices = c("Single Year Age Groups", "5-year Age Groups", "Totals", 
                             "Custom Age Groups" = "custom"),
                 selected = "Totals")
  })

  ## example table for custom age groups (as text to keep decimals out)
  output$example_table <- renderTable({
    matrix(data = c("15", "24",  "25", "54",  "55", "64",  "65", "74"),
           nrow = 4, ncol = 2, byrow = TRUE, dimnames = list(c(1:4), c("From", "To")))
  })

  ## initial table with default selections ----
  
  ## initVals <- c(Region.Type, Region.Name, Year, Gender)
  data_init <- function(data1, initVals) {
    
    data1[data1$Region.Type == initVals[1], ] %>%
      filter(Region.Name == initVals[2]) %>%
      filter(Year == initVals[3]) %>%
      # filter(Gender == initVals[4]) %>%
      select(Region, !!initVals[1] := Region.Name, Year, Gender, Total)
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
  
  ## reactive resetButton send analytics when reset ----
  observeEvent(input$resetButton, {
    
    ga_collect_event(event_category = "resetButton", 
                     event_label = "Reset", 
                     event_action = "Reset application")
    
    ## just reload the session
    session$reload()

  })

  ## reactive send analytics when download ----
  rv <- reactiveValues(download_flag = 0)
  
  observeEvent(rv$download_flag, {
    
    ga_collect_event(event_category = "downloadButton", 
                     event_label = paste0("Download: ", input$Age_Type, ", ", input$Region.Type), 
                     event_action = "Download data")
    
  }, ignoreInit = TRUE)
  
  ## reactive send analytics when query table ----
  observeEvent(input$goButton, {
    
    ga_collect_event(event_category = "goButton", 
                     event_label = paste0("Query: ", input$Age_Type, ", ", input$Region.Type), 
                     event_action = "Generate data")
    
  })


  ## reactive data table and download ----
  ## create reactive values for input data to create table and download data
  data_df <- eventReactive(input$goButton, {
    
    ## with input$goButton in eventReactive(), nothing will happen until button clicked
    
    showDefaultTable(FALSE)  ## now hide initial default table
    
    ## A. set df as appropriate dataset depending on age group type chosen
    if(input$Age_Type == "Totals") {
      df <- data1 %>% select(Region, Region.Name, Region.Type, Year, Gender, Total)
    }

    if(input$Age_Type == "Single Year Age Groups") {
      df <- data1
    }

    if(input$Age_Type == "5-year Age Groups") {
      df <- data1 %>%
        mutate(`LT1` = `0`,
               `1-4` = `1` + `2` + `3` + `4`,
               `5-9` = `5` + `6` + `7` + `8` + `9`,
               `10-14` = `10` + `11` + `12` + `13` + `14`,
               `15-19` = `15` + `16` + `17` + `18` + `19`,
               `20-24` = `20` + `21` + `22` + `23` + `24`,
               `25-29` = `25` + `26` + `27` + `28` + `29`,
               `30-34` = `30` + `31` + `32` + `33` + `34`,
               `35-39` = `35` + `36` + `37` + `38` + `39`,
               `40-44` = `40` + `41` + `42` + `43` + `44`,
               `45-49` = `45` + `46` + `47` + `48` + `49`,
               `50-54` = `50` + `51` + `52` + `53` + `54`,
               `55-59` = `55` + `56` + `57` + `58` + `59`,
               `60-64` = `60` + `61` + `62` + `63` + `64`,
               `65-69` = `65` + `66` + `67` + `68` + `69`,
               `70-74` = `70` + `71` + `72` + `73` + `74`,
               `75-79` = `75` + `76` + `77` + `78` + `79`,
               `80-84` = `80` + `81` + `82` + `83` + `84`,
               `85-89` = `85` + `86` + `87` + `88` + `89`,
               `90++` = `90+`,
               Total2 = Total) %>%
        select(-(which(names(data1) == "0"):which(names(data1) == "90+")), `90+` = `90++`, -Total2)

    }

    # if(input$Age_Type == "Custom Age Groups") {
    if(input$Age_Type == "custom") {

      ## 0a. create data frame of custom age groups user typed in
      custom_ages <- data.frame(S = c(input$start1, input$start2, input$start3, input$start4),
                                E = c(input$end1, input$end2, input$end3, input$end4),
                                stringsAsFactors = FALSE)

      ## 0b. if input (start or end) is decimal,  drop it from calculations.
      ## if input (start or end) is < min or > max single age, replace with min/max in df and label.
      ## if startX > endX, calculation will be correct, and use min and max in labelling for right order
      min_single_age <- 0
      max_single_age <- 89
      custom_ages <- custom_ages %>%
        mutate_at(c("S", "E"), ~ case_when(str_detect(.x, pattern = "\\.") ~ NA_character_,  ## anything with decimal will be dropped
                                           .x < min_single_age ~ "0",    ## any whole # < 0 will be re-set to 0
                                           .x > max_single_age ~ "90+",  ## any whole # > 89 will be re-set to 90+
                                           TRUE ~ as.character(.x)))

      ## 1. if custom age group 1 is not NA, calculate and display its data
      if(!is.na(custom_ages$S[1]) & !is.na(custom_ages$E[1])){

        ## 1a. create label of custom age group 1 (to be able to use as dynamic name in select)
        ## use min number first, then max number (in case entered backwards)
        A1 <- c(paste0(min(custom_ages$S[1], custom_ages$E[1]), " - ", max(custom_ages$S[1], custom_ages$E[1])))

        ## 1b. create custom age group 1, drop single-year columns, place Total at end (after new variable)
        df <- data1 %>%
          mutate(
            !!A1 := rowSums(data1[which(names(data1) == custom_ages$S[1]):
                                    which(names(data1) == custom_ages$E[1])], dims = 1)) %>%
          select(-(which(names(data1) == 0):which(names(data1) == "90+")))

      } else {
        ## otherwise, just select out single-age columns (on odd chance someone skips row 1)
        df <- data1 %>%
          select(-(which(names(data1) == 0):which(names(data1) == "90+")))
      }

      ## 2. if custom age group 2 is not NA, calculate and display its data
      if(!is.na(custom_ages$S[2]) & !is.na(custom_ages$E[2])){

        A2 <- c(paste0(min(custom_ages$S[2], custom_ages$E[2]), " - ", max(custom_ages$S[2], custom_ages$E[2])))
        df <- df %>%
          mutate(
            !!A2 := rowSums(data1[which(names(data1) == custom_ages$S[2]):
                                    which(names(data1) == custom_ages$E[2])], dims = 1))

      }

      ## 3. if custom age group 3 is not NA, calculate and display its data
      if(!is.na(custom_ages$S[3]) & !is.na(custom_ages$E[3])){

        A3 <- c(paste0(min(custom_ages$S[3], custom_ages$E[3]), " - ", max(custom_ages$S[3], custom_ages$E[3])))
        df <- df %>%
          mutate(
            !!A3 := rowSums(data1[which(names(data1) == custom_ages$S[3]):
                                    which(names(data1) == custom_ages$E[3])], dims = 1))

      }

      ## 4. if custom age group 4 is not NA, calculate and display its data
      if(!is.na(custom_ages$S[4]) & !is.na(custom_ages$E[4])){

        A4 <- c(paste0(min(custom_ages$S[4], custom_ages$E[4]), " - ", max(custom_ages$S[4], custom_ages$E[4])))
        df <- df %>%
          mutate(
            !!A4 := rowSums(data1[which(names(data1) == custom_ages$S[4]):
                                    which(names(data1) == custom_ages$E[4])], dims = 1))

      }

      ## put Total column at end
      df <- df %>%
        mutate(Total2 = Total) %>%
        select(-Total) %>%
        rename(Total = Total2)

    }

    ## B. make selections
    Reg.Type <- c(input$Region.Type)  ## to be able to use as dynamic name in select
    df[df$Region.Type == input$Region.Type, ] %>%
      filter(Region.Name %in% input$Region.Name) %>%
      filter(Year %in% input$Year) %>%
      filter(Gender %in% input$Gender) %>%
      select(Region, !!Reg.Type := Region.Name, everything(), -Region.Type)

    ## C. call data_df() in renderDataTable to create table in app
    ## D. call data_df() in downloadHandler to download data

  })

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
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      c("Population_Estimates.csv")
    },

    content = function(file) {
      write.csv(data_df(), file, row.names = FALSE, na = "")  ## col.names = FALSE, append = TRUE,
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

}

## Knit together ui and server ----
shinyApp(ui = ui, server = server)