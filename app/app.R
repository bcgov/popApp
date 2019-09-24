# This is a Shiny web application. You can run the application locally by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#   http://shiny.rstudio.com/
#
#   http://rstudio.github.io/shinydashboard/get_started.html
#

## load libraries  ----
## installs any missing packages this script uses
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('shiny')) install.packages('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard')
if (!require('rsconnect')) install.packages('rsconnect')

## read data ----
data1 <- readRDS("data/data1.rds")  ## by single-year intervals

# UI demonstrating column layouts
ui <- fluidPage(
  title = "Hello Shiny!",
  fluidRow(
    column(width = 12, 
           style = "background-color:#003366; border-bottom:2px solid #fcba19;",
           
           tags$header(class="header", style="padding:0 80px 0 80px; display:flex; height:80px; width:100%;",
             tags$div(class="banner", style="display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px",
               a(href="https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
                 img(src = "bcstats_logo_rev.png", title = "BC Stats", height = "80px")
               ),
               h4("British Columbia - Population Estimates", style="font-family:Verdana; font-weight:600; color:white; margin: 5px 5px 0 18px;")
             )
           )
            
    ),
    column(width = 12,
           sidebarLayout(
             sidebarPanel(style="background-color:#F2F2F2",
               tags$em(helpText("This interactive tool allows you to select the region(s), year(s) and sex(es)
                                 of interest for age groupings of your choice.",
                                style="font-size:14px; margin-left:10px; margin-top:10px; display:block; color:#494949")),
               uiOutput("Region.Type"),
               uiOutput("Region.Name"),
               uiOutput("Year"),
               uiOutput("Gender")
             ),
             mainPanel(
               fluidRow(
                       box(
                           helpText("Select a region type, and then the region(s), year(s) and sex(es) of interest.
                                Use the Ctrl and/or Shift keys to select multiple entries. Then select whether you'd
                                like results by 5-year age groups, totals, or your own custom age groupings. If you
                                would like to specify your own custom age groups, select 'Custom Age Groups' and
                                enter them in the boxes to the right as seen in the example below. Then click
                                'Generate output'. You can view the results on screen or download a CSV file.",
                                 style="font-size:14px; color:#494949"),
                           column(width = 3, br(),
                                  uiOutput("Age_Type")
                                  ),  ## end of column
                           column(width = 3, br(),
                                  tags$b("Example:"),
                                  tableOutput(outputId = "example_table")
                                  ),  ## end of column
                            fluidRow(
                              tags$b("Custom Age Types:"),
                              br(),
                              column(width = 3,
                                        tags$b("From"),
                                        numericInput(inputId = "start1", label = NULL, value = NA, min = 0, max = 99),
                                        numericInput(inputId = "start2", label = NULL, value = NA, min = 0, max = 99),
                                        numericInput(inputId = "start3", label = NULL, value = NA, min = 0, max = 99),
                                        numericInput(inputId = "start4", label = NULL, value = NA, min = 0, max = 99),
                                        numericInput(inputId = "start5", label = NULL, value = NA, min = 0, max = 99)
                                        ), ## end of column (starts)
                                 column(width = 3,
                                        tags$b("To"),
                                        numericInput(inputId = "end1", label = NULL, value = NA, min = 0, max = 99),
                                        numericInput(inputId = "end2", label = NULL, value = NA, min = 0, max = 99),
                                        numericInput(inputId = "end3", label = NULL, value = NA, min = 0, max = 99),
                                        numericInput(inputId = "end4", label = NULL, value = NA, min = 0, max = 99),
                                        numericInput(inputId = "end5", label = NULL, value = NA, min = 0, max = 99)
                                        ) ## end of column (ends)
                             ),  ## end of fluidRow
                           actionButton(inputId = "goButton", label = "Generate output"),
                           actionButton(inputId = "resetButton", label = "Reset selection"),
                           downloadButton(outputId = "downloadData", label = "Download data as csv"),
                           br(),br(),
                           width = 12,
                           dataTableOutput(outputId = "table")
                           )  ## end of box; boxes need to be put in a row (or column)
                       )  ## end of fluidRow
             )
           )
    ),
    column(width = 12,
           style = "background-color:#003366; border-top:2px solid #fcba19;",
           
            tags$footer(class="footer", style="font-family:Verdana;",
              tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                  tags$li(a(href=".", "Home", style="font-size:0.813em; font-weight:normal; font-family:Verdana; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href=".", "Disclaimer", style="font-size:0.813em; font-weight:normal; font-family:Verdana; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href=".", "Privacy", style="font-size:0.813em; font-weight:normal; font-family:Verdana; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href=".", "Accessibility", style="font-size:0.813em; font-weight:normal; font-family:Verdana; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href=".", "Copyright", style="font-size:0.813em; font-weight:normal; font-family:Verdana; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href=".", "Contact", style="font-size:0.813em; font-weight:normal; font-family:Verdana; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                )
              )
             )
    )
  )
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
                label = "Select a region type:",
                choices = unique(data1$Region.Type),
                selected = "Local Health Area"
                , selectize = FALSE, size = 9    ## forces all 9 options to be shown at once (not drop-down)
                )
  })

  ## select Region(s) within selected Region.Type, multiples OK
  output$Region.Name <- renderUI({
    selectInput(inputId = "Region.Name",
                label = "Select region(s):",
                choices = unique(data1$Region.Name[data1$Region.Name == unique(data1$Region.Name)[1]]),
                multiple = TRUE,
                selectize = FALSE, size = 7)
  })

  ## update Region.Name choices based on selected Region.Type
  observeEvent(input$Region.Type,{
    updateSelectInput(session,
                      inputId = "Region.Name",
                      choices = unique(data1$Region.Name[data1$Region.Type == input$Region.Type]))
  })

  ## select Year(s), multiples OK
  output$Year <- renderUI({
    selectInput(inputId = "Year",
                label = "Select year(s):",
                choices = unique(data1$Year),
                multiple = TRUE,
                selectize = FALSE, size = 7)
  })

  ## select Sex(es), multiples OK
  output$Gender <- renderUI({
    selectInput(inputId = "Gender",
                label = "Select sex(es):",
                choices = c("Males" = "M", "Females" = "F", "Totals" = "T"),
                multiple = TRUE,
                selectize = FALSE, size = 3) ## QQ: Is 4 a minimum? It's ignoring size=3
  })

  ## select type of age group, just one
  output$Age_Type <- renderUI({
    radioButtons(inputId = "Age_Type:",
                 label = "Select type of age group:",
                 choices = c("Single Year Age Groups", "5-year Age Groups", "Totals", "Custom Age Groups"),
                 selected = "Totals")
  })

  ## example table for custom age groups (as text to keep decimals out)
  output$example_table <- renderTable({
    matrix(data = c("15", "24",  "25", "54",  "55", "64",  "65", "99", "__", "__"),
           nrow = 5, ncol = 2, byrow = TRUE, dimnames = list(c(1:5), c("From", "To")))
  })


  ## reactive resetButton ----
  observeEvent(input$resetButton, {

    ## just reload the session
    session$reload()

  })


  ## reactive data table and download ----
  ## Create reactive values for input data to create table and download data
  data_df <- eventReactive(input$goButton, {
    ## with input$goButton in eventReactive(), nothing will happen until button clicked

    ## A. set df as appropriate dataset depending on age group type chosen
    if(input$Age_Type == "Totals") {
      df <- data1 %>%
        select(Region, Region.Name, Region.Type, Year, Gender, Total)
    }

    if(input$Age_Type == "Single Year Age Groups") {
      df <- data1
    }

    if(input$Age_Type == "5-year Age Groups") {
      #df <- data5
      df <- data1 %>%
        mutate(`<1` = `0`,
               `1-4` = `1` + `2` + `3` + `4`,
               `5-9` = `5` + `6` + `7` + `8` + `9`,
               `10-14` = `10` + `11` + `12` + `13` + `14`,
               `15-19` = `15` + `16` + `17` + `18` + `19`,
               `20-24` = `20` + `21` + `22` + `23` + `24`,
               `25-29` = `25` + `26` + `27` + `28` + `29`,
               `30-34` = `30` + `31` + `32` + `33` + `34`,
               `35-39` = `35` + `36` + `37` + `38` + `49`,
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

    if(input$Age_Type == "Custom Age Groups") {

      ## 0a. create data frame of custom age groups user typed in
      custom_ages <- data.frame(S = c(input$start1, input$start2, input$start3, input$start4, input$start5),
                                E = c(input$end1, input$end2, input$end3, input$end4, input$end5),
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

      ## 5. if custom age group 5 is not NA, calculate and display its data
      if(!is.na(custom_ages$S[5]) & !is.na(custom_ages$E[5])){

        A5 <- c(paste0(min(custom_ages$S[5], custom_ages$E[5]), " - ", max(custom_ages$S[5], custom_ages$E[5])))
        df <- df %>%
          mutate(
            !!A5 := rowSums(data1[which(names(data1) == custom_ages$S[5]):
                                    which(names(data1) == custom_ages$E[5])], dims = 1))

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


  output$table <- renderDataTable({

    ## call function to create specified data table
    data_df()

    },
    ## table options: https://shiny.rstudio.com/articles/datatables.html
    options = list(
      pageLength = 10,       ## show only X rows/page; https://datatables.net/reference/option/pageLength
      lengthMenu = c(10, 20, 25, 50), ## choices of pageLength to display
      scrollX = TRUE,        ## allows horizontal scrolling; https://datatables.net/reference/option/scrollX
      searchable = FALSE
    )
  )


  output$downloadData <- downloadHandler(

    filename = function() {
      c("Population_Estimates.csv")
    },

    content = function(file) {
      write.csv(data_df(), file, row.names = FALSE, na = "")  ## col.names = FALSE, append = TRUE,
    }
  )

}

## Knit together ui and server ----
shinyApp(ui = ui, server = server)

############################
############################

# # This is a Shiny web application. You can run the application locally by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #   http://shiny.rstudio.com/
# #
# #   http://rstudio.github.io/shinydashboard/get_started.html
# #
# 
# ## load libraries  ----
# ## installs any missing packages this script uses
# if (!require('tidyverse')) install.packages('tidyverse')
# if (!require('shiny')) install.packages('shiny')
# if (!require('shinydashboard')) install.packages('shinydashboard')
# if (!require('rsconnect')) install.packages('rsconnect')
# 
# 
# ## read data ----
# data1 <- readRDS("data/data1.rds")  ## by single-year intervals
# #data5 <- readRDS("data/data5.rds")  ## by 5-year intervals
# 
# 
# ## Define UI for application ----
# 
# ui <- shinydashboard::dashboardPage(
# 
#   ## header ----
#   dashboardHeader(title = h4(HTML("Sub-Provincial<br/>Population Estimates")),  ## add break to header
#                   titleWidth = 350,
#                   tags$li(class = "dropdown",
#                           style = "width:auto;",
#                           tags$td(a(href = "https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
#                                     img(src = "bcstats_logo_rev.png", title = "BC Stats", height = "100px"),
#                                     style = "display:block; padding-top:1px; padding-bottom:1px; padding-right:10px;"
#                                     )  ## end of a(href)
#                                   )#,  ## end of tags$td
#                           #tags$script(HTML("$('body').addClass('fixed');"))  ## Forces header and sidebar to not scroll,
#                           )  ## end of tags$li
#                   ),  ## end of dashboardHeader
# 
# 
#   ## sidebar ----
#   dashboardSidebar(
#     width = 350,
#     ## align left side of download button with selectInput menu
#     tags$style(HTML(".button1{margin-left: 15px} ")),
#     br(),
#     tags$em(helpText("This interactive tool allows you to select the region(s), year(s) and sex(es)
#                       of interest for age groupings of your choice.",
#                      style="font-size:16px; margin-left:10px; margin-top:10px; display:block; color:#fff")),
#     uiOutput("Region.Type"),
#     uiOutput("Region.Name"),
#     uiOutput("Year"),
#     uiOutput("Gender")
#     ## Moved Download Data button beside other buttons (Generate output, Reset selection)
#     # , br(),br(),
#     # tags$b("Download as csv:",
#     #        style = "font-size:14px; margin-left:15px; margin-top:10px; margin-bottom:5px; display:block; color:#fff"),
#     # downloadButton(outputId = "downloadData", label = "Download data", class = "button1")
#   ),  ## end of dashboardSidebar
# 
#     
#   ## body ----
#   dashboardBody(
#     fluidRow(#h2("First tab content"),
#       box(
#           helpText("Select a region type, and then the region(s), year(s) and sex(es) of interest. 
#                Use the Ctrl and/or Shift keys to select multiple entries. Then select whether you'd 
#                like results by 5-year age groups, totals, or your own custom age groupings. If you 
#                would like to specify your own custom age groups, select 'Custom Age Groups' and 
#                enter them in the boxes to the right as seen in the example below. Then click 
#                'Generate output'. You can view the results on screen or download a CSV file.",
#                 style="font-size:16px"),
#           column(width = 3, br(),
#                  uiOutput("Age_Type")
#                  ),  ## end of column
#           column(width = 3, br(),
#                  tags$b("Example:"),
#                  tableOutput(outputId = "example_table")
#                  ),  ## end of column
#            fluidRow(
#              tags$b("Custom Age Types:"),
#              br(),
#              column(width = 3,
#                        tags$b("From"),
#                        numericInput(inputId = "start1", label = NULL, value = NA, min = 0, max = 99),
#                        numericInput(inputId = "start2", label = NULL, value = NA, min = 0, max = 99),
#                        numericInput(inputId = "start3", label = NULL, value = NA, min = 0, max = 99),
#                        numericInput(inputId = "start4", label = NULL, value = NA, min = 0, max = 99),
#                        numericInput(inputId = "start5", label = NULL, value = NA, min = 0, max = 99)
#                        ), ## end of column (starts)
#                 column(width = 3,
#                        tags$b("To"),
#                        numericInput(inputId = "end1", label = NULL, value = NA, min = 0, max = 99),
#                        numericInput(inputId = "end2", label = NULL, value = NA, min = 0, max = 99),
#                        numericInput(inputId = "end3", label = NULL, value = NA, min = 0, max = 99),
#                        numericInput(inputId = "end4", label = NULL, value = NA, min = 0, max = 99),
#                        numericInput(inputId = "end5", label = NULL, value = NA, min = 0, max = 99)
#                        ) ## end of column (ends)
#             ),  ## end of fluidRow
#           actionButton(inputId = "goButton", label = "Generate output"),
#           actionButton(inputId = "resetButton", label = "Reset selection"),
#           downloadButton(outputId = "downloadData", label = "Download data as csv"),
#           br(),br(),
#           width = 12,
#           dataTableOutput(outputId = "table")
#           )  ## end of box; boxes need to be put in a row (or column)
#       )  ## end of fluidRow
#   )  ## end of dashboardBody
# )  ## end of dashboardPage
# 
# 
# 
# 
# ## Define server logic ----
# server <- function(input, output, session) {
#   
#   ## selections ----
#   ## defaults: selectInput(inputId, label, choices, selected = NULL, multiple = FALSE, 
#   ##                       selectize = TRUE, width = NULL, size = NULL)
#   ## size = how many items to show in box, requires selectize = FALSE
#   
#   ## select Region.Type, just one
#   output$Region.Type <- renderUI({
#     selectInput(inputId = "Region.Type",
#                 label = "Select a region type:",
#                 choices = unique(data1$Region.Type),
#                 selected = "Local Health Area"
#                 , selectize = FALSE, size = 9    ## forces all 9 options to be shown at once (not drop-down)
#                 )
#   })
# 
#   ## select Region(s) within selected Region.Type, multiples OK
#   output$Region.Name <- renderUI({
#     selectInput(inputId = "Region.Name",
#                 label = "Select region(s):",
#                 choices = unique(data1$Region.Name[data1$Region.Name == unique(data1$Region.Name)[1]]),
#                 multiple = TRUE,
#                 selectize = FALSE, size = 7)
#   })
# 
#   ## update Region.Name choices based on selected Region.Type
#   observeEvent(input$Region.Type,{
#     updateSelectInput(session,
#                       inputId = "Region.Name",
#                       choices = unique(data1$Region.Name[data1$Region.Type == input$Region.Type]))
#   })
# 
#   ## select Year(s), multiples OK
#   output$Year <- renderUI({
#     selectInput(inputId = "Year",
#                 label = "Select year(s):",
#                 choices = unique(data1$Year),
#                 multiple = TRUE,
#                 selectize = FALSE, size = 7)
#   })
# 
#   ## select Sex(es), multiples OK
#   output$Gender <- renderUI({
#     selectInput(inputId = "Gender",
#                 label = "Select sex(es):",
#                 choices = c("Males" = "M", "Females" = "F", "Totals" = "T"),
#                 multiple = TRUE,
#                 selectize = FALSE, size = 3) ## QQ: Is 4 a minimum? It's ignoring size=3
#   })
#   
#   ## select type of age group, just one
#   output$Age_Type <- renderUI({
#     radioButtons(inputId = "Age_Type:",
#                  label = "Select type of age group:", 
#                  choices = c("Single Year Age Groups", "5-year Age Groups", "Totals", "Custom Age Groups"),
#                  selected = "Totals")
#   })
# 
#   ## example table for custom age groups (as text to keep decimals out)
#   output$example_table <- renderTable({
#     matrix(data = c("15", "24",  "25", "54",  "55", "64",  "65", "99", "__", "__"), 
#            nrow = 5, ncol = 2, byrow = TRUE, dimnames = list(c(1:5), c("From", "To")))
#   })
# 
# 
#   ## reactive resetButton ----
#   observeEvent(input$resetButton, {
#     
#     ## just reload the session
#     session$reload()
#     
#   })
# 
# 
#   ## reactive data table and download ----
#   ## Create reactive values for input data to create table and download data
#   data_df <- eventReactive(input$goButton, {
#     ## with input$goButton in eventReactive(), nothing will happen until button clicked
#     
#     ## A. set df as appropriate dataset depending on age group type chosen
#     if(input$Age_Type == "Totals") {
#       df <- data1 %>% 
#         select(Region, Region.Name, Region.Type, Year, Gender, Total)
#     }
#     
#     if(input$Age_Type == "Single Year Age Groups") {
#       df <- data1
#     }
#     
#     if(input$Age_Type == "5-year Age Groups") {
#       #df <- data5
#       df <- data1 %>%
#         mutate(`<1` = `0`,
#                `1-4` = `1` + `2` + `3` + `4`,
#                `5-9` = `5` + `6` + `7` + `8` + `9`,
#                `10-14` = `10` + `11` + `12` + `13` + `14`,
#                `15-19` = `15` + `16` + `17` + `18` + `19`,
#                `20-24` = `20` + `21` + `22` + `23` + `24`,
#                `25-29` = `25` + `26` + `27` + `28` + `29`,
#                `30-34` = `30` + `31` + `32` + `33` + `34`,
#                `35-39` = `35` + `36` + `37` + `38` + `49`,
#                `40-44` = `40` + `41` + `42` + `43` + `44`,
#                `45-49` = `45` + `46` + `47` + `48` + `49`,
#                `50-54` = `50` + `51` + `52` + `53` + `54`,
#                `55-59` = `55` + `56` + `57` + `58` + `59`,
#                `60-64` = `60` + `61` + `62` + `63` + `64`,
#                `65-69` = `65` + `66` + `67` + `68` + `69`,
#                `70-74` = `70` + `71` + `72` + `73` + `74`,
#                `75-79` = `75` + `76` + `77` + `78` + `79`,
#                `80-84` = `80` + `81` + `82` + `83` + `84`,
#                `85-89` = `85` + `86` + `87` + `88` + `89`,
#                `90++` = `90+`, 
#                Total2 = Total) %>%
#         select(-(which(names(data1) == "0"):which(names(data1) == "90+")), `90+` = `90++`, -Total2)
# 
#     }
#     
#     if(input$Age_Type == "Custom Age Groups") {
#       
#       ## 0a. create data frame of custom age groups user typed in
#       custom_ages <- data.frame(S = c(input$start1, input$start2, input$start3, input$start4, input$start5), 
#                                 E = c(input$end1, input$end2, input$end3, input$end4, input$end5), 
#                                 stringsAsFactors = FALSE)
#       
#       ## 0b. if input (start or end) is decimal,  drop it from calculations.
#       ## if input (start or end) is < min or > max single age, replace with min/max in df and label.
#       ## if startX > endX, calculation will be correct, and use min and max in labelling for right order
#       min_single_age <- 0
#       max_single_age <- 89
#       custom_ages <- custom_ages %>% 
#         mutate_at(c("S", "E"), ~ case_when(str_detect(.x, pattern = "\\.") ~ NA_character_,  ## anything with decimal will be dropped
#                                            .x < min_single_age ~ "0",    ## any whole # < 0 will be re-set to 0
#                                            .x > max_single_age ~ "90+",  ## any whole # > 89 will be re-set to 90+
#                                            TRUE ~ as.character(.x)))
#       
#       ## 1. if custom age group 1 is not NA, calculate and display its data
#       if(!is.na(custom_ages$S[1]) & !is.na(custom_ages$E[1])){
#         
#         ## 1a. create label of custom age group 1 (to be able to use as dynamic name in select)
#         ## use min number first, then max number (in case entered backwards)
#         A1 <- c(paste0(min(custom_ages$S[1], custom_ages$E[1]), " - ", max(custom_ages$S[1], custom_ages$E[1])))
#         
#         ## 1b. create custom age group 1, drop single-year columns, place Total at end (after new variable)
#         df <- data1 %>%
#           mutate(
#             !!A1 := rowSums(data1[which(names(data1) == custom_ages$S[1]):
#                                     which(names(data1) == custom_ages$E[1])], dims = 1)) %>%
#           select(-(which(names(data1) == 0):which(names(data1) == "90+")))
#         
#       } else {
#         ## otherwise, just select out single-age columns (on odd chance someone skips row 1)
#         df <- data1 %>%
#           select(-(which(names(data1) == 0):which(names(data1) == "90+")))
#       }
#       
#       ## 2. if custom age group 2 is not NA, calculate and display its data
#       if(!is.na(custom_ages$S[2]) & !is.na(custom_ages$E[2])){
#         
#         A2 <- c(paste0(min(custom_ages$S[2], custom_ages$E[2]), " - ", max(custom_ages$S[2], custom_ages$E[2])))
#         df <- df %>%
#           mutate(
#             !!A2 := rowSums(data1[which(names(data1) == custom_ages$S[2]):
#                                     which(names(data1) == custom_ages$E[2])], dims = 1))
#         
#       } 
#       
#       ## 3. if custom age group 3 is not NA, calculate and display its data
#       if(!is.na(custom_ages$S[3]) & !is.na(custom_ages$E[3])){
#         
#         A3 <- c(paste0(min(custom_ages$S[3], custom_ages$E[3]), " - ", max(custom_ages$S[3], custom_ages$E[3])))
#         df <- df %>%
#           mutate(
#             !!A3 := rowSums(data1[which(names(data1) == custom_ages$S[3]):
#                                     which(names(data1) == custom_ages$E[3])], dims = 1))
#         
#       }
#       
#       ## 4. if custom age group 4 is not NA, calculate and display its data
#       if(!is.na(custom_ages$S[4]) & !is.na(custom_ages$E[4])){
#         
#         A4 <- c(paste0(min(custom_ages$S[4], custom_ages$E[4]), " - ", max(custom_ages$S[4], custom_ages$E[4])))
#         df <- df %>%
#           mutate(
#             !!A4 := rowSums(data1[which(names(data1) == custom_ages$S[4]):
#                                     which(names(data1) == custom_ages$E[4])], dims = 1))
#         
#       }
#       
#       ## 5. if custom age group 5 is not NA, calculate and display its data
#       if(!is.na(custom_ages$S[5]) & !is.na(custom_ages$E[5])){
#         
#         A5 <- c(paste0(min(custom_ages$S[5], custom_ages$E[5]), " - ", max(custom_ages$S[5], custom_ages$E[5])))
#         df <- df %>%
#           mutate(
#             !!A5 := rowSums(data1[which(names(data1) == custom_ages$S[5]):
#                                     which(names(data1) == custom_ages$E[5])], dims = 1))
#         
#       }
#       
#       ## put Total column at end
#       df <- df %>% 
#         mutate(Total2 = Total) %>%
#         select(-Total) %>%
#         rename(Total = Total2)
#       
#     }
#     
#     ## B. make selections
#     Reg.Type <- c(input$Region.Type)  ## to be able to use as dynamic name in select
#     df[df$Region.Type == input$Region.Type, ] %>%
#       filter(Region.Name %in% input$Region.Name) %>%
#       filter(Year %in% input$Year) %>%
#       filter(Gender %in% input$Gender) %>%
#       select(Region, !!Reg.Type := Region.Name, everything(), -Region.Type)
#     
#     ## C. call data_df() in renderDataTable to create table in app
#     ## D. call data_df() in downloadHandler to download data
#   
#   })
#   
# 
#   output$table <- renderDataTable({
#     
#     ## call function to create specified data table
#     data_df()
#     
#     },
#     ## table options: https://shiny.rstudio.com/articles/datatables.html
#     options = list(
#       pageLength = 10,       ## show only X rows/page; https://datatables.net/reference/option/pageLength
#       lengthMenu = c(10, 20, 25, 50), ## choices of pageLength to display
#       scrollX = TRUE,        ## allows horizontal scrolling; https://datatables.net/reference/option/scrollX
#       searchable = FALSE
#     )
#   )
# 
#   
#   output$downloadData <- downloadHandler(
#     
#     filename = function() {
#       c("Population_Estimates.csv")
#     },
#     
#     content = function(file) {
#       write.csv(data_df(), file, row.names = FALSE, na = "")  ## col.names = FALSE, append = TRUE, 
#     }
#   )
# 
# }
# 
# 
# ## Knit together ui and server ----
# shinyApp(ui = ui, server = server)
# 
