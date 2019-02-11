#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#   http://shiny.rstudio.com/
#
#   http://rstudio.github.io/shinydashboard/get_started.html
#

## load libraries  ----
library(tidyverse)
library(shiny)  ## for regular shiny app built with fluidPage()
library(shinydashboard)  ## for prepackaged layout dashboard app built with dashboardPage()
#library(shinythemes)
#library(flexdashboard)  ## for gaugeOutput
##library(ggplot2)
##library(plotly)


## read data ----
data <- readRDS("data.rds")


## Define UI for application ----

ui <- shinydashboard::dashboardPage(
  
  ## header ----
  dashboardHeader(title = "dashboardHeader"),
  
  
  ## sidebar ----
  dashboardSidebar("dashboardSidebar",
                   sidebarMenu(
                     menuItem("menuItem1", tabName = "menuItem1", icon = icon("dashboard")),
                     menuItem("menuItem2", tabName = "menuItem2", icon = icon("th"))
                     )
                   ),
  
  
  ## body ----
  dashboardBody(#"dashboardBody starts here",

    tabItems(

      ## first tab content ----
      tabItem(tabName = "menuItem1",
              fluidRow(
                h2("First tab content"),
                box(plotOutput("plot1", height = 250)),  # Boxes need to be put in a row (or column)
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),  ## end of tabItem 1
      
      ## second tab content ----
      tabItem(tabName = "menuItem2",
              h2("Second tab content")
      )  ## end of tabItem 2
    )  ## end of tabItems
  )  ## end of dashboardBody
)  ## end of dashboardPage


## Define server logic ----
server <- function(input, output) {
  
}


## Knit together ui and server ----
shinyApp(ui = ui, server = server)