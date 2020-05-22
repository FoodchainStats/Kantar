###---------------------------------------------------------------------------------------------------------------
###############Packages##################
#load packages
rm(list=ls())
library(shiny)
library(DT)
library(plotly)
library(ggplot2)
library(leaflet)
library(shinycssloaders)
library(tidyverse)
library(tidyxl)
library(dplyr)
library(lubridate)

setwd("S:\\Analysis\\Covid-19\\Kantar")

###---------------------------------------------------------------------------------------------------------------
#Loads some of the useful functions and wrappers we use below
source("r//functions and wrappers.r")

#Loads the descriptions which popup under each tab. Modify as needed.
tabdesc <- c(
  "Plot and Table" = "Shows and example of a simple plot with an interactive slider and a data table with download buttons.",
  "Plotly" = "Gives an example of a plotly plot which you can export as a PNG easily and explore some interactive features.",
  "Leaflet" = "Shows an example of a leaflet map with some interactive features."
)

###---------------------------------------------------------------------------------------------------------------
#data
all_total <- read_rds("S:\\Analysis\\Covid-19\\Kantar\\data\\all_total_2020-05-03.rds")
kpi_1_list <- c("Penetration","Trips per Household","Volume (Nutritional)","Volume (Nutritional) per Trip","Calories")
kpi_1 <- all_total$kpi_total %>%
  filter(kpi %in% kpi_1_list) %>%
  filter(week_ending>=dmy("01/01/2020"))
kpi_2 <- all_total$kpi_total %>%
  filter(!(kpi %in% kpi_1_list)) %>%
  filter(week_ending>=dmy("01/01/2020"))
kpi_list <- unique (all_total$kpi_total$kpi)
breakdown_list <- unique(gsub("_total","",names(all_total)))[-1]

###---------------------------------------------------------------------------------------------------------------
#Initialises the dashboard

ui<-shinyUI(
  dashboardPage(
    ###---------------------------------------------------------------------------------------------------------------      
    "Dashbord Template", ##Change this to change the title of the page
    
    ###---------------------------------------------------------------------------------------------------------------
    #Don't fiddle with this unless you know what you're doing
    thead = tagList(
      tags$head(
        
        includeCSS("r/mbie-styles.css"),
        includeCSS("r/tdstyles.css")
      ),
      ## Place mbie_header inside a container-fluid for correct positioning
      div(class = "container-fluid"
          , mbie_header()
      )
    ),
    
    
    ###---------------------------------------------------------------------------------------------------------------
    #landing Page
    tabPanel("KPIs",
             mainPanel(width=9, 
                       loading_icon(plotOutput("plot1"))),
             mainPanel(width=9, 
                       loading_icon(plotOutput("plot2")))
    ),#tabpanel close
    
    
    ###---------------------------------------------------------------------------------------------------------------
    #This is the code for the second tab
    tabPanel("KPI breakdowns",
          sidebarPanel(
                     selectInput(inputId="kpi",label="kpi",choices=kpi_list),
                     selectInput(inputId="breakdown",label="breakdown",choices=breakdown_list)
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     loading_icon(plotOutput("plot3"))
                   )
          )#tabpanel close
      

    
  ))# close UI


################## Plots #############################################################################################

server <- function(input, output) {
  
  output$plot1 <- renderPlot ({
    ggplot(kpi_1,aes(x=week_ending,y=value))+
      facet_wrap(~ kpi,ncol=1,scales="free") +
      geom_line(size=2)+
      viridis::scale_fill_viridis(discrete = T) +
      viridis::scale_colour_viridis(discrete = T) +
      ggthemes::theme_calc()
  })
  
  output$plot2 <- renderPlot ({
    ggplot(kpi_2,aes(x=week_ending,y=value))+
      geom_line(aes(colour=kpi),size=2) +
      viridis::scale_fill_viridis(discrete = T) +
      viridis::scale_colour_viridis(discrete = T) +
      ggthemes::theme_calc()
  })

  output$plot3 <- renderPlot ({
    data <- all_total[names(all_total)==paste0(input$breakdown,"_total")][[1]] %>%
      filter(week_ending>=dmy("01/01/2020")) %>%
      filter(kpi==input$kpi)
    ggplot(data,aes(x=week_ending,y=value))+
      geom_line(aes(colour=type),size=2) +
      viridis::scale_fill_viridis(discrete = T) +
      viridis::scale_colour_viridis(discrete = T) +
      ggthemes::theme_calc()
  })
  

  
  
  ##end of server code
}

# Run the application 
shinyApp(ui = ui, server = server)

