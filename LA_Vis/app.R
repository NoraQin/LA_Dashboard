library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(stringr)
library(lubridate)

header <- dashboardHeader(
  title = "LA Homelessness"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("311 Requests", tabName = "calls"),
    menuItem("Homeless Count", tabName = "hc"),
    menuItem("Crime", tabName = "crime"),
    menuItem("Shelter", tabName = "shelter"),
    menuItem("Others", tabName = "others")
  )
)

body <- dashboardBody(
  tabItems(
    
    # 311 calls tab
    tabItem(
      tabName = "calls",
      fluidRow(
        column(
          width = 7,
          box(
            width = NULL, 
            plotOutput(outputId = "plotCalls1")
          ),
          box(
            width = NULL,
            sliderInput(
              inputId = "hour", label = "Hour",
              min = 0, max = 23, value = 0, step = 1, ticks = F, animate = T
            ),
            sliderInput(
              inputId = "month", label = "Month",
              min = 1, max = 12, value = 1, step = 1, ticks = F, animate = T
            ),
            radioButtons(
              inputId = "year", label = "Year", 
              choices = c(2016, 2017), selected = 2017
            )
          )
        ),
        column(
          width = 5,
          box(
            width = NULL,
            height = 350,
            plotOutput(outputId = "plotCalls2")
          ),
          box(
            width = NULL,
            height = 350,
            plotOutput(outputId = "plotCalls3")
          )
        )
      )
    ),
    
    # Homeless count tab
    tabItem(
      tabName = "hc",
      fluidRow(
        column(
          width = 7,
          box(
            width = NULL, 
            plotOutput(outputId = "plotHc1")
          ),
          box(
            width = NULL,
            selectInput(
              inputId = "tbd", label = "TBD",
              choices = c("a", "b", "c"), selected = "a"
            ),
            selectInput(
              inputId = "age", label = "Age",
              choices = c("<18", "18-24", ">24"), selected = "18-24"
            ),
            radioButtons(
              inputId = "year", label = "Year", 
              choices = c(2016, 2017), selected = 2017
            )
          )
        ),
        column(
          width = 5,
          box(
            width = NULL,
            plotOutput(outputId = "plotHc2")
          ),
          box(
            width = NULL,
            plotOutput(outputId = "plotHc3")
          )
        )
      )
    ),
    
    # Crime tab
    tabItem(
      tabName = "crime",
      fluidRow(
        column(
          width = 7,
          box(
            width = NULL, 
            plotOutput(outputId = "plotCrime1")
          ),
          box(
            width = NULL,
            sliderInput(
              inputId = "hour", label = "Hour",
              min = 0, max = 23, value = 0, step = 1, ticks = F, animate = T
            ),
            sliderInput(
              inputId = "month", label = "Month",
              min = 1, max = 12, value = 1, step = 1, ticks = F, animate = T
            )
          )
        ),
        column(
          width = 5,
          box(
            width = NULL,
            plotOutput(outputId = "plotCrime2")
          ),
          box(
            width = NULL,
            checkboxGroupInput(
              inputId = "gender", label = "Gender",
              choices = c("Male", "Female"), selected = c("Male", "Female")
            )
          )
        )
      )
    ),
    
    # Shelter tab
    tabItem(
      tabName = "shelter",
      plotOutput(outputId = "plotShelter1")
    ),
    
    # Others tab
    tabItem(tabName = "others")
   )
)

ui <- dashboardPage(
  header,
  sidebar,
  body,
  tags$script(HTML("$('body').addClass('sidebar-mini');"))
)





server <- function(input, output) {
  
  calls = read.csv("Data/311_calls_w_CTs20171102134828.csv")
  calls = calls %>%
    mutate(CREATEDDATE = mdy_hm(as.character(CREATEDDATE))) %>%
    mutate(Month = month(CREATEDDATE), 
           Day = day(CREATEDDATE),
           Hour = hour(CREATEDDATE),
           Min = minute(CREATEDDATE),
           Time = paste(Hour, Min))
  
  output$plotCalls2 = renderPlot (height = 300, {
    calls %>%
      group_by(Hour) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = Hour, y = count)) +
      geom_line(color = "brown3")+
      scale_x_continuous(breaks = seq(0, 24, 1)) +
      ggtitle("Number of Calls per Hour") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_bw()
  })
  
  output$plotCalls3 = renderPlot (height = 300, {
    calls %>%
      group_by(Month) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = Month, y = count)) +
      geom_line(color = "brown3")+
      scale_x_continuous(breaks = seq(1, 12, 1)) +
      ggtitle("Number of Calls per Month") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_bw()
  })
}

shinyApp(ui, server)