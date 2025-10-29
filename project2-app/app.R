### Load necessary packages ----
library(shiny)
library(shinyalert)
library(tidyverse)
library(ggcorrplot)

### Create UI oject ----
ui <- fluidPage(
  
  ### Create sidebar layout ----
  sidebarLayout(
    
    ### Add objects to the sidebar panel ----
    sidebarPanel(
      
      ### Add select boxes to choose subsets of categorical variables ----
      selectInput(
        inputId = 'device',
        label = 'Choose one or more device models',
        choices = c('Google Pixel 5',
                    'iPhone 12',
                    'OnePlus 0',
                    'Samsung Galaxy S21',
                    'Xiaomi Mi 11'),
        selected = 'Google Pixel 5',
        multiple = T
      ),
      selectInput(
        inputId = 'os',
        label = 'Choose one or more operating systems',
        choices = c('Android',
                    'iO2'),
        selected = 'Android',
        multiple = T
      ),
      selectInput(
        inputId = 'gender',
        label = 'Choose one or more genders',
        choices = c('Female',
                    'Male'),
        selected = 'Female',
        multiple = T
      ),
      selectInput(
        inputId = 'class',
        label = 'Choose one or more user behavior classes',
        choices = c('1',
                    '2',
                    '3',
                    '4',
                    '5'),
        selected = '1',
        multiple = T
      ),
      
      ### Add select boxes to choose 2 quantitative variables ---
      ### Include dynamic sliders for sub-setting chosen variables ----
      selectInput(
        inputId = 'num1',
        label = 'Choose a first numeric variable',
        choices = c('None' = 'none',
                    'App Usage Time' = 'app_use_time',
                    'Screen On Time' = 'screen_on_time',
                    'Battery Drain' = 'battery_drain',
                    'Number of Apps Installed' = 'app_number',
                    'Data Usage' = 'data_usage',
                    'Age' = 'Age'),
        selected = 'none'
      ),
      
      
      conditionalPanel(
        condition = "input.num1 != 'none'",
        sliderInput(
          inputId = 'num1_range',
          label = 'Choose a range to subset values of the first numeric variable',
          min = 0,
          max = 3000,
          value = c(0, 3000)
        )
      ),
        
      selectInput(
        inputId = 'num2',
        label = 'Choose a second numeric variable',
        choices = c('None' = 'none',
                    'App Usage Time' = 'app_use_time',
                    'Screen On Time' = 'screen_on_time',
                    'Battery Drain' = 'battery_drain',
                    'Number of Apps Installed' = 'app_number',
                    'Data Usage' = 'data_usage',
                    'Age' = 'Age'),
        selected = 'none'
      ),
      
      conditionalPanel(
        condition = "input.num2 != 'none'",
        sliderInput(
          inputId = 'num2_range',
          label = 'Choose a range to subset values of the second numeric variable',
          min = 0,
          max = 3000,
          value = c(0, 3000)
        )
      ),
      actionButton(
        inputId = 'subset',
        label = 'Click here to subset the data'
      )
    ),
    
    mainPanel(
      
    )
  )
  
)



server <- function(input, output, session){
  
}

shinyApp(ui = ui, server = server)