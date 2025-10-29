### Load necessary packages ----
library(shiny)
library(shinyalert)
library(DT)
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
                    'OnePlus 9',
                    'Samsung Galaxy S21',
                    'Xiaomi Mi 11'),
        multiple = T
      ),
      selectInput(
        inputId = 'os',
        label = 'Choose one or more operating systems',
        choices = c('Android',
                    'iOS'),
        multiple = T
      ),
      selectInput(
        inputId = 'gender',
        label = 'Choose one or more genders',
        choices = c('Female',
                    'Male'),
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
        multiple = T
      ),
      
      ### Add select boxes to choose 2 quantitative variables ---
      ### Include dynamic sliders for sub-setting chosen variables ----
      selectInput(
        inputId = 'num1',
        label = 'Choose a first numeric variable',
        choices = c('App Usage Time' = 'app_use_time',
                    'Screen On Time' = 'screen_on_time',
                    'Battery Drain' = 'battery_drain',
                    'Number of Apps Installed' = 'app_number',
                    'Data Usage' = 'data_usage',
                    'Age' = 'Age'),
        selected = 'app_use_time'
      ),
      
      
      sliderInput(
        inputId = 'num1_range',
        label = 'Choose a range to subset values of the first numeric variable',
        min = 0,
        max = 600,
        value = c(0, 600),
      ),
        
      selectInput(
        inputId = 'num2',
        label = 'Choose a second numeric variable',
        choices = c('App Usage Time' = 'app_use_time',
                    'Screen On Time' = 'screen_on_time',
                    'Battery Drain' = 'battery_drain',
                    'Number of Apps Installed' = 'app_number',
                    'Data Usage' = 'data_usage',
                    'Age' = 'Age'),
        selected = 'screen_on_time'
      ),
      
      sliderInput(
        inputId = 'num2_range',
        label = 'Choose a range to subset values of the second numeric variable',
        min = 0,
        max = 12,
        value = c(0, 12)
      ),
      
      actionButton(
        inputId = 'subset',
        label = 'Click here to update the requested data subset'
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = 'About'
        ),
        tabPanel(
          title = 'Data Download',
          'Click the button at the bottom of the sidebar panel to create a subsetted data table. Make sure to choose the categorical and numeric variables you wish to subset the data by.',
          br(),
          br(),
          DT::dataTableOutput('subsetted_data')
        ),
        tabPanel(
          title = 'Data Exploration'
        )
      )
      
    )
  )
  
)

### Import the data and clean it up----
### We take the code for doing so from the static portion of this project done in the Quarto document ----

user_data <- read_csv('user_behavior_dataset.csv')
user_data <- user_data |>
  mutate(`Device Model` = as.factor(`Device Model`),
         `Operating System` = as.factor(`Operating System`),
         Gender = as.factor(Gender),
         `User Behavior Class` = as.factor(`User Behavior Class`))
user_data <- user_data |>
  rename('user_id' = `User ID`,
         'device_model' = `Device Model`,
         'operating_system' = `Operating System`,
         'app_use_time' = `App Usage Time (min/day)`,
         'screen_on_time' = `Screen On Time (hours/day)`,
         'battery_drain' = `Battery Drain (mAh/day)`,
         'app_number' = `Number of Apps Installed`,
         'data_usage' = `Data Usage (MB/day)`,
         'behavior_class' = `User Behavior Class`)

### Create a server function ----
server <- function(input, output, session){
  
  ### create a reactive object which is the desired subset of the data, chosen by the user; store it in a data frame; use eventReactive() to ensure this only updates when the action button is selected ----
  user_data_subset <- eventReactive(input$subset, {
    user_data |> 
      filter(device_model %in% input$device,
             operating_system %in% input$os,
             Gender %in% input$gender,
             behavior_class %in% input$class, 
             (input$num1_range[1] <= user_data[input$num1]) & (user_data[input$num1] <= input$num1_range[2]),
             (input$num2_range[1] <= user_data[input$num2]) & (user_data[input$num2] <= input$num2_range[2]))
  })
  
  ### create output object which is a subsetted data table ----
  output$subsetted_data <- DT::renderDataTable({
    user_data_subset()
  })
  
  ### Update slider maxes dynamically in the UI based on which numeric variables are chosen ---
  observe({
    updateSliderInput(
      session,
      inputId = 'num1_range',
      max = max(user_data[input$num1]),
      value = c(0, max(user_data[input$num1]))
    )
  })
  
  observe({
    updateSliderInput(
      session,
      inputId = 'num2_range',
      max = max(user_data[input$num2]),
      value = c(0, max(user_data[input$num2]))
    )
  })

    
  
}

shinyApp(ui = ui, server = server)