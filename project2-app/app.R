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
      ### The slider functions will be updated dynamically using                     updateSliderInput() function in the server ----
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
      
      ### create action button to update the subsetted data ----
      actionButton(
        inputId = 'subset',
        label = 'Click here to update the requested data subset'
      )
    ),
    
    ### create a main panel ----
    mainPanel(
      
      ### Create 3 tabs in the main panel ----
      tabsetPanel(
        
        ### Create the 'About' panel ----
        tabPanel(
          title = 'About'
        ),
        
        ### Create the data download panel ----
        tabPanel(
          title = 'Data Download',
          br(),
          'Click the button at the bottom of the sidebar panel to create a subsetted data table. Make sure to choose the categorical and numeric variables you wish to subset the data by.',
          br(),
          br(),
          DT::dataTableOutput('subsetted_data'),
          
          ### Add download button
          conditionalPanel(
            condition = "input.subset >= 1",
            downloadButton(
              outputId = 'data_download',
              label = 'Click here to download your selected data table'
            )
          )
        ),
      
        
        ### Create the Data Exploration panel ----
        tabPanel(
          title = 'Data Exploration',
          br(),
          
          'Here we explore the Mobile Usage Data with numeric and graphical summaries. You will have the opportunity to choose the variables you wish to explore in the tabs below.',
          
          ### Create different tabs for numerical summaries and plotting
          tabsetPanel(
            
            
            ### create tab for numerical summaries ----
            tabPanel(
              title = 'Numerical Summaries',
              br(),
              fluidRow(
                column(width = 4,
                  selectInput(
                    inputId = 'num_var_choice',
                    label = 'Select a quantitative variable you wish to explore.',
                    choices = c('App Usage Time' = 'app_use_time',
                                'Screen On Time' = 'screen_on_time',
                                'Battery Drain' = 'battery_drain',
                                'Number of Apps Installed' = 'app_number',
                                'Data Usage' = 'data_usage',
                                'Age' = 'Age')
                  )
                ),
                
                column(width = 4,
                  selectInput(
                    inputId = 'cat_var_choices',
                    label = 'Choose two categorical variables you wish to explore. If you select more than two, only the first two you choose will be used. You must have at least two selected to proceed.',
                    choices = c('Device Model' = 'device_model',
                                'Operating System' = 'operating_system',
                                'Gender' = 'Gender',
                                'User Behavior Class' = 'behavior_class'),
                    multiple = T
                  )
                ),
                column(width = 4,
                  selectInput(
                    inputId = 'num_summary',
                    label = 'Choose what type of summary you wish to see.',
                    choices = c('One way contingency table (using first selected categorical variable)' = 'one-way',
                                'Two way contingency table' = 'two-way',
                                'Numerical summary - no grouping' = 'num_none',
                                'Numerical summary - one grouping variable (using the first selected categorical variable)' = 'num_one',
                                'Numerical summary - two grouping variables' = 'num_two')
                  )
                  
                )
              ),
              
              
              conditionalPanel(
                condition = 'input.cat_var_choices.length >= 2',
                actionButton(
                  inputId = 'num_sum_update',
                  label = 'Click here to update summary.'
                )
              ),
              
              br(),
              'When the above selections are modified, the displayed table will not update until the action button is pressed. If you do not see the action button, it means you do not have at least two categorical variables selected.',
              br(),
              br(),
              
              tableOutput('numeric_summary')
            
              ),
              
            
            ### create tab for plotting ----
            tabPanel(
              title = 'Plotting'
            )
          )
          
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
             (input$num1_range[1] <= user_data[[input$num1]]) & (user_data[[input$num1]] <= input$num1_range[2]),
             (input$num2_range[1] <= user_data[[input$num2]]) & (user_data[[input$num2]] <= input$num2_range[2]))
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
  
  ### create server function for downloading data table ----
  output$data_download <- 
    downloadHandler(
      filename = 'mobile_use_data_subset.csv',
      content = function(file){
        write.csv(user_data_subset(), file)
      }
    )
    
  ### create output for numeric summary chosen by user ----
  numeric_table <- eventReactive(input$num_sum_update, {
    if (input$num_summary == 'one-way'){
      user_data |>
        select(input$cat_var_choices[1]) |>
        table()
    } else if (input$num_summary == 'two-way'){
      user_data |>
        select(input$cat_var_choices[1], input$cat_var_choices[2]) |>
        table()
    } else if (input$num_summary == 'num_none'){
      user_data |>
        summarize('mean' = mean(user_data[[input$num_var_choice]]), 
                  'median' = median(user_data[[input$num_var_choice]]),
                  'SD' = sd(user_data[[input$num_var_choice]]),
                  'IQR' = IQR(user_data[[input$num_var_choice]]),
                  'minimum' = min(user_data[[input$num_var_choice]]),
                  'maximum' = max(user_data[[input$num_var_choice]]))
    } else if (input$num_summary == 'num_one'){
      user_data |>
        group_by(get(input$cat_var_choices[1])) |>
        summarize('mean' = mean(get(input$num_var_choice)), 
                  'median' = median(get(input$num_var_choice)),
                  'SD' = sd(get(input$num_var_choice)),
                  'IQR' = IQR(get(input$num_var_choice)),
                  'minimum' = min(get(input$num_var_choice)),
                  'maximum' = max(get(input$num_var_choice))) |>
        rename('First Grouping Variable' = 'get(input$cat_var_choices[1])')
    } else {
      user_data |>
        group_by(get(input$cat_var_choices[1]), get(input$cat_var_choices[2])) |>
        summarize('mean' = mean(get(input$num_var_choice)), 
                  'median' = median(get(input$num_var_choice)),
                  'SD' = sd(get(input$num_var_choice)),
                  'IQR' = IQR(get(input$num_var_choice)),
                  'minimum' = min(get(input$num_var_choice)),
                  'maximum' = max(get(input$num_var_choice)),
                  .groups = 'keep') |>
        rename('First Grouping Variable' = 'get(input$cat_var_choices[1])',
               'Second Grouping Variable' = 'get(input$cat_var_choices[2])')
      
    }
  })
  
  output$numeric_summary <- renderTable({
    numeric_table()
  })
  
}

shinyApp(ui = ui, server = server)