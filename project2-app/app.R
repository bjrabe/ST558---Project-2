### Load necessary packages ----
library(shiny)
library(shinyalert)
library(DT)
library(tidyverse)
library(ggcorrplot)

### Create UI object ----
ui <- fluidPage(
  
  ### Create sidebar layout ----
  sidebarLayout(
    
    ### Add objects to the sidebar panel ----
    sidebarPanel(
      'Make selections below to subset the Mobile Device Usage Data set. Each time you make a change to your selections, you must hit the action button at the bottom of this sidebar to update the changes.',
      br(),
      br(),
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
          title = 'About',
          br(),
          'Greetings! You are using an app that will allow you to explore the Mobile Device Usage and User Behavior Dataset. This dataset provides information on mobile device usage patterns and user behavior classification measured by 10 different variables for 700 individuals. More information about this data set, as well as a link to download the data set, can be found at : https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset. Below we include some more information about each of the portions of this app, including the different tabs. Before we get into this, take a look at the following key which associates each variable with a corresponding name in the data set. This key will help you decipher the variable names as you encounter them in tables and plots in the other tabs. You will notice that the variable names in the data set are relatively intuitive.',
          br(),
          br(),
          'User ID: user_id',
          br(),
          'Device Model: device_model',
          br(),
          'Operating System: operating_system',
          br(),
          'App Usage Time (min/day): app_use_time',
          br(),
          'Screen on Time (hr/day): screen_on_time',
          br(),
          'Battery Drain (mAh/day): battery_drain',
          br(),
          'Number of Apps Installed: app_number',
          br(),
          'Data Usage (megabytes/day): data_usage',
          br(),
          'Age of user in years: Age',
          br(),
          'Gender of user: Gender',
          br(),
          'User Behavior Class (1 to 5 scale with 1 being light usage and 5 being extreme usage): behavior_class',
        
          br(),
          br(),
          h3('Sidebar Panel'),
          'The sidebar panel is where you will generate your desired subset of the Mobile Device Usage Data set. You will select variables to subset by and then click the action button at the bottom of the sidebar to update your selection. Keep in mind that it is possible to generate an empty subset of the data which will affect your ability to meaninfully explore the rest of the app features.',
          br(),
          h3('Main Panel'),
          h4('About'),
          'The "About" tab in the main panel is the tab you are currently in. It simply gives an overview of the features of the app',
          br(),
          h4('Data Download'),
          'This tab allows you to view your up-to-date selection for the subset of the Mobile Device Usage Data set. If it is an empty table, it means you have chosen an empty subset of the data. To correct this, head back over the sidebar and make some changes. You can click the action button at the bottom of the Data Download tab if you wish to download the data.',
          br(),
          h4('Data Exploration'),
          h5('Numerical Summaries'),
          'In this tab, you can choose categorical and numeric variables you wish to learn more about and generate contingency tables or numeric summaries, optionally grouped by categorical variables. Note these summaries will be generated from the subsetted data you chose in the sidebar on the left, so the outputs will be affected by your subsetting choices.',
          h5('Plotting'),
          'In this tab you can create a variety of different plots from the subsetted data you chose in the sidebar on the left. There is flexibility for you to choose which variables you would like to explore. Because they are created from subsetted data, the plot outputs will be affected by your subsetting choices in the sidebar to the left.',
          br(),
          br(),
          imageOutput('image'),
          br(),
          br()
         
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
          
          'Here we explore the Mobile Usage Data with numeric and graphical summaries. You will have the opportunity to choose the variables you wish to explore in the tabs below. You must create a subset of the data set using the sidebar to the left, and all summaries and plots will be generated from this subsetted data.',
          
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
              'When either the above selections or the subsetted data chosen to the left are modified, the displayed table will not update until the action button is pressed. If you do not see the action button, it means you do not have at least two categorical variables selected.',
              br(),
              br(),
              
              tableOutput('numeric_summary')
            
              ),
              
            
            ### create tab for plotting ----
            tabPanel(
              title = 'Plotting',
              br(),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = 'plot_type',
                    label = 'What type of plot would you like to create?',
                    choices = c('Bar Plot',
                                'Box Plot',
                                'Density Plot',
                                'Scatter Plot',
                                'Correlation Matrix')
                  )
                ),
                column(
                  width = 4,
                  uiOutput('plot_followup_1')
                ),
                column(
                  width = 4,
                  uiOutput('plot_followup_2')
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  uiOutput('plot_followup_3')
                ),
                column(
                  width = 4
                ),
                column(
                  width = 4
                )
              ),
              'When either the above selections or the subsetted data chosen to the left are modified, the displayed plot will not update until the action button below is pressed.',
              br(),
              br(),
              actionButton(
                inputId = 'plot_create',
                label = 'Click here to create or update your plot.'
              ),
              plotOutput('user_plot')
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
  
  ### create output object for image in "About" tab ---
  output$image <- renderImage(
    list('src' = 'phone.jpg',
         width = 450,
         height = 300),
    deleteFile = F)
  
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
      user_data_subset() |>
        select(input$cat_var_choices[1]) |>
        table()
    } else if (input$num_summary == 'two-way'){
      user_data_subset() |>
        select(input$cat_var_choices[1], input$cat_var_choices[2]) |>
        table()
    } else if (input$num_summary == 'num_none'){
      user_data_subset() |>
        summarize('mean' = mean(user_data[[input$num_var_choice]]), 
                  'median' = median(user_data[[input$num_var_choice]]),
                  'SD' = sd(user_data[[input$num_var_choice]]),
                  'IQR' = IQR(user_data[[input$num_var_choice]]),
                  'minimum' = min(user_data[[input$num_var_choice]]),
                  'maximum' = max(user_data[[input$num_var_choice]]))
    } else if (input$num_summary == 'num_one'){
      user_data_subset() |>
        group_by(get(input$cat_var_choices[1])) |>
        summarize('mean' = mean(get(input$num_var_choice)), 
                  'median' = median(get(input$num_var_choice)),
                  'SD' = sd(get(input$num_var_choice)),
                  'IQR' = IQR(get(input$num_var_choice)),
                  'minimum' = min(get(input$num_var_choice)),
                  'maximum' = max(get(input$num_var_choice))) |>
        rename('First Grouping Variable' = 'get(input$cat_var_choices[1])')
    } else {
      user_data_subset() |>
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
  
  ### create a shinyalert that tells user they must select a data subset using the sidebar before they can display a summary table ----
  observeEvent(input$num_sum_update, {
    if (input$subset < 1){
      shinyalert(title = 'Oh no!', 'You must generate a subset of data using the sidebar to the left before you can request a summary table', type = 'error')
    }
  })
  
  ### create a shinyalert that warns user their chosen data subset is empty so will create empty summary tables ----
  observeEvent(input$num_sum_update, {
    if (length(rownames(user_data_subset())) == 0 & input$subset >= 1){
      shinyalert(title = 'Caution!', 'You have created an empty subset of the data. Your summary tables will be empty if you do not modify your subset to include some data. Use the sidebar to the left to do so', type = 'warning')
    }
  })
  
  ### create dynamic UI elements so the user can select plot type and the variables to be included in the plot ----
  output$plot_followup_1 <- renderUI({
    if (input$plot_type == 'Bar Plot'){
      selectInput(
        inputId = 'bar_plot_var1',
        label = 'Choose a first variable for your bar plot',
        choices = c('Device Model' = 'device_model',
                    'Operating System' = 'operating_system',
                    'Gender' = 'Gender',
                    'User Behavior Class' = 'behavior_class')
      )
    } else if (input$plot_type == 'Box Plot'){
      selectInput(
        inputId = 'box_plot_num',
        label = 'Choose a quantitative variable for your box plot',
        choices = c('App Usage Time' = 'app_use_time',
                    'Screen On Time' = 'screen_on_time',
                    'Battery Drain' = 'battery_drain',
                    'Number of Apps Installed' = 'app_number',
                    'Data Usage' = 'data_usage',
                    'Age' = 'Age')
      ) 
    } else if (input$plot_type == 'Density Plot'){
      selectInput(
        inputId = 'density_plot_num',
        label = 'Choose a quantitative variable for your density plot',
        choices = c('App Usage Time' = 'app_use_time',
                    'Screen On Time' = 'screen_on_time',
                    'Battery Drain' = 'battery_drain',
                    'Number of Apps Installed' = 'app_number',
                    'Data Usage' = 'data_usage',
                    'Age' = 'Age')
      )
    } else if (input$plot_type == 'Scatter Plot'){
      selectInput(
        inputId = 'scatter_plot_xvar',
        label = 'Choose a quantitative variable to display on x-axis',
        choices = c('App Usage Time' = 'app_use_time',
                    'Screen On Time' = 'screen_on_time',
                    'Battery Drain' = 'battery_drain',
                    'Number of Apps Installed' = 'app_number',
                    'Data Usage' = 'data_usage',
                    'Age' = 'Age')
      )
    } else {
      ''
    }
  })
  
  output$plot_followup_2 <- renderUI({
    if (input$plot_type == 'Bar Plot'){
      selectInput(
        inputId = 'bar_plot_var2',
        label = 'Choose a second variable for your bar plot',
        choices = c('Device Model' = 'device_model',
                    'Operating System' = 'operating_system',
                    'Gender' = 'Gender',
                    'User Behavior Class' = 'behavior_class')
        )
    } else if (input$plot_type == 'Box Plot'){
      selectInput(
        inputId = 'box_plot_cat1',
        label = 'Choose a first categorical variable for your box plot',
        choices = c('Device Model' = 'device_model',
                    'Operating System' = 'operating_system',
                    'Gender' = 'Gender',
                    'User Behavior Class' = 'behavior_class')
      )
    } else if (input$plot_type == 'Density Plot'){
      selectInput(
        inputId = 'density_plot_cat1',
        label = 'Choose a first categorical variable for your density plot',
        choices = c('Device Model' = 'device_model',
                    'Operating System' = 'operating_system',
                    'Gender' = 'Gender',
                    'User Behavior Class' = 'behavior_class')
      )
    } else if (input$plot_type == 'Scatter Plot'){
      selectInput(
        inputId = 'scatter_plot_yvar',
        label = 'Choose a quantitative variable to display on y-axis',
        choices = c('App Usage Time' = 'app_use_time',
                    'Screen On Time' = 'screen_on_time',
                    'Battery Drain' = 'battery_drain',
                    'Number of Apps Installed' = 'app_number',
                    'Data Usage' = 'data_usage',
                    'Age' = 'Age')
      )
    } else {
      ''
    }
  })
  
  output$plot_followup_3 <- renderUI({
    if (input$plot_type == 'Bar Plot'){
      selectInput(
        inputId = 'bar_plot_var3',
        label = 'Choose a third variable for your bar plot',
        choices = c('Device Model' = 'device_model',
                    'Operating System' = 'operating_system',
                    'Gender' = 'Gender',
                    'User Behavior Class' = 'behavior_class')
      )
    } else if (input$plot_type == 'Box Plot'){
      selectInput(
        inputId = 'box_plot_cat2',
        label = 'Choose a second categorical variable for your box plot',
        choices = c('Device Model' = 'device_model',
                    'Operating System' = 'operating_system',
                    'Gender' = 'Gender',
                    'User Behavior Class' = 'behavior_class')
      )
    } else if (input$plot_type == 'Density Plot'){
      selectInput(
        inputId = 'density_plot_cat2',
        label = 'Choose a second categorical variable for your density plot',
        choices = c('Device Model' = 'device_model',
                    'Operating System' = 'operating_system',
                    'Gender' = 'Gender',
                    'User Behavior Class' = 'behavior_class')
      )
    } else if (input$plot_type == 'Scatter Plot'){
      selectInput(
        inputId = 'scatter_plot_cat',
        label = 'Choose a categorical variable for your scatter plot',
        choices = c('Device Model' = 'device_model',
                    'Operating System' = 'operating_system',
                    'Gender' = 'Gender',
                    'User Behavior Class' = 'behavior_class')
      )
    } else {
      ''
    }
  })
  
  ### create a reactive plotting object from user selections.  ----
  plotting_object <- reactive({
    if (length(rownames(user_data_subset())) == 0){
      ggplot(user_data_subset())
    } else if (input$plot_type == 'Bar Plot'){
      user_data_subset() |> 
        ggplot(aes(get(input$bar_plot_var1))) + geom_bar(aes(fill = get(input$bar_plot_var2)), position = 'dodge') + facet_wrap(~get(input$bar_plot_var3)) + labs(x = input$bar_plot_var1, fill = input$bar_plot_var2, title = paste((input$bar_plot_var1), 'counts by', (input$bar_plot_var2), 'and', (input$bar_plot_var3))) 
    } else if (input$plot_type == 'Box Plot'){
      user_data_subset() |>
        ggplot(aes(x = get(input$box_plot_cat1), y = get(input$box_plot_num))) + geom_boxplot() + facet_wrap(~get(input$box_plot_cat2)) + labs(x = input$box_plot_cat1, y = input$box_plot_num, title = paste(input$box_plot_num, 'by', input$box_plot_cat1, 'and', input$box_plot_cat2))
    } else if (input$plot_type == 'Density Plot'){
      user_data_subset() |>
        ggplot(aes(get(input$density_plot_num))) + geom_density(aes(fill = get(input$density_plot_cat1)), alpha = 0.5) + facet_wrap(~get(input$density_plot_cat2)) + labs(x = input$density_plot_num, y = 'Density', title = paste(input$density_plot_num, 'by', input$density_plot_cat1, 'and', input$density_plot_cat2), fill = input$density_plot_cat1)
    } else if (input$plot_type == 'Scatter Plot'){
      user_data_subset() |>
        ggplot(aes(x = get(input$scatter_plot_xvar), y = get(input$scatter_plot_yvar))) + geom_point(aes(color = get(input$scatter_plot_cat))) + labs(x = input$scatter_plot_xvar, y = input$scatter_plot_yvar, title = paste(input$scatter_plot_yvar, 'by', input$scatter_plot_xvar, 'and', input$scatter_plot_cat), color = input$scatter_plot_cat)
    } else {
      corr_matrix <- user_data_subset()[unlist(lapply(user_data_subset(), is.numeric))] |>
        select(!user_id) |>
        cor () |>
        round(1)
      
      ggcorrplot(corr_matrix, lab = T, title = 'Correlation Matrix for Mobile Device Usage Data', colors = c('yellow', 'blue', 'green'))
      
    }
  })


  
  ### save plotting object in output object using renderplot. Use bindEvent so this only occurs when the action button is pressed ----
  output$user_plot <- bindEvent(
    renderPlot({
      plotting_object()
    }),
    input$plot_create
  )
  
  ### Create an error if the user tries to make a plot before selecting a subset of data ---
  observeEvent(input$plot_create, {
    if (input$subset < 1){
      shinyalert(title = 'Oh no!', 'You must generate a subset of data using the sidebar to the left before you can request a plot', type = 'error')
    }
  })
  
  ### create an error for if the user tries to make a plot with an empty subset of data ----
  observeEvent(input$plot_create, {
    if (length(rownames(user_data_subset())) == 0){
      shinyalert(title = 'Caution!', 'You have created an empty subset of the data. Your plot will be empty if you do not modify your subset to include some data. Use the sidebar to the left to do so', type = 'warning')
    }
  })
  
}


shinyApp(ui = ui, server = server)