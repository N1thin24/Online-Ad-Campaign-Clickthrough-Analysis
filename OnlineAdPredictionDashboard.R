library(shiny)
library(ggplot2)
library(dplyr)
library(caret)
library(DT)
library(randomForest)

# Loading the dataset
data <- ClickTraining
data_ref <- ClickTraining
train_data <- ClickTraining
test_data <- ClickPrediction

# Cutoff parameter
cutoffs <- c(0.5)

# Replacing nan values
train_data$Restaurant_Type[is.na(train_data$Restaurant_Type)] <- 'Burger'
test_data$Restaurant_Type[is.na(test_data$Restaurant_Type)] <- 'Burger'

#Ensure the dependent variable is a factor for logistic regression
train_data[sapply(train_data, is.character)] <- lapply(train_data[sapply(train_data, is.character)], as.factor)
test_data[sapply(test_data, is.character)] <- lapply(test_data[sapply(test_data, is.character)], as.factor)

# Convert factor columns to numeric
numeric_columns <- sapply(train_data, is.numeric)
factor_columns <- sapply(train_data, is.factor)

numeric_columns_test <- sapply(test_data, is.numeric)
factor_columns_test <- sapply(test_data, is.factor)
#train_data[factor_columns] <- lapply(train_data[factor_columns], as.numeric)
#test_data[factor_columns_test] <- lapply(test_data[factor_columns_test], as.numeric)

# Splitting the data into train and test
index <- createDataPartition(train_data$Clicks_Conversion, p = 0.8, list = FALSE)

trainval <- train_data[index, ]
testval <- train_data[-index, ]

# Model - Random Forest -----------------------------------------------
features_trainval <- trainval[, -which(names(trainval) == "Clicks_Conversion")]
target_trainval <- trainval$Clicks_Conversion

# For reproducibility, setting a seed
set.seed(1)  
rf_model <- randomForest(features_trainval, target_trainval, ntree = 400, mtry = 3)

# Calculate and print the performance measures
predicted_probabilities <- predict(rf_model, testval, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
actual_classes <- testval$Clicks_Conversion

conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(actual_classes))

# Extract performance measures from confusion matrix
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']

# Convert Weekday to a factor indicating Weekday or Weekend
data$Weekday <- ifelse(data$Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Grouping orders into categories
data$Orders_Grouped <- cut(data$Number_of_Previous_Orders, breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, Inf), 
                           labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10+"), right = FALSE)

printFactorLevels <- function(df) {
  for (col in names(df)) {
    if (is.factor(df[[col]])) {
      cat("Levels for", col, ":", levels(df[[col]]), "\n\n")
    }
  }
}

# Apply this function to your reference data
printFactorLevels(train_data)

alignFactorLevels <- function(input_df, reference_df) {
  for (col in names(reference_df)) {
    if (is.factor(reference_df[[col]])) {
      input_df[[col]] <- factor(input_df[[col]], levels = levels(reference_df[[col]]))
    }
  }
  return(input_df)
}


# UI setup
ui <- fluidPage(
  titlePanel("Clickthrough Percentage Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.mainTabset == 'User Group by'",
        selectInput("groupingVariable", "Choose a variable to group by:",
                    choices = c("Region", "Orders_Grouped", "Social_Network", "Weekday"),
                    selected = "Region"),
        sliderInput("timeSlider", "Time Spent on Previous Website:",
                    min = min(data$Time_On_Previous_Website), 
                    max = max(data$Time_On_Previous_Website), 
                    value = c(min(data$Time_On_Previous_Website), max(data$Time_On_Previous_Website)))
      ),
      conditionalPanel(
        condition = "input.mainTabset == 'Model Performance'",
        tagList(
          h3("Random Forest"),
          p("The selected model is a Random Forest with the following characteristics:"),
          p("-Number of Trees : 400"),
          p("-Number of variables randomly sampled as candidates at each split : 3")
        )
      ),
      conditionalPanel(
        condition = "input.mainTabset == 'Data Insights'",
        tagList(
          h3("Data Insights"),
          p("The variable with the biggest impact is the carrier."),
        )
      ),
      conditionalPanel(
        condition = "input.mainTabset == 'Simulation'",
        tagList(
          h3("Simulation"),
          p("In this tab you can select the values you want for each variable and see if the model predicts if there will be a conversion"),
        )
      )
    ),
    mainPanel(
      tabsetPanel(id = "mainTabset",
                  # Your existing tabs
                  tabPanel("User Group by", 
                           plotOutput("plotVariableGroup")),
                  tabPanel("Model Performance", 
                           verbatimTextOutput("modelMetrics"),
                           dataTableOutput("confusionMatrix")),
                  tabPanel("Data Insights", 
                           plotOutput("featureImportancePlot")),
                  tabPanel("Simulation", 
                           uiOutput("simulationInputs"),
                           actionButton("predictButton", "Predict"),
                           verbatimTextOutput("simulationResult"))
      )
    ))
)

# Server logic
server <- function(input, output) {
  # Function to calculate clickthrough percentage
  calc_clickthrough_percentage <- function(df, group_var) {
    df %>%
      group_by(!!group_var) %>%
      summarise(Clickthrough_Percentage = mean(Clicks_Conversion) * 100, .groups = 'drop') %>%
      arrange(!!group_var)
  }
  
  # Plot with dynamic grouping
  output$plotVariableGroup <- renderPlot({
    group_var <- sym(input$groupingVariable)
    filtered_data <- data[data$Time_On_Previous_Website >= input$timeSlider[1] & data$Time_On_Previous_Website <= input$timeSlider[2], ]
    
    calc_clickthrough_percentage(filtered_data, group_var) %>%
      ggplot(aes_string(x = as.character(group_var), y = "Clickthrough_Percentage", fill = as.character(group_var))) +
      geom_bar(stat = "identity") +
      coord_cartesian(ylim = c(0, 100)) +
      scale_fill_viridis_d() +
      theme_minimal() +
      labs(title = paste("Clickthrough Percentage by", input$groupingVariable), x = input$groupingVariable, y = "Clickthrough Percentage (%)")
  })
  
  output$modelMetrics <- renderText({
    paste("Accuracy: ", round(accuracy, 3), 
          "\nPrecision: ", round(precision, 3), 
          "\nRecall: ", round(recall, 3), sep = "")
  })
  
  output$confusionMatrix <- renderDataTable({
    # Convert the confusion matrix to a DataTable
    datatable(as.table(conf_matrix$table), options = list(pageLength = 5, autoWidth = TRUE)) %>%
      formatStyle(
        columns = 1:ncol(conf_matrix$table),
        backgroundColor = styleInterval(cutoffs, c('pink', 'lightgreen')),
        color = 'black',
        fontWeight = 'bold'
      )
  })
  
  output$featureImportancePlot <- renderPlot({
    varImpPlot(rf_model, main="Feature Importance")
  })
  
  output$simulationInputs <- renderUI({
    tagList(
      selectInput("simRegion", "Region:", 
                  choices = unique(data$Region)),
      numericInput("simDaytime", "Daytime:", 
                   value = 0.1, max=1, min = 0),
      numericInput("simTimeOnPrevWebsite", "Time on Previous Website:", 
                   value = 10, min = 0),
      selectInput("simCarrier", "Carrier:", 
                  choices = unique(data$Carrier)),
      selectInput("simWeekday", "Weekday:", 
                  choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
      selectInput("simSocialNetwork", "Social_Network:", 
                  choices = unique(data$Social_Network)),
      numericInput("simNumberofPreviousOrders", "Number_of_Previous_Orders:", 
                   value = 1, min = 0),
      selectInput("simRestaurantType", "Restaurant_Type:", 
                  choices = unique(data$Restaurant_Type)),
      # Add other input elements as needed
    )
  })
  
  output$simulationResult <- renderText({
    # Initialize with an empty string or message
    "Click 'Predict' to see the simulation results."
  })
  
  observeEvent(input$predictButton, {
    inputValues <- data.frame(
      Region = input$simRegion,
      Daytime = as.numeric(input$simDaytime),
      Carrier = input$simCarrier,
      Time_On_Previous_Website = as.numeric(input$simTimeOnPrevWebsite),
      Weekday = input$simWeekday,
      Social_Network = input$simSocialNetwork,
      Number_of_Previous_Orders = as.numeric(input$simNumberofPreviousOrders),
      Restaurant_Type = input$simRestaurantType
    )
    
    # Convert character inputs to factors with the same levels as in test_data
    inputValues$Region <- factor(inputValues$Region, levels = levels(test_data$Region))
    inputValues$Carrier <- factor(inputValues$Carrier, levels = levels(test_data$Carrier))
    inputValues$Weekday <- factor(inputValues$Weekday, levels = levels(test_data$Weekday))
    inputValues$Social_Network <- factor(inputValues$Social_Network, levels = levels(test_data$Social_Network))
    inputValues$Restaurant_Type <- factor(inputValues$Restaurant_Type, levels = levels(test_data$Restaurant_Type))
    
    # Print input values after setting levels
    print(paste("Input values after setting levels:", toString(inputValues)))
    
    # Predict using the selected model
    predictedValue <- predict(rf_model, newdata = inputValues, type = "response")

    # Output the prediction
    output$simulationResult <- renderText({
      paste("Predicted Click Conversion Probability:", round(predictedValue, 3))
    })
  }, ignoreNULL = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)
