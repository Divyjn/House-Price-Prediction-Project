library(shiny)
library(shinyjs)  # Load shinyjs for hiding and showing UI components
library(randomForest)
library(caTools)
library(ggplot2)
library(reshape2)
library(shinythemes)

# Define the UI (User Interface)
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  theme = shinytheme("flatly"), # Apply a modern theme
  
  # Login and Registration Pages
  uiOutput("login_page"),
  uiOutput("register_page"),
  
  # Main App Content (hidden initially)
  hidden(
    div(
      id = "main_content",
      navbarPage(
        title = "Boston Housing Price Prediction",
        
        # Home Page Tab
        tabPanel(
          "Home",
          fluidRow(
            column(
              12,
              h2("Welcome to the Boston Housing Price Prediction App!", style = "color: #2c3e50; text-align: center;"),
              p("This app allows you to upload a dataset, train a predictive model, and analyze housing data.",
                style = "text-align: center; font-size: 18px; color: #34495e;")
            )
          )
        ),
        
        # Model Training Tab
        tabPanel(
          "Model Training",
          sidebarLayout(
            sidebarPanel(
              fileInput("file1", "Upload CSV File", accept = c(".csv")),
              tags$hr(),
              actionButton("train_model", "Train Model", class = "btn btn-primary"),
              tags$hr(),
              h4("Model Evaluation Metrics", style = "color: #2c3e50;"),
              verbatimTextOutput("rmse"),
              verbatimTextOutput("r2"),
              verbatimTextOutput("mae"),
              verbatimTextOutput("accuracy")
            ),
            mainPanel(
              h4("Predicted vs Actual Prices", style = "color: #2c3e50;"),
              plotOutput("pricePlot", height = "400px")
            )
          )
        ),
        
        # Data Analysis Tab
        tabPanel(
          "Data Analysis",
          sidebarLayout(
            sidebarPanel(
              h4("Explore the Data", style = "color: #2c3e50;"),
              selectInput(
                "variable",
                "Choose a variable for analysis:",
                choices = NULL, # Populated dynamically
                selected = NULL
              )
            ),
            mainPanel(
              h4("Data Distribution", style = "color: #2c3e50;"),
              plotOutput("distPlot", height = "300px"),
              tags$hr(),
              h4("Correlation Heatmap", style = "color: #2c3e50;"),
              plotOutput("corrPlot", height = "300px")
            )
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive value for login state
  user_logged_in <- reactiveVal(FALSE)
  user_credentials <- reactiveValues(users = list(admin = "password"))
  
  # Render login page UI
  output$login_page <- renderUI({
    if (!user_logged_in()) {
      fluidPage(
        titlePanel("Login Page"),
        fluidRow(
          column(
            4, offset = 4,
            wellPanel(
              h4("Please Login", style = "text-align: center; color: #2c3e50;"),
              textInput("username", "Username:"),
              passwordInput("password", "Password:"),
              actionButton("login_btn", "Login", class = "btn btn-primary", style = "width: 100%;"),
              actionButton("register_btn", "Register", class = "btn btn-secondary", style = "width: 100%; margin-top: 10px;")
            )
          )
        )
      )
    }
  })
  
  # Render registration page UI
  output$register_page <- renderUI({
    if (isTRUE(input$register_btn)) {
      fluidPage(
        titlePanel("Register Page"),
        fluidRow(
          column(
            4, offset = 4,
            wellPanel(
              h4("Create an Account", style = "text-align: center; color: #2c3e50;"),
              textInput("new_username", "Username:"),
              passwordInput("new_password", "Password:"),
              passwordInput("confirm_password", "Confirm Password:"),
              actionButton("create_account_btn", "Create Account", class = "btn btn-primary", style = "width: 100%;"),
              actionButton("back_to_login", "Back to Login", class = "btn btn-secondary", style = "width: 100%; margin-top: 10px;")
            )
          )
        )
      )
    }
  })
  
  # Handle navigation between login and registration
  observeEvent(input$register_btn, {
    output$register_page <- renderUI({
      fluidPage(
        titlePanel("Register Page"),
        fluidRow(
          column(
            4, offset = 4,
            wellPanel(
              h4("Create an Account", style = "text-align: center; color: #2c3e50;"),
              textInput("new_username", "Username:"),
              passwordInput("new_password", "Password:"),
              passwordInput("confirm_password", "Confirm Password:"),
              actionButton("create_account_btn", "Create Account", class = "btn btn-primary", style = "width: 100%;"),
              actionButton("back_to_login", "Back to Login", class = "btn btn-secondary", style = "width: 100%; margin-top: 10px;")
            )
          )
        )
      )
    })
  })
  
  observeEvent(input$back_to_login, {
    output$register_page <- renderUI({})
  })
  
  # Handle account creation
  observeEvent(input$create_account_btn, {
    req(input$new_username, input$new_password, input$confirm_password)
    if (input$new_password == input$confirm_password) {
      user_credentials$users[[input$new_username]] <- input$new_password
      showModal(modalDialog(
        title = "Success",
        "Account created successfully! You can now log in.",
        easyClose = TRUE,
        footer = NULL
      ))
      output$register_page <- renderUI({})
    } else {
      showModal(modalDialog(
        title = "Error",
        "Passwords do not match. Please try again.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Observe login button
  observeEvent(input$login_btn, {
    req(input$username, input$password)
    if (!is.null(user_credentials$users[[input$username]]) && 
        user_credentials$users[[input$username]] == input$password) {
      user_logged_in(TRUE)
      shinyjs::show("main_content")
      shinyjs::hide("login_page")
    } else {
      showModal(modalDialog(
        title = "Error",
        "Invalid username or password. Please try again.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Reactive value to store the dataset
  data <- reactiveVal(NULL)
  
  # Load dataset and update UI inputs dynamically
  observeEvent(input$file1, {
    req(input$file1)
    dataset <- read.csv(input$file1$datapath)
    data(dataset)
    
    # Update variable choices for the analysis tab
    updateSelectInput(session, "variable", choices = names(dataset))
  })
  
  # Reactive values to store the model and predictions
  model <- reactiveVal(NULL)
  predictions <- reactiveVal(NULL)
  
  # Train the Random Forest model
  observeEvent(input$train_model, {
    req(data())
    housing.df <- data()
    
    # Split data
    set.seed(12345)
    sample <- sample.split(housing.df$MEDV, SplitRatio = 0.7)
    train <- subset(housing.df, sample == TRUE)
    test <- subset(housing.df, sample == FALSE)
    
    # Train model
    rf_model <- randomForest(MEDV ~ ., data = train)
    model(rf_model)
    pred.rf <- predict(rf_model, test)
    predictions(pred.rf)
    
    # Calculate metrics
    rmse.rf <- sqrt(mean((pred.rf - test$MEDV)^2))
    r2.rf <- cor(pred.rf, test$MEDV)^2
    mae.rf <- mean(abs(pred.rf - test$MEDV))
    accuracy.rf <- 1 - sum((pred.rf - test$MEDV)^2) / sum((mean(test$MEDV) - test$MEDV)^2)
    
    # Display metrics
    output$rmse <- renderText({ paste("RMSE: ", round(rmse.rf, 2)) })
    output$r2 <- renderText({ paste("R²: ", round(r2.rf, 2)) })
    output$mae <- renderText({ paste("MAE: ", round(mae.rf, 2)) })
    output$accuracy <- renderText({ paste("Explained Variance (Accuracy): ", round(accuracy.rf, 2)) })
  })
  
  # Predicted vs Actual Prices Plot
  output$pricePlot <- renderPlot({
    req(predictions())
    housing.df <- data()
    sample <- sample.split(housing.df$MEDV, SplitRatio = 0.7)
    test <- subset(housing.df, sample == FALSE)
    
    ggplot() +
      geom_point(aes(x = predictions(), y = test$MEDV), color = 'blue') +
      geom_abline(slope = 1, intercept = 0, color = 'red') +
      labs(x = 'Predicted Price', y = 'Actual Price') +
      ggtitle('Predicted vs Actual Prices') +
      theme_minimal()
  })
  
  # Distribution Plot
  output$distPlot <- renderPlot({
    req(data(), input$variable)
    ggplot(data(), aes_string(x = input$variable)) +
      geom_histogram(fill = "#3498db", color = "white", bins = 30) +
      labs(title = paste("Distribution of", input$variable), x = input$variable, y = "Frequency") +
      theme_minimal()
  })
  
  # Correlation Heatmap
  output$corrPlot <- renderPlot({
    req(data())
    corr_matrix <- round(cor(data(), use = "complete.obs"), 2)
    corr_melt <- melt(corr_matrix)
    
    ggplot(corr_melt, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = value), color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
