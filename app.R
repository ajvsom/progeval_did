library(shiny)
library(shinythemes)
library(readr)
library(readxl)
library(dplyr)
library(MatchIt)
library(ggplot2)
library(cobalt)
library(fixest)
library(lmtest)
library(sandwich)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Causal Analysis App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV or Excel File", accept = c(".csv", ".xlsx")),
      selectInput("analysis_type", "Choose Analysis Type", 
                  choices = c("Compare by Group", "Compare by Event", "Compare by Groups & Event")),
      uiOutput("dynamic_inputs"),
      actionButton("run_analysis", "Run Analysis")
    ),
    mainPanel(
      verbatimTextOutput("regression_output"),
      # plotOutput("love_plot"),
      # plotOutput("parallel_trends"),
      fluidRow(
        column(6, plotOutput("love_plot")),
        column(6, plotOutput("parallel_trends"))
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read_csv(input$file$datapath)
    } else if (ext == "xlsx") {
      read_excel(input$file$datapath)
    } else {
      stop("Unsupported file type")
    }
  })
  
  output$dynamic_inputs <- renderUI({
    req(dataset())
    df <- dataset()
    cols <- names(df)
    
    if (input$analysis_type == "Compare by Group") {
      tagList(
        selectInput("outcome", "Outcome Variable", choices = cols),
        selectInput("treatment", "Treatment Variable", choices = cols),
        selectizeInput("covariates", "Matching Covariates", choices = cols, multiple = TRUE, options = list(maxItems = 4))
      )
    } else if (input$analysis_type == "Compare by Event") {
      tagList(
        selectInput("outcome", "Outcome Variable", choices = cols),
        selectInput("event", "Event Variable", choices = cols)
      )
    } else {
      tagList(
        selectInput("outcome", "Outcome Variable", choices = cols),
        selectInput("treatment", "Treatment Variable", choices = cols),
        selectInput("timepoint", "Time Variable", choices = cols),
        selectizeInput("covariates", "Matching Covariates", choices = cols, multiple = TRUE, options = list(maxItems = 4))
      )
    }
  })
  
  observeEvent(input$run_analysis, {
    req(dataset())
    df <- dataset()
    
    if (input$analysis_type == "Compare by Group") {
      match_formula <- as.formula(paste(input$treatment, "~", paste(input$covariates, collapse = "+")))
      matched_data <- matchit(match_formula, data = df, method = "nearest", caliper = 0.1)
      matched_df <- match.data(matched_data)
      model <- lm(as.formula(paste(input$outcome, "~", input$treatment)), data = matched_df)
      summary_output <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
      total_treated <- sum(df[[input$treatment]] == 1, na.rm = TRUE)
      matched_treated <- sum(matched_df[[input$treatment]] == 1, na.rm = TRUE)
      
      output$regression_output <- renderPrint({ summary_output })
      output$love_plot <- renderPlot({
        love.plot(matched_data, var.order = "unadjusted", abs = TRUE, line = FALSE, 
                  thresholds = c(m = .1, ks = .05), colors = c("red", "blue"), 
                  shapes = c("triangle filled", "circle filled")) +
          annotate("text", x = 0.8, y = Inf, 
                   label = paste("Matched Obs: ", matched_treated, "(", round((matched_treated / total_treated) * 100, 1), "% )"),
                   hjust = 1, vjust = 1, size = 3, color = "black")
      })
      output$parallel_trends <- renderPlot({ NULL })
      
    } else if (input$analysis_type == "Compare by Event") {
      model <- lm(as.formula(paste(input$outcome, "~", input$event)), data = df)
      summary_output <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
      
      output$regression_output <- renderPrint({ summary_output })
      output$love_plot <- renderPlot({ NULL })
      output$parallel_trends <- renderPlot({ NULL })
      
    } else {
      df$interaction <- df[[input$treatment]] * df[[input$timepoint]]
      df$treatment <- ifelse(is.na(df[[input$treatment]]), 0, df[[input$treatment]])
      df$treatment <- as.factor(df[[input$treatment]])
      
      if (!is.null(input$covariates) && length(input$covariates) > 0) {
        # Perform matching if covariates are supplied
        match_formula <- as.formula(paste(input$treatment, "~", paste(input$covariates, collapse = "+")))
        matched_data <- matchit(match_formula, data = df, method = "nearest", caliper = 0.1)
        matched_df <- match.data(matched_data)
        analysis_data <- matched_df
      } else {
        # Skip matching and use original df
        analysis_data <- df
      }
      
      model <- lm(as.formula(paste(input$outcome, "~", input$treatment, "+", input$timepoint, "+ interaction")), data = analysis_data)
      summary_output <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
      
      output$regression_output <- renderPrint({ summary_output })
      
      if (!is.null(input$covariates) && length(input$covariates) > 0) {
        total_treated <- sum(df[[input$treatment]] == 1, na.rm = TRUE)
        matched_treated <- sum(analysis_data[[input$treatment]] == 1, na.rm = TRUE)
        
        output$love_plot <- renderPlot({
          love.plot(matched_data, var.order = "unadjusted", abs = TRUE, line = FALSE,
                    thresholds = c(m = .1, ks = .05), colors = c("red", "blue"),
                    shapes = c("triangle filled", "circle filled")) +
            annotate("text", x = 0.8, y = Inf,
                     label = paste("Matched Obs: ", matched_treated, "(", round((matched_treated / total_treated) * 100, 1), "% )"),
                     hjust = 1, vjust = 1, size = 3, color = "black")
        })
      } else {
        output$love_plot <- renderPlot({ NULL })  # No love plot if no matching
      }
      
      output$parallel_trends <- renderPlot({
        ggplot(analysis_data, aes(x = .data[[input$timepoint]], y = .data[[input$outcome]], 
                                  color = as.factor(.data[[input$treatment]]), 
                                  group = as.factor(.data[[input$treatment]]))) +
          stat_summary(fun = mean, geom = "line", size = 1.2) +
          labs(title = "Parallel Trends Plot", x = "Time", y = "Outcome", color = "Treatment Status") +
          theme_minimal()
      })
    }
  })
}

shinyApp(ui, server) 


