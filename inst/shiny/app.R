# Shiny app shipped with the modelimpact package.
# Launch with modelimpact::run_app().

library(shiny)
library(ggplot2)
library(modelimpact)

theme_set(theme_minimal())

ui <- fluidPage(
  titlePanel("modelimpact explorer"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("source", "Data",
                   choices = c("Sample data" = "sample", "Upload CSV" = "upload")),
      conditionalPanel(
        "input.source == 'upload'",
        fileInput("file", "CSV file", accept = c(".csv", ".txt")),
        radioButtons("sep", "Separator",
                     choices = c("Comma (,)" = ",", "Semicolon (;)" = ";"),
                     inline = TRUE),
        helpText("Needs a numeric probability column and an outcome column.")
      ),
      selectInput("prob_col", "Probability column", choices = NULL),
      selectInput("truth_col", "Outcome column", choices = NULL),
      selectInput("positive", "Positive (event) value", choices = NULL),
      tags$hr(),
      numericInput("fixed_cost", "Fixed cost", value = 1000, min = 0),
      numericInput("var_cost", "Variable cost", value = 100, min = 0),
      numericInput("tp_val", "True-positive value", value = 2000),
      sliderInput("prob_accept", "Offer acceptance probability",
                  min = 0, max = 1, value = 1, step = 0.05),
      sliderInput("bins", "Bins (marginal profit)", min = 4, max = 20, value = 10),
      tags$hr(),
      helpText("Threshold payoff model"),
      numericInput("fp_val", "False-positive value", value = 0),
      numericInput("tn_val", "True-negative value", value = 0),
      numericInput("fn_val", "False-negative value", value = -2000)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput("summary")),
        tabPanel("Profit", plotOutput("profit", height = "650px")),
        tabPanel("Costs & revenue", plotOutput("costrev", height = "650px")),
        tabPanel("ROI", plotOutput("roi", height = "650px")),
        tabPanel("Gains", plotOutput("gains", height = "650px")),
        tabPanel("Lift", plotOutput("lift", height = "650px")),
        tabPanel("Marginal profit", plotOutput("marginal", height = "650px")),
        tabPanel("Optimal threshold", plotOutput("thresholds", height = "650px"))
      )
    )
  )
)

server <- function(input, output, session) {

  # ---- data ----------------------------------------------------------------
  raw <- reactive({
    if (identical(input$source, "upload")) {
      req(input$file)
      tryCatch(
        utils::read.csv(input$file$datapath, sep = input$sep,
                        stringsAsFactors = FALSE, check.names = TRUE),
        error = function(e) {
          validate(need(FALSE, paste("Could not read the file:", conditionMessage(e))))
        }
      )
    } else {
      as.data.frame(modelimpact::predictions)
    }
  })

  # keep column selectors in sync with the data
  observeEvent(raw(), {
    df <- raw()
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (!length(num_cols)) num_cols <- names(df)
    all_cols <- names(df)
    prob_sel  <- if ("Yes" %in% num_cols) "Yes" else num_cols[1]
    truth_sel <- if ("Churn" %in% all_cols) "Churn" else {
      cat_cols <- setdiff(all_cols, num_cols)
      if (length(cat_cols)) cat_cols[1] else all_cols[1]
    }
    updateSelectInput(session, "prob_col", choices = num_cols, selected = prob_sel)
    updateSelectInput(session, "truth_col", choices = all_cols, selected = truth_sel)
  })

  # positive-class choices depend on the chosen outcome column
  observeEvent(list(raw(), input$truth_col), {
    df <- raw()
    req(input$truth_col %in% names(df))
    vals <- unique(as.character(df[[input$truth_col]]))
    sel <- if ("Yes" %in% vals) "Yes" else vals[1]
    updateSelectInput(session, "positive", choices = vals, selected = sel)
  })

  # standardised data with fixed helper column names
  std <- reactive({
    df <- raw()
    req(input$prob_col %in% names(df), input$truth_col %in% names(df))
    df$.prob  <- suppressWarnings(as.numeric(df[[input$prob_col]]))
    df$.truth <- as.character(df[[input$truth_col]])
    validate(
      need(!all(is.na(df$.prob)),
           "The selected probability column is not numeric."),
      need(isTRUE(input$positive %in% df$.truth),
           "Pick a positive value that occurs in the outcome column.")
    )
    df
  })

  # ---- outputs -------------------------------------------------------------
  output$summary <- renderTable({
    impact_summary(std(), fixed_cost = input$fixed_cost, var_cost = input$var_cost,
                   tp_val = input$tp_val, prob_accept = input$prob_accept,
                   prob_col = .prob, truth_col = .truth, positive = input$positive)
  }, digits = 2)

  output$profit <- renderPlot({
    autoplot(profit(std(), fixed_cost = input$fixed_cost, var_cost = input$var_cost,
                    tp_val = input$tp_val, prob_accept = input$prob_accept,
                    prob_col = .prob, truth_col = .truth, positive = input$positive))
  })

  output$costrev <- renderPlot({
    autoplot(cost_revenue(std(), fixed_cost = input$fixed_cost, var_cost = input$var_cost,
                          tp_val = input$tp_val, prob_accept = input$prob_accept,
                          prob_col = .prob, truth_col = .truth, positive = input$positive))
  })

  output$roi <- renderPlot({
    autoplot(roi(std(), fixed_cost = input$fixed_cost, var_cost = input$var_cost,
                 tp_val = input$tp_val, prob_accept = input$prob_accept,
                 prob_col = .prob, truth_col = .truth, positive = input$positive))
  })

  output$gains <- renderPlot({
    autoplot(cumulative_gains(std(), prob_col = .prob, truth_col = .truth,
                              positive = input$positive))
  })

  output$lift <- renderPlot({
    autoplot(lift_curve(std(), prob_col = .prob, truth_col = .truth,
                        positive = input$positive))
  })

  output$marginal <- renderPlot({
    autoplot(marginal_profit(std(), fixed_cost = input$fixed_cost, var_cost = input$var_cost,
                             tp_val = input$tp_val, prob_accept = input$prob_accept,
                             bins = input$bins,
                             prob_col = .prob, truth_col = .truth, positive = input$positive))
  })

  output$thresholds <- renderPlot({
    autoplot(profit_thresholds(std(), var_cost = input$var_cost, prob_accept = input$prob_accept,
                               tp_val = input$tp_val, fp_val = input$fp_val,
                               tn_val = input$tn_val, fn_val = input$fn_val,
                               prob_col = .prob, truth_col = .truth, positive = input$positive))
  })
}

shinyApp(ui, server)
