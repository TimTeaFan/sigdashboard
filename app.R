
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(waiter)
library(DT)
library(tidyverse)
library(plotly)

source("Util.R")

ui <- fluidPage(
  
  theme = shinytheme("paper"),
  
  use_waiter(),
  
  titlePanel("Significance and Sample Size Dashboard"),
  
  includeCSS("custom.css"),
  
  sidebarLayout(
    sidebarPanel(
      
      materialSwitch("n2_switch",
                     label = HTML("<b>Use two different samples sizes:</b>"),
                     status = "primary"),
      
      uiOutput("n_input"),
      
      conditionalPanel("input.n2_switch == true & (input.tabs == 1 | input.tabs == 3)",
      numericInput("n2",
                   HTML("<b>Sample size</b> (target group)"),
                   value = 1000,
                   min = 0,
                   max = NA,
                   step = 100)),
      
      conditionalPanel("input.tabs != 3",
      numericInput("base",
                   HTML("<b>Base level</b> (in %)"),
                   value = 10,
                   min = 0,
                   max = 100,
                   step = 1)),
      
      conditionalPanel("input.tabs == 3",
                       numericInput("diff",
                                    HTML("<b>Difference between base and target level</b> (in %)"),
                                    value = 2,
                                    min = 0,
                                    max = 100,
                                    step = 0.25)),
      
      conditionalPanel("input.tabs == 2",
                       numericInput("target",
                                    HTML("<b>Target level</b> (in %)"),
                                    value = 20,
                                    min = 0,
                                    max = 100,
                                    step = 1)),
      
      conditionalPanel("input.tabs == 1",
        numericRangeInput("target_range",
                          HTML("<b>Target range</b> (in %)"), 
                          value = c(15,20),
                          separator = " to ")),
      
      conditionalPanel("input.tabs == 1",
        numericInput("steps",
                     HTML("<b>Target range steps</b> (in %)"),
                     value = 1,
                     min = 0.25,
                     max = 5,
                     step = 0.25)),
      
      radioGroupButtons(
        inputId = "chi2",
        label = HTML("<b>Statistical test</b>"), 
        choices = c("Chi-squared" = TRUE, "T-test" = FALSE),
        status = "primary"
      ),

      conditionalPanel("input.tabs == 1",
                 actionButton("go","calculate")),
      
      conditionalPanel("input.tabs == 2",
                       actionButton("go2","find sample size")),
      
      conditionalPanel("input.tabs == 3",
                       actionButton("go3","find sample size"))
      
    ), # close sidebarPanel 
    
    mainPanel(
      
      tabsetPanel(id = "tabs",
                  type = "pills",
                  
        tabPanel(HTML("Target Range"),
                 value = 1,
                 dataTableOutput("calc")),
        
        tabPanel("Find sample size",
                 value = 2,
                 # verbatimTextOutput("sim"),
                 plotlyOutput("plot"),
                 htmlOutput("n_text")),
        
        tabPanel("Target Difference",
                 plotlyOutput("range"),
                 value = 3)
      
      ) # close tabsetPanel
      
      ) # close mainPanel
    ) # close sidebarLayout
  ) # close fluidPage

server <- function(input, output, session) {
  
  target_range1 <- reactive(input$target_range[[1]] / 100)
  target_range2 <- reactive(input$target_range[[2]] / 100)
  base <- reactive(input$base / 100)
  steps <- reactive(input$steps / 100)
  
  w <- Waiter$new(id = "plot",
                  html = spin_3k(),
                  color = "white")
  
  w2 <- Waiter$new(id = "range",
                  html = spin_3k(),
                  color = "white")

  n2_reac <- reactive(

    if (input$n2_switch) input$n2
      else input$n1

  )
  
  flag <- reactiveValues(one = 0,
                         two = 0)
    
  observeEvent(input$go2, {
    newValue <- flag$one + 1    
    flag$one <- newValue             
  })
  
  observeEvent(input$go3, {
    newValue <- flag$two + 1    
    flag$two <- newValue             
  })
  
  
  output$n_input <- renderUI({
    
    # req(input$n2_switch)
    
    label <- if(input$n2_switch) HTML("<b>Sample size</b> (base group)") 
              else HTML("<b>Sample size</b>")
    
    if (input$tabs != 2 | (input$tabs == 2 & input$n2_switch))
      
      numericInput("n1",
                   label,
                   value = 1000,
                   min = 0,
                   max = NA,
                   step = 100)
    
    })
  
  # calculates range
  calc_dat <- eventReactive(input$go, {

    tibble(target = seq(target_range1(),
                        target_range2(),
                        by = steps())) %>%
      mutate(data = map(target,
                        ~ create_df(n = input$n1,
                                    n2 = n2_reac(),
                                    base = base(),
                                    target = .x)),
           real_target = map_dbl(data, check_df),
           p_value = map_dbl(data, ~ sig_test(.x, input$chi2)),
           sig_level = case_when(
             p_value < .001 ~ "0.1 %",
             p_value < .01 ~ "1 %",
             p_value < .05 ~ "5 %",
             p_value < .1  ~ "10 %",
             T ~ "Not sig.")
    ) %>%
    select(target, real_target, p_value, sig_level)
    
  })
  
  # finds sample size
  sim_n <- eventReactive(input$go2, {
    
    n1_arg <- if(input$n2_switch) input$n1 else NULL
    
    w$show()

    res <- find_sample_size(.n1 = n1_arg,
                     .base = input$base / 100,
                     .target = input$target / 100,
                     chi2 = input$chi2)
    
    w$hide()
    
    res

  })
  
  # shows whole range min to max for given difference between base and target
  calc_range <- eventReactive(input$go3, {
    
    w2$show()
    
    if (sign(input$diff) == -1) {
      
      min <- base <- abs(input$diff) / 100
      target <- -1 * base
      max <- 1
      
    } else if (sign(input$diff) == 1) {
      
      min <- base <- 0
      target <- (base + input$diff) / 100 
      max <- 1 - target 
      
    } else if (input$diff == 0) {
      stop("difference must not be zero")
      
    }
    
    res <-  tibble(base = seq(min, max, by = .005),
                   target = base + target) %>% 
              mutate(data = map2(base, target,
                                 ~ create_df(n = input$n1,
                                             n2 = n2_reac(),
                                             base = .x,
                                             target = .y)),
                     p_value = map_dbl(data, ~ sig_test(.x, chi2 = input$chi2)),
                     sig_level = case_when(
                       p_value < .001 ~ "0.1 %",
                       p_value < .01 ~ "1 %",
                       p_value < .05 ~ "5 %",
                       p_value < .1  ~ "10 %",
                       T ~ "Not sig.")) %>%
              select(base, target, p_value, sig_level)
    
    w2$hide()
    
    res
    
  })
  
  output$sim <- renderPrint({sim_n()})
  
  output$calc <- DT::renderDataTable({ 
    
    sig_lvls <- c("0.1 %", "1 %", "5 %", "10 %")
    col_lvls <- c("rgba(33, 150, 243, 0.5)",
                  "rgba(33, 150, 243, 0.35)",
                  "rgba(33, 150, 243, 0.2)",
                  "rgba(33, 150, 243, 0.1)")
    
    datatable(calc_dat(), options = list(dom = 'tp'), rownames = FALSE) %>%
      formatPercentage(c("target", "real_target"), 2) %>% 
      formatRound('p_value', 3) %>% 
      formatStyle("sig_level",
                  # target = "row",
                  backgroundColor = styleEqual(sig_lvls, col_lvls))
  })
  
  output$plot <- renderPlotly({
    
    if(flag$one == 0) {
      
      plotly_empty() %>% config(displayModeBar = FALSE,
                                displaylogo = FALSE)
    
    } else {

    fig <- plot_ly(sim_n()$data,
                   x = ~n,
                   y = ~p_value,
                  type = 'scatter',
                  mode = 'lines') %>%
      config(displaylogo = FALSE ,
             modeBarButtonsToRemove = c("zoomIn2d",
                                        "zoomOut2d",
                                        "pan2d",
                                        "lasso2d",
                                        "select2d",
                                        "zoom2d",
                                        "autoScale2d",
                                        "hoverClosestCartesian"))

    lines <- list(list(
      type = "line",
      line = list(color = "rgba(33, 150, 243, 0.5)",
                  dash = "dot"),
      xref = "x",
      yref = "y",
      x0 = 0,
      x1 = max(sim_n()$data$n),
      y0 = .05,
      y1 = .05
    ))
    
    a <- list(
      x = 50,
      y = 0.05,
      text = "5% level",
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      xanchor = 'left',
      yanchor = 'bottom'
    )
    
    fig %>% 
      layout(shapes = lines) %>% 
      layout(annotations = a) %>% 
      layout(xaxis = list(title = "Sample size"),
             yaxis= list(title = "p value"))

    }

  })
  
  output$n_text <- renderText({
    
    if (flag$one == 0) {
      
    } else if (is.null(sim_n()$n)) {
      
    HTML(paste0("For the choosen parameters a change from <b>", isolate(input$base) , "%</b>", "to <b>",
         isolate(input$target),
           "%</b> will need a sample size above <b>20.000</b> to be statistically significant."))  
      
    } else if (isolate(input$n2_switch)) {
      
      HTML(paste0("Based on a base level of <b>", isolate(input$base), "%</b>, with a sample size of <b>", 
           isolate(input$n1), "</b>,",
            " and a target level of <b>", isolate(input$target), "%</b>, a sample size of <b>", sim_n()$n, "</b> is needed for",
            " statistical signficance."))
      
    } else {
      
      HTML(paste0("Based on a base level of <b>", isolate(input$base), "%</b>",
            " and a target level of <b>", isolate(input$target), "%</b>, a sample size of <b>", sim_n()$n, "</b> for each group",
            " is needed for statistical significance."))
      
    }
    
  })
  
  
  output$range <- renderPlotly({
    
    if(flag$two == 0) {

      plotly_empty() %>% config(displayModeBar = FALSE,
                                displaylogo = FALSE)

    } else {

      fig <- plot_ly(calc_range(),
                     x = ~base * 100,
                     y = ~p_value,
                     type = 'scatter',
                     mode = 'lines') %>%
        config(displaylogo = FALSE ,
               modeBarButtonsToRemove = c("zoomIn2d",
                                          "zoomOut2d",
                                          "pan2d",
                                          "lasso2d",
                                          "select2d",
                                          "zoom2d",
                                          "autoScale2d",
                                          "hoverClosestCartesian"))

      lines <- list(list(
        type = "line",
        line = list(color = "rgba(33, 150, 243, 0.5)",
                    dash = "dot"),
        xref = "x",
        yref = "y",
        x0 = 0,
        x1 = 100,
        y0 = .05,
        y1 = .05
      ))

      a <- list(
        x = 0.00,
        y = 0.05,
        text = "5% level",
        xref = "x",
        yref = "y",
        showarrow = FALSE,
        xanchor = 'left',
        yanchor = 'bottom'
      )

      fig %>%
        layout(shapes = lines) %>%
        layout(annotations = a) %>% 
        layout(xaxis = list(title = "Base level",
                            ticksuffix = "%"),
               yaxis = list(title = "p value"))

    }
    
  })


  }

shinyApp(ui, server)