
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
      
      conditionalPanel("input.n2_switch == true & (input.tabs == 1)",
      numericInput("n2",
                   HTML("<b>Sample size</b> (target group)"),
                   value = 1000,
                   min = 0,
                   max = NA,
                   step = 100)),
      
      numericInput("base",
                   HTML("<b>Base level</b> (in %)"),
                   value = 10,
                   min = 0,
                   max = 100,
                   step = 1),
      
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
                       actionButton("go3","find target level"))
      
    ), # close sidebarPanel 
    
    mainPanel(
      
      tabsetPanel(id = "tabs",
                  type = "pills",
                  
        tabPanel("Calculator",
                 value = 1,
                 dataTableOutput("calc")),
        
        tabPanel("Find sample size",
                 value = 2,
                 # verbatimTextOutput("sim"),
                 plotlyOutput("plot"))
      
      ) # close tabsetPanel
      
      ) # close mainPanel
    ) # close sidebarLayout
  ) # close fluidPage

server <- function(input, output, session) {
  
  target_range1 <- reactive(input$target_range[[1]] / 100)
  target_range2 <- reactive(input$target_range[[2]] / 100)
  base <- reactive(input$base / 100)
  steps <- reactive(input$steps / 100)
  
  w <- Waiter$new(id = "plot", color = "white")

  n2_reac <- reactive(

    if (input$n2_switch) input$n2
      else input$n1

  )
  
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
      line = list(color = "grey"),
      xref = "x",
      yref = "y",
      x0 = 0,
      x1 = max(sim_n()$data$n),
      y0 = .05,
      y1 = .05
    ))

    layout(fig, shapes = lines)

  })

}

shinyApp(ui, server)