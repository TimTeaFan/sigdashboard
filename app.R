
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(tidyverse)

source("Util.R")

ui <- fluidPage(
  
  theme = shinytheme("paper"),
  
  titlePanel("Significance and Sample Size Dashboard"),
  
  includeCSS("custom.css"),
  
  sidebarLayout(
    sidebarPanel(
      
      materialSwitch("n2_switch",
                     label = HTML("<b>Use two different samples sizes:</b>"),
                     status = "primary"),
      
      uiOutput("n_input"),
      
      conditionalPanel("input.n2_switch == true & input.tabs == 1",
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
      
      numericRangeInput("target_range",
                        HTML("<b>Target range</b> (in %)"), 
                        value = c(15,20),
                        separator = " to "),
      
      numericInput("steps",
                   HTML("<b>Target range steps</b> (in %)"),
                   value = 1,
                   min = 0.25,
                   max = 5,
                   step = 0.25),
      
      # numericInput("target",
      #              "Target level (in %)",
      #              value = 20,
      #              min = 0,
      #              max = 100,
      #              step = 1),

                 actionButton("go","calculate")
      
    ), # close sidebarPanel 
    
    mainPanel(
      
      tabsetPanel(id = "tabs",
                  type = "pills",
                  
        tabPanel("Calculator",
                 value = 1,
                 dataTableOutput("sim")),
        
        tabPanel("Find sample size",
                 value = 2),
        
        tabPanel("Find target value",
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

  n2_reac <- reactive(

    if (input$n2_switch) input$n2
      else input$n1

  )
  
  output$n_input <- renderUI({
    
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
  
  sim_dat <- eventReactive(input$go, {

    tibble(target = seq(target_range1(),
                        target_range2(),
                        by = steps())) %>%
      mutate(data = map(target,
                        ~ create_df(n = input$n1,
                                    n2 = n2_reac(),
                                    base = base(),
                                    target = .x)),
           real_target = map_dbl(data, check_df),
           p_value = map_dbl(data, sig_test),
           sig_level = case_when(
             p_value < .001 ~ "0.1 %",
             p_value < .01 ~ "1 %",
             p_value < .05 ~ "5 %",
             p_value < .1  ~ "10 %",
             T ~ "Not sig.")
    ) %>%
    select(target, real_target, p_value, sig_level)
    
  })
  
  output$sim <- DT::renderDataTable({
    
    sig_lvls <- c("0.1 %", "1 %", "5 %", "10 %")
    col_lvls <- c("#5FCFFF", "#80D7FF", "#A3E2FF", "#EBF9FF")
    
    datatable(sim_dat(), options = list(dom = 'tp'), rownames = FALSE) %>%
      formatPercentage(c("target", "real_target"), 2) %>% 
      formatRound('p_value', 3) %>% 
      formatStyle("sig_level",
                  # target = "row",
                  backgroundColor = styleEqual(sig_lvls, col_lvls))
  })
  

}

shinyApp(ui, server)