
library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)

source("Util.R")

ui <- fluidPage(
  
  titlePanel("Significance and Sample Size Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n1",
                   "Sample size",
                   value = 1000,
                   min = 0,
                   max = NA,
                   step = 100),
      
      materialSwitch("n2_switch",
                     label = "Use two different samples sizes:",
                     status = "primary"),
      
      conditionalPanel("input.n2_switch == true",
      numericInput("n2",
                   "Sample size (n2)",
                   value = 1000,
                   min = 0,
                   max = NA,
                   step = 100)),
      
      numericInput("base",
                   "Base level (in %)",
                   value = 10,
                   min = 0,
                   max = 100,
                   step = 1),
      
      numericRangeInput("target_range",
                        "Target range (in %)", 
                        value = c(15,20),
                        separator = " to "),
      
      numericInput("steps",
                   "Target range steps (in %)",
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
      # textOutput("n2_text"),
      # tableOutput("sim"),
      dataTableOutput("sim")
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