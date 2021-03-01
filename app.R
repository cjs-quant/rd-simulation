
# clear
rm(list=ls())

# load packages
library(rdrobust)
library(ggplot2)
library(shiny)

# source helper functions
source("rd_data_gen.R")
source("rd_regress.R")
source("rd_clean.R")

# create ui
ui = pageWithSidebar(
  
  # app title
  headerPanel("RD Simulation"),
  
  # sidebar panel for inpute
  sidebarPanel(withMathJax(),
    sliderInput("discontinuity", "Set True Discontinuity (\\(\\beta\\)):", min=-1, max=1, value=0.5, step=0.01),
    selectInput("order", "Polynomial Order:", c("1" = 1, "2" = 2, "3" = 3, "4" = 4), selected="1"),
    sliderInput("variance", "Variance of Observations:", min=0.1, max=2, value=0.2, step=0.01),
    sliderInput("n", "# of Observations (draws from normal dist.):", min=100, max=500, value=300),
    sliderInput("bw_l", "Left Bandwidth:", min=0.1, max=1, value=1),
    sliderInput("bw_r", "Right Bandwidth:", min=0.1, max=1, value=1),
    selectInput("kernel", "Kernel Weight:", c("Uniform" = "uniform", "Triangular" = "triangular"), selected="triangular")
  ),
  
  # main panel for outputs
  mainPanel(uiOutput("equation"), plotOutput("rd_plot"), div(withMathJax(tableOutput("reg_tbl")), align="center"), uiOutput("note"))
  
)

# server logic to plot variables
server = function(session, input, output) {
  
  # draw data
  draws = reactive({rd_data_gen(input$discontinuity, input$order, input$variance, input$n)})
  
  # find optimal bandwidth
  bws = reactive({rdbwselect(draws()$y, draws()$x, p=as.numeric(input$order), kernel=input$kernel, bwselect="msetwo")})
  bw_l = reactive({bws()[["bws"]][1]})
  bw_r = reactive({bws()[["bws"]][2]})
  
  # set default bw inputs to optimal bw inputs
  observeEvent(bws(), {
    updateSliderInput(session, "bw_l", "Left Bandwidth", value=bw_l())
    updateSliderInput(session, "bw_r", "Right Bandwidth", value=bw_r())
  })
  
  # run RD regression
  results = reactive({rd_regress(draws(), input$order, input$bw_l, input$bw_r, input$kernel, input$discontinuity)})
  
  # clean RD data for plotting
  temp = reactive({rd_clean(draws(), results(), input$order, input$bw_l, input$bw_r)})
  
  # rd plot title
  title <- reactive({paste("RD Plot, Order ", input$order)})
  
  # plot RD
  output$rd_plot<-renderPlot({
    ggplot(temp()) + 
      geom_point(mapping = aes(x, y)) + xlab("Running Variable (X)") + ylab("Outcome (Y)") + 
      geom_vline(xintercept = 0) + 
      geom_line(mapping = aes(x, y_hat_above, colour = "y_hat"), show.legend = FALSE) + 
      geom_line(mapping = aes(x, y_hat_below, colour = "y_hat"), show.legend = FALSE)
  })
  
  # create regression table
  output$reg_tbl = renderTable({results()[["out_table"]]}, rownames=FALSE)
  
  # links
  url = a("Calonico et. al. (2019).", 
           href="https://arxiv.org/pdf/1911.09511.pdf")
  chris_email = a("christopher.simard@ny.frb.org,", href="christopher.simard@ny.frb.org")
  paper = a("Haughout, Hyman, and Shachar (2021).", href="https://static1.squarespace.com/static/5acbd8e736099b27ba4cfb36/t/603967a13349450838d8a496/1614374818753/HHS_MLF_Draft_25Feb2021.pdf")
  github = a("here.", href="https://github.com/csimard-econ/rd-simulation")
  
  # create note
  output$note = renderUI({
    tagList("This simulation demonstrates the properties of using a data-driven 
            IMSE-Optimal Bandwidth selection criterion for RD estimation, following ",
            url, "The IMSE selection method chooses an asymmetric 
            bandwidth on either side of the cutoff to minimize IMSE, which trades 
            off the estimator's mis-specification or smoothing bias against its variance. 
            Simulation author: Christopher Simard, contact:", chris_email, "originally created for", 
            paper, "The source code for this simulation can be found", github, 
            "The views expressed here are our own and do not necessarily 
            represent the views of the Federal Reserve Bank of New York or the Federal Reserve System.",
            tags$br(), tags$br(), "Notes: (1) The true model assumes data generating process is of the order 
            selected by the user. (2) Outcome data use a data generating process that adds 
            noise to a polynomial fitted over the entire support, with a level shift 
            above the cutoff. (3) Control Mean is the intercept of the regression from the 
            left-hand side polynomial. (4) Standard errors are assumed homoskedastic for exposition. 
            (5) Left and Right bandwidths default to IMSE-optimal value.")
  })
  
  # create regression equation
  output$equation = renderUI({
    withMathJax(paste0("$$Y = ", gsub(",", "", toString(results()$equation), fixed=TRUE), " + \\varepsilon$$"))
  })
  
}

# creates shinyapp object
shinyApp(ui, server)

