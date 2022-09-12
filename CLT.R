library(shiny)
library(shinyBS)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(Rlab)
library(stats)
library(shinydashboard)


##########header

header <- dashboardHeader(title = "Central Limit Theorem",
                          titleWidth = "300px")
sidebar <- dashboardSidebar(
  sidebarMenu(id = "pages",
    menuItem("Overview",icon = icon("tachometer-alt"),
             tabName = "Overview"),
    menuItem(
      "Explore",icon = icon("wpexplorer"),
      tabName = "Explore")
  )
)
#########Overview


over_page <- fluidPage(
  h1("Central Limit Theorem"),
  p("This app is designed to help you understand the Central Limit Theorem under
    continuos uniform, exponential and gamma distribution."),
  p("The Central Limit Theorem tells us when you have a sufficiently large sample size, the sampling distribution starts to approximate a normal distribution, even if the original variables themselves are not normally distributed."),
  br(),
  h2("Instructions"),
  tags$ol(
    tags$li(
      "Pick a population from one of the continuous types (continuos uniform, exponential and gamma distribution) ."
    ),
    tags$li("Use the sliders to adjust the parameters of the population
                    model you have chosen."),
    tags$li("Use the sliders to decide the number of observations for
                    each sample and the sample size (i.e how many reps you will repeat the process of taking samples from the population) ."),
    tags$li("Observe the histograms from all samples."),
  tags$li("Adjust the parameters (height, flatness, skewness) to see how the Population Density Plot and the Sample Histogram Plot with its approximated normal distribution change. "),
),
  div(
    style = "text-align: center",
    bsButton(
      inputId = "go",
      label = "Go!",
      icon = icon("bolt"),
      size = "large"
    )
  ),
  br(),
  br()
)

over_tab <- tabItem(tabName = "Overview",over_page)

##########################CENTRAL LIMIT THEOREM UI##############################

clm_row1 <- fluidRow(
   br(),
   width = 12, 
   box(title = "Inputs",             
             solidHeader = TRUE,
             collapsible = TRUE,
             status = "warning",
             width = 8,
             column(width = 6,
             verticalLayout(
               
             selectInput(inputId = "disn",
                         label = "Population Type",
                         list(
                           "Continuous Uniform Ditribution" = "cud",
                           "Exponential Distribution" = "exp",
                           "Gamma Distribution" = "gamma")),
             conditionalPanel(
               condition = "input.disn=='cud'",
               sliderInput(
                 inputId = "b",
                 label = "Set a = 0, and choose the value of b",
                 min = 0,
                 max = 10,
                 value = 1,
                 ticks = FALSE
               )
             ),bsPopover(
               id = "plotright1",
               title = "All Samples Histogram",
               content = "The red line and the blue dashed line represents the population mean and sample mean respectively. While the blue curve shows the normal approximation. ",
               trigger = "hover",
               placement = "top"
             ),
             conditionalPanel(
               condition = "input.disn=='exp'",
               sliderInput(
                 inputId = "lamda",
                 label = "Lamda",
                 min = 0.1,
                 max = 2,
                 value = 1,
                 ticks = FALSE
               )),bsPopover(
                 id = "plotright2",
                 title = "Histogram of Means",
                 content = "The red line and the blue dashed line represents the population mean and sample mean respectively. While the blue curve shows the normal approximation. ",
                 trigger = "hover",
                 placement = "top"
               ),
             
             conditionalPanel(
               condition = "input.disn=='gamma'",
               radioButtons("stype","Select Skewness Type",choices = c("Left Skewness","Right Skewness")),
               conditionalPanel(
                 condition = "input.stype=='Left Skewness'",
                 sliderInput(
                   inputId = "ls",
                   label = "Skewness",
                   min = 0,
                   max = 1,
                   value = .2,
                   ticks = FALSE
                 )),
                 conditionalPanel(
                 condition = "input.stype=='Right Skewness'",
                 sliderInput(
                   inputId = "rs",
                   label = "Skewness",
                   min = 0,
                   max = 1,
                   value = .2,
                   ticks = FALSE
                 )),span(textOutput("help"), style="color:grey")
               ),bsPopover(
                 id = "plotright3",
                 title = "Histogram of Means",
                 content = "The red line and the blue dashed line represents the population mean and sample mean respectively. While the blue curve shows the normal approximation. ",
                 trigger = "hover",
                 placement = "top"
               ))
               
               
             ),
         column(width = 6,verticalLayout(
           sliderInput("obs",paste("Number of observations in each sample"),min = 10,max = 50,value = 40 ),sliderInput("reps",paste("Sample Size, n"),min = 30,max = 10000,value = 50 )
                )
  )
  ))


clm_row2 <- fluidRow( 
  br(),
  column(width = 12,
         
         box(title = "Population Density Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6,
           conditionalPanel(condition = "input.disn == 'cud'",plotOutput("plotleft1")),
           conditionalPanel(condition = "input.disn == 'exp'",plotOutput("plotleft2")),
           conditionalPanel(condition = "input.disn == 'gamma'",plotOutput("plotleft3"))
           ),
         box(title = "Sample Histogram Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6,
             conditionalPanel(condition = "input.disn == 'cud'",plotOutput("plotright1")),
             conditionalPanel(condition = "input.disn == 'exp'",plotOutput("plotright2")),
             conditionalPanel(condition = "input.disn == 'gamma'",plotOutput("plotright3"))
             )))


clm_tab <- tabItem(tabName = "Explore", 
                   h2("The Central Limit Theorem"),
                   clm_row1,clm_row2)



body <- dashboardBody(tabItems(over_tab,clm_tab))

ui <- dashboardPage(header, sidebar, body)


#################################CLT SERVER#######################################
server <- function(input, output, session) {
  
  #####
  
  observeEvent(input$go, {
    updateTabItems(session, "pages", "Explore")
  })
  
  ###FUNCTION to create population plot
  
  makeDensityPlot <- function(data, xlims, ylims) {
    plot <- ggplot2::ggplot(aes(x = x, y = y), data = data) +
      geom_path(color = "#0072B2", size = 1.5) +
      xlim(xlims) +
      ylim(ylims) +
      xlab("Value") +
      ylab("Density") +
      ggtitle("Population Graph") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black")
      )
    plot
  }
  
  
  
  ###FUNCTION to create sample plot
  allSample <- function(vector, reps,ylims,realmean) {
    ggplot(data = data.frame(gg = vector), aes(x = gg)) +
      geom_histogram(aes(y = ..density..),
                     bins = reps,
                     fill = "lightblue",
                     col = "black"
                     
      ) +
      geom_vline(xintercept = realmean,color = "red",size = 1)+
      geom_vline(xintercept = mean(vector),color = "blue",size = 1, linetype="dashed")+
      ylim(ylims)+
      xlab("Sample mean") +
      ylab("Density") +
      ggtitle("Histogram of Means") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black")
      ) +
      stat_function(
        fun = dnorm,
        args = list(mean = mean(vector), sd = sd(vector)),
        color = "blue",
        lwd = 1,
        n=1000
      )
  }
  
  ######FUNCTION to create continuous uniform population plot
  plotunif <- function(x, min = 0, max = 1, lwd = 1, col = 1, ...) {
    
    # Grid of X-axis values
    if (missing(x)) {
      x <- seq(min - 0.5, max + 0.5, 0.01)
    }
    
    if(max < min) {
      stop("'min' must be lower than 'max'")
    }
    
    plot(x, dunif(x, min = min, max = max),
         xlim = c(min - 0.25, max + 0.25), type = "l",
         lty = 0, ylab = "f (x)", ...) 
    segments(min, 1/(max - min), max, 1/(max - min), col = col, lwd = lwd)
    segments(min - 2, 0, min, 0, lwd = lwd, col = col)
    segments(max, 0, max + 2, 0, lwd = lwd, col = col)
    points(min, 1/(max - min), pch = 19, col = col)
    points(max, 1/(max - min), pch = 19, col = col)
    segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = lwd)
    segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = lwd)
    points(0, min, pch = 21, col = col, bg = "white")
    points(max, min, pch = 21, col = col, bg = "white")
  } 
  
  ############CONTINUOUS UNIFORM#############
  
  # Matrix for first 50 reps of data
  firstfifData1 <- reactive(matrix(
    runif(n=50 * input$obs,min=0,max=input$b),
    nrow = 50,
    ncol = input$obs
  ))
  
  # Write the mean of first 50 data into vector
  firstfif1 <- reactive({
    matrix <- firstfifData1()
    matrix.means <- matrix(0, nrow = 50, ncol = 1)
    for (i in 1:50) {
      matrix.means[i, 1] <- mean(matrix[i, 1:10])
    }
    
    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })
  
  # Merge the first 50 means with the rest of data
  data1 <- reactive({
    datameans <- firstfif1()
    for (i in 1:(input$reps - 50)) {
      datameans <- append(datameans, mean(
        runif(n=input$obs,min=0,max=input$b)
      ))
    }
    return(datameans)
  })
  
  output$plotleft1 <- renderPlot({
    
    plotunif(min = 0, max = input$b, ylim = c(0,1), lwd = 2, col = 4, main = "Population Graph")
    
   })
  
  output$plotright1 <- renderPlot({
    vector <- data1()
    RY = c(0,10)
    realmean <- 1/2*(input$b)
    allSample(vector, min(30, input$reps),RY,realmean)  
    })
  
  
  
  
  
  
  
  
  ############EXPONENTIAL#############
  
  output$plotleft2 <- renderPlot({
    # Define parameters for density plot
    x <- seq(0,10,0.01)
    y <- dexp(x, rate = input$lamda)
    data <- data.frame(x = x, y = y)
    
    # Make Density Plot
    makeDensityPlot(data = data, xlims = c(0,10),ylims = c(0,2))
  })
  
  # Matrix for first 50 reps of data
  firstfifData2 <- reactive(matrix(
    
    rexp(n=50 * input$obs,rate = input$lamda) ,
    nrow = 50,
    ncol = input$obs
  ))
  
  # Write the mean of first 50 data into vector
  firstfif2 <- reactive({
    matrix <- firstfifData2()
    matrix.means <- matrix(0, nrow = 50, ncol = 1)
    for (i in 1:50) {
      matrix.means[i, 1] <- mean(matrix[i, 1:10])
    }
    
    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })
  
  # Merge the first 50 means with the rest of data
  data2 <- reactive({
    datameans <- firstfif2()
    for (i in 1:(input$reps - 50)) {
      datameans <- append(datameans, mean(
        rexp(n=input$obs,rate = input$lamda)
      ))
    }
    return(datameans)
  })
  
  
  output$plotright2 <- renderPlot({
    vector <- data2()
    RY = c(0,6)
    realmean <- 1/input$lamda
    allSample(vector, 30,RY,realmean) 
  })
  
  
  
  
  ############GAMMA#############
  # Define parameters for density plot
   
   Skew <- reactive(
     if(input$stype == "Left Skewness"){
       11 - 10 * input$ls
     }
     else{
       11 - 10 * input$rs
     }
   )
  
  x <- reactive(
    if(input$stype == "Left Skewness"){
      seq(0, Skew() + 9 * sqrt(Skew()), length = 100)
    }
    else{
      seq((Skew() - 9 * sqrt(Skew())), 0, length = 100)
    }
  )
  
  y <- reactive({
    if(input$stype == "Left Skewness"){
    dgamma(x(), shape = Skew(), beta = 1)
    }
    else{
    dgamma(-x(), shape = Skew(), beta = 1)
    }
  })
  
  RX <- reactive(
    if(input$stype == "Left Skewness"){
      c(0, (Skew() + 9 * sqrt(Skew())))
    }
    else{
      c((Skew() - 9 * sqrt(Skew())), 0)
    }
  )
  
  a <- reactive(
    if(input$stype == "Left Skewness"){
      1
    }
    else{
      -1
    }
  )
  
      
      # Population of right skewed
      output$plotleft3 <- renderPlot({
        data <- data.frame(x = x(), y = y())
        # Make the density plot
        makeDensityPlot(data = data, xlims = RX(),ylims = c(0,1))
        
      })
   
    
        

#DEFINE sample mean plot  
  
  # Matrix for first 50 reps of data
  firstfifData3 <- reactive(matrix(
    a()*rgamma(
      n = 50 * input$obs,
      Skew(),
      beta = 1
    ),
    nrow = 50,
    ncol = input$obs
  ))
  
  # Write the mean of first 50 data into vector
  firstfif3 <- reactive({
    matrix <- firstfifData3()
    matrix.means <- matrix(0, nrow = 50, ncol = 1)
    for (i in 1:50) {
      matrix.means[i, 1] <- mean(matrix[i, 1:10])
      }
    
    
    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })
  
  # Merge the first 50 means with the rest of data
  data3 <- reactive({
    datameans <- firstfif3()
    for (i in 1:(input$reps - 50)) {
      datameans <- append(datameans, mean(
        a()*rgamma(
          n = input$obs,
          shape = Skew(),
          beta = 1
        )
      ))
    }
    return(datameans)
  })
  
  output$plotright3 <- renderPlot({
    vector <- data3()
    RY = c(0,3.25) 
    realmean <- Skew()
    if(input$stype == "Left Skewness"){
     
    allSample(vector, min(90, input$reps),RY,a()*realmean)
    }
    else{
    
    allSample(vector, min(90, input$reps),RY,a()*realmean) 
    }
  })  
  
  output$help <- renderText({
   
      paste("Note: At this skewness, the value for parameter alpha = ",Skew(),"and beta = ",1," .")

    
  })
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
