library(shiny)
library(shinyBS)
library(shinyWidgets)
library(ggplot2)
library(shinydashboard)
library(mvtnorm)

##########header

header <- dashboardHeader(title = "Correlation",
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
  h1("Correlation"),
  p("This app is designed to help you better understand the correlation types from scatterplots."),
  
  br(),
  h2("Instructions"),
  tags$ol(
    tags$li(
      "Select different values of r (i.e correlation coefficients) between -1 and 1."
    ),
    tags$li("Show the regression line."),
    tags$li("Try as much as you can to see what happens to scatterplot and regression line when you choose different values of correlation coefficients."),
    tags$li("Identify different types of correlation and you may find the gray notes helpful."),
    br(),
    br()
   
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

##########################CORRELATION UI##############################

corr_row1 <- fluidRow(
  br(),
  box(title = "Inputs",             
      solidHeader = TRUE,
      collapsible = TRUE,
      status = "warning",
      width = 4,sliderInput(
                   inputId = "coef",
                   label = "Choose correlation coefficients, r",
                   min = -1,
                   max = 1,
                   value = 0,
                   step = 0.1,
                   ticks = FALSE
                 ),
                checkboxInput("regression", "Show regression line", FALSE),
                span(textOutput("help"), style="color:grey"),
                bsPopover(
                 id = "plot",
                 title = "Current Scatter Plot",
                 content = "Select different correlation coefficients to find different types of linear correlation relationship. See what happens when r = 0, -1 and 1.",
                 trigger = "hover",
                 placement = "top"
               )
      ),
  box(title = "Scatter Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 8,plotOutput("plot")))


corr_tab <- tabItem(tabName = "Explore", 
                   h2("The Correlation Relationship"),
                   corr_row1)



body <- dashboardBody(tabItems(over_tab,corr_tab))

ui <- dashboardPage(header, sidebar, body)


#################################CLT SERVER#######################################
server <- function(input, output, session) {
  
  
  
  ####
  
  observeEvent(input$go, {
    updateTabItems(session, "pages", "Explore")
  })
  
  data <- reactive(
    
    data.frame(rmvnorm(n=100,mean=c(0,0),sigma=matrix(c(1,input$coef,input$coef,1),ncol=2)))
    
    
  )
  
  updateCheckboxInput(
    session = session,
    inputId = "regression",
    value = FALSE
  )
  
  output$plot <- renderPlot({
    ggplot(
      data = data(),
      mapping = aes(x = data()[,1], y = data()[,2])
    ) +
      geom_point(color = "orange", size = 4) +
      theme_bw() +
      labs(title = "") +
      xlab("X") +
      ylab("Y") +
      theme(
        text = element_text(size = 18)
      )
  })
  
  ## Add linear regression line ----
  observeEvent(
    eventExpr = input$regression, 
    handlerExpr = {
      if (input$regression) {
        output$plot <- renderPlot({
          ggplot(
            data = data(),
            mapping = aes(x = data()[,1], y = data()[,2])
          ) +
            geom_point(color = "orange", size = 4) +
            theme_bw() +
            labs(title = "") +
            xlab("X") +
            ylab("Y") +
            theme(
              text = element_text(size = 18)
            ) +
            geom_smooth(
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              na.rm = TRUE,
              color = "blue",
              size = 2
            )
        })
      } else {
        output$plot <- renderPlot({
          ggplot(
            data = data(),
            mapping = aes(x = data()[,1], y = data()[,2])
          ) +
            geom_point(color = "orange", size = 4) +
            theme_bw() +
            labs(title = "") +
            xlab("X") +
            ylab("Y") +
            theme(
              text = element_text(size = 18)
            )
        })
      }
    })
  
  output$help <- renderText({
    if(input$coef==0){
      paste("Note: When r = ",input$coef,", this is no correlation between X and Y.")
    }
    else if(input$coef==1){
      paste("Note: When r = ",input$coef,", this is a perfect positive correlation between X and Y.")
    }
    else if(input$coef==-1){
      paste("Note: When r = ",input$coef,", this is a perfect negative correlation between X and Y.")
    }
    else if(input$coef<1 && input$coef>=0.5){
      paste("Note: When r = ",input$coef,", this is a strong positive correlation between X and Y.")
    }
    else if(input$coef<0.5 && input$coef>0){
      paste("Note: When r = ",input$coef,", this is a weak positive correlation between X and Y.")
    }
    else if(input$coef<0 && input$coef>-0.5){
      paste("Note: When r = ",input$coef,", this is a weak negative correlation between X and Y.")
    }
    else{
      paste("Note: When r = ",input$coef,", this is a strong negative correlation between X and Y.")
    }
    
    
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)
