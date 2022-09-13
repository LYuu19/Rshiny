library(shiny)
library(shinydashboard)
library(Rlab)
library(extraDistr)
library(purrr)
library(ggplot2)
library(ggthemes)
library(latex2exp)

header <- dashboardHeader(title = "Distribution",
                          titleWidth = "300px")
sidebar <- dashboardSidebar(
  sidebarMenu(
  menuItem(
    "Discrete Distribution",
    tabName = "disd",icon = icon("bell"),
    menuSubItem("Bernoulli Distribution",tabName = "ber"),
    menuSubItem("Discrete Unifrom Distribution",tabName = "dun"),
    menuSubItem("Geometric Distribution",tabName = "geo"),
    menuSubItem("Hypergeometric Distribution",tabName = "hyper"),
    menuSubItem("Poisson Distribution",tabName = "poi"),
    menuSubItem("Binomial Distribution",tabName = "bin"),
    menuSubItem("Negative Binomial Distribution",tabName = "nebin")),
  menuItem(
    span("Continuous distribution"),
    tabName = "contd",icon = icon("tshirt"),
    menuSubItem("Normal Distribution",tabName = "norm"),
    menuSubItem("Continuous Unifrom Distribution",tabName = "cun"),
    menuSubItem("Chi-squared Distribution",tabName = "chi"),
    menuSubItem("Gamma Distribution",tabName = "gamma"),
    menuSubItem("Beta Distribution",tabName = "beta")
    )
  )
)
######################################Bernoulli ui############################

ber_row1 <- fluidRow( 
  column(width = 6,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             verticalLayout(sliderInput("p",paste("Success probability, p"),min = 0,max = 1,value = 0.4 ),numericInput("N", "Number of simulations", value = 100,min=1,max=1000000)))))

ber_row2 <- fluidRow( 
  column(width = 12,
         box(plotOutput("berpmfPlot"),title = "Probability Mass Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
         box(plotOutput("bersimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6)))

ber_row3 <- fluidRow(column(width = 6),column(width = 4,
                     box(title = "Frequncy Table",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = NULL,
                         tableOutput('table')))
)


ber_tab <- tabItem(tabName = "ber",
                   h2("The Bernoulli Distribution"),
                   ber_row1,ber_row2,ber_row3)

######################################Dis Uni ui############################
dun_row1 <- fluidRow( 
  column(width = 12,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = 6,verticalLayout(sliderInput("ab","Choose the value of a and b",min = 0,max=10,value=c(1,10)),numericInput("N1", "Number of simulations", value = 100,min=1,max=1000000))))
  )

dun_row2 <- fluidRow(width = 12,
                     box(plotOutput("dunpmfPlot"),title = "Probability Mass Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
                     box(plotOutput("dunsimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6))

dun_row3 <- fluidRow(column(width = 6),column(width = 4,
                      box(title = "Frequncy Table",
                                           status = "primary",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           width = NULL,
                                           tableOutput('duntable')))
                     )



dun_tab <- tabItem(tabName = "dun",
                   h2("The Discrete Unifrom Distribution"),
                   dun_row1,dun_row2,dun_row3)


######################################geom ui############################
geo_row1 <- fluidRow(width=12,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = 6,verticalLayout(sliderInput("gp","Success probability, p", min = 0, max = 1, value = 0.5),span(textOutput("grange"), style="color:grey"),br(),numericInput("N2", "Number of simulations", value = 100,min=1,max=1000000),checkboxInput("geomline", "Overlay the pmf", FALSE) )))

geo_row2 <- fluidRow(width = 12,
         box(plotOutput("geopmfPlot"),title = "Probability Mass Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
         box(plotOutput("geosimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6))

geo_row3 <- fluidRow(column(width = 6),column(width = 4,
                     box(title = "Frequncy Table",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = NULL,
                         tableOutput('geotable')))
)





geo_tab <- tabItem(tabName = "geo",
                   h2("The Geometric Distribution"),
                   geo_row1,geo_row2,geo_row3)

######################################hyper ui############################
hyper_row1 <- fluidRow( 
  column(width = 12,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = 6,verticalLayout(sliderInput("hm","Number of the white ball, m",min = 0,max=50,value=50),sliderInput("hn","Number of the black ball, n", min = 0, max = 50, value = 30),numericInput("hk","Number of draws, k", min = 0, max = 50, value = 30),span(textOutput("range"), style="color:grey"),br(),numericInput("N3", "Number of simulations", value = 100,min=1,max=1000000) ))))

hyper_row2 <- fluidRow(width = 12,
                     box(plotOutput("hyperpmfPlot"),title = "Probability Mass Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
                     box(plotOutput("hypersimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6))

hyper_row3 <- fluidRow(column(width = 6),column(width = 4,
                     box(title = "Frequncy Table",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = NULL,
                         tableOutput('hypertable')))
)



hyper_tab <- tabItem(tabName = "hyper",
                   h2("The Hypergeometric Distribution"),
                   hyper_row1,hyper_row2,hyper_row3)

#################################Poisson ui#####################################
poi_row1 <- fluidRow( 
  column(width = 12,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = 6,verticalLayout(sliderInput("lambda",paste("Poisson Mean, lambda"),min = 0,max = 10,value = 1),numericInput("N4", "Number of simulations", value = 100,min=1,max=1000000),radioButtons("prag", paste("Range"),choices = list("Full range" = 1, "Reduced range" = 2),selected = 1) ))))

poi_row2 <- fluidRow(width = 12,
                       box(plotOutput("poipmfPlot"),title = "Probability Density Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
                       box(plotOutput("poisimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6))


poi_row3 <- fluidRow(column(width = 6),column(width = 4,
                     box(title = "Frequncy Table",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = NULL,
                         tableOutput('poitable')))
)



poi_tab <- tabItem(tabName = "poi",
                     h2("The Poisson Distribution"),
                     poi_row1,poi_row2,poi_row3)


#################################Binomial ui#####################################
bin_row1 <- fluidRow( 
  column(width = 12,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = 6,verticalLayout(sliderInput("bn",paste("Number of trials, n"),min = 1,max = 40,value = 20),sliderInput("bp",paste("Success probability, p"),min = 0,max = 1,value = 0.5),radioButtons("brag", paste("Range"),choices = list("Full range" = 1, "Reduced range" = 2),selected = 1),numericInput("N5", "Number of simulations", value = 100,min=1,max=1000000),checkboxInput("normal", "Overlay Normal distribution", FALSE) ))))

bin_row2 <- fluidRow(width = 12,
                     box(plotOutput("binpmfPlot"),title = "Probability Mass Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
                     box(plotOutput("binsimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6))

bin_row3 <- fluidRow(column(width = 6),column(width = 4,
                     box(title = "Frequncy Table",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = NULL,
                         tableOutput('bintable')))
)

bin_tab <- tabItem(tabName = "bin",
                   h2("The Binomial Distribution"),
                   bin_row1,bin_row2,bin_row3)

########################Negative Binomial ui######################################
nebin_row1 <- fluidRow( 
  column(width = 12,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = 6,verticalLayout(sliderInput("r",paste("Number of successes, r"),min = 1,max = 40,value = 5),sliderInput("nbp",paste("Success probability, p"),min = 0,max = 1,value = 0.4),numericInput("N6", "Number of simulations", value = 100,min=1,max=1000000) ))))

nebin_row2 <- fluidRow(width = 12,
                       box(plotOutput("nebinpmfPlot"),title = "Probability Mass Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
                       box(plotOutput("nebinsimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6))


nebin_row3 <- fluidRow(column(width = 6),column(width = 4,
                     box(title = "Frequncy Table",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = NULL,
                         tableOutput('nbtable')))
)
nebin_tab <- tabItem(tabName = "nebin",
                     h2("The Negative Binomial Distribution"),
                     nebin_row1,nebin_row2,nebin_row3)

######################Normal Distribution ui################################
norm_row1 <- fluidRow( 
  column(width = 12,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = 6,verticalLayout(sliderInput("mean","Mean",min = -5,max=5,value=0),sliderInput("sd","Standard Diviation",min = 0,max=2,value=1,step=0.1),numericInput("N7", "Number of simulations", value = 100,min=1,max=1000000),checkboxInput("normalline", "Overlay the pdf", FALSE) ))))

norm_row2 <- fluidRow(width = 12,
                      box(plotOutput("normpdfPlot"),title = "Probability Density Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
                      box(plotOutput("normsimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6))

norm_tab <- tabItem(tabName = "norm",
                    h2("The Normal Distribution"),
                    norm_row1,norm_row2)

############################Continuous Uniform ui###############################
cun_row1 <- fluidRow( 
  column(width = 12,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = 6,verticalLayout(sliderInput("cuab","Choose the value of a and b",min = -5,max=5,value=c(1,3)),numericInput("N8", "Number of simulations", value = 100,min=1,max=1000000),checkboxInput("contline", "Overlay the pdf", FALSE) ))))

cun_row2 <- fluidRow(width = 12,
                     box(plotOutput("cunpdfPlot"),title = "Probability Density Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
                     box(plotOutput("cunsimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6))

cun_tab <- tabItem(tabName = "cun",
                   h2("The Continuous Uniform Distribution"),
                   cun_row1,cun_row2)

##########################Chi-squared ui########################################
chi_row1 <- fluidRow( 
  column(width = 12,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = 6,verticalLayout(sliderInput("df","Degree of freedom",min = 1,max=10,value=9) ,numericInput("N9", "Number of simulations", value = 100,min=1,max=1000000),checkboxInput("chiline", "Overlay the pdf", FALSE) ))))

chi_row2 <- fluidRow(width = 12,
                     box(plotOutput("chipdfPlot"),title = "Probability Density Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
                     box(plotOutput("chisimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6))

chi_tab <- tabItem(tabName = "chi",
                   h2("The chi-squared Distribution"),
                   chi_row1,chi_row2)


########################Gamma ui##############################################
gamma_row1 <- fluidRow( 
  column(width = 12,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = 6,verticalLayout(sliderInput("ga",paste("Shape parameter, alpha "),min = 0.01,max = 10,value = 2),sliderInput("gb",paste("Shape parameter, beta"),min = 0.1,max = 10,value = 0.5),numericInput("N10", "Number of simulations", value = 100,min=1,max=1000000),checkboxInput("gammaline", "Overlay the pdf", FALSE) ))))

gamma_row2 <- fluidRow(width = 12,
                       box(plotOutput("gammapdfPlot"),title = "Probability Density Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
                       box(plotOutput("gammasimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6))

gamma_tab <- tabItem(tabName = "gamma",
                     h2("The Gamma Distribution"),
                     gamma_row1,gamma_row2)

###########################beta ui###############################################

beta_row1 <- fluidRow( 
  column(width = 12,
         box(title = "Inputs",
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = 6,verticalLayout(sliderInput("ba",paste("Parameter, alpha "),min = 0.1,max = 10,value = 5),sliderInput("bb",paste("Beta"),min = 0.1,max = 10,value = 5),numericInput("N11", "Number of simulations", value = 100,min=1,max=1000000),checkboxInput("betaline", "Overlay the pdf", FALSE) ))))

beta_row2 <- fluidRow(width = 12,
                      box(plotOutput("betapdfPlot"),title = "Probability Density Function",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6),
                      box(plotOutput("betasimPlot"),title = "Simulation Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6))

beta_tab <- tabItem(tabName = "beta",
                    h2("The Beta Distribution"),
                    beta_row1,beta_row2)


body <- dashboardBody(tabItems(ber_tab, dun_tab,geo_tab,hyper_tab,poi_tab,bin_tab,nebin_tab,norm_tab,cun_tab,chi_tab,gamma_tab,beta_tab))












ui <- dashboardPage(header, sidebar, body)

########################################server logic################################

server <- function(input, output, session) {
  
  ####Bernoulli
  output$berpmfPlot<- renderPlot({
    titx=paste0("X~Bernoulli(",input$p,")") 
   df <- data.frame(A=c("0", "1"),
                     B=c(1-input$p, input$p))
    ggplot(data=df,aes(x=A,y=B))+geom_bar(stat = "identity", fill="#E69F00", width = 0.4)+
      labs(x="Possible Outcomes",y="P(X=x)",title = titx)+
      theme_minimal()+
      theme(plot.title = element_text (hjust = 0.5))+
      coord_cartesian(ylim = c(0,1))
  })
  
  output$bersimPlot<- renderPlot({
    Simulation_Outcomes <- rbern(input$N, prob = input$p) 
    tity=paste0("Histogram ",input$N," simulations from X~Bernoulli(",input$p,")") 
    df<-as.data.frame(prop.table(table(Simulation_Outcomes)))
    ggplot(data=df,aes(x=Simulation_Outcomes,y=Freq))+geom_bar(stat = "identity", fill="#56B4E9", width = 0.4)+
      labs(x="Possible Outcomes",y="Proportion",title = tity)+
      theme_minimal()+
      theme(plot.title = element_text (hjust = 0.5))+
      coord_cartesian(ylim = c(0,1))
    
  })
  
  output$table <- renderTable({
      Simulation_Outcomes <- rbern(input$N, prob = input$p) 
      table(Simulation_Outcomes)
      })
  #####Discrete uniform distribution
  output$dunpmfPlot<- renderPlot({
    yy<-input$ab[1]:input$ab[2] 
    y1<-ddunif(yy, input$ab[1], input$ab[2])
    titx=paste0("X~U(",input$ab[1],",",input$ab[2],")")
    df <- data.frame(yy,y1)
    ggplot(data=df,aes(x=yy,y=y1))+geom_bar(stat = "identity", fill="steelblue", width = 0.2)+
      labs(x="Possible Outcomes",y="P(X=x)",title = titx)+
      theme_minimal()+
      theme(plot.title = element_text (hjust = 0.5))+
      scale_x_continuous(breaks = seq(input$ab[1],input$ab[2]))+
      coord_cartesian(ylim = c(0,1))
  })
  
  output$dunsimPlot<- renderPlot({
    x <- rdunif(input$N1, input$ab[1], input$ab[2]) 
    df<-data.frame(prop.table(table(x)))
    tity=paste0(input$N1," simulations from X~U(",input$ab[1],",",input$ab[2],")")
    df$x <- as.numeric(df$x)
    ggplot(data=df,aes(x,Freq))+geom_bar(stat = "identity", fill="steelblue", width = 0.2)+
      labs(x="Possible Outcomes",y="Proportion",title = titx)+
      theme_minimal()+
      theme(plot.title = element_text (hjust = 0.5))+
      scale_x_continuous(breaks = seq(input$ab[1], input$ab[2],1))+
      coord_cartesian(ylim = c(0,1))
  })
  
  output$duntable <- renderTable({
    Simulation_Outcomes <- rdunif(input$N1, input$ab[1], input$ab[2]) 
    table(Simulation_Outcomes)
  })
  
  #####geom distribution
  output$geopmfPlot<- renderPlot({
    validate(
      
      need(input$gp > 0,"Check the probability, it should be greater than 0."))
    
    u=10
    x <- seq(0, u)
    y<- dgeom(x, input$gp)
    titx=paste0("X~Geom(",input$gp,")")
    plot(-1,1,xlim=c(0,10),ylim=c(0,1),xlab="x",ylab="P(X=x)",main=titx)
    Kcol=rep(c(2,3),length(x)+1)
    for(i in 0:50) polygon((i+c(-0.5,-0.5,0.5,0.5,-0.5)),c(0,y[i+1],y[i+1],0,0),col=Kcol[i+1]) 
  })
  
  output$geosimPlot<- renderPlot({
    set.seed(6674)
    validate(
      need(input$gp > 0,""))
    u=10
    Z=rgeom(input$N2,input$gp)
    x<-seq(0,u)
    y<- dgeom(x, input$gp)
    A=seq(-0.5,(0.5+max(Z)))
    Kcol=rep(c(4,6),u)
    RX=c(0,u)
    tity=paste0("Histogram ", input$N2," simulation from X~Geom(",input$gp,")")
    hist(Z,breaks=A,col=Kcol,freq=F,xlab="x",ylab="Proportion",xlim=RX,ylim=c(0,1),main=tity)
    if (input$geomline == TRUE){
      
      lines(x,y,typ=c("b"),col="red",lwd=2)
    }
  })
  
  output$grange <- renderText({
    paste("Note: the probability of the Geometry Distribution should be in the range of (0,1].")
  })
  output$geotable <- renderTable({
    validate(
      
      need(input$gp > 0.2,"Please choose the success probability greater than 0.2 to show the frequency table."))
    Simulation_Outcomes <- rgeom(input$N2,input$gp) 
    table(Simulation_Outcomes)
  })
  
  
  ##########hyper
  
  output$hyperpmfPlot<- renderPlot({
    validate(
      need(input$hk <= (input$hn+input$hm),"Check the number of draws, it should be no more than the sum of black and white balls.")
    )
    u=50
    x <- seq(0, u)
    y<- dhyper(x, m = input$hm, n = input$hn, k = input$hk)  
    titx=paste0("X~Hypergeometric(",input$hm,",",input$hn,",",input$hk,")")
    plot(-1,1,xlim=c(0,50),ylim=c(0,1),xlab="x",ylab="P(X=x)",main=titx)
    Kcol=rep(c(2,3),length(x)+1)
    for(i in 0:50) polygon((i+c(-0.5,-0.5,0.5,0.5,-0.5)),c(0,y[i+1],y[i+1],0,0),col=Kcol[i+1]) 
   
  })
  
  output$hypersimPlot<- renderPlot({
    validate(
      need(input$hk <= (input$hn+input$hm)," ")
    )
    u=50
    Z=rhyper(input$N3, m = input$hm, n = input$hn, k = input$hk)
    x<-seq(0,u)
    y<-dhyper(x, m = input$hm, n = input$hn, k = input$hk)
    A=seq(-0.5,(0.5+max(Z)))
    Kcol=rep(c(4,6),u)
    RX=c(0,50)
    tity=paste0("Histogram ",input$N3," simulation from X~Hypergeometric(",input$hm,",",input$hn,",",input$hk,")")
    hist(Z,breaks=A,col=Kcol,freq=F,xlab="x",ylab="Proportion",xlim=RX,ylim=c(0,1),main=tity)
  })
  
  output$range <- renderText({
    paste("Note: the number of draws should be in the range of [0,",input$hm+input$hn,"].")
  })
  
  output$hypertable <- renderTable({
    validate(
      need(input$hk <= (input$hn+input$hm)," ")
    )
    Simulation_Outcomes <- rhyper(input$N3, m = input$hm, n = input$hn, k = input$hk) 
    table(Simulation_Outcomes)
  })
  
  #####Poi 
  output$poipmfPlot<- renderPlot({
    x=seq(0,20)
    y=dpois(x,input$lambda)
    titx=paste0("X~Po(",input$lambda,")")
    if(input$prag==1) 
    {
      RX=c(-1,20)
      RY=c(0,1)
    }
    if(input$prag==2)
    {
      r=x[y>0.01*max(y)]
      RX=c(min(r)-0.5,max(r)+0.5)
      RY=c(0,1.1*max(y))
    }
    plot(-1,-1,xlim=RX,ylim=RY,xlab="x",ylab="P(X=x)",main=titx)
    Kcol =rep(c(3,4),20)
    for(i in 0:20) polygon((i+c(-0.5,-0.5,0.5,0.5,-0.5)),c(0,y[i+1],y[i+1],0,0),col=Kcol[i+1])
  })
  
  
  output$poisimPlot<- renderPlot({
    Z=rpois(input$N4,input$lambda)
    tity=paste0("Histogram ",input$N4," simulations from X~Po(",input$lambda,")")
    x=seq(0,20)
    y=dpois(x,input$lambda)
    A=seq(-0.5,(0.5+max(Z)))
    Kcol=rep(c(2,7),20)
    if(input$prag==1) 
    {
      RX=c(-1,20)
      RY=c(0,1)
    }
    if(input$prag==2)
    {
      r=x[y>0.01*max(y)]
      RX=c(min(r)-0.5,max(r)+0.5)
      RY=c(0,1.1*max(y))
    }
    hist(Z,breaks=A,col=Kcol,freq=F,xlab="x",ylab="Proportion",xlim=RX,ylim=RY,main=tity)
  })
  
  output$poitable <- renderTable({
    Simulation_Outcomes <- rpois(input$N4,input$lambda) 
    table(Simulation_Outcomes)
  })
  
  #######Bin
  
  output$binpmfPlot<- renderPlot({
    x=seq(0,input$bn)
    y=dbinom(x,input$bn,input$bp)
    titx=paste0("X~Bin(",input$bn,",",input$bp,")")
    if(input$brag==1) 
    {
      RX=c(0,40)
      RY=c(0,1)
    }
    if(input$brag==2)
    {
      r=x[y>0.01*max(y)]
      RX=c(min(r)-0.5,max(r)+0.5)
      RY=c(0,1.1*max(y))
    }
    plot(-1,-1,xlim=RX,ylim=RY,xlab="x",ylab="P(X=x)",main=titx)
    Kcol=rep(c(2,3),input$bn)
    for(i in 0:input$bn) polygon((i+c(-0.5,-0.5,0.5,0.5,-0.5)),c(0,y[i+1],y[i+1],0,0),col=Kcol[i+1])
  })
  
  output$binsimPlot<- renderPlot({
    set.seed(6674)
    Z=rbinom(input$N5,input$bn,input$bp)
    tity=paste0("Histogram ",input$N5," simulations from X~Bin(",input$bn,",",input$bp,")")
    x=seq(0,input$bn)
    y=dbinom(x,input$bn,input$bp)
    A=seq(-0.5,(0.5+input$bn))
    Kcol=rep(c(4,7),input$bn)
    if(input$brag==1) 
    {
      RX=c(0,40)
      RY=c(0,1)
    }
    if(input$brag==2)
    {
      r=x[y>0.01*max(y)]
      RX=c(min(r)-0.5,max(r)+0.5)
      RY=c(0,1.1*max(y))
    }
    hist(Z,breaks=A,col=Kcol,freq=F,xlab="x",ylab="Proportion",xlim=RX,ylim=RY,main=tity)
    if (input$normal == TRUE){
      curve(dnorm(x,mean=input$bn*input$bp,sd = sqrt(input$bn*input$bp*(1-input$bp))),add=TRUE,col = "red", lwd = 2)}
  })
  
  output$bintable <- renderTable({
    Simulation_Outcomes <- rbinom(input$N5,input$bn,input$bp) 
    table(Simulation_Outcomes)
  })


###nebin

  output$nebinpmfPlot<- renderPlot({
    validate(
      need(input$nbp > 0,"When p = 0, P(X=x) = 0."))
    
  u=round(5*input$r/input$nbp,0)
  x=seq(0,u)
  y=dnbinom((x-input$r),input$r,input$nbp)
  r=x[y>0.0001*max(y)]
  RX=c(input$r-0.5,max(r)+0.5)
  titx=paste0("X~Neg Bin(",input$r,",",input$nbp,")")
  plot(-1,-1,xlim=RX,ylim=c(0,1.1*max(y)),xlab="x",ylab="P(X=x)",main=titx)
  Kcol=rep(c("pink",6),u)
  for(i in 0:u) polygon((i+c(-0.5,-0.5,0.5,0.5,-0.5)),c(0,y[i+1],y[i+1],0,0),col=Kcol[i+1])
})

  output$nebinsimPlot<- renderPlot({
    validate(
      need(input$nbp > 0," "))
    
  set.seed(6674)
  u=round(5*input$r/input$nbp,0)
  Z=rnbinom(input$N6,input$r,input$nbp)
  Z=Z+input$r
  tity=paste0("Histogram ",input$N6," simulations from X~Neg Bin(",input$r,",",input$nbp,")")
  x=seq(0,u)
  y=dnbinom((x-input$r),input$r,input$nbp)
  
  r=x[y>0.0001*max(y)]
  RX=c(input$r-0.5,max(r)+0.5)
  A=seq(-0.5,(0.5+max(Z)))
  Kcol=rep(c(4,6),u)
  hist(Z,breaks=A,col=Kcol,freq=F,xlab="x",ylab="Proportion",xlim=RX,ylim=c(0,1.1*max(y)),main=tity)
})
  
  output$nbtable <- renderTable({
    validate(
      need(input$nbp > 0.7,"Please choose the success probability greater than 0.7 to show the frequency table."))
    Simulation_Outcomes <- rnbinom(input$N6,input$r,input$nbp) 
    table(Simulation_Outcomes)
  })

  ####norm
  output$normpdfPlot<- renderPlot({
    x<-seq(-5,5,0.1)
    y<- dnorm(x,mean = input$mean,sd = input$sd)
    RX=c((-5-0.1),(5+0.1))
    titx=paste0("X~N(",input$mean,",",input$sd,")")
    plot(-1,-1,xlim=RX,ylim=c(0,1),xlab="x",ylab="f (x)",main=titx)
    lines(x, y, col = "black", lwd = 2)
  })
  
  output$normsimPlot<- renderPlot({
    set.seed(6674)
    x<-seq(-5,5,0.1)
    y<- dnorm(x,mean = input$mean,sd = input$sd)
    data<-rnorm(input$N7,mean=input$mean,sd=input$sd) 
    tity=paste0("Histogram ",input$N7," simulations from X~N(",input$mean,",",input$sd,")")
    hist(data, main = tity, xlab = "", ylab = "f (x)",prob = TRUE,ylim=c(0,1),xlim=c(-5.1,5.1),breaks = 100)
    if (input$normalline == TRUE){
      lines(x, y, col = "red", lwd = 2,xlim=c(-5.1,5.1),ylim=c(0,1))
    }
  })
 
 
  
  ######continuous uniform 
  output$cunpdfPlot<- renderPlot({
    titx = paste0("X~U(",input$cuab[1],",",input$cuab[2],")")
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
    
    plotunif(min = input$cuab[1], max = input$cuab[2], ylim = c(0,1), lwd = 2, col = 4, main = titx)
  })
  
  output$cunsimPlot<- renderPlot({
    set.seed(6674)
    tity = paste0("Histogram ", input$N8, " simulation from X~U(",input$cuab[1],",",input$cuab[2],")")
    x <- seq(input$cuab[1]-0.25, input$cuab[2]+0.25, 0.01)
    hist(runif(n=input$N8,min=input$cuab[1],max=input$cuab[2]), main = tity, xlim = c(input$cuab[1]-0.25, input$cuab[2]+0.25),ylim = c(0,1),xlab = "", prob = TRUE,col="lightblue",ylab="f (x)")
    if (input$contline == TRUE){
      lines(x, dunif(x,min=input$cuab[1],max=input$cuab[2]), col = "red", lwd = 2)
    }
  })
  
 
  
  
 ######chi

  
  output$chipdfPlot<- renderPlot({
    x<- seq(0,10,0.01)
    xx<- dchisq(x, df = input$df)
    titx = paste0("Chi-squared distribution with df=",input$df)
    plot(x,xx,type = "line",lwd = 2, col = 6,ylim = c(0,0.5),xlim = c(0,8),main = titx,ylab="f (x)")
  })
  
  output$chisimPlot<- renderPlot({
    set.seed(6674)
    tity = paste0("Histogram ", input$N9, " simulation from Chi-squared distribution with df=",input$df)
    x <- rchisq(input$N9, df = input$df)
    hist(x, 
         freq = FALSE, 
         xlim = c(0,8), 
         ylim = c(0,0.5),
         breaks = 100,main = tity,ylab="f (x)")
    
    if (input$chiline == TRUE){
      curve(dchisq(x, df = input$df), from = 0, to = 8, 
            n = 5000, col= 'red', lwd=2, add = T)}
  })
  
  ####gamma
  output$gammapdfPlot<- renderPlot({
    x=seq(0,20,0.01)
    y=dgamma(x,input$ga,input$gb)
    tit1=paste0("X~Gamma(",input$ga,",",input$gb,")")
    plot(x,
         y,type="l",lwd=2,
         xlab="x",ylab="f (x)",ylim=c(0,0.5),xlim=c(0,20),main=tit1)
    
    xx=c(0,x,x[length(x):1],0)
    yy=c(0,y,rep(0,length(y)),0)
    polygon(xx,yy,col="red",border="black")
  })
  
  output$gammasimPlot<- renderPlot({
    set.seed(6674)
    x=seq(0,20,0.01)
    y=dgamma(x,input$ga,input$gb)
    Z=rgamma(input$N10,input$ga,input$gb)
    tity=paste0("Histogram ",input$N10," simulations from X~Gamma(",input$ga,",",input$gb,")")
    Q=density(Z)
    plot(Q$x,Q$y,type="l",lwd=2,xlab="x",ylab="f (x)",main=tity,ylim=c(0,0.5),xlim=c(0,20),col=4)
    
    
    
    if (input$gammaline == TRUE){
      lines(x,y,lwd=2)
      }
  })
  
  #####beta
  output$betapdfPlot<- renderPlot({
    x=seq(0,1,0.001)
    y=dbeta(x,input$ba,input$bb)
    m=length(x)
    tit1=paste0("X~Beta(",input$ba,",",input$bb,")")
    plot(x,y,type="l",lwd=2,xlab="x",ylab="f (x)",ylim=c(0,20),xlim=c(0,1),main=tit1)
    xx=c(0,x,x[length(x):1],0)
    if(input$bb<1) y[m]=2*y[m-1]
    if(input$ba<1) y[1]=2*y[2]
    yy=c(0,y,rep(0,length(y)),0)
    polygon(xx,yy,col="red",border="black")
  })
  
  output$betasimPlot<- renderPlot({
    set.seed(6674)
    x=seq(0,1,0.001)
    y=dbeta(x,input$ba,input$bb)
    m=length(x)
    Z=rbeta(input$N11,input$ba,input$bb)
    tity=paste0("Histogram ",input$N11," simulations from X~Beta(",input$ba,",",input$bb,")")
    Q=density(Z)
    plot(Q$x,Q$y,type="l",lwd=2,col=4,xlab="x",ylab="f (x)",main=tity,ylim=c(0,20),xlim=c(0,1))
    
    if (input$betaline == TRUE){
      lines(x,y,lwd=2)
      }
    })


}



shinyApp(ui = ui, server = server)