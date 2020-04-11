#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls()) 
set.seed(333) # reproducible
library(directlabels)
library(shiny) 
library(shinyWidgets)
library(shinythemes)  # more funky looking apps
library(DT)
library(shinyalert)
library(Hmisc)
library(reshape)
library(rms)
library(ormPlot)
library(ordinal)
library(ggplot2)
library(tidyverse)
#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
options(max.print=1000000)    
fig.width <- 400
fig.height <- 300
fig.width1 <- 1380
fig.height1 <- 700
fig.width2 <- 1400
fig.height2 <- 300
fig.width3 <- 1400  
fig.height3 <- 600
fig.width4 <- 1380
fig.height4 <- 450
fig.width5 <- 1380
fig.height5 <- 225
fig.width6 <- 400
fig.height6 <- 550
fig.width7 <- 600
fig.widthx <- 593
fig.heightx <- 268
fig.height7 <- 600
fig.width9 <- 1380
fig.height9 <- 500

 


## convenience functions
p0 <- function(x) {formatC(x, format="f", digits=1)}
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p3 <- function(x) {formatC(x, format="f", digits=3)}
p5 <- function(x) {formatC(x, format="f", digits=5)}
logit <- function(p) log(1/(1/p-1))
expit <- function(x) 1/(1/exp(x) + 1)
inv_logit <- function(logit) exp(logit) / (1 + exp(logit))
is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful
options(width=200)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                # paper
                useShinyalert(),  # Set up shinyalert
                setBackgroundColor(
                    color = c( "#2171B5", "#F7FBFF"), 
                    gradient = "linear",
                    direction = "bottom"
                ),
                
                h2("Differential treatment effects"), 
                
                h4("xxxxxxxxxxxxxxxxxx
         "), 
                
                h3("  "), 

                sidebarLayout(
                    
                    sidebarPanel( width=3 ,
                                  
                                  tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                                  
                                  
                                  actionButton(inputId='ab1', label="R Shiny ",   icon = icon("th"),   
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/proportional-odds-model2/master/app.R', '_blank')"), 
                                  actionButton(inputId='ab1', label="R code",   icon = icon("th"),   
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/proportional-odds-model2/master/app%20stripped%20code.R', '_blank')"),  
                                  actionButton("resample", "Simulate a new sample"),
                                  br(),  
                                  tags$style(".well {background-color:#b6aebd ;}"), 
                                  
                                  h4("Instructions: The first input below isxxxxxxxxxxxxxxxxxx."),
                                  div(
                                      
                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                      
                                      tags$head(
                                          tags$style(HTML('#ab1{background-color:orange}'))
                                      ),
                                      
                                      tags$head(
                                          tags$style(HTML('#resample{background-color:orange}'))
                                      ),
                                      
                                      textInput('n',
                                                div(h5(tags$span(style="color:blue", "Sample size"))), value= "1000"),
                                      
                                      tags$hr(),
                                      
                                      selectInput("Design",
                                                  strong("Select design preference:"),
                                                  choices=c( "Treatment interacts with all variables" , 
                                                             "Treatment interacts with smoking only" ,
                                                             "Main effects model" ), width='70%'),
                                      
                                      selectInput("Model",
                                                  strong("Select modelling preference:"),
                                                  choices=c( "Treatment interacts with all variables" , 
                                                             "Treatment interacts with smoking only" ,
                                                             "Main effects model" ), width='70%'),
                                      
                             
                                      splitLayout(
                                          textInput("v1", div(h5(tags$span(style="color:blue", "treatment coef"))), value= "1"),
                                          textInput("v2", div(h5(tags$span(style="color:blue", "age coef"))), value= "1/(65-18)"),
                                          textInput("v3", div(h5(tags$span(style="color:blue", "smoking coef"))), value= "0.4")
                                          
                                      ),
                                      
                                      
                                      splitLayout(
                                          textInput("v4", div(h5(tags$span(style="color:blue", "bmi coef"))), value= "0"),
                                          textInput("v5", div(h5(tags$span(style="color:blue", "crp coef"))), value= "1/3"),
                                          textInput("v6", div(h5(tags$span(style="color:blue", "berlin coef"))), value= "-.5/10")
                                          
                                      ),
                                       
                           
                                  )

                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(width=9,
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              navbarPage(       
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                                  tags$style(HTML("
                            .navbar-default .navbar-brand {color: orange;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: #b6aebd;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}
                   ")),
                                  
                                tabPanel("2 xxxxxxxxx", value=3, 
                                
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      h4(paste("Figure 3. xxxxxxxxxxxxxxxxxx")), 
                                                      div( verbatimTextOutput("Cx") ),
                                                      div( verbatimTextOutput("Bx") )
                                                      
                                               ),
                                               
                                               fluidRow(
                                                   column(width = 5, offset = 0, style='padding:1px;',
                                                          h4(paste("Figure 3. xxxxxxxxxxxxxxxxxx")), 
                                                          div( verbatimTextOutput("Ax") )
                                                   ))),
                                     
                                  ),
                                   tabPanel("11 Data", 
                                           
                                           fluidRow(
                                           
                                               column(width = 9, offset = 0, style='padding:1px;',
                                                      # h4("Notes"),
                                                      h4("Table 9 xxxxxxxxxxxxx"),
                                                      div( verbatimTextOutput("datx") ),
                                                      
                                               )
                                               
                                               
                                           )
                                  )##end
                              
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   END NEW   
                              )
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                       )
                ) 
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- shinyServer(function(input, output   ) {
    
    shinyalert("Welcome! \nModelling Differential Treatment effects!",
               "Treatment covariate interactions", 
               type = "info")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # This is where a new sample is instigated 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    random.sample <- reactive({
        
        foo <- input$resample
        
        n <- as.numeric(input$n )
        
        # writing like this I can write log and fraction into input boxes!
        v1 <- as.numeric(    eval(parse(text= (input$v1)) ) )
        v2 <- as.numeric(    eval(parse(text= (input$v2)) ) )
        v3 <- as.numeric(    eval(parse(text= (input$v3)) ) )    
        v4 <- as.numeric(    eval(parse(text= (input$v4)) ) )   
        
        check =c(v1 , v2 , v3   )
        
        return(list(
            n=n, v1=v1, v2=v2, v3=v3  , v4=v4
          
        ))
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tab 1 simulate po model data and analyse
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mcmc <- reactive({
        
        sample <- random.sample()
        
        n   <- sample$n
        v1  <- sample$v1
        v2  <- sample$v2
        v3  <- sample$v3
        v4  <- sample$v4
         
        trt      <- sample(1:3,   n, replace=TRUE)      # trt 3 levels
        age      <- sample(18:65, n, replace=TRUE)      # continuous
        bmi      <- sample(1:3,   n, replace=TRUE)      # assume 3 equal groups?
        smoking  <- sample(1:3,   n, replace=TRUE)      # categorical assume 3 equal groups?
         
        dat <- as.data.frame(cbind(trt, age, bmi, smoking ))
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        return(list(  dat=dat, n=n,
                      n=n, v1=v1, v2=v2, v3=v3, v4=v4  )) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$datx <- renderPrint({
        
        return(print(mcmc()$dat, digits=3))
        
    }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    analysis <- reactive({
        
        coefs <- mcmc()
         
        d <- mcmc()$dat
        
        n        <-coefs$n
        trt      <-d$trt      
        age      <-d$age       
        bmi      <-d$bmi       
        smoking  <-d$smoking  
        
        trt.coef       <- coefs$v1     # log odds ratio so 1 -> 2.718, so 1 is LARGE
        age.coef       <- coefs$v2     # log odds of 1 over the age range
        smoke.coef     <- coefs$v3     # this is odds of 1.5
        bmi.coef       <- coefs$v4     # this is an odds of 1..50:50
        
        intercept <- -3
        
        
        if ( (input$Design) == "Treatment interacts with all variables" )  {
            
            lp = intercept + trt*trt.coef*(smoking*smoke.coef   +   age*age.coef  + bmi*bmi.coef   ) 
            
        }   else if ( (input$Design ) == "Treatment interacts with smoking only" ) {    
            
            # truth  only smoking interacts  with trt
            lp = intercept + (trt*trt.coef*smoking*smoke.coef)   +   age*age.coef   + bmi*bmi.coef  
            
            
        }   else if ( (input$Design) == "Main effects model" ) {  
            
            # truth no interactions
            lp = intercept + trt*trt.coef + smoking*smoke.coef + age*age.coef  + bmi*bmi.coef  
            
            
        }
        
        trt <-     factor(trt)
        smoking <- factor(smoking)
        bmi <-     factor(bmi)
        
        y <- ifelse(runif(n) < plogis(lp), 1, 0)   # one liner
        
        d <<- datadist(y,  trt ,  smoking, age,   bmi)
        options(datadist="d")
        
        A<-lrm(y~   trt * (smoking  + age  + bmi  )) # all interact with trt
        B<-lrm(y~  (trt *  smoking) + age  + bmi  )  # smoking * trt only
        C<-lrm(y~   trt +  smoking  + age +  bmi  )  # main effect
        
        if ( isolate(input$Model) == "Treatment interacts with all variables" )  {
            f <- A
            
        }   else if ( isolate(input$Model) == "Treatment interacts with smoking only" ) {    
            # all interact with trt
            f <- B
            
        }   else if ( isolate(input$Model) == "Main effects model" ) {  
            
            f <- C
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        return(list(  lp=lp , y=y , A=A, B=B, C=C)) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        

    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    output$Ax <- renderPrint({
        return(print(analysis()$A, digits=3))
    }) 
    output$Bx <- renderPrint({
        return(print(analysis()$B, digits=3))
    }) 
    output$Cx <- renderPrint({
        return(print(analysis()$C, digits=3))
    }) 
 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})

# Run the application 
shinyApp(ui = ui, server = server)