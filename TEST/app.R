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
                                                  
                                                  choices=c(  "Main effects model",
                                                             "Treatment interacts with smoking only" ,
                                                             "Treatment interacts with all variables" 
                                                             ), width='70%'),
                                                  
                                                   
                                      
                                      selectInput("Model",
                                                  strong("Select modelling preference:"),
                                                  choices=c(  "Main effects model",
                                                              "Treatment interacts with smoking only" ,
                                                              "Treatment interacts with all variables" 
                                                  ), width='70%'),
                                      
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
                                      splitLayout(
                                          textInput("v4", div(h5(tags$span(style="color:blue", "bmi coef"))), value= "0"),
                                          textInput("v5", div(h5(tags$span(style="color:blue", "crp coef"))), value= "1/3"),
                                          textInput("v6", div(h5(tags$span(style="color:blue", "berlin coef"))), value= "-.5/10")
                                          
                                      ),
                                      
                                      splitLayout(
                                          textInput("v7", div(h5(tags$span(style="color:blue", "vas coef"))), value= "0.25/30"),
                                          textInput("v8", div(h5(tags$span(style="color:blue", "time coef"))), value= "-.1/10"),
                                          textInput("v9", div(h5(tags$span(style="color:blue", "joints coef"))), value= "1/50")
                                          
                                      ),
                                      
                                      
                                      splitLayout(
                                          textInput("v10", div(h5(tags$span(style="color:blue", "nails coef"))), value= "log(2)"),
                                          textInput("v11", div(h5(tags$span(style="color:blue", "evidence coef"))), value= "log(1)"),
                                          textInput("v12", div(h5(tags$span(style="color:blue", "sex coef"))), value= "log(0.5)")
                                          
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
                                  
                                tabPanel("1 xxxxxxxxx", value=3, 
                                
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
                                   tabPanel("2 Data", 
                                           
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
        v5 <- as.numeric(    eval(parse(text= (input$v5)) ) )  
        v6 <- as.numeric(    eval(parse(text= (input$v6)) ) ) 
        
        v7 <- as.numeric(    eval(parse(text= (input$v7)) ) )
        v8 <- as.numeric(    eval(parse(text= (input$v8)) ) )
        v9 <- as.numeric(    eval(parse(text= (input$v9)) ) )
        
        v10 <- as.numeric(    eval(parse(text= (input$v10)) ) )
        v11 <- as.numeric(    eval(parse(text= (input$v11)) ) )
        v12 <- as.numeric(    eval(parse(text= (input$v12)) ) )
        
        
        check =c(v1 , v2 , v3 , v4 , v5,  v6, v7,  v8 , v9 , v10 , v11 , v12  )
        
        return(list(
            v1=v1, v2=v2, v3=v3, v4=v4, v5=v5, v6=v6, v7=v7, v8=v8, v9=v9, v10=v10, v11=v11, v12=v12 ,
            check=check, n=n
            
        ))
     })
      
    
    randomness <- reactive({
    
        n <- as.numeric(input$n )
        randomi <- runif(n)
        
        return(list(
            randomi=randomi
        ))
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    design <- reactive({
       
        randomi <- randomness()$randomi
        
        sample <- random.sample()
        
        n <-            as.numeric(input$n )
        
        trt      <- sample(1:3,   n, replace=TRUE)      # trt 3 levels
        age      <- sample(18:65, n, replace=TRUE)      # continuous
        bmi      <- sample(1:3,   n, replace=TRUE)      # assume 3 equal groups?
        smoking  <- sample(1:3,   n, replace=TRUE)      # categorical assume 3 equal groups?
        
        
        trt.coef      <-as.numeric(    eval(parse(text= (input$v1)) ) )    
        age.coef      <-as.numeric(    eval(parse(text= (input$v2)) ) )      
        smoke.coef    <-as.numeric(    eval(parse(text= (input$v3)) ) )       
        bmi.coef      <-as.numeric(    eval(parse(text= (input$v4)) ) ) 
        
       
         
        #randomi <- runif(n)
        
        return(list(trt=trt,
                    age=age,
                    bmi=bmi, 
                    smoking=smoking,   
                    trt.coef=trt.coef,
                    age.coef=age.coef,
                    smoke.coef=smoke.coef,
                    bmi.coef=bmi.coef, 
                    randomi=randomi)
             )
        
        
    })    
        
    
    lp1 <- reactive({
        
        
          d <- design()
          
           trt=d$trt
           age=d$age
           bmi=d$bmi 
           smoking=d$smoking
           
           trt.coef=d$trt.coef
           age.coef=d$age.coef
           smoke.coef=d$smoke.coef
           bmi.coef=d$bmi.coef
        
           randomi <- d$randomi
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
        
        y <- ifelse(randomi < plogis(lp), 1, 0)   # one liner RANDOM!!!
        
        dat <- data.frame(cbind(y, trt, smoking, age, bmi))
        return(list(datx=dat))
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    analysis <- reactive({
        
        da <- lp1()$datx 
        
        dd <<- datadist(da)
        options(datadist="dd")

        A<-lrm(y~   trt * (smoking  + age  + bmi  ), da) # all interact with trt
        B<-lrm(y~  (trt *  smoking) + age  + bmi  , da )  # smoking * trt only
        C<-lrm(y~   trt +  smoking  + age +  bmi  , da)  # main effect
        
        if (  (input$Model) == "Treatment interacts with all variables" )  {
            f <- A
            
        }   else if (  (input$Model) == "Treatment interacts with smoking only" ) {    
            # all interact with trt
            f <- B
            
        }   else if (  (input$Model) == "Main effects model" ) {  
            
            f <- C
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        return(list(  A=A, B=B, C=C)) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
    output$datx <- renderPrint({
        return(print(lp()$datx, digits=3))
    }) 
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