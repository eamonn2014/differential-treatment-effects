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

# trt      <- sample(1:3, n, replace=TRUE)      # trt 3 levels
# age      <- sample(18:65, n, replace=TRUE)    # continuous 
# bmi      <- sample(1:3, n, replace=TRUE)      # assume 3 equal groups?
# smoking  <- sample(1:3, n, replace=TRUE)      # categorical assume 3 equal groups?
# crp      <- round(runif(n,0,3),2)  
# berlin   <- round(runif(n,0,10),2)  
# vas      <- sample(1:30, n, replace=TRUE)
# time     <- round(runif(n,0,10),2)            # years
# joints   <- sample(1:50, n, replace=TRUE)
# nails    <- sample(0:1, n, replace=TRUE)
# evidence <- sample(0:1, n, replace=TRUE)
# sex      <- sample(0:1, n, replace=TRUE)


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
                                      
                                      splitLayout(
                                        textInput("v1", div(h5(tags$span(style="color:blue", "covariate 1 coef"))), value= "1"),
                                        textInput("v2", div(h5(tags$span(style="color:blue", "covariate 2 coef"))), value= "1"),
                                        textInput("v3", div(h5(tags$span(style="color:blue", "covariate 3 coef"))), value= "1")
                                     
                                      ),


                                      splitLayout(
                                        textInput("v4", div(h5(tags$span(style="color:blue", "covariate 4 coef"))), value= "1"),
                                        textInput("v5", div(h5(tags$span(style="color:blue", "covariate 5 coef"))), value= "1"),
                                        textInput("v6", div(h5(tags$span(style="color:blue", "covariate 6 coef"))), value= "1")
                                        
                                      ),

                                      splitLayout(
                                        textInput("v7", div(h5(tags$span(style="color:blue", "covariate 7 coef"))), value= "1"),
                                        textInput("v8", div(h5(tags$span(style="color:blue", "covariate 8 coef"))), value= "1"),
                                        textInput("v9", div(h5(tags$span(style="color:blue", "covariate 9 coef"))), value= "1")
                                        
                                      ),
                                      
                                      
                                      splitLayout(
                                        textInput("v10", div(h5(tags$span(style="color:blue", "covariate 10 coef"))), value= "1"),
                                        textInput("v11", div(h5(tags$span(style="color:blue", "covariate 11 coef"))), value= "1"),
                                        textInput("v12", div(h5(tags$span(style="color:blue", "covariate 12 coef"))), value= "1")
                                        
                                      ),
                                      
                                      
                                      
                                      
                                    
                                      textInput('X2',
                                                div(h5(tags$span(style="color:blue", "Age coefficient"))), "22,21"),

                                      textInput('X3',
                                                div(h5(tags$span(style="color:blue", "Smoking coefficient"))), "10"),
                                      tags$hr(),
                                      textInput('X4',
                                                div(h5(tags$span(style="color:blue", "BMI coefficient"))), "2"),


                                      textInput('X5',
                                                div(h5(tags$span(style="color:blue", "CRP-Protein"))), "1"),


                                      textInput('X6',
                                                div(h5(tags$span(style="color:blue", "xxxxxxxxxxx"))), "1000"),

                                      tags$hr(),
                                      textInput('X7',
                                                div(h5(tags$span(style="color:blue", "xxxxxxxxxxxx"))), "22,21"),

                                      textInput('X8',
                                                div(h5(tags$span(style="color:blue", "xxxxxxxxxxxx"))), "10"),
                                      tags$hr(),
                                      textInput("X9",
                                                div(h5(tags$span(style="color:blue", "xxxxxxxxxxxx"))), "2"),


                                      textInput('X10',
                                                div(h5(tags$span(style="color:blue", "xxxxxxxxxxxxx"))), "1"),

                                      tags$hr(),
                                      textInput("X11",
                                                div(h5(tags$span(style="color:blue", "xxxxxxxxxxxx"))), "2"),


                                      textInput('X12',
                                                div(h5(tags$span(style="color:blue", "xxxxxxxxxxxxx"))), "1"),

                                      
                                      # trt.coef       <- 1           # log odds ratio so 1 -> 2.718, so 1 is LARGE
                                      # age.coef       <- 1/(65-18)   # log odds of 1 over the age range
                                      # smoke.coef     <- 0.4         # this is odds of 1.5
                                      # bmi.coef       <- 0           # this is an odds of 1..50:50
                                      # crp.coef       <- 1/3         # log odds 1 over range of 3
                                      # berlin.coef    <- -.5/10      # log odds -.05 per unit change
                                      # vas.coef       <- 0.25/30     # log odds .008 per unit change. log odds .25 over 30 units odds 1.27
                                      # time.coef      <- -.1/10      # log odds -.01 per year, log odds -.1 over 10 years or odds .90
                                      # joints.coef    <- 1/50        # log odds 0.02 per joint, log odds 1 over 50 units or odds 2.7
                                      # nails.coef     <- log(2)      # log odds 0.693 per change in binary, or odds of 2   
                                      # evidence.coef  <- log(1)      # log odds 0 per change in binary, or odds of 1  
                                      # sex.coef       <- log(0.5)    # log odds -0.693 per change in binary, or odds of .5  
                                      
                                      
                                
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      #  textInput('n2y2', 
                                      # #      div(h5("Enter the true correlation (tab 2)")), ".8"),
                                      # div(h5(tags$span(style="color:blue", "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))), "0.8"),
                                      # tags$hr(),
                                      
                                      # div(h5("References:")),  
                                      # tags$a(href = "https://en.wikipedia.org/wiki/Bootstrapping_%28statistics%29", tags$span(style="color:blue", "[1] PRO"),),   
                                      # div(p(" ")),
                                      # tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176345338",  tags$span(style="color:blue", "[2] PO"),),   
                                      # div(p(" ")),
                                      # tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176344552", tags$span(style="color:blue", "[3] Krushke"),),
                                      # div(p(" ")),
                                      # tags$a(href = "https://blogs.sas.com/content/iml/2017/09/20/fishers-transformation-correlation.html", tags$span(style="color:blue", "[4] xxxxxx"),),  
                                      # div(p(" ")),
                                      # tags$a(href = "https://rdrr.io/cran/rms/man/predict.lrm.html", tags$span(style="color:blue", "prediction of model mean"),),  
                                      # div(p(" ")),
                                      # tags$hr()
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
                                  
                                  
                                  tabPanel("1 xxxxxxxxxx", value=7, 
                                           h4("xxxxxxxxxxxxxxxxxxxxx."),
                                         
                                           
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      div( verbatimTextOutput("datx") )
                                                      #div(plotOutput("beta",  width=fig.width7, height=fig.height7)),
                                                      
                                               ) ,
                                               
                                               
                                               fluidRow(
                                                   column(width = 5, offset = 0, style='padding:1px;',
                                                          
                                                        #  div(plotOutput("reg.plotx",  width=fig.width7, height=fig.height7)) 
                                                          
                                                   ))),
                                           h4(paste("Figures 1 & 2. xxxxxxxxxxxxxxxxx")), 
                                           
                                  ) ,
                                  
                                  tabPanel("2 xxxxxxxxx", value=3, 
                                           
                                          # div(plotOutput("reg.plot99", width=fig.width1, height=fig.height1)),
                                           
                                           fluidRow(
                                               column(width = 7, offset = 0, style='padding:1px;',
                                                      h4(paste("Figure 3. xxxxxxxxxxxxxxxxxx")), 
                                                      
                                               )),
                                           
                                           
                                  ),
                                  
                                  tabPanel("3 xxxxxxx", value=7, 
                                           
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      h4("Table 1 xxxxxxxxxxxxxx"), 
                                            #          div( verbatimTextOutput("reg.summary1") )
                                               ) ,
                                               
                                               
                                               
                                               h4("Table 2 xxxxxxxxxxxxxxxxx"),
                                               fluidRow(
                                                   column(width = 6, offset = 0, style='padding:1px;',
                                                          
                                                          splitLayout(
                                                              #textInput("bas1", div(h5("Enter a baseline low effect")), value="1", width=100),
                                                             # textInput("bas2", div(h5("Enter a baseline high effect")),value="2", width=100)
                                                          ),
                                                          
                                                          
                                                          #div( verbatimTextOutput("reg.summary3")),
                                                          
                                                          #h4(htmlOutput("textWithNumber",) ),
                                                   ))),
                                           
                                  ) ,
                                  
                                  
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  
                                  tabPanel("4 xxxxxxxxxx", value=3, 
                                           
                                          # h5(paste("Enter 999 in the box below to see all the levels or enter level(s) of interest separated by a comma")), 
                                           #textInput('rcat2', 
                                            #         div(h5(tags$span(style="color:blue",
                                             #        ))), "999"),
                                           
                                           
                                           #div(plotOutput("preds2", width=fig.width1, height=fig.height3)),
                                           
                                           
                                           
                                           fluidRow(
                                               column(width = 7, offset = 0, style='padding:1px;',
                                            #          h4(paste("Figure 4. Plot of the predicted probabilities")), 
                                                      
                                               )),
                                  ),
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  tabPanel("5 xxxxxxxxxxxxx", 
                                           h4(paste("Figure 5 & 6. xxxxxxxxxxxxxx")),
                                           
                                           h4("xxxxxxxxxxxxxxxx
"),
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      
                                                     # div(plotOutput("preds", width=fig.width7, height=fig.height3)),
                                                      
                                                      fluidRow(
                                                          
                                                     #     textInput('base', 
                                                      #              div(h5(tags$span(style="color:blue", 
                                                       #                              "xxxxxxxxxxxxxxxx"))), "1")
                                                          
                                                          
                                                      ),
                                               ) ,
                                               
                                               fluidRow(
                                                   
                                                   
                                                   column(width = 5, offset = 0, style='padding:1px;',
                                                          
                                                      #    div(plotOutput("predicts", width=fig.width7, height=fig.height3)),
                                                          
                                                          fluidRow(
                                                              
                                                              # textInput('group', 
                                                              #           div(h5(tags$span(style="color:blue", 
                                                              #                            "select treatment group: 0 for placebo, 1 for treatment, 2 for both"))), "1"),
                                                              # 
                                                              # textInput('rcat', 
                                                              #           div(h5(tags$span(style="color:blue", 
                                                              #                            "Response category, enter 999 to see all levels or enter level(s) of interest"))), "999"),
                                                              
                                                          ),
                                                          
                                                   ))),
                                           
                                           
                                           width = 30 )     ,
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  tabPanel("6 xxxxxxxx",
                                          # h4(paste("Table 3 Predicted probabilities, the estimated mean Y (meanY) is calculated by summing values of Y multiplied by the estimated Prob(Y=j)")),
                                           fluidRow(
                                               column(width = 12, offset = 0, style='padding:1px;',
                                                      
                                           #           div( verbatimTextOutput("reg.summaryp") ),
                                            #          h4(paste("Table 4 Predicted cummulative probabilities ")),
                                             #         div( verbatimTextOutput("reg.summaryc") ),
                                               ) ,
                                               
                                           ),
                                           
                                  ),
                                  
                                  tabPanel("7 xxxxxxxxxxxx", value=3, 
                                           h4("Tables 5 & 6 and Figure 7"),
                                           
                                           
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      
                                                    #  div( verbatimTextOutput("reg.summary4") )
                                               ) ,
                                               
                                               fluidRow(
                                                   column(width = 5, offset = 0, style='padding:1px;',
                                                          
                                                     #     div( verbatimTextOutput("reg.summary5")),
                                                      #    div(plotOutput("predictl", width=fig.widthx, height=fig.heightx)),
                                                          
                                                   ))),
                                #           h4("Perhaps fit the model with restricted cubic splines for the baseline predictor to test or describe non linear relationships."),
                                  ),
                                  
                                  tabPanel("8 xxxxxxx", value=3, 
                                           
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      #h4("Sometimes it is helpful to present the mean Y as a function of one or more model predictors. 
                                                       #    \n Enter an intercept for the ordinal model in the box below.."),
                                                      #textInput('kints',
                                                       #         div(h5(tags$span(style="color:blue",
                                                        #                         ""))), ""), 
                                                      
                                                      #div(plotOutput("PP.plot", width=fig.width7, height=fig.height6)),
                                                       h4("Figure 8 xxxxxxxxxxxxxx"),
                                                      br() , 
                                                      
                                                      h4(""),
                                                      
                                                      h4("Table 7 xxxxxxxxxxxx"),
                                                      #div( verbatimTextOutput("predz"), width = 2), # 
                                               ),
                                               
                                               fluidRow(
                                                   
                                                   
                                                 #  h4(" This assumes a spacing for the Y levels."),
                                                 #  h4("Try different odds ratios to see when the linear model 
                                                  #         and PO model are no longer similar."),
                                                   br(), br(), br() ,  
                                                   
                                                   
                                                   column(width = 5, offset = 0, style='padding:0px;',
                                                          
                                                    #      div(plotOutput("PP.plot2", width=fig.width7, height=fig.height6)),
                                                   #       h4("Figure 9 Predictions for each model arm by trial arm to assess similarity in the two model predictions"),
                                                          
                                                   )))
                                           
                                  ) ,
                                  
                                  
                                  tabPanel("9 xxxxxxxxxx", value=3, 
                                           
                                           #h5(paste("Checking assumptions")), 
                                          # div(plotOutput("assumption", width=fig.width1, height=fig.height3)),
                                           h4("Figure 10 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                                    #       h4( "Checking assumptions, for each predictor separately We stratify each predictor and calculate the logit of all proportions pf the form 
                                  #Y>=j, j=1,2,...,k.
                                   #   When proportional odds hold, the differences in logits between different values of j should be the same for all values of X. (This may
                                    #  get crowded with many levels of Y)" ),
                                           h4("xxxxxxxxxxxxxxxx"),
                                     #      div( verbatimTextOutput("assump")),  
                                           
                                  ),
                                  
                                  
                                  tabPanel("10 xxxxxxxx", value=3, 
                                           
                                           #div(plotOutput("ecdfs", width=fig.width1, height=fig.height3)),
                                           h4(" ."), 
                                           h4(" "), 
                                          # div(plotOutput("logitseries", width=fig.width1, height=fig.height3)),
                                           
                                           
                                           h4("Figure 12 xxxxxxxxxxxxxxxxxxxxxxxxxxx"),  
                                           
                                           h4("xxxxxxxxxxxxxxxxx")
                                           
                                  ),
                                  
                                  
                                  tabPanel("11 xxxxxxxxx", 
                                           
                                           fluidRow(
                                               column(width = 3, offset = 0, style='padding:1px;',
                                                      h4("Table 9 xxxxxxxxxxxxx"),
                                                      #div( verbatimTextOutput("dat")),
                                               ),
                                               
                                               column(width = 9, offset = 0, style='padding:1px;',
                                                     # h4("Notes"),
                                                      h4("xxxxxxxxxxxxxxxxx
                                                  \n"),
                                                      
                                                      tags$hr(),
                                                      div(h4("References:")),  
                                                      tags$a(href = "https://stats.stackexchange.com/search?q=proportional+odds+model", tags$span(style="color:blue", "[1] Proportional odds model"),),   
                                                      div(p(" ")),
                                                      tags$a(href = "hhttps://en.wikipedia.org/wiki/Ordered_logit",  tags$span(style="color:blue", "[2] Proportional odds wiki"),),   
                                                      div(p(" ")),
                                                      #  tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176344552", tags$span(style="color:blue", "[3] Krushke"),),
                                                      #  div(p(" ")),
                                                      tags$a(href = "http://hbiostat.org/doc/rms.pdf", tags$span(style="color:blue", "[3] Regression modelling strategies"),),  
                                                      div(p(" ")),
                                                      tags$a(href = "https://rdrr.io/cran/rms/man/predict.lrm.html", tags$span(style="color:blue", "[4] Prediction of model mean"),),  
                                                      div(p(" ")),
                                                      tags$hr()
                                                      
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
        
        v1 <- as.numeric(input$v1 )
        v2 <- as.numeric(input$v2 )
        v3 <- as.numeric(input$v3 )
        
        v4 <- as.numeric(input$v4 )
        v5 <- as.numeric(input$v5 )
        v6 <- as.numeric(input$v6 )
        
        v7 <- as.numeric(input$v7 )
        v8 <- as.numeric(input$v8 )
        v9 <- as.numeric(input$v9 )
        
        v10 <- as.numeric(input$v10 )
        v11 <- as.numeric(input$v11 )
        v12 <- as.numeric(input$v12 )
        
        
        
      
        
        return(list(  
            n=n, v1=v1, v2=v2, v3=v3, v4=v4, v5=v5, v6=v6, v7=v7, v9=v8, v9=v9, v10=v10, v11=v11, v12=v12   
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
        v5  <- sample$v5
        v6  <- sample$v6
        v7  <- sample$v7
        v8  <- sample$v8
        v9  <- sample$v9
        v10  <- sample$v10
        v11  <- sample$v11
        v12  <- sample$v12
       
        trt      <- sample(1:3,   n, replace=TRUE)      # trt 3 levels
        age      <- sample(18:65, n, replace=TRUE)      # continuous 
        bmi      <- sample(1:3,   n, replace=TRUE)      # assume 3 equal groups?
        smoking  <- sample(1:3,   n, replace=TRUE)      # categorical assume 3 equal groups?
        crp      <- round(runif(n,0,3),2)  
        berlin   <- round(runif(n,0,10),2)  
        vas      <- sample(1:30, n, replace=TRUE)
        time     <- round(runif(n,0,10),2)              # years
        joints   <- sample(1:50, n, replace=TRUE)
        nails    <- sample(0:1,  n, replace=TRUE)
        evidence <- sample(0:1,  n, replace=TRUE)
        sex      <- sample(0:1,  n, replace=TRUE)
        
        dat <- as.data.frame(cbind(trt, age, bmi, smoking,crp,berlin, vas, time, joints, nails , evidence, sex)) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        return(list(  dat=dat )) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DO THE ANALYSIS~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$datx <- renderPrint({
      
     return(print(mcmc()$dat, digits=3))
     # print(mcmc()$dat, digits=3)
      
    }) 
    
    
    
    analysis <- reactive({
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # beta dist plot 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
    
    output$beta <- renderPlot({        
        
       
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #  end ggplot barplot of beta distribution
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    output$reg.plotx <- renderPlot({         
        
        
    })
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$reg.plot99 <- renderPlot({         
        
       
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # non cummulative predicted probabilities plot run the analysis again
    # not efficient I know
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$preds2 <- renderPlot({
        
         
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # non cummulative predicted probabilities plot run the analysis again
    # not efficient I know
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$preds <- renderPlot({
        
       
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tables of predictions
    
    predictz <- reactive({  
        
         
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plots of predictions
    
    output$predicts <- renderPlot({   
        
        
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # text 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    output$PP.plot <- renderPlot({   
     
        
        
    }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~baseline plots~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    output$PP.plot2 <- renderPlot({   
        
      
        
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # a plot of coef from series of logistic regression models. checking assumptions
    
    output$logitseries <- renderPlot({   
        
        
        
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~assumption plot~~~~~~~~~~~~~~~~~~~~~~~~    
    # on the fly plot harrell's PO assumption plot...
    
    output$assumption <- renderPlot({   
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
    }) 
    
    
    assump <- reactive({
        
       
    #    return(list( s=s  )) 
        
    })  
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~baseline predictions~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    predz <- reactive({
        
         
    })  
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # text 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    output$textWithNumber <- renderText({ 
        
         
        
    })
    
    
  
    
    output$textWithNumber1 <- renderText({ 
        
      #  A <- analysis()$f2     
        
        
    })
    
    output$dat <- renderPrint({
        
       # d <- mcmc()$dat
        
        #d <- plyr::arrange(d, baseline, treatment)
        
    #   return(print(d, digits=4))
    })
    
    
    output$predz <- renderPrint({
        
     #  return(print(predz()$p, digits=4))
    })
    
    output$predt <- renderPrint({
        
      # return(print(predt()$pt, digits=4))
    })
    
    
    output$reg.summary1 <- renderPrint({
        
       #return( (analysis()$f2 ))
        
    })
    
    output$reg.summary3 <- renderPrint({
        
        #eturn(print(analysis()$sf1, digits=4))
        
    })
    
    output$reg.summary4 <- renderPrint({
        
    #   return(print(lmx()$linear, digits=4))
        
    })
    
    output$reg.summary5 <- renderPrint({
        
      # return(print(lmx()$an, digits=4))
     #  
    })
    
    output$reg.summaryp <- renderPrint({
        
       #return(print(predictz()$prob, digits=4))
        
    })
    
    output$reg.summaryc <- renderPrint({
        
       #return(print(predictz()$cprob, digits=4))
        
    })
    
    output$reg.summaryci <- renderPrint({
        
    #   return(print(predictz()$plotci, digits=4))
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    lmx <- reactive({
        
      #
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$predictl <- renderPlot({   
        
     #  
        
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$ecdfs <- renderPlot({   
        
      #
        
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})

# Run the application 
shinyApp(ui = ui, server = server)