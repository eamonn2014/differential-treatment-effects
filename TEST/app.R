#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls()) 
set.seed(3333) # reproducible
#library(directlabels)
library(shiny) 
library(shinyWidgets)
library(shinythemes)  # more funky looking apps
#library(DT)
library(shinyalert)
#library(Hmisc)
library(reshape)
library(rms)

#pkg <- "package:ormPlot"

#detach(pkg, character.only = TRUE)
#library(ormPlot)
#library(ordinal)
#library(ggplot2)
#library(tidyverse)
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
p4 <- function(x) {formatC(x, format="f", digits=4)}
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
                
                h4("It is is desired to investigate if there is evidence of different treatment effects depending levels of factor variables or the level 
                of continuous variables following a RCT. One or more interactions between baseline covariates and treatment are then investigated.
                Here we investigate a binary response. Note this objective will be extremely underpowered, typically wants to detect a
                differential effect that is smaller than the overall detectable treatment effect [FH Ref]
                
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
                                  tabPanel("1 Main effects", 
                                           
                                           fluidRow(
                                             column(width = 6, offset = 0, style='padding:1px;',
                                                    h4(paste("Table 1. No-interaction logit-additive model that assumes constancy of treatment ORs")), 
                                                    div( verbatimTextOutput("Cx2") )
                                                    
                                                    
                                             ),
                                             
                                             fluidRow(
                                               column(width = 5, offset = 0, style='padding:1px;',
                                                      h4(paste("An explanation of the inputs")), 
                                                      h4(htmlOutput("textWithNumber",) ),
                                               ))),
                                  ),
                                tabPanel("2 All 3 models", value=3, 
                                
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
                                  
                                tabPanel("3 LR tests", value=7, 
                                         
                                         fluidRow(
                                           column(width = 6, offset = 0, style='padding:1px;',
                                                  h4("Table 1 [Main effects model] vrs [Treatment x Smoking interaction model]"), 
                                                  div( verbatimTextOutput("L1c") ),
                                                  h4("Table 2 [Main effects model] vrs [Treatment x all predcitors interaction mode]l"), 
                                                  div( verbatimTextOutput("L1b") ),
                                                  h4("Table 3 [Treatment x Smoking interaction model] vrs [Treatment x all predcitors interaction model]"), 
                                                  div( verbatimTextOutput("L1a") )
                                                  
                                           ) ,
                                           
                                           
                                           
                                           h4("Table 2 xxxxxxxxxxxxxxxxx"),
                                           fluidRow(
                                             column(width = 6, offset = 0, style='padding:1px;',
                                                     h4("A small P-Value in the top most table provides evidence against
                                                        the simpler model fitting the data better. The simpler model 
                                                        being the main effects model."),
                                                    div(  "") ,
                                                    h4("A small P-Value in the middle table provides evidence against
                                                        the simpler model fitting the data better. The simpler model 
                                                        being the main effects model."),
                                                    div(  "") ,
                                                    h4("A small P-Value in the bottom table provides evidence against
                                                        the simpler model fitting the data better. The simpler model 
                                                        being the Treatment x Smoking interaction model."),
                                                    
                                                    
                                                    
                                                    
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
                                
                                tabPanel("4 Forest plot trt x all", value=3, 

                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                         fluidRow(
                                           column(width = 6, offset = 0, style='padding:1px;',
                                                  #          h4(paste("Figure 4. Plot of the predicted probabilities")), 
                                                  div(plotOutput("f.plot1", width=fig.width4, height=fig.height7)),
                                   
                                           )),
                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                         
                                         fluidRow(
                                           column(12,
                                                #  div( verbatimTextOutput("int.trt1" ) ),
                                                  fluidRow(
                                                    column(12,
                                                         #  div( verbatimTextOutput("int.trt2" ) ),
                                                           fluidRow(
                                                             column(4, 
                                                                    div( verbatimTextOutput("int.trt1" ) )),
                                                             column(4,
                                                                    div( verbatimTextOutput("int.trt2" ) )),
                                                             column(4,
                                                                    div( verbatimTextOutput("int.trt3" ) )),
                                                             
                                                           )
                                                    )#,
                                                    #column(width = 6,
                                                          # "Fluid 6")
                                                  )
                                           )
                                         ),
                                         
              
                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          
                                         # fluidRow(
                                         #   column(width = 4, offset = 0, style='padding:1px;',
                                         #          
                                         #          div( verbatimTextOutput("int.trt1" ) ),
                                         #          div( verbatimTextOutput("int.trt2" ) )
                                         #         # div( verbatimTextOutput("int.trt2" ) )
                                         #   )),
                                ),
                                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                tabPanel("5 Forest plot trt x smoking", value=3, 
                                         
                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                         fluidRow(
                                           column(width = 6, offset = 0, style='padding:1px;',
                                                  #          h4(paste("Figure 4. Plot of the predicted probabilities")), 
                                                  div(plotOutput("f.plot2", width=fig.width4, height=fig.height7)),
                                                  
                                           )),
                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                         
                                         fluidRow(
                                           column(12,
                                                  #  div( verbatimTextOutput("int.trt1" ) ),
                                                  fluidRow(
                                                    column(12,
                                                           #  div( verbatimTextOutput("int.trt2" ) ),
                                                           fluidRow(
                                                             column(4, 
                                                                    div( verbatimTextOutput("int.trt1B" ) )),
                                                             column(4,
                                                                    div( verbatimTextOutput("int.trt2B" ) )),
                                                             column(4,
                                                                    div( verbatimTextOutput("int.trt3B" ) )),
                                                             
                                                           )
                                                    )#,
                                                    #column(width = 6,
                                                    # "Fluid 6")
                                                  )
                                           )
                                         ),
                                         
                                         
                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                         
                                         # fluidRow(
                                         #   column(width = 4, offset = 0, style='padding:1px;',
                                         #          
                                         #          div( verbatimTextOutput("int.trt1" ) ),
                                         #          div( verbatimTextOutput("int.trt2" ) )
                                         #         # div( verbatimTextOutput("int.trt2" ) )
                                         #   )),
                                ),
                                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                tabPanel("6 Forest plot main effects",
                                         # h4(paste("Table 3 Predicted probabilities, the estimated mean Y (meanY) is calculated by summing values of Y multiplied by the estimated Prob(Y=j)")),
                                         fluidRow(
                                           column(width = 12, offset = 0, style='padding:1px;',
                                                  div(plotOutput("f.plot3", width=fig.width4, height=fig.height7)),
                                                  #           div( verbatimTextOutput("reg.summaryp") ),
                                                  #          h4(paste("Table 4 Predicted cummulative probabilities ")),
                                                  #         div( verbatimTextOutput("reg.summaryc") ),
                                                  div( verbatimTextOutput("int.trt1C" ) ),
                                           ) ,
                                           
                                         ),
                                         
                                ),
                                
                                tabPanel("7 Rel. expl. variaton", value=3, 
                                       #  h4("Tables 5 & 6 and Figure 7"),
                                         
                                         h4(htmlOutput("textWithNumber1",) ),
                                         fluidRow(
                                           column(width = 6, offset = 0, style='padding:1px;',
                                                  # div( verbatimTextOutput("rev1" ) ),
                                                  # div( verbatimTextOutput("rev2" ) ),
                                                  # div( verbatimTextOutput("rev3" ) ),
                                                  #  div( verbatimTextOutput("reg.summary4") )
                                           ) ,
                                           
                                           fluidRow(
                                             column(width = 5, offset = 0, style='padding:1px;',
                                                 #   div( verbatimTextOutput("AIC1" ) ),
                                                  #  div( verbatimTextOutput("AIC2" ) ),
                                                   # div( verbatimTextOutput("AIC3" ) ),
                                                    #     div( verbatimTextOutput("reg.summary5")),
                                                    #    div(plotOutput("predictl", width=fig.widthx, height=fig.heightx)),
                                                    
                                             ))),
                                         #           h4("Perhaps fit the model with restricted cubic splines for the baseline predictor to test or describe non linear relationships."),
                                ),
                       
                                
                                # tabPanel("8 xxxxxxx", value=3, 
                                #          
                                #          fluidRow(
                                #            column(width = 6, offset = 0, style='padding:1px;',
                                #                   #h4("Sometimes it is helpful to present the mean Y as a function of one or more model predictors. 
                                #                   #    \n Enter an intercept for the ordinal model in the box below.."),
                                #                   #textInput('kints',
                                #                   #         div(h5(tags$span(style="color:blue",
                                #                   #                         ""))), ""), 
                                #                   
                                #                   #div(plotOutput("PP.plot", width=fig.width7, height=fig.height6)),
                                #                   h4("Figure 8 xxxxxxxxxxxxxx"),
                                #                   br() , 
                                #                   
                                #                   h4(""),
                                #                   
                                #                   h4("Table 7 xxxxxxxxxxxx"),
                                #                   #div( verbatimTextOutput("predz"), width = 2), # 
                                #            ),
                                #            
                                #            fluidRow(
                                #              
                                #              
                                #              #  h4(" This assumes a spacing for the Y levels."),
                                #              #  h4("Try different odds ratios to see when the linear model 
                                #              #         and PO model are no longer similar."),
                                #              br(), br(), br() ,  
                                #              
                                #              
                                #              column(width = 5, offset = 0, style='padding:0px;',
                                #                     
                                #                     #      div(plotOutput("PP.plot2", width=fig.width7, height=fig.height6)),
                                #                     #       h4("Figure 9 Predictions for each model arm by trial arm to assess similarity in the two model predictions"),
                                #                     
                                #              )))
                                #          
                                # ) ,
                                # 
                                # 
                                # tabPanel("9 xxxxxxxxxx", value=3, 
                                #          
                                #          #h5(paste("Checking assumptions")), 
                                #          # div(plotOutput("assumption", width=fig.width1, height=fig.height3)),
                                #          h4("Figure 10 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                                #          #       h4( "Checking assumptions, for each predictor separately We stratify each predictor and calculate the logit of all proportions pf the form 
                                #          #Y>=j, j=1,2,...,k.
                                #          #   When proportional odds hold, the differences in logits between different values of j should be the same for all values of X. (This may
                                #          #  get crowded with many levels of Y)" ),
                                #          h4("xxxxxxxxxxxxxxxx"),
                                #          #      div( verbatimTextOutput("assump")),  
                                #          
                                # ),
                                # 
                                # 
                                # tabPanel("10 xxxxxxxx", value=3, 
                                #          
                                #          #div(plotOutput("ecdfs", width=fig.width1, height=fig.height3)),
                                #          h4(" ."), 
                                #          h4(" "), 
                                #          # div(plotOutput("logitseries", width=fig.width1, height=fig.height3)),
                                #          
                                #          
                                #          h4("Figure 12 xxxxxxxxxxxxxxxxxxxxxxxxxxx"),  
                                #          
                                #          h4("xxxxxxxxxxxxxxxxx")
                                #          
                                # ),
                                
                                
                                tabPanel("7 Data/References", 
                                         
                                         fluidRow(
                                           
                                           
                                           column(width = 3, offset = 0, style='padding:1px;',
                                                  tags$hr(),
                                                  div(h4("References:")),  
                                                  tags$a(href = "https://www.fharrell.com/post/varyor/", tags$span(style="color:blue", "[1] Frank Harrell"),),   
                                                  div(p(" ")),
                                                  # tags$a(href = "hhttps://en.wikipedia.org/wiki/Ordered_logit",  tags$span(style="color:blue", "[2] Proportional odds wiki"),),   
                                                  # div(p(" ")),
                                                  #  tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176344552", tags$span(style="color:blue", "[3] Krushke"),),
                                                  #  div(p(" ")),
                                                  # tags$a(href = "http://hbiostat.org/doc/rms.pdf", tags$span(style="color:blue", "[3] Regression modelling strategies"),),  
                                                  # div(p(" ")),
                                                  # tags$a(href = "https://rdrr.io/cran/rms/man/predict.lrm.html", tags$span(style="color:blue", "[4] Prediction of model mean"),),  
                                                  # div(p(" ")),
                                                  # tags$hr()
                                           ),
                                           
                                           
                                           
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
        n <- as.numeric(input$n )
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
        
        trt.coef       <-  v1     # log odds ratio so 1 -> 2.718, so 1 is LARGE
        age.coef       <-  v2     # log odds of 1 over the age range
        smoke.coef     <-  v3     # this is odds of 1.5
        bmi.coef       <-  v4     # this is an odds of 1..50:50
        crp.coef       <-  v5     # log odds 1 over range of 3
        berlin.coef    <-  v6     # log odds -.05 per unit change
        vas.coef       <-  v7     # log odds .008 per unit change. log odds .25 over 30 units odds 1.27
        time.coef      <-  v8     # log odds -.01 per year, log odds -.1 over 10 years or odds .90
        joints.coef    <-  v9     # log odds 0.02 per joint, log odds 1 over 50 units or odds 2.7
        nails.coef     <-  v10    # log odds 0.693 per change in binary, or odds of 2   
        evidence.coef  <-  v11    # log odds 0 per change in binary, or odds of 1  
        sex.coef       <-  v12    # log odds -0.693 per change in binary, or odds of .5  
        
        intercept <- -5
        
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
        

        return(list(    
          
          trt.coef       =trt.coef ,
          age.coef       =age.coef,
          smoke.coef     =smoke.coef,
          bmi.coef       =bmi.coef,
          crp.coef       =crp.coef,
          berlin.coef    =berlin.coef,
          vas.coef       =vas.coef,
          time.coef      =time.coef,
          joints.coef    =joints.coef,
          nails.coef     =nails.coef,
          evidence.coef  =evidence.coef,
          sex.coef       =sex.coef,
          
          trt= trt, 
          age=age, 
          bmi=bmi, 
          smoking=smoking,
          crp=crp,
          berlin=berlin, 
          vas=vas, 
          time=time, 
          joints=joints, 
          nails=nails , 
          evidence=evidence, 
          sex=sex,
          
          randomi=randomi))
        
        
    })    
        
 
    
    lp1 <- reactive({
        
          d <- design()

          trt      <-d$trt      
          age      <-d$age       
          bmi      <-d$bmi       
          smoking  <-d$smoking  
          crp      <-d$crp      
          berlin   <-d$berlin   
          vas      <-d$vas       
          time     <-d$time      
          joints   <-d$joints    
          nails    <-d$nails     
          evidence <-d$evidence  
          sex      <-d$sex  
         
           trt.coef      =d$trt.coef 
           age.coef      =d$age.coef
           smoke.coef    =d$smoke.coef
           bmi.coef      =d$bmi.coef
           crp.coef      =d$crp.coef
           berlin.coef   =d$berlin.coef
           vas.coef      =d$vas.coef
           time.coef     =d$time.coef
           joints.coef   =d$joints.coef
           nails.coef    =d$nails.coef
           evidence.coef =d$evidence.coef
           sex.coef      =d$sex.coef
        
           randomi <- d$randomi
           intercept <- -3
           
        
           if ( (input$Design) == "Treatment interacts with all variables" )  {
             
             lp = intercept + trt*trt.coef*(smoking*smoke.coef   +   age*age.coef  + bmi*bmi.coef + crp*crp.coef +
                                              berlin*berlin.coef + vas*vas.coef + time*time.coef + joints*joints.coef +
                                              nails*nails.coef +
                                              evidence*evidence.coef + sex*sex.coef) 
             
           }   else if ( (input$Design ) == "Treatment interacts with smoking only" ) {    
             
             # truth  only smoking interacts  with trt
             lp = intercept + (trt*trt.coef*smoking*smoke.coef)   +   age*age.coef   + bmi*bmi.coef + crp*crp.coef +
               berlin*berlin.coef + vas*vas.coef + time*time.coef + joints*joints.coef + nails*nails.coef +
               evidence*evidence.coef + sex*sex.coef
             
           }   else if ( (input$Design) == "Main effects model" ) {  
             
             # truth no interactions
             lp = intercept + trt*trt.coef + smoking*smoke.coef + age*age.coef  + bmi*bmi.coef + crp*crp.coef +
               berlin*berlin.coef + vas*vas.coef + time*time.coef + joints*joints.coef + nails*nails.coef +
               evidence*evidence.coef + sex*sex.coef
           }
           

           y <- ifelse(randomi < plogis(lp), 1, 0)   # one liner RANDOM!!!
        
        dat <- data.frame(cbind(y,  trt ,  smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi))
        
        return(list(datx=dat))
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    analysis <- reactive({
        
        da <- lp1()$datx 
        
        da$trt <-     factor(da$trt)
        da$smoking <- factor(da$smoking)
        da$nails <-   factor(da$nails)
        da$evidence <-factor(da$evidence)
        da$sex <-     factor(da$sex)
        da$bmi <-     factor(da$bmi)
        
        dd <<- datadist(da)
        options(datadist="dd")

        A<-lrm(y~   trt * (smoking  + age  + bmi + crp + berlin + vas + time + joints + nails + evidence +sex),da)  # all interact with trt
        B<-lrm(y~  (trt *  smoking) + age  + bmi + crp + berlin + vas + time + joints + nails + evidence +sex, da)  # smoking * trt only
        C<-lrm(y~   trt +  smoking  + age +  bmi + crp + berlin + vas + time + joints + nails + evidence +sex, da)  # main effect
        
        if (  (input$Model) == "Treatment interacts with all variables" )  {
            f <- A
            
        }   else if (  (input$Model) == "Treatment interacts with smoking only" ) {    
           
            f <- B
            
        }   else if (  (input$Model) == "Main effects model" ) {  
            
            f <- C
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        return(list(  A=A, B=B, C=C)) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
    })
    
    # relative explained variation on the risk scale, see frank harrell
    
    rexv <- reactive({
      
      X <- analysis() 
      
      L1 <-   var(predict(X$B, type='fitted')) / 
              var(predict(X$A, type='fitted')) 
      
      L2 <-   var(predict(X$C, type='fitted')) / 
              var(predict(X$A, type='fitted')) 
      
      L3 <-   var(predict(X$C, type='fitted')) / 
              var(predict(X$B, type='fitted')) 
      
      AICA <-   AIC(X$A)  
      AICB <-   AIC(X$B)  
      AICC <-   AIC(X$C) 
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      return(list(  L1=L1, L2= L2, L3= L3, AICA = AICA, AICB = AICB, AICC = AICC)) 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$rev1 <- renderPrint({
      return(print(rexv()$L1, digits=3))
    }) 
    output$rev2 <- renderPrint({
      return(print(rexv()$L2, digits=3))
    }) 
    output$rev3<- renderPrint({
      return(print(rexv()$L3, digits=3))
    }) 
   
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$AIC1 <- renderPrint({
      return(print(rexv()$AICA, digits=3))
    }) 
    output$AIC2 <- renderPrint({
      return(print(rexv()$AICB, digits=3))
    }) 
    output$AIC3<- renderPrint({
      return(print(rexv()$AICC, digits=3))
    }) 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    lrtestx<- reactive({
      
      X <- analysis() 
      
      L1 <- lrtest(X$A, X$B)
      
      L2 <- lrtest(X$A, X$C)
      
      L3 <- lrtest(X$B, X$C)
       
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      return(list(  L1=L1, L2= L2, L3= L3)) 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    })
    
    output$L1a <- renderPrint({
      return(print(lrtestx()$L1, digits=3))
    }) 
    output$L1b <- renderPrint({
      return(print(lrtestx()$L2, digits=3))
    }) 
    output$L1c <- renderPrint({
      return(print(lrtestx()$L3, digits=3))
    }) 
    
 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$textWithNumber <- renderText({ 
      
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
      
    HTML(paste0( "In the case of treatment  mean treatment level 2 coefficient expectation is "
 , tags$span(style="color:red",  p4( v1*1) ) ,
" and treatment level 3 coefficient expectation is "
  , tags$span(style="color:red",  p4(v1*2) ) ,

br(), br(),  
"For smoking, the true log odds is "
, tags$span(style="color:red",  p4(v3) ) , 
" so we expect 'smoking=2' to be "
, tags$span(style="color:red",  p4(v3*1) ) , 
" and 'smoking=3' to be "
, tags$span(style="color:red",  p4(v3*2) ) ,

br(), br(),  
" In the case of age, the true effect is a change of "
, tags$span(style="color:red",  p4(v2) ) ,
" log odds over the age range. ",

br(), br(),  

" The true coefficient for BMI is "
, tags$span(style="color:red",  p4(v4) ) ,
" CRP is a continuous variable and the true coefficient for CRP is "
, tags$span(style="color:red",  p4(v5) ) ,
" so for each unit change in CRP the log odds of p(y=1|x) increases by  "
, tags$span(style="color:red",  p4(v5) ) ,

br(), br(),  
"Berlin is also continuous and the true coefficient is "
, tags$span(style="color:red",  p4(v6) ) , 
". So for each unit change in berlin the log odds of p(y=1|x) changes by "
, tags$span(style="color:red",  p4(v6) ) , 

br(), br(), 
" Vas again is continuous and the true coeffient is "
, tags$span(style="color:red",  p4(v7) ) , 
". So for each unit change in vas the log odds of p(y=1|x) changes by "
, tags$span(style="color:red",  p4(v7) ) , 

br(), br(), 
"Time is continuous and the true coeffient is "
, tags$span(style="color:red",  p4(v8) ) , 
". So for each unit change in time the log odds of p(y=1|x) shifts by "
, tags$span(style="color:red",  p4(v8) ) , 

br(), br(),

"Joint is treated as continuous and the coeffient "
, tags$span(style="color:red",  p4(v9) ) , 
". So for each unit change in joints the log odds of p(y=1|x) shifts by "
, tags$span(style="color:red",  p4(v9) ) , 
br(), br(),
"Nails, Evidence and Sex are binary predictors. For nails the default coefficent is "
, tags$span(style="color:red",  p4(v10) ) , 
". So  the change to the next level of Nails results in a "
, tags$span(style="color:red",  p4(v10) ) ,  
" shift in the log odds of p(y=1|x).",
br(), br(),
"For evidence, in truth there is an effect in truth of "
, tags$span(style="color:red",  p4(v11) ) , 
". So  the change to the next level results in a "
, tags$span(style="color:red",  p4(v11) ) ,   
". sex =1 compared to sex=0 in truth the coefficient is "
, tags$span(style="color:red",  p4(v12) ) ,""
                     
        ))    
        
      })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # rexv <- reactive({
    #   
    #   X <- analysis() 
    #   
    #   L1 <-   var(predict(X$B, type='fitted')) / 
    #     var(predict(X$A, type='fitted')) 
    #   
    #   L2 <-   var(predict(X$C, type='fitted')) / 
    #     var(predict(X$A, type='fitted')) 
    #   
    #   L3 <-   var(predict(X$C, type='fitted')) / 
    #     var(predict(X$B, type='fitted')) 
    #   
    #   AICA <-   AIC(X$A)  
    #   AICB <-   AIC(X$B)  
    #   AICC <-   AIC(X$C) 
    #   
    #   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   return(list(  L1=L1, L2= L2, L3= L3, AICA = AICA, AICB = AICB, AICC = AICC)) 
    #   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   
    # })
    
    output$textWithNumber1 <- renderText({ 
      
      est <- rexv()
 
      HTML(paste0(  tags$hr(),
      "Comparing the model with treatment x smoking interaction to 
                   the one with all covarites interacting with treatment the relative explained variation on the risk scale is "  
                   , tags$span(style="color:red",  p4( est$L1)  ),
                   " From this we see that even without penalizing for overfitting, 
                   all the treatment interactions 
                   account for only "  
                   , tags$span(style="color:red",  p4(1- est$L1) ) ,
                   " of the predictive information. The treatment x smoking 
                   model is at least "
                  , tags$span(style="color:red",  p4( est$L1)  ),
                  " adequate on a scale from 0 to 1.", 
                  br(), br(),
                  "Comparing the main effects model to 
                   the one with all covarites interacting with treatment the relative explained variation on the risk scale is "    
                  , tags$span(style="color:red",  p4( est$L2)  ),
                  " From this we see that even without penalizing for overfitting, 
                   all the treatment interactions 
                   account for only "  
                  , tags$span(style="color:red",  p4(1- est$L2) ) ,
                  " of the predictive information. The no-interaction logit-additive 
                   model that assumes constancy of treatment ORs is at least "
                  , tags$span(style="color:red",  p4( est$L2)  ),
                  " adequate on a scale from 0 to 1. ",
                  br(), br(),
                  "Comparing the main effects model to 
                   the one with only smoking interacting with treatment the relative explained variation on the risk scale is "    
                  , tags$span(style="color:red",  p4( est$L3)  ),
                  " From this we see that even without penalizing for overfitting, 
                   all the treatment interactions 
                   account for only "  
                  , tags$span(style="color:red",  p4(1- est$L3) ) ,
                  " of the predictive information. The no-interaction logit-additive 
                   model that assumes constancy of treatment ORs is at least "
                  , tags$span(style="color:red",  p4( est$L3)  ),
                  " adequate on a scale from 0 to 1. ",
                  tags$hr(),
                  # br(), br(),
                  "We can also use AIC to assess whether allowing for interactions will likely result in better patient-specific outcome predictions. 
                  The lower the AIC the better:",
                  br(), br(),
                  "The AIC of the  no-interaction logit-additive model that assumes constancy of treatment ORs "  
                  , tags$span(style="color:red",  p4( est$AICC)  ),
                  br(), br(),
                  "The AIC of the  treatment X smoking interaction model is "  
                  , tags$span(style="color:red",  p4( est$AICB)  ),
                  br(), br(),
                  "The AIC of the  treatment X all predictor interaction model is "  
                  , tags$span(style="color:red",  p4( est$AICA)  ),
                  br(), br()
                  
                  
                  
      ))    
      
    })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # FULL INTERACTION MODEL
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## prep some table
    zummary<- reactive({
      
      X <- analysis() 
      
      A1 <- summary(X$A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=1, est.all=FALSE, vnames=c( "names"))
      
      A2 <- summary(X$A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=2, est.all=FALSE, vnames=c( "names"))
      
      A3 <- summary(X$A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=3, est.all=FALSE, vnames=c( "names"))
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      return(list(  A1=A1, A2= A2, A3= A3)) 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    })
    output$int.trt1 <- renderPrint({
      return(print(zummary()$A1))
    }) 
    output$int.trt2 <- renderPrint({
      return(print(zummary()$A2))
    }) 
    output$int.trt3 <- renderPrint({
      return(print(zummary()$A3))
    }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # SMOKING TRT INTERACTION MODEL
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    zummaryB<- reactive({
      
      X <- analysis() 
      
      A1 <- summary(X$B, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=1, est.all=FALSE, vnames=c( "names"))
      
      A2 <- summary(X$B, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=2, est.all=FALSE, vnames=c( "names"))
      
      A3 <- summary(X$B, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=3, est.all=FALSE, vnames=c( "names"))
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      return(list(  A1=A1, A2= A2, A3= A3)) 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    })
    
    output$int.trt1B <- renderPrint({
      return(print(zummaryB()$A1))
    }) 
    output$int.trt2B <- renderPrint({
      return(print(zummaryB()$A2))
    }) 
    output$int.trt3B <- renderPrint({
      return(print(zummaryB()$A3))
    }) 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # MAIN EFFECTS MODELL
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    zummaryC<- reactive({
      
      X <- analysis() 
      
      A1 <- summary(X$C, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=1, est.all=FALSE, vnames=c( "names"))
      
           #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      return(list(  A1=A1 )) 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    })
    
    output$int.trt1C <- renderPrint({
      return(print(zummaryC()$A1))
    }) 
   
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$f.plot1 <- renderPlot({   
      
      X <- analysis() 
      
      A <- X$A
 
      par(mfrow=c(1,3)) 
      
      par(oma=c(3,4,1,1)) 
      
      options(digits=1)
      
      plot(summary(A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=1, est.all=FALSE, vnames=c( "names")), 
           log=TRUE, xlim=c(log(.01),log(40)),
           q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
           col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
           col.points='black', cex=1, main= "Odds Ratio (main effects model)", cex.main=1.8
      )
      
      plot(summary(A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=2, est.all=FALSE, vnames=c( "names")), 
           log=TRUE, xlim=c(log(.01),log(40)),
           q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
           col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
           col.points='black', cex=1, main= "Odds Ratio (main effects model)", cex.main=1.8
      )
      
      plot(summary(A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex=0, bmi, trt=3, est.all=FALSE, vnames=c( "names")), 
           log=TRUE, xlim=c(log(.01),log(40)),
           q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
           col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
           col.points='black', cex=1, main= "Odds Ratio (main effects model)", cex.main=1.8
      )
      
      par(mfrow=c(1,1))
      
      
    }) 
    

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    

    output$f.plot2 <- renderPlot({

      X <- analysis()

      A <- X$B

      par(mfrow=c(1,3))

      par(oma=c(3,4,1,1))

      options(digits=1)


      plot(summary(A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=1, est.all=FALSE, vnames=c( "names")),
           log=TRUE, xlim=c(log(.01),log(40)),
           q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
           col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
           col.points='black', cex=1, main= "Odds Ratio (main effects model)", cex.main=1.8
      )

      plot(summary(A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=2, est.all=FALSE, vnames=c( "names")),
           log=TRUE, xlim=c(log(.01),log(40)),
           q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
           col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
           col.points='black', cex=1, main= "Odds Ratio (main effects model)", cex.main=1.8
      )

      plot(summary(A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex=0, bmi, trt=3, est.all=FALSE, vnames=c( "names")),
           log=TRUE, xlim=c(log(.01),log(40)),
           q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
           col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
           col.points='black', cex=1, main= "Odds Ratio (main effects model)", cex.main=1.8
      )
      

      par(mfrow=c(1,1))


    })

    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    output$f.plot3 <- renderPlot({
      
      X <- analysis()
      
      A <- X$C
      
     # par(mfrow=c(1,3))
      
      par(oma=c(3,4,1,1))
      
      options(digits=1)
      
      
      plot(summary(A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=1, est.all=FALSE, vnames=c( "names")),
           log=TRUE, xlim=c(log(.2),log(10)),
           q=c( 0.95 ), at=c( .1,.2,.3,.5,.75,1, 1.2,1.5, 2,3,4,6,8,10), lwd=3, pch=17,
           col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
           col.points='black', cex=1, main= "Odds Ratio (main effects model)", cex.main=1.8
           )
      
    
      
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    
    
    
    
   
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
    output$datx <- renderPrint({
        return(print(lp1()$datx, digits=3))
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
    output$Cx2 <- renderPrint({
      return(print(analysis()$C, digits=3))
    }) 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})

# Run the application 
shinyApp(ui = ui, server = server)