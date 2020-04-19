#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R code stripped from R shiny 12April2020.
# Investigating differential trt effects (logistic regression)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())   
set.seed(3333)          # reproducible
 
library(reshape)
library(rms)
 
options(max.print=1000000)    
options(scipen=999)

## convenience functions
p0 <- function(x) {formatC(x, format="f", digits=1)}
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p3 <- function(x) {formatC(x, format="f", digits=3)}
p4 <- function(x) {formatC(x, format="f", digits=4)}
p5 <- function(x) {formatC(x, format="f", digits=5)}

logit <-     function(p) log(1/(1/p-1))
expit <-     function(x) 1/(1/exp(x) + 1)
inv_logit <- function(logit) exp(logit) / (1 + exp(logit))
is.even <-   function(x){ x %% 2 == 0 }                       # function to id. odd maybe useful
options(width=200)

 # select the design to simulate
 #Design = "Treatment interacts with all variables" 
 Design = "Treatment interacts with smoking only" 
 #Design = "No-interaction logit-additive model"
                                  
 # coefficients on log odds scale
 
                                  intercept <- -3
                                  n=1000
                                  v1 = 1
                                  v2 = 1/(65-18)
                                  v3 = 0.4
                                  v4 = 0
                                  v5 = 1/3
                                  v6 = -.5/10
                                  v7 = 0.25/30
                                  v8 = -.1/10
                                  v9 = -1/50
                                  v10 = log(2)
                                  v11 = -log(1)
                                  v12 = log(0.5)
                 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # for Rshiny needed this separate so could control this
    randomi <- runif(n)    # this determine Y=1 later, 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
    trt.coef       <-  v1     # log odds ratio so 1 -> 2.718, so 1 is LARGE
    age.coef       <-  v2     # log odds of 1 over the age range
    smoke.coef     <-  v3     # this is odds of 1.5
    bmi.coef       <-  v4     # this is an odds of 1..50:50
    covar3.coef    <-  v5     # log odds 1 over range of 3
    covar1.coef    <-  v6     # log odds -.05 per unit change
    vas.coef       <-  v7     # log odds .008 per unit change. log odds .25 over 30 units odds 1.27
    time.coef      <-  v8     # log odds -.01 per year, log odds -.1 over 10 years or odds .90
    covar2.coef    <-  v9     # log odds 0.02 per unit, log odds 1 over 50 units or odds 2.7
    fact1.coef     <-  v10    # log odds 0.693 per change in binary, or odds of 2   
    binary2.coef   <-  v11    # log odds 0 per change in binary, or odds of 1  
    sex.coef       <-  v12    # log odds -0.693 per change in binary, or odds of .5  
    
    # made up data structure 
    trt      <- sample(1:3,   n, replace=TRUE)      # trt 3 levels
    age      <- sample(18:65, n, replace=TRUE)      # continuous
    bmi      <- sample(1:3,   n, replace=TRUE)      # assume 3 equal groups
    smoking  <- sample(1:3,   n, replace=TRUE)      # categorical assume 3 equal groups
    covar3   <- round(runif(n,0,3),2)
    covar1   <- round(runif(n,0,10),2)
    vas      <- sample(1:30, n, replace=TRUE)
    time     <- round(runif(n,0,10),2)              # years
    covar2   <- sample(1:50, n, replace=TRUE)
    fact1    <- sample(0:1,  n, replace=TRUE)
    binary2  <- sample(0:1,  n, replace=TRUE)
    sex      <- sample(0:1,  n, replace=TRUE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~linear predictor
    if ( ( Design) == "Treatment interacts with all variables" )  {
      
      lp = intercept + trt*trt.coef*(smoking*smoke.coef   +   age*age.coef  + bmi*bmi.coef   + covar3*covar3.coef +
                                       covar1*covar1.coef +   vas*vas.coef  + time*time.coef + covar2*covar2.coef +
                                       fact1*fact1.coef   +
                                       binary2*binary2.coef + sex*sex.coef) 
      
    }   else if ( ( Design ) == "Treatment interacts with smoking only" ) {    
      
      # truth  only smoking interacts  with trt
      lp = intercept + (trt*trt.coef*smoking*smoke.coef)   +   age*age.coef   + bmi*bmi.coef + covar3*covar3.coef +
                                       covar1*covar1.coef  + vas*vas.coef   + time*time.coef + covar2*covar2.coef + fact1*fact1.coef +
                                       binary2*binary2.coef + sex*sex.coef
      
    }   else if ( ( Design) == "No-interaction logit-additive model" ) {  
      
      # truth no interactions
      lp = intercept + trt*trt.coef + smoking*smoke.coef + age*age.coef  + bmi*bmi.coef  + covar3*covar3.coef  +
                                      covar1*covar1.coef + vas*vas.coef  + time*time.coef + covar2*covar2.coef + fact1*fact1.coef +
                                      binary2*binary2.coef + sex*sex.coef
    }
    
    y <- ifelse(randomi < plogis(lp), 1, 0)   # one liner  randomi object is necessary for R shiny only
    
    datx <- data.frame(cbind(y,  trt ,smoking,  age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi))
 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    da <-  datx 
    
    da$trt <-     factor(da$trt)
    da$smoking <- factor(da$smoking)
    da$fact1 <-   factor(da$fact1)
    da$binary2 <- factor(da$binary2)
    da$sex <-     factor(da$sex)
    da$bmi <-     factor(da$bmi)
    
    label(da$age)                <- 'Age'                       # labels for forest plots
    label(da$trt)                <- 'Treatment'
    label(da$bmi)                <- 'Body Mass Index'
    label(da$smoking)            <- 'Smoking'
    label(da$covar3)             <- 'Biomarker'
    label(da$covar1)             <- 'Blood score'
    label(da$vas)                <- 'Visual analogue score'
    label(da$time)               <- 'Time since diagnosis'
    label(da$covar2)             <- 'Fitness score'
    label(da$fact1)              <- "History"
    label(da$binary2)            <- "Employed"
    label(da$sex)                <- 'Sex'
    
    dd <<- datadist(da)
    options(datadist="dd")
    
    # RUN REGRESSONS
    A<-lrm(y~   trt * (smoking  + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex),da, y=TRUE ,x=TRUE)  # all interact with trt
    B<-lrm(y~  (trt *  smoking) + age  + bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex, da, y=TRUE, x=TRUE)  # smoking * trt only
    C<-lrm(y~   trt +  smoking  + age +  bmi + covar3 + covar1 + vas + time + covar2 + fact1 + binary2 +sex, da, y=TRUE, x=TRUE)  # main effect
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # rel explained variation
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    L1 <-   var(predict(B, type='fitted')) / 
            var(predict(A, type='fitted')) 
    
    L2 <-   var(predict(C, type='fitted')) / 
            var(predict(A, type='fitted')) 
    
    L3 <-   var(predict(C, type='fitted')) / 
            var(predict(B, type='fitted')) 
    
    L1
    L2  
    L3 
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # LRTESTS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    k1 <- contrast(A, list(smoking=c(2), trt=c(1:3)), list(smoking=c(1), trt=c(1:3)) , fun=exp) 
    z1 <- print(k1, X=TRUE)   
    
    lrtest( A, B)
    
    lrtest( A, C)
    
    lrtest( B, C)
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # FULL INTERACTION MODEL SUMMARY
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
    (A1 <- summary(A, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=1, est.all=FALSE, vnames=c( "labels")))
    
    (A2 <- summary(A, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=2, est.all=FALSE, vnames=c( "labels")))
    
    (A3 <- summary(A, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=3, est.all=FALSE, vnames=c( "labels")))
    
    
    (A1 <- summary(A, smoking=1, age=0, covar3=0, covar1=0, vas=0, time=0, covar2=0, fact1=0, binary2=0, sex=0, bmi=1, trt=1, est.all=FALSE, vnames=c( "labels")))
    
    (A2 <- summary(A, smoking=1, age=0, covar3=0, covar1=0, vas=0, time=0, covar2=0, fact1=0, binary2=0, sex=0, bmi=1, trt=2, est.all=FALSE, vnames=c( "labels")))
    
    (A3 <- summary(A, smoking=1, age=0, covar3=0, covar1=0, vas=0, time=0, covar2=0, fact1=0, binary2=0, sex=0, bmi=1, trt=3, est.all=FALSE, vnames=c( "labels")))
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # SMOKING TRT INTERACTION MODEL SUMMARY
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    (A1 <- summary(B, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=1, est.all=FALSE, vnames=c( "labels")))
    
    (A2 <- summary(B, smoking=1,   trt=2, est.all=FALSE, vnames=c( "labels")))
    
    (A3 <- summary(B, smoking=1,   trt=3, est.all=FALSE, vnames=c( "labels")))
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # MAIN EFFECTS MODEL SUMMARY
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
    (A1 <- summary(C, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=1, est.all=FALSE, vnames=c( "labels")))
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plotting the summaries in forest plots
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
    par(mfrow=c(1,3)) 
    
    par(oma=c(3,6,1,1)) 
    
    options(digits=1)
    
    plot(summary(A, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=1, est.all=FALSE, vnames=c( "labels")), 
         log=TRUE, xlim=c(log(.01),log(40)),
         q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
         col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
         col.points='black', cex=1, main= "Odds Ratio (Treatment 1)", cex.main=1.8
    )
    
    plot(summary(A, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=2, est.all=FALSE, vnames=c( "labels")), 
         log=TRUE, xlim=c(log(.01),log(40)),
         q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
         col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
         col.points='black', cex=1, main= "Odds Ratio (Treatment 2)", cex.main=1.8
    )
    
    plot(summary(A, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=3, est.all=FALSE, vnames=c( "labels")), 
         log=TRUE, xlim=c(log(.01),log(40)),
         q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
         col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
         col.points='black', cex=1, main= "Odds Ratio (Treatment 3)", cex.main=1.8
    )
    
    par(mfrow=c(1,1))
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Ax <- B
    
    par(mfrow=c(1,3))
    
    par(oma=c(3,6,1,1))
    
    options(digits=1)
    
    plot(summary(Ax, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=1, est.all=FALSE, vnames=c( "labels")),
         log=TRUE, xlim=c(log(.01),log(40)),
         q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
         col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
         col.points='black', cex=1, main= "Odds Ratio (Treatment 1)", cex.main=1.8
    )
    
    plot(summary(Ax, smoking=1,   trt=2, est.all=FALSE, vnames=c( "labels")),
         log=TRUE, xlim=c(log(.01),log(40)),
         q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
         col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
         col.points='black', cex=1, main= "Odds Ratio (Treatment 2)", cex.main=1.8
    )
    
    plot(summary(Ax, smoking=1,   trt=3, est.all=FALSE, vnames=c( "labels")),
         log=TRUE, xlim=c(log(.01),log(40)),
         q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20), lwd=3, pch=17,
         col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
         col.points='black', cex=1, main= "Odds Ratio (Treatment 3)", cex.main=1.8
    )
    
    par(mfrow=c(1,1))
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
    Ax <- C
    
    par(oma=c(3,4,1,1))
    
    options(digits=1)
    
    
    plot(summary(Ax, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=1, est.all=FALSE, vnames=c( "labels")),
         log=TRUE, xlim=c(log(.2),log(10)),
         q=c( 0.95 ), at=c( .1,.2,.3,.5,.75,1, 1.2,1.5, 2,3,4,6,8,10), lwd=3, pch=17,
         col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
         col.points='black', cex=1,  main=" <- worse outcomes      Odds Ratio       better outcomes ->                ", cex.main=1.8 
    )
    

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # EXPLORE SUMMARY AND CONTRAST EQUIVALENCE
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # focus on smoking effects
    
  (k1 <- contrast(A, list(smoking=c(2), trt=c(1:3)), list(smoking=c(1), trt=c(1:3)) , fun=exp) )
    
  (A1 <- summary(A, smoking=1,  trt=1, est.all=FALSE, vnames=c( "labels")))
  (A2 <- summary(A, smoking=1,  trt=2, est.all=FALSE, vnames=c( "labels"))) 
  (A3 <- summary(A, smoking=1,  trt=3, est.all=FALSE, vnames=c( "labels"))) 
  
  (A1 <- summary(A, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=1, est.all=FALSE, vnames=c( "labels")))
  (A2 <- summary(A, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=2, est.all=FALSE, vnames=c( "labels")))
  (A3 <- summary(A, smoking=1, age, covar3, covar1, vas, time, covar2, fact1, binary2, sex, bmi=1, trt=3, est.all=FALSE, vnames=c( "labels")))
    
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # anova(A)
    # x<- lrtest(A,C)
    # print(x$stats[3], digits=6)
    # plot(anova(A))
    # 
   
    # do this if interaction! 
    # shows total effect and interaction only
    # for example if we interact with smoking in reality but report the trt x all interactions
    # we can see smoking is important overall and interacts
    # we can see another var is important overall but does not interact
    x <- anova(A, india=FALSE, vnames='labels' )
    print(x, which=c( 'subscripts' ))
    plot(x)
 
    
    # do this if no interaction
    x <- anova(C, india=FALSE, vnames='labels' )
    print(x, which=c( 'subscripts' ))
    plot(x)
 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #Variance inflation
    car::vif(A)
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # model checking
    library(tidyverse)
    logits <- predict(A)
    
    mydata <- da %>%
    dplyr::select_if(is.numeric) 
    
    predictors <- colnames(mydata)
    
    mydata <- cbind(mydata, logits)
    
    #http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
    L <- gather(mydata, condition, predictor, predictors, factor_key=TRUE)

    
    ggplot(L, aes(logits, predictor))+
      geom_point(size = 0.5, alpha = 0.5) +
      geom_smooth(method = "loess") + 
      theme_bw() + 
      facet_wrap(~condition, scales = "free_y")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    resid(A, "partial", pl="loess")  # same as last 3 lines
    resid(A, "partial", pl=TRUE)     # plots for all columns of X using supsmu
    resid(A, "gof")                  # global test of goodness of fit
    #'Expected value|H0' is so coincidental with the 'Sum of squared errors'...keep the model
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    anova(A)
 
    
    # (k1 <- contrast(A, list(smoking=c(2), trt=c(1:3)), 
    #                    list(smoking=c(1), trt=c(1:3)) , fun=exp) )
    # 
    # contrast(A, list(trt=1, smoking=1),
    #             list(trt=2, smoking=1),
    #             list(trt=1, smoking=2),
    #             list(trt=2, smoking=2))
  #  The first contrast (double difference; differential treatment effect) has a standard error that 
  #  is larger than that of the second contrast (treatment effect averaged over the two smoking locations), 
  #  and only the latter overall treatment effect has evidence for being non-zero (i.e., a benefit of t-PA)
    
    # contrast(A, list(trt=1, smoking=1),
    #             list(trt=2, smoking=1),
    #             list(trt=1, smoking=2),
    #             list(trt=2, smoking=2))
    
    contrast(A, list(trt=1, smoking=2),
             list(trt=2, smoking=2),
             list(trt=1, smoking=3),
             list(trt=2, smoking=3))

    
    
    w <- with(da, c(sum(smoking==1), sum(smoking==2)))
    contrast(A, list(trt=1,  smoking=c(1,2)),
                list(trt=2,  smoking=c(1,2)),
                 type='average', weights=w)
    
    # 
    # contrast(A, list(trt=1, smoking=1),
    #          list(trt=2, smoking=1),
    #          list(trt=1, smoking=3),
    #          list(trt=2, smoking=3))
    # 
    # w <- with(da, c(sum(smoking==1), sum(smoking==3)))
    # contrast(A, list(trt=1,  smoking=c(1,3)),
    #          list(trt=2,  smoking=c(1,3)),
    #          type='average', weights=w)
    # 
    # 
    # 
    # varz <-  c(  "smoking", "age", "bmi", "covar3", "covar1", "vas", "time", 
    #              "covar2", "fact1", "binary2")
    # 
    # 
    #   i = varz[1]           
    #   v <-  eval(parse(text= (i)))
    #   
    # 
    #              contrast(A, list(trt=1, v=2),
    #                       list(trt=2, v=2),
    #                       list(trt=1, v=3),
    #                       list(trt=2, v=3))
    #              
    #              
    #              
    #              w <- with(da, c(sum(v==1), sum(v==2)))
    #              contrast(A, list(trt=1,  v=c(1,2)),
    #                       list(trt=2,  v=c(1,2)),
    #                       type='average', weights=w)
                 
    
    # Frank Harrell example
                 require(Hmisc)
                
                 load(url('http://hbiostat.org/data/gusto.rda'))
                 gusto <- upData(gusto, keep=Cs(day30, tx, age, Killip, sysbp, pulse, pmi, miloc))
    
                 
                 
                 dd <- datadist(gusto); options(datadist='dd')
                 
                 x <- lrm(day30 ~ tx + miloc , data=gusto,
                          eps=0.005, maxit=30)
                 
                 i <- lrm(day30 ~ tx * miloc , data=gusto,
                          eps=0.005, maxit=30)
                 
                 lrtest(x,i)  # no eveidence 
                 
                 i; anova(i)
                 
               z <-  contrast(i, list(tx='tPA', miloc='Other'),
                          list(tx='SK',  miloc='Other'),
                          list(tx='tPA', miloc='Anterior'),
                          list(tx='SK',  miloc='Anterior'))
                 
               print(z, digits=4)
               
               plot(summary(i, tx='tPA'))
               plot(summary(i ))
    
                
               
               
               gusto$tx <- relevel(gusto$tx, "SK+tPA")
               gusto$miloc <- relevel(gusto$miloc , "Other")
               A <- lrm(day30 ~ tx * miloc ,gusto, y=TRUE, x=TRUE)    
               gusto$tx <- relevel(gusto$tx, "SK")
               B <- lrm(day30 ~ tx * miloc ,gusto, y=TRUE, x=TRUE)   # all interact with trt
               gusto$tx <- relevel(gusto$tx, "tPA")
               C <- lrm(day30 ~ tx * miloc ,gusto, y=TRUE, x=TRUE) 
               
               B
               print(z, digits=4)
               
               par(mfrow=c(1,3)) 
               
               par(oma=c(3,6,1,1)) 
               
               options(digits=1)
               
               plot(summary(A, miloc='Other', tx="SK", est.all=FALSE, vnames=c( "labels")), 
                    log=TRUE, xlim=c(log(.01),log(40)),
                    q=c(  0.95 ), at=c( .2,.5,1,2,4 ), lwd=3, pch=17,abbrev=TRUE,
                    col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
                    col.points='black', cex=1, main= "Odds Ratio (Treatment SK)", cex.main=1.8
               )
               
               plot(summary(A, miloc='Other', tx="tPA", est.all=FALSE, vnames=c( "labels")), 
                    log=TRUE, xlim=c(log(.01),log(40)),
                    q=c(  0.95 ), at=c( .2,.5,1,2,4 ), lwd=3, pch=17,
                    col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
                    col.points='black', cex=1, main= "Odds Ratio (Treatment tPA)", cex.main=1.8
               )
               
               plot(summary(A, miloc='Other', tx="SK+tPA", est.all=FALSE, vnames=c( "labels")), 
                    log=TRUE, xlim=c(log(.01),log(40)),
                    q=c(  0.95 ), at=c( .2,.5,1,2,4 ), lwd=3, pch=17,
                    col=   rgb(red=.4,green=.1,blue=.5,alpha=c(.5,.3,.2)),
                    col.points='black', cex=1, main= "Odds Ratio (Treatment SK+tPA)", cex.main=1.8
               )
               
               
               par(mfrow=c(1,1))        
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               