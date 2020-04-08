# 
# 
# # https://sas-and-r.blogspot.com/2014/06/example-20147-simulate-logistic.html
# set.seed(321)
# n=999
# #trt = rep(0:2,each=n/3) 
# 
# trt <-sample( 0:2, n, replace=TRUE)  #trt 3 levels
# 
# x2 = rnorm(n,0,.2)   #continuous var
# x3 = rnorm(n, 0,.35) #continuous var
# x4 = rnorm(n, 0,.25) #continuous var
# smoking = sample(0:2, n, replace=TRUE) # categorical var
# 
# lp = -3 + 2*trt*(x2 +1.2*x3 + 1*x4  +1.5*smoking) # truth all interact with trt
# lp = -3 + 2*trt* x2 +1.2*x3 + 1*x4  +1.5*smoking  # truth  only x2 interacts only with trt
# lp = -3 + 2*trt+ x2 +1.2*x3 + 1*x4  +1.5*smoking  # truth no interactions
# 
# link_lp = exp(lp)/(1 + exp(lp))
# y = 1*(runif(n) < link_lp) 
# trt <- factor(trt)
# smoking <- factor(smoking)
# 
# 
# d <<- datadist(y,  trt , x2 , x3 , x4 , smoking)
# options(datadist="d")
# 
# 
# require(rms)
# A<-lrm(y~  trt * (x2 + x3 + x4 + smoking))  # all interact with trt
# B<-lrm(y~  trt +  x2 + x3 + x4 + smoking)   # main effects
# lrtest(A,B)
# #############################################################################################
#  
# A
# summary(A)
# plot(summary(A), log=TRUE)
# 


# treatment 3 levels
# age continuous
# bmi 3 levels
# smoking 3 levels 
# sex
# CRP continuous
# berlin continuous
# vas continuous
# time continuous
# number of swollen joints continuous
# naildys binary
# evidence binary


 
# k <- contrast(A, list(smoking=0, trt=0), list(smoking=1, trt=0)) # method 5
# print(k, X=TRUE)
# k <- contrast(A, list(smoking=0, trt=1), list(smoking=1, trt=1)) # method 5
# print(k, X=TRUE)
# k <- contrast(A, list(smoking=0, trt=2), list(smoking=1, trt=2)) # method 5
# print(k, X=TRUE)


# I want X2/x3/x4/smoking by treatments''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# showing non interacting levels do not matter
# k <- contrast(A, list(smoking=0, trt=c(0,1,2) , x2=(.5)), list(smoking=1, trt=c(0,1,2), x2=.5) )#method 5
# print(k, X=TRUE)   
# smoking effect
# k1 <- contrast(A, list(smoking=c(1), trt=c(0,1,2)), list(smoking=c(0), trt=c(0,1,2)) , fun=exp)#method 5 
# print(k1, X=TRUE)   
# k2 <- contrast(A, list(smoking=c(2), trt=c(0,1,2)), list(smoking=c(0), trt=c(0,1,2)) , fun=exp)#method 5 
# print(k2, X=TRUE)  
# 
# # x2 effect 
# k3 <- contrast(A, list(x2=-.1, trt=c(0,1,2)), list(x2=.1, trt=c(0,1,2)) , fun=exp)#method 5 
# print(k3, X=TRUE)   
# 
# # x3 effect 
# k4 <- contrast(A, list(x3=-.24, trt=c(0,1,2)), list(x3=.24, trt=c(0,1,2)) , fun=exp)#method 5 
# print(k4, X=TRUE)   
# 
# # x3 effect 
# k5 <- contrast(A, list(x4=-.16, trt=c(0,1,2)), list(x4=.16, trt=c(0,1,2)) , fun=exp)#method 5 
# print(k5, X=TRUE)  
# 
# 
# # summary(A , smoking=c(1,0,2), est.all=FALSE, antilog=FALSE)
# # summary(A , x2=c(-.1,.1), est.all=FALSE, antilog=FALSE)
# namz <- c("trt" , "Contrast", "SE", "Lower", "Upper")
#  
# 
# 
# # smoking 1 v 0 in 2 trt groups
# x <- k1
# c1 <- cbind(x$tr, (x$Contrast), (x$Lower), (x$Upper), x$Pvalue)
# 
# # smoking 2 v 0 in 2 trt groups
# x <- k2
# c2 <- cbind(x$tr, (x$Contrast), (x$Lower), (x$Upper), x$Pvalue)
# 
# # this is the interacting var
# x <- k3
# c3 <- cbind(x$tr, (x$Contrast), (x$Lower), (x$Upper), x$Pvalue)
# 
# x <- k4
# c4 <- cbind(x$tr, (x$Contrast), (x$Lower), (x$Upper), x$Pvalue)
# 
#  
# rbind(c1,c2,c3,c4)
# 







































# contrast(A, list(trt='0', smoking="1"),
#          list(trt='0', smoking="2"),  type='average') 
# contrast(A, list(trt='1', smoking="1"),
#             list(trt='1', smoking="2"),  type='average') 
# contrast(A, list(trt='2', smoking="1"),
#             list(trt='2', smoking="2"),  type='average') 
# 
# 
# contrast(A, list(trt=c(0,1,2), smoking="1"),
#             list(trt=c(0,1,2), smoking="2") ) 
# 
# summary(A, trt='0')  # trt 0 ref
# summary(A, trt='1')  # trt 1 ref
# summary(A, trt='2')  # trt 2 ref
# 
# s <- summary(A, trt=c(0,1,2)) 
# # Increase age from 50 to 70, adjust to
# # 60 when estimating effects of other factors
# #Could have omitted datadist if specified 3 values for all non-categorical
# #variables (1 value for categorical ones - adjustment level)
# plot(s, log=TRUE, at=c(.01,0.5,1,1.5,2,4,8,12,16,30))
# 
# 
# 
# 
# 
# 
# 
# lc <- levels(trt)
# contrast(f, list(x2=0,          trt=lc),
#             list(x2=1,          trt=lc))


## start over


# treatment 3 levels
# age continuous
# bmi 3 levels
# smoking 3 levels 
# sex
# CRP continuous
# berlin continuous
# vas continuous
# time continuous
# number of swollen joints continuous
# naildys binary
# evidence binary



# https://sas-and-r.blogspot.com/2014/06/example-20147-simulate-logistic.html
# require(rms)
# set.seed(321)
# n=999
# trt <-      sample(0:2, n, replace=TRUE)   # trt 3 levels
# age <-      sample(18:65, n, replace=TRUE)  # rnorm(n,0,.2)                  # continuous 
# bmi <-      sample(0:2, n, replace=TRUE)   # assume 3 equal groups?
# smoking <-  sample(0:2, n, replace=TRUE)   # categorical assume 3 equal groups?
# crp <-      rnorm(n,0,.2) 
# berlin <-   rnorm(n,0,.2) 
# vas <-      sample(1:30, n, replace=TRUE)
# time <-     rnorm(n,0,.2)
# joints <-   rnorm(n,0,.2)
# nails<-     sample(0:1, n, replace=TRUE)  #trt 3 levels
# evidence <- sample(0:1, n, replace=TRUE)
# sex <-      sample(0:1, n, replace=TRUE)
# 
# trt.coef    <- log(1.2) 
# age.coef    <- log(1.2) #log(1/(65-18))  # 1.2
# bmi.coef    <- log(0.8)
# smoke.coef  <- log(1.1)
# crp.coef    <- log(1.0)
# berlin.coef <- log(1.0)
# vas.coef    <- log(0.9)
# time.coef   <- log(1.1)
# joint.coef  <- log(1.1)
# nail.coef   <- log(1.0)
# evid.coef   <- log(0.9)  
# sex.coef   <-  log(1.0)
# 
# # truth all interact with trt
# lp = -3 + trt*trt.coef*(  smoking*smoke.coef   +
#                           age*age.coef +
#                           bmi*bmi.coef +
#                           crp*crp.coef +
#                           berlin*berlin.coef +
#                           vas*vas.coef +
#                           time*time.coef +
#                           joints*joint.coef +
#                           nails*nail.coef +
#                           evidence*evid.coef +
#                           sex*sex.coef) 
# 
# 
#  # truth  only smoking interacts  with trt
# lp = -3 + (trt*trt.coef*smoking*smoke.coef)   +
#                           age*age.coef +
#                           bmi*bmi.coef +
#                           crp*crp.coef +
#                           berlin*berlin.coef +
#                           vas*vas.coef +
#                           time*time.coef +
#                           joints*joint.coef +
#                           nails*nail.coef +
#                           evidence*evid.coef  +
#   sex*sex.coef
# 
# 
# 
#   # truth no interactions
# lp = -3 + trt*trt.coef+smoking*smoke.coef   +
#   age*age.coef +
#           bmi*bmi.coef +
#           crp*crp.coef +
#           berlin*berlin.coef +
#           vas*vas.coef +
#           time*time.coef +
#           joints*joint.coef +
#           nails*nail.coef +
#           evidence*evid.coef +
#   sex*sex.coef
#  
# 
# 
# link_lp = exp(lp)/(1 + exp(lp))   # 1/(1/exp(lp)+1)  # (1/(1+exp(-lp)))
# y = 1*(runif(n) < link_lp) 
# 
# trt <- factor(trt)
# smoking <- factor(smoking)
# sex <- factor(sex)
# bmi <- factor(bmi)
# nails  <- factor(nails)
# evidence <- factor(evidence)
# 
# d <<- datadist(y,  trt ,  smoking, sex, bmi, nails, evidence, crp,age, berlin, vas, joints, sex)
# options(datadist="d")
# 
# 
# require(rms)
# A<-lrm(y~   trt * (smoking  + age + bmi + sex + nails + evidence + crp  + berlin + vas + joints )) # all interact with trt
# B<-lrm(y~  (trt *  smoking) + age + bmi + sex + nails + evidence + crp  + berlin + vas + joints )  # smoking * trt only
# C<-lrm(y~   trt +  smoking  + age + bmi + sex + nails + evidence + crp  + berlin + vas + joints )  # main effects
#  lrtest(A,B)
#  lrtest(B,C)
# #############################################################################################
# 
# C
# summary(C)
# plot(summary(C), log=TRUE)


############################################################################################
rm(list=ls())
set.seed(321)
#?summary.rms 
require(rms)
n= 1e5  #600

trt      <- sample(1:3, n, replace=TRUE)      # trt 3 levels
age      <- sample(18:65, n, replace=TRUE)    # continuous 
bmi      <- sample(1:3, n, replace=TRUE)      # assume 3 equal groups?
smoking  <- sample(1:3, n, replace=TRUE)      # categorical assume 3 equal groups?
crp      <- round(runif(n,0,3),2)  
berlin   <- round(runif(n,0,10),2)  
vas      <- sample(1:30, n, replace=TRUE)
time     <- round(runif(n,0,10),2)            # years
joints   <- sample(1:50, n, replace=TRUE)
nails    <- sample(0:1, n, replace=TRUE)
evidence <- sample(0:1, n, replace=TRUE)
sex      <- sample(0:1, n, replace=TRUE)

label(age)                <- 'Age'      # label is in Hmisc
label(trt)                <- 'Treatment'
label(bmi)                <- 'Body Mass Index'
label(smoking)            <- 'Smoking'
label(crp)                <- 'C-reactive protein'
label(berlin)             <- 'berlin score'
label(vas)                <- 'Visual analogue score'
label(time)               <- 'Time since diagnosis'
label(joints)             <- 'No of joints affected'
label(nails)              <-"not sure"
label(evidence)           <- "not sure 2"
label(sex)                <- 'Sex'

units(age) <- units(time) <- "years"
units(bmi) <- "kg/m2"
units(crp) <- "mg/dL"

trt.coef       <- 1           # log odds ratio so 1 -> 2.718, so 1 is LARGE
age.coef       <- 1/(65-18)   # log odds of 1 over the age range
smoke.coef     <- 0.4         # this is odds of 1.5
bmi.coef       <- 0           # this is an odds of 1..50:50
crp.coef       <- 1/3         # log odds 1 over range of 3
berlin.coef    <- -.5/10      # log odds -.05 per unit change
vas.coef       <- 0.25/30     # log odds .008 per unit change. log odds .25 over 30 units odds 1.27
time.coef      <- -.1/10      # log odds -.01 per year, log odds -.1 over 10 years or odds .90
joints.coef    <- 1/50        # log odds 0.02 per joint, log odds 1 over 50 units or odds 2.7
nails.coef     <- log(2)      # log odds 0.693 per change in binary, or odds of 2   
evidence.coef  <- log(1)      # log odds 0 per change in binary, or odds of 1  
sex.coef       <- log(0.5)    # log odds -0.693 per change in binary, or odds of .5  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Specify population model for log odds that Y=1

intercept <- -5
# truth all interact with trt
lp = intercept + trt*trt.coef*(smoking*smoke.coef   +   age*age.coef  + bmi*bmi.coef + crp*crp.coef +
                            berlin*berlin.coef + vas*vas.coef + time*time.coef + joints*joints.coef +
                            nails*nails.coef +
                            evidence*evidence.coef + sex*sex.coef) 

# truth  only smoking interacts  with trt
lp = intercept + (trt*trt.coef*smoking*smoke.coef)   +   age*age.coef   + bmi*bmi.coef + crp*crp.coef +
  berlin*berlin.coef + vas*vas.coef + time*time.coef + joints*joints.coef + nails*nails.coef +
  evidence*evidence.coef + sex*sex.coef

# truth no interactions
lp = intercept + trt*trt.coef + smoking*smoke.coef + age*age.coef  + bmi*bmi.coef + crp*crp.coef +
               berlin*berlin.coef + vas*vas.coef + time*time.coef + joints*joints.coef + nails*nails.coef +
evidence*evidence.coef + sex*sex.coef
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#L = exp(lp)/(1 + exp(lp))   # 1/(1/exp(lp)+1)  # (1/(1+exp(-lp)))
#y = 1*(runif(n) < L) 



trt <-     factor(trt)
smoking <- factor(smoking)
nails <-   factor(nails)
evidence <-factor(evidence)
sex <-     factor(sex)
bmi <-     factor(bmi)
y <- ifelse(runif(n) < plogis(lp), 1, 0)   # one liner

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
d <<- datadist(y,  trt ,  smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi)
options(datadist="d")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A <- B <- C <- NULL
A<-lrm(y~   trt * (smoking  + age  + bmi + crp + berlin + vas + time + joints + nails + evidence +sex)) # all interact with trt
B<-lrm(y~  (trt *  smoking) + age  + bmi + crp + berlin + vas + time + joints + nails + evidence +sex)  # smoking * trt only
C<-lrm(y~   trt +  smoking  + age +  bmi + crp + berlin + vas + time + joints + nails + evidence +sex)  # main effects
# lrtest(A,C)
C
C <- A
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# remove treatment ?  yes

# read the plot as, odds of R to L the 

par(mfrow=c(1,3)) 
par(oma=c(3,4,1,1)) # all sides have 3 lines of space
options(digits=1)C
plot(summary(A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=1, est.all=FALSE, 
             vnames=c( "labels"), abbrev = TRUE),  
     log=TRUE, xlim=c(log(.01),log(40)),
     q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20))

plot(summary(A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi, trt=2, est.all=FALSE, vnames=c( "names")), 
     log=TRUE, xlim=c(log(.01),log(40)),
     q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20))

plot(summary(A, smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex=1, bmi, trt=3, est.all=FALSE, vnames=c( "names")), 
     log=TRUE, xlim=c(log(.01),log(40)),
     q=c(  0.95 ), at=c(.02,0.05,.1,.2,.5,1,2,4,8,20))
par(mfrow=c(1,1))
 
##############################################################################################################


k3 <- contrast(A, list(age=c(29), trt=c(1:3)), 
                  list(age=c(53), trt=c(1:3)) , fun=exp)  
  




#trt ,  smoking, age, crp, berlin, vas, time, joints, nails, evidence, sex, bmi
###################presentation########################################################################
# expectation .4;      .4+.4=.8   .4+.8=1.2
k1 <- contrast(A, list(smoking=c(2), trt=c(1:3)), list(smoking=c(1), trt=c(1:3)) , fun=exp)#method 5 
print(k1, X=TRUE)   
# expectattoin 
k2 <- contrast(A, list(smoking=c(3), trt=c(1:3)), list(smoking=c(1), trt=c(1:3)) , fun=exp)#method 5 
print(k2, X=TRUE)  

 
k3 <- contrast(A, list(age=c(quantile(age,.25)), trt=c(1:3)), 
                  list(age=c(quantile(age,.75)), trt=c(1:3)) , fun=exp)#method 5 
print(k3, X=TRUE)   

 
k4 <- contrast(A, list(crp=c(quantile(crp,.25)), trt=c(1:3)), 
                  list(crp=c(quantile(crp,.75)), trt=c(1:3)) , fun=exp)#method 5
print(k4, X=TRUE)   

 
k5 <- contrast(A, list(berlin=c(quantile(berlin,.25)), trt=c(1:3)), 
                  list(berlin=c(quantile(berlin,.75)), trt=c(1:3)) , fun=exp)#method 5
print(k5, X=TRUE)   

k6 <- contrast(A, list(vas=c(quantile(vas,.25)), trt=c(1:3)), 
                  list(vas=c(quantile(vas,.75)), trt=c(1:3)) , fun=exp)#method 5
print(k6, X=TRUE)   

k7 <- contrast(A, list(time=c(quantile(time,.25)), trt=c(1:3)), 
                  list(time=c(quantile(time,.75)), trt=c(1:3)) , fun=exp)#method 5
print(k7, X=TRUE)   

k8 <- contrast(A, list(joints=c(quantile(joints,.25)), trt=c(1:3)), 
                  list(joints=c(quantile(joints,.75)), trt=c(1:3)) , fun=exp)#method 5
print(k8, X=TRUE)   

k9 <- contrast(A, list(nails=c(0), trt=c(1:3)), 
                  list(nails=c(1), trt=c(1:3)) , fun=exp)#method 5
print(k9, X=TRUE)   



k10 <- contrast(A, list(evidence=c(0), trt=c(1:3)), 
                   list(evidence=c(1), trt=c(1:3)) , fun=exp)#method 5
print(k10, X=TRUE)   

k11 <- contrast(A, list(sex=c(0), trt=c(1:3)), 
                   list(sex=c(1), trt=c(1:3)) , fun=exp)#method 5
print(k11, X=TRUE)   


k12 <- contrast(A, list(bmi=c(2), trt=c(1:3)), list(bmi=c(1), trt=c(1:3)) , fun=exp)#method 5 
print(k12, X=TRUE)   
k13 <- contrast(A, list(bmi=c(3), trt=c(1:3)), list(bmi=c(1), trt=c(1:3)) , fun=exp)#method 5 
print(k13, X=TRUE)  





plot(summary(A))





# summary(A , smoking=c(1,0,2), est.all=FALSE, antilog=FALSE)
# summary(A , x2=c(-.1,.1), est.all=FALSE, antilog=FALSE)
namz <- c("trt" , "Contrast", "SE", "Lower", "Upper")



# smoking 1 v 0 in 2 trt groups
x <- k1
c1 <- cbind(x$tr, (x$Contrast), (x$Lower), (x$Upper), x$Pvalue)
# smoking 2 v 0 in 2 trt groups
x <- k2
c2 <- cbind(x$tr, (x$Contrast), (x$Lower), (x$Upper), x$Pvalue)

# this is the interacting var
x <- k3
c3 <- cbind(x$tr, (x$Contrast), (x$Lower), (x$Upper), x$Pvalue)

x <- k4
c4 <- cbind(x$tr, (x$Contrast), (x$Lower), (x$Upper), x$Pvalue)


rbind(c1,c2,c3,c4)













