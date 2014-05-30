### R code from vignette source 'RSGHB_HowTo.rnw'

###################################################
### code chunk number 1: RSGHB_HowTo.rnw:41-71
###################################################

library(RSGHB)

# load example data
data(choicedata)

# We typically work with one one row per choice observations. This isn't necessary 
# however but it does lend it self to faster computation of the choice likelihoods
head(choicedata)

# We can then specify any variables from the choicedata data.frame that you'd like 
# to use in the utility equations in the likelihood function below. These can be 
# any variables within the data or transformations of those variables. This example 
# comes from transport so each alternative is defined by travel times and toll costs.
TT1     <- choicedata$tt1
TT2     <- choicedata$tt2
TOLL2   <- choicedata$toll2

# Here we specify the choice vectors. Note in this example there are only two 
# alternatives. Also, dummying coding the choice vector is not necessary but allows 
# for easier coding of the likelihood.
choice1    <- (choicedata$Choice==1)
choice2    <- (choicedata$Choice==2)

# Frequency of choice for the first alternative
table(choice1)

# Frequency of choice for the second alternative.
table(choice2)



###################################################
### code chunk number 2: RSGHB_HowTo.rnw:78-121
###################################################

# ------------------
# ESTIMATION CONTROL
# ------------------

# Setting control list for estimation (see ?doHB for more estimation options)

# modelname is used for naming the output files
modelname <- "MNL"          

# gVarNamesFixed contains the names for the fixed (non-random) variables in your 
# model. This will be used in output and also when displaying iteration detail to 
# the screen.
gVarNamesFixed <- c("ASC1","BTime","BCost")

# FC contains the starting values for the fixed coefficients.
FC <- c(0,0,0)                 

# ITERATION SETTINGS

# gNCREP contains the number of iterations to use prior to convergence
gNCREP    <- 30000
# gNEREP contains the number of iterations to keep for averaging after convergence 
# has been reached
gNEREP    <- 20000 	       
# gNSKIP contains the number of iterations to do in between retaining draws for 
# averaging
gNSKIP    <- 1		       
# gINFOSKIP controls how frequently to print info about the iteration process
gINFOSKIP <- 250           

# To simplify the doHB functional call, we put all of the control parameters into 
# a single list that can be passed directly to doHB.
control <- list(
     modelname=modelname,
     gVarNamesFixed=gVarNamesFixed,
     FC=FC,
     gNCREP=gNCREP,
     gNEREP=gNEREP,
     gNSKIP=gNSKIP,
     gINFOSKIP=gINFOSKIP
)



###################################################
### code chunk number 3: RSGHB_HowTo.rnw:131-157
###################################################

# ------------------
# likelihood
# ------------------

likelihood <- function(fc,b)
{

  # defining the parameters          

  # using cc var to index the fc vector simplifies the addition/subtraction of 
  # new parameters     
  cc <- 1
  ASC1   <- fc[cc];cc <- cc+1
  Btime  <- fc[cc];cc <- cc+1
  Btoll  <- fc[cc];cc <- cc+1  

  # utility functions
  v1 <- ASC1       + Btime * TT1                   
  v2 <-              Btime * TT2 + Btoll * TOLL2   
 
  # mnl probability statement
  p  <- (exp(v1)*choice1 + exp(v2)*choice2) / (exp(v1) + exp(v2))
	
  return(p)
}


###################################################
### code chunk number 4: RSGHB_HowTo.rnw:194-199
###################################################

data(Example1_F)

head(Example1_F)



###################################################
### code chunk number 5: RSGHB_HowTo.rnw:212-243
###################################################

library(RSGHB)

# load example data
data(choicedata)

# We typically work with one one row per choice observations. This isn't necessary
# however but it does lend it self to faster computation of the choice likelihoods
head(choicedata)

# We can then specify any variables from the choicedata data.frame that you'd like 
# to use in the utility equations in the likelihood function below. These can be 
# any variables within the data or transformations of those variables. This example 
# comes from transport so each alternative is defined by travel times and toll costs.
TT1     <- choicedata$tt1
TT2     <- choicedata$tt2
TOLL2   <- choicedata$toll2

# Here we specify the choice vectors. Note in this example there are only two examples. 
# Also, dummying coding the choice vector is not necessary but allows for easier 
# coding of the likelihood.
choice1    <- (choicedata$Choice==1)
choice2    <- (choicedata$Choice==2)

# Frequency of choice for the first alternative
table(choice1)

# Frequency of choice for the second alternative.
table(choice2)




###################################################
### code chunk number 6: RSGHB_HowTo.rnw:251-308
###################################################

# ------------------
# ESTIMATION CONTROL
# ------------------

# Setting control list for estimation (see ?doHB for more estimation options)

# modelname is used for naming the output files
modelname <- "MNL"          

# gVarNamesNormal provides names for the random parameters
gVarNamesNormal <- c("ASC1","BTime","BCost")

# gDIST specifies the type of continuous distribution to use for the random parameters. 
# gDIST must have an entry for each value in gVarNamesNormal
# The options are:
# 1. normal
# 2. log-nomal
# 3. negative log-normal
# 4. normal with all values below zero massed at zero
# 5. normal with all values greater than zero massed at zero
# 6. Johnson SB with a specified min and max

# In this example, we use normal distributions for all 3 of the parameters.
gDIST <- c(1,1,1)

# svN contains the starting values for the means of the normal distributions for each 
# of the random parameters
svN <- c(0,0,0)  

# ITERATION SETTINGS

# gNCREP contains the number of iterations to use prior to convergence
gNCREP    <- 30000
# gNEREP contains the number of iterations to keep for averaging after convergence 
# has been reached
gNEREP    <- 20000             
# gNSKIP contains the number of iterations to do in between retaining draws for 
# averaging
gNSKIP    <- 1		       
# gINFOSKIP controls how frequently to print info about the iteration process
gINFOSKIP <- 250   

# To simplify the doHB functional call, we put all of the control parameters into 
# a single list that can be passed directly to doHB.
control <- list(
     modelname=modelname,
     gVarNamesNormal=gVarNamesNormal,
     gDIST=gDIST,
     svN=svN,
     gNCREP=gNCREP,
     gNEREP=gNEREP,
     gNSKIP=gNSKIP,
     gINFOSKIP=gINFOSKIP
)




###################################################
### code chunk number 7: RSGHB_HowTo.rnw:315-334
###################################################

likelihood <- function(fc,b)
{  
     
     # the change from using fc to b is the only change in the likelihood 
     # function required to allow for mixing.
     cc     <- 1
     ASC1   <- b[,cc];cc<-cc+1
     Btime  <- b[,cc];cc<-cc+1
     Btoll  <- b[,cc];cc<-cc+1  
  
     v1 <- ASC1       + Btime * TT1                   
     v2 <-              Btime * TT2 + Btoll * TOLL2   
 
     p  <- (exp(v1)*choice1 + exp(v2)*choice2) / (exp(v1) + exp(v2))
     
     return(p)
}



###################################################
### code chunk number 8: RSGHB_HowTo.rnw:368-373
###################################################

data(Example2_A)

head(Example2_A)



###################################################
### code chunk number 9: RSGHB_HowTo.rnw:378-387
###################################################

data(Example2_B)

head(Example2_B)

data(Example2_Bsd)

head(Example2_Bsd)



###################################################
### code chunk number 10: RSGHB_HowTo.rnw:392-401
###################################################

data(Example2_C)

head(Example2_C)

data(Example2_Csd)

head(Example2_Csd)



###################################################
### code chunk number 11: RSGHB_HowTo.rnw:407-420
###################################################

data(Example2_D)

head(Example2_D)

# building the covariance matrix

covMat <- xpnd(colMeans(Example2_D[-1]))

rownames(covMat) <- c("ASC1","BTime","BCost")
colnames(covMat) <- c("ASC1","BTime","BCost")

covMat


