# Modified to go to 3 time points

# Modified to have a latent intercept and slope construction as in latent growth modeling


#This script simulates, then fits, data for a multiple indicator univariate Latent Change Score model.
#This script is part of the manuscript 
#'Developmental cognitive neuroscience using Latent Change Score models: A tutorial and applications'
#Rogier A. Kievit, Andreas M. Brandmaier, Gabriel Ziegler, Anne-Laura van Harmelen, 
#Susanne de Mooij, Michael Moutoussisa, Ian Goodyer, Ed Bullmore, Peter Jones, 
#Peter Fonagy, NSPN Consortium, Ulman Lindenberger & Raymond J. Dolan                                                         



#This code was written by Rogier A. Kievit (rogier.kievit@mrc-cbu.cam.ac.uk), 30 January 2017.
#It may be used, (re)shared and modified freely under a CC-BY license 

# DISCLAIMER: SEM model choices and conventions vary - final responsibility for 
# model fitting and interpretation lies with the researcher 





# Define the simulation model  ----
MILCS_simulate<-'

####    The following two lines specify the measurement model for multiple indicators
####    measured on multiple occasions
FOAVSP_T1 =~ .63*VISSPNRN_T1 + .62*LOCSPNRN_T1 + .61*VISSPAN_T1 + .55*LOCSPAN_T1 + .60*VISSPANB_T1 + .47*CROSMODB_T1 + .31*DIGSPNRN_T1 + .10*PHONOLB_T1   
FOAVSP_T2 =~ .63*VISSPNRN_T2 + .62*LOCSPNRN_T2 + .61*VISSPAN_T2 + .55*LOCSPAN_T2 + .60*VISSPANB_T2 + .47*CROSMODB_T2 + .31*DIGSPNRN_T2 + .10*PHONOLB_T2   
FOAVSP_T3 =~ .63*VISSPNRN_T3 + .62*LOCSPNRN_T3 + .61*VISSPAN_T3 + .55*LOCSPAN_T3 + .60*VISSPANB_T3 + .47*CROSMODB_T3 + .31*DIGSPNRN_T3 + .10*PHONOLB_T3   
#FOAVSP_T4 =~ .63*VISSPNRN_T4 + .62*LOCSPNRN_T4 + .61*VISSPAN_T4 + .55*LOCSPAN_T4 + .60*VISSPANB_T4 + .47*CROSMODB_T4 + .31*DIGSPNRN_T4 + .10*PHONOLB_T4   
#FOAVSP_T5 =~ .63*VISSPNRN_T5 + .62*LOCSPNRN_T5 + .61*VISSPAN_T5 + .55*LOCSPAN_T5 + .60*VISSPANB_T5 + .47*CROSMODB_T5 + .31*DIGSPNRN_T5 + .10*PHONOLB_T5   


# Error variances for indicators
VISSPNRN_T1~~0.6031*VISSPNRN_T1          # This specifies the indicators residual variances 
LOCSPNRN_T1~~0.6156*LOCSPNRN_T1          # This specifies the indicators residual variances 
VISSPAN_T1 ~~0.6279*VISSPAN_T1           # This specifies the indicators residual variances 
LOCSPAN_T1~~0.6975*LOCSPAN_T1          # This specifies the indicators residual variances 
VISSPANB_T1~~0.6400*VISSPANB_T1          # This specifies the indicators residual variances 
CROSMODB_T1~~0.7755*CROSMODB_T1          # This specifies the indicators residual variances 
DIGSPNRN_T1~~0.8555*DIGSPNRN_T1          # This specifies the indicators residual variances 
PHONOLB_T1~~0.8300*PHONOLB_T1          # This specifies the indicators residual variances

# Error variances for indicators
VISSPNRN_T2~~0.6031*VISSPNRN_T2          # This specifies the indicators residual variances 
LOCSPNRN_T2~~0.6156*LOCSPNRN_T2          # This specifies the indicators residual variances 
VISSPAN_T2 ~~0.6279*VISSPAN_T2           # This specifies the indicators residual variances 
LOCSPAN_T2~~0.6975*LOCSPAN_T2          # This specifies the indicators residual variances 
VISSPANB_T2~~0.6400*VISSPANB_T2          # This specifies the indicators residual variances 
CROSMODB_T2~~0.7755*CROSMODB_T2          # This specifies the indicators residual variances 
DIGSPNRN_T2~~0.8555*DIGSPNRN_T2          # This specifies the indicators residual variances 
PHONOLB_T2~~0.8300*PHONOLB_T2          # This specifies the indicators residual variances 

# Error variances for indicators
VISSPNRN_T3~~0.6031*VISSPNRN_T3          # This specifies the indicators residual variances 
LOCSPNRN_T3~~0.6156*LOCSPNRN_T3          # This specifies the indicators residual variances 
VISSPAN_T3 ~~0.6279*VISSPAN_T3           # This specifies the indicators residual variances 
LOCSPAN_T3~~0.6975*LOCSPAN_T3          # This specifies the indicators residual variances 
VISSPANB_T3~~0.6400*VISSPANB_T3          # This specifies the indicators residual variances 
CROSMODB_T3~~0.7755*CROSMODB_T3          # This specifies the indicators residual variances 
DIGSPNRN_T3~~0.8555*DIGSPNRN_T3          # This specifies the indicators residual variances 
PHONOLB_T3~~0.8300*PHONOLB_T3          # This specifies the indicators residual variances 

# Error variances for indicators
#VISSPNRN_T4~~0.6031*VISSPNRN_T4          # This specifies the indicators residual variances 
#LOCSPNRN_T4~~0.6156*LOCSPNRN_T4          # This specifies the indicators residual variances 
#VISSPAN_T4 ~~0.6279*VISSPAN_T4           # This specifies the indicators residual variances 
#LOCSPAN_T4~~0.6975*LOCSPAN_T4          # This specifies the indicators residual variances 
#VISSPANB_T4~~0.6400*VISSPANB_T4          # This specifies the indicators residual variances 
#CROSMODB_T4~~0.7755*CROSMODB_T4          # This specifies the indicators residual variances 
#DIGSPNRN_T4~~0.8555*DIGSPNRN_T4          # This specifies the indicators residual variances 
#PHONOLB_T4~~0.8300*PHONOLB_T4          # This specifies the indicators residual variances 

# Error variances for indicators
#VISSPNRN_T5~~0.6031*VISSPNRN_T5          # This specifies the indicators residual variances 
#LOCSPNRN_T5~~0.6156*LOCSPNRN_T5          # This specifies the indicators residual variances 
#VISSPAN_T5 ~~0.6279*VISSPAN_T5           # This specifies the indicators residual variances 
#LOCSPAN_T5~~0.6975*LOCSPAN_T5          # This specifies the indicators residual variances 
#VISSPANB_T5~~0.6400*VISSPANB_T5          # This specifies the indicators residual variances 
#CROSMODB_T5~~0.7755*CROSMODB_T5          # This specifies the indicators residual variances 
#DIGSPNRN_T5~~0.8555*DIGSPNRN_T5          # This specifies the indicators residual variances 
#PHONOLB_T5~~0.8300*PHONOLB_T5          # This specifies the indicators residual variances 

# Measurement model intercepts
VISSPNRN_T1~0*1
LOCSPNRN_T1~0*1
VISSPAN_T1~0*1
LOCSPAN_T1~0*1
VISSPANB_T1~0*1
CROSMODB_T1~0*1
DIGSPNRN_T1~0*1
PHONOLB_T1~0*1   

VISSPNRN_T2~0*1
LOCSPNRN_T2~0*1
VISSPAN_T2~0*1
LOCSPAN_T2~0*1
VISSPANB_T2~0*1
CROSMODB_T2~0*1
DIGSPNRN_T2~0*1
PHONOLB_T2~0*1   

VISSPNRN_T3~0*1
LOCSPNRN_T3~0*1
VISSPAN_T3~0*1
LOCSPAN_T3~0*1
VISSPANB_T3~0*1
CROSMODB_T3~0*1
DIGSPNRN_T3~0*1
PHONOLB_T3~0*1   

#VISSPNRN_T4~0*1
#LOCSPNRN_T4~0*1
#VISSPAN_T4~0*1
#LOCSPAN_T4~0*1
#VISSPANB_T4~0*1
#CROSMODB_T4~0*1
#DIGSPNRN_T4~0*1
#PHONOLB_T4~0*1   

#VISSPNRN_T5~0*1
#LOCSPNRN_T5~0*1
#VISSPAN_T5~0*1
#LOCSPAN_T5~0*1
#VISSPANB_T5~0*1
#CROSMODB_T5~0*1
#DIGSPNRN_T5~0*1
#PHONOLB_T5~0*1   

#####     The following lines specify the core assumptions of the LCS 
#####     and should not generally be modified

iFOAVSP=~1*FOAVSP_T1                   # This defines the FOAVSP intercept measurement model
sFOAVSP=~1*dFOAVSP1+1*dFOAVSP2            # This defines the FOAVSP slope measurement model

FOAVSP_T2 ~ 1*FOAVSP_T1           # Fixed regression of FOAVSP_T2 on FOAVSP_T1
dFOAVSP1 =~ 1*FOAVSP_T2           # Fixed regression of FOAVSP_T2 on dFOAVSP1
FOAVSP_T2 ~ 0*1                # This line constrains the intercept of FOAVSP_T2 to 0
FOAVSP_T2 ~~ 0*FOAVSP_T2          # This fixes the variance of the FOAVSP_T2 to 0  

FOAVSP_T3 ~ 1*FOAVSP_T2           # Fixed regression of FOAVSP_T3 on FOAVSP_T2
dFOAVSP2 =~ 1*FOAVSP_T3           # Fixed regression of FOAVSP_T3 on dFOAVSP2
FOAVSP_T3 ~ 0*1                # This line constrains the intercept of FOAVSP_T3 to 0
FOAVSP_T3 ~~ 0*FOAVSP_T3          # This fixes the variance of the FOAVSP_T3 to 0  

#FOAVSP_T4 ~ 1*FOAVSP_T3           # Fixed regression of FOAVSP_T4 on FOAVSP_T3
#dFOAVSP3 =~ 1*FOAVSP_T4           # Fixed regression of FOAVSP_T4 on dFOAVSP2
#FOAVSP_T4 ~ 0*1                # This line constrains the intercept of FOAVSP_T4 to 0
#FOAVSP_T4 ~~ 0*FOAVSP_T4          # This fixes the variance of the FOAVSP_T4 to 0  

#FOAVSP_T5 ~ 1*FOAVSP_T4           # Fixed regression of FOAVSP_T5 on FOAVSP_4
#dFOAVSP4 =~ 1*FOAVSP_T5           # Fixed regression of FOAVSP_T5 on dFOAVSP2
#FOAVSP_T5 ~ 0*1                # This line constrains the intercept of FOAVSP_T5 to 0
#FOAVSP_T5 ~~ 0*FOAVSP_T5          # This fixes the variance of the FOAVSP_T5 to 0 




###### The following five parameters will be estimated in the model. 
###### Values can be modified manually to examine the effect on the model


# This is changed
FOAVSP_T1 ~ 0*1              # This fixes the intercept of FOAVSP_T1 to 0. 
FOAVSP_T1 ~~ 0*FOAVSP_T1        # This fixes the residual variance of the FOAVSP_T1 to 0. 

dFOAVSP1 ~ 0*1            # This fixes the intercept of the change score to 0 
dFOAVSP1 ~~ .1*dFOAVSP1        # This fixes the residual variance of the change scores to .1. 

dFOAVSP2 ~ 0*1            # This fixes the intercept of the change score to 0 
dFOAVSP2 ~~ .1*dFOAVSP2        # This fixes the residual variance of the change scores to .1. 

#dFOAVSP3 ~ 0*1            # This fixes the intercept of the change score to 0 
#dFOAVSP3 ~~ .1*dFOAVSP3        # This fixes the residual variance of the change scores to .1. 

#dFOAVSP4 ~ 0*1            # This fixes the intercept of the change score to 0 
#dFOAVSP4 ~~ .1*dFOAVSP4        # This fixes the residual variance of the change scores to .1. 
 
# This is new
iFOAVSP~0*1                           # This specifies the FOAVSP intercept intercept (mean)
iFOAVSP~~.5*iFOAVSP                       # This specifies the FOAVSP intercept variance
sFOAVSP~.57*1                           # This specifies the FOAVSP slope intercept
sFOAVSP~~4*sFOAVSP                       # This specifies the FOAVSP slope variance


'

# Generate the data ----
# set.seed(1234)
simdatMILCS<-simulateData(MILCS_simulate,sample.nobs = sample.size,meanstructure = T) #Simulate data

# colMeans(simdatMILCS) #sanity check the means
# write.csv(simdatMILCS,'2_simdatMILCS.csv')

# Define the fitted model ------
MILCS<-'

FOAVSP_T1 =~ .63*VISSPNRN_T1 + LOCSPNRN_T1 + VISSPAN_T1 + LOCSPAN_T1 + VISSPANB_T1 + CROSMODB_T1 + DIGSPNRN_T1 + PHONOLB_T1   
FOAVSP_T2 =~ .63*VISSPNRN_T2 + equal("FOAVSP_T1=~LOCSPNRN_T1")*LOCSPNRN_T2 + equal("FOAVSP_T1=~VISSPAN_T1")*VISSPAN_T2 + equal("FOAVSP_T1=~LOCSPAN_T1")*LOCSPAN_T2 + equal("FOAVSP_T1=~VISSPANB_T1")*VISSPANB_T2 + equal("FOAVSP_T1=~CROSMODB_T1")*CROSMODB_T2 + equal("FOAVSP_T1=~DIGSPNRN_T1")*DIGSPNRN_T2 + equal("FOAVSP_T1=~PHONOLB_T1")*PHONOLB_T2
FOAVSP_T3 =~ .63*VISSPNRN_T3 + equal("FOAVSP_T1=~LOCSPNRN_T1")*LOCSPNRN_T3 + equal("FOAVSP_T1=~VISSPAN_T1")*VISSPAN_T3 + equal("FOAVSP_T1=~LOCSPAN_T1")*LOCSPAN_T3 + equal("FOAVSP_T1=~VISSPANB_T1")*VISSPANB_T3 + equal("FOAVSP_T1=~CROSMODB_T1")*CROSMODB_T3 + equal("FOAVSP_T1=~DIGSPNRN_T1")*DIGSPNRN_T3 + equal("FOAVSP_T1=~PHONOLB_T1")*PHONOLB_T3
#FOAVSP_T4 =~ .63*VISSPNRN_T4 + equal("FOAVSP_T1=~LOCSPNRN_T1")*LOCSPNRN_T4 + equal("FOAVSP_T1=~VISSPAN_T1")*VISSPAN_T4 + equal("FOAVSP_T1=~LOCSPAN_T1")*LOCSPAN_T4 + equal("FOAVSP_T1=~VISSPANB_T1")*VISSPANB_T4 + equal("FOAVSP_T1=~CROSMODB_T1")*CROSMODB_T4 + equal("FOAVSP_T1=~DIGSPNRN_T1")*DIGSPNRN_T4 + equal("FOAVSP_T1=~PHONOLB_T1")*PHONOLB_T4
#FOAVSP_T5 =~ .63*VISSPNRN_T5 + equal("FOAVSP_T1=~LOCSPNRN_T1")*LOCSPNRN_T5 + equal("FOAVSP_T1=~VISSPAN_T1")*VISSPAN_T5 + equal("FOAVSP_T1=~LOCSPAN_T1")*LOCSPAN_T5 + equal("FOAVSP_T1=~VISSPANB_T1")*VISSPANB_T5 + equal("FOAVSP_T1=~CROSMODB_T1")*CROSMODB_T5 + equal("FOAVSP_T1=~DIGSPNRN_T1")*DIGSPNRN_T5 + equal("FOAVSP_T1=~PHONOLB_T1")*PHONOLB_T5


FOAVSP_T2 ~ 1*FOAVSP_T1     # Fixed regression of FOAVSP_T2 on FOAVSP_T1
dFOAVSP1 =~ 1*FOAVSP_T2     # Fixed regression of dFOAVSP1 on FOAVSP_T2
FOAVSP_T2 ~ 0*1          # This line constrains the intercept of FOAVSP_T2 to 0
FOAVSP_T2 ~~ 0*FOAVSP_T2    # This fixes the variance of the FOAVSP_T2 to 0 

FOAVSP_T3 ~ 1*FOAVSP_T2           # Fixed regression of FOAVSP_T3 on FOAVSP_T2
dFOAVSP2 =~ 1*FOAVSP_T3           # Fixed regression of dFOAVSP2 on FOAVSP_T3
FOAVSP_T3 ~ 0*1                # This line constrains the intercept of FOAVSP_T3 to 0
FOAVSP_T3 ~~ 0*FOAVSP_T3          # This fixes the variance of the FOAVSP_T3 to 0  

#FOAVSP_T4 ~ 1*FOAVSP_T3           # Fixed regression of FOAVSP_T4 on FOAVSP_T3
#dFOAVSP3 =~ 1*FOAVSP_T4           # Fixed regression of dFOAVSP3 on FOAVSP_T4
#FOAVSP_T4 ~ 0*1                # This line constrains the intercept of FOAVSP_T4 to 0
#FOAVSP_T4 ~~ 0*FOAVSP_T4          # This fixes the variance of the FOAVSP_T4 to 0  

#FOAVSP_T5 ~ 1*FOAVSP_T4           # Fixed regression of FOAVSP_T5 on FOAVSP_T4
#dFOAVSP4 =~ 1*FOAVSP_T5           # Fixed regression of dFOAVSP4 on FOAVSP_T5
#FOAVSP_T5 ~ 0*1                # This line constrains the intercept of FOAVSP_T5 to 0
#FOAVSP_T5 ~~ 0*FOAVSP_T5          # This fixes the variance of the FOAVSP_T5 to 0  




# This is changed
dFOAVSP1 ~ 0*1            # This fixes the intercept of the change score to 0 
dFOAVSP2 ~ 0*1            # This fixes the intercept of the change score to 0 
#dFOAVSP3 ~ 0*1            # This fixes the intercept of the change score to 0 
#dFOAVSP4 ~ 0*1            # This fixes the intercept of the change score to 0 

dFOAVSP1 ~~  dFOAVSP1       # This estimates the residual variance of the change scores 
dFOAVSP2 ~~  equal("dFOAVSP1~~dFOAVSP1")*dFOAVSP2       # This estimates the residual variance of the change scores 
#dFOAVSP3 ~~  equal("dFOAVSP1~~dFOAVSP1")*dFOAVSP3       # This estimates the residual variance of the change scores 
#dFOAVSP4 ~~  equal("dFOAVSP1~~dFOAVSP1")*dFOAVSP4       # This estimates the residual variance of the change scores 

FOAVSP_T1 ~ 0*1              # This fixes the intercept of FOAVSP_T1 to 0. 
FOAVSP_T1 ~~ 0*FOAVSP_T1        # This fixes the residual variance of the FOAVSP_T1 to 0. 



# This is new
iFOAVSP=~1*FOAVSP_T1                   # This defines the FOAVSP intercept measurement model
sFOAVSP=~1*dFOAVSP1+1*dFOAVSP2            # This defines the FOAVSP slope measurement model
iFOAVSP~1                           # This specifies the FOAVSP intercept intercept (mean)
iFOAVSP~~iFOAVSP                       # This specifies the FOAVSP intercept variance
sFOAVSP~1                           # This specifies the FOAVSP slope intercept
sFOAVSP~~sFOAVSP                       # This specifies the FOAVSP slope variance


# Error variances for indicators
VISSPNRN_T1~~VISSPNRN_T1          # This specifies the indicators residual variances 
LOCSPNRN_T1~~LOCSPNRN_T1          # This specifies the indicators residual variances 
VISSPAN_T1 ~~VISSPAN_T1           # This specifies the indicators residual variances 
LOCSPAN_T1~~LOCSPAN_T1          # This specifies the indicators residual variances 
VISSPANB_T1~~VISSPANB_T1          # This specifies the indicators residual variances 
CROSMODB_T1~~CROSMODB_T1          # This specifies the indicators residual variances 
DIGSPNRN_T1~~DIGSPNRN_T1          # This specifies the indicators residual variances 
PHONOLB_T1~~PHONOLB_T1          # This specifies the indicators residual variances 

# Error variances for indicators
VISSPNRN_T2~~equal("VISSPNRN_T1~~VISSPNRN_T1")*VISSPNRN_T2          # This specifies the indicators residual variances 
LOCSPNRN_T2~~equal("LOCSPNRN_T1~~LOCSPNRN_T1")*LOCSPNRN_T2          # This specifies the indicators residual variances 
VISSPAN_T2~~equal("VISSPAN_T1 ~~VISSPAN_T1")*VISSPAN_T2           # This specifies the indicators residual variances 
LOCSPAN_T2~~equal("LOCSPAN_T1~~LOCSPAN_T1")*LOCSPAN_T2          # This specifies the indicators residual variances 
VISSPANB_T2~~equal("VISSPANB_T1~~VISSPANB_T1")*VISSPANB_T2          # This specifies the indicators residual variances 
CROSMODB_T2~~equal("CROSMODB_T1~~CROSMODB_T1")*CROSMODB_T2          # This specifies the indicators residual variances 
DIGSPNRN_T2~~equal("DIGSPNRN_T1~~DIGSPNRN_T1")*DIGSPNRN_T2          # This specifies the indicators residual variances 
PHONOLB_T2~~equal("PHONOLB_T1~~PHONOLB_T1")*PHONOLB_T2          # This specifies the indicators residual variances 

# Error variances for indicators
VISSPNRN_T3~~equal("VISSPNRN_T1~~VISSPNRN_T1")*VISSPNRN_T3          # This specifies the indicators residual variances 
LOCSPNRN_T3~~equal("LOCSPNRN_T1~~LOCSPNRN_T1")*LOCSPNRN_T3          # This specifies the indicators residual variances 
VISSPAN_T3~~equal("VISSPAN_T1 ~~VISSPAN_T1")*VISSPAN_T3           # This specifies the indicators residual variances 
LOCSPAN_T3~~equal("LOCSPAN_T1~~LOCSPAN_T1")*LOCSPAN_T3          # This specifies the indicators residual variances 
VISSPANB_T3~~equal("VISSPANB_T1~~VISSPANB_T1")*VISSPANB_T3          # This specifies the indicators residual variances 
CROSMODB_T3~~equal("CROSMODB_T1~~CROSMODB_T1")*CROSMODB_T3          # This specifies the indicators residual variances 
DIGSPNRN_T3~~equal("DIGSPNRN_T1~~DIGSPNRN_T1")*DIGSPNRN_T3          # This specifies the indicators residual variances 
PHONOLB_T3~~equal("PHONOLB_T1~~PHONOLB_T1")*PHONOLB_T3          # This specifies the indicators residual variances 

# Error variances for indicators
#VISSPNRN_T4~~equal("VISSPNRN_T1~~VISSPNRN_T1")*VISSPNRN_T4          # This specifies the indicators residual variances 
#LOCSPNRN_T4~~equal("LOCSPNRN_T1~~LOCSPNRN_T1")*LOCSPNRN_T4          # This specifies the indicators residual variances 
#VISSPAN_T4~~equal("VISSPAN_T1 ~~VISSPAN_T1")*VISSPAN_T4           # This specifies the indicators residual variances 
#LOCSPAN_T4~~equal("LOCSPAN_T1~~LOCSPAN_T1")*LOCSPAN_T4          # This specifies the indicators residual variances 
#VISSPANB_T4~~equal("VISSPANB_T1~~VISSPANB_T1")*VISSPANB_T4          # This specifies the indicators residual variances 
#CROSMODB_T4~~equal("CROSMODB_T1~~CROSMODB_T1")*CROSMODB_T4          # This specifies the indicators residual variances 
#DIGSPNRN_T4~~equal("DIGSPNRN_T1~~DIGSPNRN_T1")*DIGSPNRN_T4          # This specifies the indicators residual variances 
#PHONOLB_T4~~equal("PHONOLB_T1~~PHONOLB_T1")*PHONOLB_T4          # This specifies the indicators residual variances 

# Error variances for indicators
#VISSPNRN_T5~~equal("VISSPNRN_T1~~VISSPNRN_T1")*VISSPNRN_T5          # This specifies the indicators residual variances 
#LOCSPNRN_T5~~equal("LOCSPNRN_T1~~LOCSPNRN_T1")*LOCSPNRN_T5          # This specifies the indicators residual variances 
#VISSPAN_T5~~equal("VISSPAN_T1 ~~VISSPAN_T1")*VISSPAN_T5           # This specifies the indicators residual variances 
#LOCSPAN_T5~~equal("LOCSPAN_T1~~LOCSPAN_T1")*LOCSPAN_T5          # This specifies the indicators residual variances 
#VISSPANB_T5~~equal("VISSPANB_T1~~VISSPANB_T1")*VISSPANB_T5          # This specifies the indicators residual variances 
#CROSMODB_T5~~equal("CROSMODB_T1~~CROSMODB_T1")*CROSMODB_T5          # This specifies the indicators residual variances 
#DIGSPNRN_T5~~equal("DIGSPNRN_T1~~DIGSPNRN_T1")*DIGSPNRN_T5          # This specifies the indicators residual variances 


# Measurement model intercepts
VISSPNRN_T1~0*1			# This fixes the intercept to 0
LOCSPNRN_T1~1
VISSPAN_T1~1
LOCSPAN_T1~1
VISSPANB_T1~1
CROSMODB_T1~1
DIGSPNRN_T1~1
PHONOLB_T1~1   

VISSPNRN_T2~0*1			# This fixes the intercept to 0
LOCSPNRN_T2~equal("LOCSPNRN_T1~1")*1
VISSPAN_T2~equal("VISSPAN_T1~1")*1
LOCSPAN_T2~equal("LOCSPAN_T1~1")*1
VISSPANB_T2~equal("VISSPANB_T1~1")*1
CROSMODB_T2~equal("CROSMODB_T1~1")*1
DIGSPNRN_T2~equal("DIGSPNRN_T1~1")*1
PHONOLB_T2~equal("PHONOLB_T1~1 ")*1   

VISSPNRN_T3~0*1			# This fixes the intercept to 0
LOCSPNRN_T3~equal("LOCSPNRN_T1~1")*1
VISSPAN_T3~equal("VISSPAN_T1~1")*1
LOCSPAN_T3~equal("LOCSPAN_T1~1")*1
VISSPANB_T3~equal("VISSPANB_T1~1")*1
CROSMODB_T3~equal("CROSMODB_T1~1")*1
DIGSPNRN_T3~equal("DIGSPNRN_T1~1")*1
PHONOLB_T3~equal("PHONOLB_T1~1 ")*1   

#VISSPNRN_T4~0*1			# This fixes the intercept to 0
#LOCSPNRN_T4~equal("LOCSPNRN_T1~1")*1
#VISSPAN_T4~equal("VISSPAN_T1~1")*1
#LOCSPAN_T4~equal("LOCSPAN_T1~1")*1
#VISSPANB_T4~equal("VISSPANB_T1~1")*1
#CROSMODB_T4~equal("CROSMODB_T1~1")*1
#DIGSPNRN_T4~equal("DIGSPNRN_T1~1")*1
#PHONOLB_T4~equal("PHONOLB_T1~1 ")*1   

#VISSPNRN_T5~0*1			# This fixes the intercept to 0
#LOCSPNRN_T5~equal("LOCSPNRN_T1~1")*1
#VISSPAN_T5~equal("VISSPAN_T1~1")*1
#LOCSPAN_T5~equal("LOCSPAN_T1~1")*1
#VISSPANB_T5~equal("VISSPANB_T1~1")*1
#CROSMODB_T5~equal("CROSMODB_T1~1")*1
#DIGSPNRN_T5~equal("DIGSPNRN_T1~1")*1
#PHONOLB_T5~equal("PHONOLB_T1~1 ")*1   


'

# Fit the model-----
fitted.model <-try( 
  lavaan(MILCS, data=simdatMILCS, estimator='mlr',fixed.x=FALSE,missing='fiml'),
  silent=TRUE
)



# summary(fitted.model, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

# inspect(fitted.model,"cov.lv")





