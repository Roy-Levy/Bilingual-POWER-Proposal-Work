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
PHSTREHLP_T1 =~ .06*CROSMODB_T1 + .22*DIGSPNRN_T1 + .40*PHONOLB_T1 + .49*DIGITSPN_T1 + .57*NONWRDRP_T1 
PHSTREHLP_T2 =~ .06*CROSMODB_T2 + .22*DIGSPNRN_T2 + .40*PHONOLB_T2 + .49*DIGITSPN_T2 + .57*NONWRDRP_T2 
PHSTREHLP_T3 =~ .06*CROSMODB_T3 + .22*DIGSPNRN_T3 + .40*PHONOLB_T3 + .49*DIGITSPN_T3 + .57*NONWRDRP_T3 
#PHSTREHLP_T4 =~ .06*CROSMODB_T4 + .22*DIGSPNRN_T4 + .40*PHONOLB_T4 + .49*DIGITSPN_T4 + .57*NONWRDRP_T4 
#PHSTREHLP_T5 =~ .06*CROSMODB_T5 + .22*DIGSPNRN_T5 + .40*PHONOLB_T5 + .49*DIGITSPN_T5 + .57*NONWRDRP_T5 


# Error variances for indicators
CROSMODB_T1~~0.7755*CROSMODB_T1          # This specifies the indicators residual variances 
DIGSPNRN_T1~~0.8555*DIGSPNRN_T1          # This specifies the indicators residual variances 
PHONOLB_T1~~0.8300*PHONOLB_T1          # This specifies the indicators residual variances
DIGITSPN_T1~~0.7599*DIGITSPN_T1          # This specifies the indicators residual variances 
NONWRDRP_T1~~0.6751*NONWRDRP_T1          # This specifies the indicators residual variances 

# Error variances for indicators
CROSMODB_T2~~0.7755*CROSMODB_T2          # This specifies the indicators residual variances 
DIGSPNRN_T2~~0.8555*DIGSPNRN_T2          # This specifies the indicators residual variances 
PHONOLB_T2~~0.8300*PHONOLB_T2          # This specifies the indicators residual variances 
DIGITSPN_T2~~0.7599*DIGITSPN_T2          # This specifies the indicators residual variances 
NONWRDRP_T2~~0.6751*NONWRDRP_T2          # This specifies the indicators residual variances 

# Error variances for indicators
CROSMODB_T3~~0.7755*CROSMODB_T3          # This specifies the indicators residual variances 
DIGSPNRN_T3~~0.8555*DIGSPNRN_T3          # This specifies the indicators residual variances 
PHONOLB_T3~~0.8300*PHONOLB_T3          # This specifies the indicators residual variances 
DIGITSPN_T3~~0.7599*DIGITSPN_T3          # This specifies the indicators residual variances 
NONWRDRP_T3~~0.6751*NONWRDRP_T3          # This specifies the indicators residual variances 

# Error variances for indicators
#CROSMODB_T4~~0.7755*CROSMODB_T4          # This specifies the indicators residual variances 
#DIGSPNRN_T4~~0.8555*DIGSPNRN_T4          # This specifies the indicators residual variances 
#PHONOLB_T4~~0.8300*PHONOLB_T4          # This specifies the indicators residual variances 
#DIGITSPN_T4~~0.7599*DIGITSPN_T4          # This specifies the indicators residual variances 
#NONWRDRP_T4~~0.6751*NONWRDRP_T4          # This specifies the indicators residual variances 

# Error variances for indicators
#CROSMODB_T5~~0.7755*CROSMODB_T5          # This specifies the indicators residual variances 
#DIGSPNRN_T5~~0.8555*DIGSPNRN_T5          # This specifies the indicators residual variances 
#PHONOLB_T5~~0.8300*PHONOLB_T5          # This specifies the indicators residual variances 
#DIGITSPN_T5~~0.7599*DIGITSPN_T5          # This specifies the indicators residual variances 
#NONWRDRP_T5~~0.6751*NONWRDRP_T5          # This specifies the indicators residual variances 

# Measurement model intercepts
CROSMODB_T1~0*1
DIGSPNRN_T1~0*1
PHONOLB_T1~0*1   
DIGITSPN_T1~0*1
NONWRDRP_T1~0*1 

CROSMODB_T2~0*1
DIGSPNRN_T2~0*1
PHONOLB_T2~0*1   
DIGITSPN_T2~0*1
NONWRDRP_T2~0*1  

CROSMODB_T3~0*1
DIGSPNRN_T3~0*1
PHONOLB_T3~0*1   
DIGITSPN_T3~0*1
NONWRDRP_T3~0*1   

#CROSMODB_T4~0*1
#DIGSPNRN_T4~0*1
#PHONOLB_T4~0*1   
#DIGITSPN_T4~0*1
#NONWRDRP_T4~0*1 

#CROSMODB_T5~0*1
#DIGSPNRN_T5~0*1
#PHONOLB_T5~0*1   
#DIGITSPN_T5~0*1
#NONWRDRP_T5~0*1  

#####     The following lines specify the core assumptions of the LCS 
#####     and should not generally be modified

iPHSTREHLP=~1*PHSTREHLP_T1                   # This defines the PHSTREHLP intercept measurement model
sPHSTREHLP=~1*dPHSTREHLP1+1*dPHSTREHLP2            # This defines the PHSTREHLP slope measurement model

PHSTREHLP_T2 ~ 1*PHSTREHLP_T1           # Fixed regression of PHSTREHLP_T2 on PHSTREHLP_T1
dPHSTREHLP1 =~ 1*PHSTREHLP_T2           # Fixed regression of PHSTREHLP_T2 on dPHSTREHLP1
PHSTREHLP_T2 ~ 0*1                # This line constrains the intercept of PHSTREHLP_T2 to 0
PHSTREHLP_T2 ~~ 0*PHSTREHLP_T2          # This fixes the variance of the PHSTREHLP_T2 to 0  

PHSTREHLP_T3 ~ 1*PHSTREHLP_T2           # Fixed regression of PHSTREHLP_T3 on PHSTREHLP_T2
dPHSTREHLP2 =~ 1*PHSTREHLP_T3           # Fixed regression of PHSTREHLP_T3 on dPHSTREHLP2
PHSTREHLP_T3 ~ 0*1                # This line constrains the intercept of PHSTREHLP_T3 to 0
PHSTREHLP_T3 ~~ 0*PHSTREHLP_T3          # This fixes the variance of the PHSTREHLP_T3 to 0  

#PHSTREHLP_T4 ~ 1*PHSTREHLP_T3           # Fixed regression of PHSTREHLP_T4 on PHSTREHLP_T3
#dPHSTREHLP3 =~ 1*PHSTREHLP_T4           # Fixed regression of PHSTREHLP_T4 on dPHSTREHLP2
#PHSTREHLP_T4 ~ 0*1                # This line constrains the intercept of PHSTREHLP_T4 to 0
#PHSTREHLP_T4 ~~ 0*PHSTREHLP_T4          # This fixes the variance of the PHSTREHLP_T4 to 0  

#PHSTREHLP_T5 ~ 1*PHSTREHLP_T4           # Fixed regression of PHSTREHLP_T5 on PHSTREHLP_4
#dPHSTREHLP4 =~ 1*PHSTREHLP_T5           # Fixed regression of PHSTREHLP_T5 on dPHSTREHLP2
#PHSTREHLP_T5 ~ 0*1                # This line constrains the intercept of PHSTREHLP_T5 to 0
#PHSTREHLP_T5 ~~ 0*PHSTREHLP_T5          # This fixes the variance of the PHSTREHLP_T5 to 0 




###### The following five parameters will be estimated in the model. 
###### Values can be modified manually to examine the effect on the model


# This is changed
PHSTREHLP_T1 ~ 0*1              # This fixes the intercept of PHSTREHLP_T1 to 0. 
PHSTREHLP_T1 ~~ 0*PHSTREHLP_T1        # This fixes the residual variance of the PHSTREHLP_T1 to 0. 

dPHSTREHLP1 ~ 0*1            # This fixes the intercept of the change score to 0 
dPHSTREHLP1 ~~ .1*dPHSTREHLP1        # This fixes the residual variance of the change scores to .1. 

dPHSTREHLP2 ~ 0*1            # This fixes the intercept of the change score to 0 
dPHSTREHLP2 ~~ .1*dPHSTREHLP2        # This fixes the residual variance of the change scores to .1. 

#dPHSTREHLP3 ~ 0*1            # This fixes the intercept of the change score to 0 
#dPHSTREHLP3 ~~ .1*dPHSTREHLP3        # This fixes the residual variance of the change scores to .1. 

#dPHSTREHLP4 ~ 0*1            # This fixes the intercept of the change score to 0 
#dPHSTREHLP4 ~~ .1*dPHSTREHLP4        # This fixes the residual variance of the change scores to .1. 
 
# This is new
iPHSTREHLP~0*1                           # This specifies the PHSTREHLP intercept intercept (mean)
iPHSTREHLP~~.1*iPHSTREHLP                       # This specifies the PHSTREHLP intercept variance
sPHSTREHLP~.42*1                           # This specifies the PHSTREHLP slope intercept
sPHSTREHLP~~.1*sPHSTREHLP                       # This specifies the PHSTREHLP slope variance


'

# Generate the data ----
# set.seed(1234)
simdatMILCS<-simulateData(MILCS_simulate,sample.nobs = sample.size,meanstructure = T) #Simulate data

# colMeans(simdatMILCS) #sanity check the means
# write.csv(simdatMILCS,'2_simdatMILCS.csv')

# Define the fitted model ------
MILCS<-'

PHSTREHLP_T1 =~ CROSMODB_T1 + DIGSPNRN_T1 + PHONOLB_T1 + DIGITSPN_T1 + .57*NONWRDRP_T1 
PHSTREHLP_T2 =~ equal("PHSTREHLP_T1=~CROSMODB_T1")*CROSMODB_T2 + equal("PHSTREHLP_T1=~DIGSPNRN_T1")*DIGSPNRN_T2 + equal("PHSTREHLP_T1=~PHONOLB_T1")*PHONOLB_T2 + equal("PHSTREHLP_T1=~DIGITSPN_T1")*DIGITSPN_T2 + .57*NONWRDRP_T2
PHSTREHLP_T3 =~ equal("PHSTREHLP_T1=~CROSMODB_T1")*CROSMODB_T3 + equal("PHSTREHLP_T1=~DIGSPNRN_T1")*DIGSPNRN_T3 + equal("PHSTREHLP_T1=~PHONOLB_T1")*PHONOLB_T3 + equal("PHSTREHLP_T1=~DIGITSPN_T1")*DIGITSPN_T3 + .57*NONWRDRP_T3
#PHSTREHLP_T4 =~ equal("PHSTREHLP_T1=~CROSMODB_T1")*CROSMODB_T4 + equal("PHSTREHLP_T1=~DIGSPNRN_T1")*DIGSPNRN_T4 + equal("PHSTREHLP_T1=~PHONOLB_T1")*PHONOLB_T4 + equal("PHSTREHLP_T1=~DIGITSPN_T1")*DIGITSPN_T4 + .57*NONWRDRP_T4
#PHSTREHLP_T5 =~ equal("PHSTREHLP_T1=~CROSMODB_T1")*CROSMODB_T5 + equal("PHSTREHLP_T1=~DIGSPNRN_T1")*DIGSPNRN_T5 + equal("PHSTREHLP_T1=~PHONOLB_T1")*PHONOLB_T5 + equal("PHSTREHLP_T1=~DIGITSPN_T1")*DIGITSPN_T5 + .57*NONWRDRP_T5


PHSTREHLP_T2 ~ 1*PHSTREHLP_T1     # Fixed regression of PHSTREHLP_T2 on PHSTREHLP_T1
dPHSTREHLP1 =~ 1*PHSTREHLP_T2     # Fixed regression of dPHSTREHLP1 on PHSTREHLP_T2
PHSTREHLP_T2 ~ 0*1          # This line constrains the intercept of PHSTREHLP_T2 to 0
PHSTREHLP_T2 ~~ 0*PHSTREHLP_T2    # This fixes the variance of the PHSTREHLP_T2 to 0 

PHSTREHLP_T3 ~ 1*PHSTREHLP_T2           # Fixed regression of PHSTREHLP_T3 on PHSTREHLP_T2
dPHSTREHLP2 =~ 1*PHSTREHLP_T3           # Fixed regression of dPHSTREHLP2 on PHSTREHLP_T3
PHSTREHLP_T3 ~ 0*1                # This line constrains the intercept of PHSTREHLP_T3 to 0
PHSTREHLP_T3 ~~ 0*PHSTREHLP_T3          # This fixes the variance of the PHSTREHLP_T3 to 0  

#PHSTREHLP_T4 ~ 1*PHSTREHLP_T3           # Fixed regression of PHSTREHLP_T4 on PHSTREHLP_T3
#dPHSTREHLP3 =~ 1*PHSTREHLP_T4           # Fixed regression of dPHSTREHLP3 on PHSTREHLP_T4
#PHSTREHLP_T4 ~ 0*1                # This line constrains the intercept of PHSTREHLP_T4 to 0
#PHSTREHLP_T4 ~~ 0*PHSTREHLP_T4          # This fixes the variance of the PHSTREHLP_T4 to 0  

#PHSTREHLP_T5 ~ 1*PHSTREHLP_T4           # Fixed regression of PHSTREHLP_T5 on PHSTREHLP_T4
#dPHSTREHLP4 =~ 1*PHSTREHLP_T5           # Fixed regression of dPHSTREHLP4 on PHSTREHLP_T5
#PHSTREHLP_T5 ~ 0*1                # This line constrains the intercept of PHSTREHLP_T5 to 0
#PHSTREHLP_T5 ~~ 0*PHSTREHLP_T5          # This fixes the variance of the PHSTREHLP_T5 to 0  




# This is changed
dPHSTREHLP1 ~ 0*1            # This fixes the intercept of the change score to 0 
dPHSTREHLP2 ~ 0*1            # This fixes the intercept of the change score to 0 
#dPHSTREHLP3 ~ 0*1            # This fixes the intercept of the change score to 0 
#dPHSTREHLP4 ~ 0*1            # This fixes the intercept of the change score to 0 

dPHSTREHLP1 ~~  dPHSTREHLP1       # This estimates the residual variance of the change scores 
dPHSTREHLP2 ~~  equal("dPHSTREHLP1~~dPHSTREHLP1")*dPHSTREHLP2       # This estimates the residual variance of the change scores 
#dPHSTREHLP3 ~~  equal("dPHSTREHLP1~~dPHSTREHLP1")*dPHSTREHLP3       # This estimates the residual variance of the change scores 
#dPHSTREHLP4 ~~  equal("dPHSTREHLP1~~dPHSTREHLP1")*dPHSTREHLP4       # This estimates the residual variance of the change scores 

PHSTREHLP_T1 ~ 0*1              # This fixes the intercept of PHSTREHLP_T1 to 0. 
PHSTREHLP_T1 ~~ 0*PHSTREHLP_T1        # This fixes the residual variance of the PHSTREHLP_T1 to 0. 



# This is new
iPHSTREHLP=~1*PHSTREHLP_T1                   # This defines the PHSTREHLP intercept measurement model
sPHSTREHLP=~1*dPHSTREHLP1+1*dPHSTREHLP2            # This defines the PHSTREHLP slope measurement model
iPHSTREHLP~1                           # This specifies the PHSTREHLP intercept intercept (mean)
iPHSTREHLP~~iPHSTREHLP                       # This specifies the PHSTREHLP intercept variance
sPHSTREHLP~1                           # This specifies the PHSTREHLP slope intercept
sPHSTREHLP~~sPHSTREHLP 


# Error variances for indicators
CROSMODB_T1~~CROSMODB_T1          # This specifies the indicators residual variances 
DIGSPNRN_T1~~DIGSPNRN_T1          # This specifies the indicators residual variances 
PHONOLB_T1~~PHONOLB_T1          # This specifies the indicators residual variances 
DIGITSPN_T1~~DIGITSPN_T1          # This specifies the indicators residual variances 
NONWRDRP_T1~~NONWRDRP_T1          # This specifies the indicators residual variances 

# Error variances for indicators
CROSMODB_T2~~equal("CROSMODB_T1~~CROSMODB_T1")*CROSMODB_T2          # This specifies the indicators residual variances 
DIGSPNRN_T2~~equal("DIGSPNRN_T1~~DIGSPNRN_T1")*DIGSPNRN_T2          # This specifies the indicators residual variances 
PHONOLB_T2~~equal("PHONOLB_T1~~PHONOLB_T1")*PHONOLB_T2          # This specifies the indicators residual variances 
DIGITSPN_T2~~equal("DIGITSPN_T1~~DIGITSPN_T1")*DIGITSPN_T2          # This specifies the indicators residual variances 
NONWRDRP_T2~~equal("NONWRDRP_T1~~NONWRDRP_T1")*NONWRDRP_T2          # This specifies the indicators residual variances 

# Error variances for indicators
CROSMODB_T3~~equal("CROSMODB_T1~~CROSMODB_T1")*CROSMODB_T3          # This specifies the indicators residual variances 
DIGSPNRN_T3~~equal("DIGSPNRN_T1~~DIGSPNRN_T1")*DIGSPNRN_T3          # This specifies the indicators residual variances 
PHONOLB_T3~~equal("PHONOLB_T1~~PHONOLB_T1")*PHONOLB_T3          # This specifies the indicators residual variances 
DIGITSPN_T3~~equal("DIGITSPN_T1~~DIGITSPN_T1")*DIGITSPN_T3          # This specifies the indicators residual variances 
NONWRDRP_T3~~equal("NONWRDRP_T1~~NONWRDRP_T1")*NONWRDRP_T3          # This specifies the indicators residual variances 

# Error variances for indicators
#CROSMODB_T4~~equal("CROSMODB_T1~~CROSMODB_T1")*CROSMODB_T4          # This specifies the indicators residual variances 
#DIGSPNRN_T4~~equal("DIGSPNRN_T1~~DIGSPNRN_T1")*DIGSPNRN_T4          # This specifies the indicators residual variances 
#PHONOLB_T4~~equal("PHONOLB_T1~~PHONOLB_T1")*PHONOLB_T4          # This specifies the indicators residual variances 
#DIGITSPN_T4~~equal("DIGITSPN_T1~~DIGITSPN_T1")*DIGITSPN_T4          # This specifies the indicators residual variances 
#NONWRDRP_T4~~equal("NONWRDRP_T1~~NONWRDRP_T1")*NONWRDRP_T4          # This specifies the indicators residual variances 

# Error variances for indicators
#CROSMODB_T5~~equal("CROSMODB_T1~~CROSMODB_T1")*CROSMODB_T5          # This specifies the indicators residual variances 
#DIGSPNRN_T5~~equal("DIGSPNRN_T1~~DIGSPNRN_T1")*DIGSPNRN_T5          # This specifies the indicators residual variances 
#PHONOLB_T5~~equal("PHONOLB_T1~~PHONOLB_T1")*PHONOLB_T5          # This specifies the indicators residual variances 
#DIGITSPN_T5~~equal("DIGITSPN_T1~~DIGITSPN_T1")*DIGITSPN_T5          # This specifies the indicators residual variances 
#NONWRDRP_T5~~equal("NONWRDRP_T1~~NONWRDRP_T1")*NONWRDRP_T5          # This specifies the indicators residual variances 


# Measurement model intercepts
CROSMODB_T1~1
DIGSPNRN_T1~1
PHONOLB_T1~1   
DIGITSPN_T1~1
NONWRDRP_T1~0*1  

CROSMODB_T2~equal("CROSMODB_T1~1")*1
DIGSPNRN_T2~equal("DIGSPNRN_T1~1")*1
PHONOLB_T2~equal("PHONOLB_T1~1 ")*1   
DIGITSPN_T2~equal("DIGITSPN_T1~1")*1
NONWRDRP_T2~0*1 		# This fixes the intercept to 0

CROSMODB_T3~equal("CROSMODB_T1~1")*1
DIGSPNRN_T3~equal("DIGSPNRN_T1~1")*1
PHONOLB_T3~equal("PHONOLB_T1~1 ")*1   
DIGITSPN_T3~equal("DIGITSPN_T1~1")*1
NONWRDRP_T3~0*1 		# This fixes the intercept to 0

#CROSMODB_T4~equal("CROSMODB_T1~1")*1
#DIGSPNRN_T4~equal("DIGSPNRN_T1~1")*1
#PHONOLB_T4~equal("PHONOLB_T1~1 ")*1   
#DIGITSPN_T4~equal("DIGITSPN_T1~1")*1
#NONWRDRP_T4~0*1 		# This fixes the intercept to 0

#CROSMODB_T5~equal("CROSMODB_T1~1")*1
#DIGSPNRN_T5~equal("DIGSPNRN_T1~1")*1
#PHONOLB_T5~equal("PHONOLB_T1~1 ")*1   
#DIGITSPN_T5~equal("DIGITSPN_T1~1")*1
#NONWRDRP_T5~0*1 		# This fixes the intercept to 0


'

# Fit the model-----
fitted.model <-try( 
  lavaan(MILCS, data=simdatMILCS, estimator='mlr',fixed.x=FALSE,missing='fiml'),
  silent=TRUE
)



# summary(fitted.model, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

# inspect(fitted.model,"cov.lv")





