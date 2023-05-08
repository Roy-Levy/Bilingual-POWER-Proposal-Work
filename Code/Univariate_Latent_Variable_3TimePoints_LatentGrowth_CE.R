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
CENEXEC_T1 =~ .46*NBACKAUD_T1 + .81*NBACKVIS_T1 + .47*NUMUPDAT_T1
CENEXEC_T2 =~ .46*NBACKAUD_T2 + .81*NBACKVIS_T2 + .47*NUMUPDAT_T2
CENEXEC_T3 =~ .46*NBACKAUD_T3 + .81*NBACKVIS_T3 + .47*NUMUPDAT_T3
#CENEXEC_T4 =~ .46*NBACKAUD_T4 + .81*NBACKVIS_T4 + .47*NUMUPDAT_T4
#CENEXEC_T5 =~ .46*NBACKAUD_T5 + .81*NBACKVIS_T5 + .47*NUMUPDAT_T5


# Error variances for indicators
NBACKAUD_T1~~0.7884*NBACKAUD_T1          # This specifies the indicators residual variances 
NBACKVIS_T1~~0.3439*NBACKVIS_T1          # This specifies the indicators residual variances 
NUMUPDAT_T1~~0.7791*NUMUPDAT_T1          # This specifies the indicators residual variances 

# Error variances for indicators
NBACKAUD_T2~~0.7884*NBACKAUD_T2          # This specifies the indicators residual variances 
NBACKVIS_T2~~0.3439*NBACKVIS_T2          # This specifies the indicators residual variances 
NUMUPDAT_T2~~0.7791*NUMUPDAT_T2          # This specifies the indicators residual variances 

# Error variances for indicators
NBACKAUD_T3~~0.7884*NBACKAUD_T3          # This specifies the indicators residual variances 
NBACKVIS_T3~~0.3439*NBACKVIS_T3          # This specifies the indicators residual variances 
NUMUPDAT_T3~~0.7791*NUMUPDAT_T3          # This specifies the indicators residual variances 

# Error variances for indicators
#NBACKAUD_T4~~0.7884*NBACKAUD_T4          # This specifies the indicators residual variances 
#NBACKVIS_T4~~0.3439*NBACKVIS_T4          # This specifies the indicators residual variances 
#NUMUPDAT_T4~~0.7791*NUMUPDAT_T4          # This specifies the indicators residual variances 

# Error variances for indicators
#NBACKAUD_T5~~0.7884*NBACKAUD_T5          # This specifies the indicators residual variances 
#NBACKVIS_T5~~0.3439*NBACKVIS_T5          # This specifies the indicators residual variances 
#NUMUPDAT_T5~~0.7791*NUMUPDAT_T5          # This specifies the indicators residual variances 


# Measurement model intercepts
NBACKAUD_T1~0*1                  # This fixes the intercept to 0
NBACKVIS_T1~0*1
NUMUPDAT_T1~0*1

NBACKAUD_T2~0*1                  # This fixes the intercept to 0
NBACKVIS_T2~0*1
NUMUPDAT_T2~0*1


NBACKAUD_T3~0*1                  # This fixes the intercept to 0
NBACKVIS_T3~0*1
NUMUPDAT_T3~0*1


#NBACKAUD_T4~0*1                  # This fixes the intercept to 0
#NBACKVIS_T4~0*1
#NUMUPDAT_T4~0*1


#NBACKAUD_T5~0*1                  # This fixes the intercept to 0
#NBACKVIS_T5~0*1
#NUMUPDAT_T5~0*1


#####     The following lines specify the core assumptions of the LCS 
#####     and should not generally be modified

iCENEXEC=~1*CENEXEC_T1                   # This defines the CENEXEC intercept measurement model
sCENEXEC=~1*dCENEXEC1+1*dCENEXEC2            # This defines the CENEXEC slope measurement model

CENEXEC_T2 ~ 1*CENEXEC_T1           # Fixed regression of CENEXEC_T2 on CENEXEC_T1
dCENEXEC1 =~ 1*CENEXEC_T2           # Fixed regression of CENEXEC_T2 on dCENEXEC1
CENEXEC_T2 ~ 0*1                # This line constrains the intercept of CENEXEC_T2 to 0
CENEXEC_T2 ~~ 0*CENEXEC_T2          # This fixes the variance of the CENEXEC_T2 to 0  

CENEXEC_T3 ~ 1*CENEXEC_T2           # Fixed regression of CENEXEC_T3 on CENEXEC_T2
dCENEXEC2 =~ 1*CENEXEC_T3           # Fixed regression of CENEXEC_T3 on dCENEXEC2
CENEXEC_T3 ~ 0*1                # This line constrains the intercept of CENEXEC_T3 to 0
CENEXEC_T3 ~~ 0*CENEXEC_T3          # This fixes the variance of the CENEXEC_T3 to 0  

#CENEXEC_T4 ~ 1*CENEXEC_T3           # Fixed regression of CENEXEC_T4 on CENEXEC_T3
#dCENEXEC3 =~ 1*CENEXEC_T4           # Fixed regression of CENEXEC_T4 on dCENEXEC2
#CENEXEC_T4 ~ 0*1                # This line constrains the intercept of CENEXEC_T4 to 0
#CENEXEC_T4 ~~ 0*CENEXEC_T4          # This fixes the variance of the CENEXEC_T4 to 0  

#CENEXEC_T5 ~ 1*CENEXEC_T4           # Fixed regression of CENEXEC_T5 on CENEXEC_4
#dCENEXEC4 =~ 1*CENEXEC_T5           # Fixed regression of CENEXEC_T5 on dCENEXEC2
#CENEXEC_T5 ~ 0*1                # This line constrains the intercept of CENEXEC_T5 to 0
#CENEXEC_T5 ~~ 0*CENEXEC_T5          # This fixes the variance of the CENEXEC_T5 to 0  



###### The following five parameters will be estimated in the model. 
###### Values can be modified manually to examine the effect on the model


# This is changed
CENEXEC_T1 ~ 0*1              # This fixes the intercept of CENEXEC_T1 to 0. 
CENEXEC_T1 ~~ 0*CENEXEC_T1        # This fixes the residual variance of the CENEXEC_T1 to 0. 

dCENEXEC1 ~ 0*1            # This fixes the intercept of the change score to 0 
dCENEXEC1 ~~ .1*dCENEXEC1        # This fixes the residual variance of the change scores to .1. 

dCENEXEC2 ~ 0*1            # This fixes the intercept of the change score to 0 
dCENEXEC2 ~~ .1*dCENEXEC2        # This fixes the residual variance of the change scores to .1. 

#dCENEXEC3 ~ 0*1            # This fixes the intercept of the change score to 0 
#dCENEXEC3 ~~ .1*dCENEXEC3        # This fixes the residual variance of the change scores to .1. 

#dCENEXEC4 ~ 0*1            # This fixes the intercept of the change score to 0 
#dCENEXEC4 ~~ .1*dCENEXEC4        # This fixes the residual variance of the change scores to .1. 

# This is new
iCENEXEC~0*1                           # This specifies the CENEXEC intercept intercept (mean)
iCENEXEC~~.5*iCENEXEC                       # This specifies the CENEXEC intercept variance
sCENEXEC~.18*1                           # This specifies the CENEXEC slope intercept
sCENEXEC~~.1*sCENEXEC                       # This specifies the CENEXEC slope variance


'

# Generate the data ----
# set.seed(1234)
simdatMILCS<-simulateData(MILCS_simulate,sample.nobs = sample.size,meanstructure = T) #Simulate data

# colMeans(simdatMILCS) #sanity check the means
# write.csv(simdatMILCS,'2_simdatMILCS.csv')

# Define the fitted model ------
MILCS<-'

CENEXEC_T1 =~ .46*NBACKAUD_T1 + NBACKVIS_T1 + NUMUPDAT_T1
CENEXEC_T2 =~ .46*NBACKAUD_T2 + equal("CENEXEC_T1=~NBACKVIS_T1")*NBACKVIS_T2 + equal("CENEXEC_T1=~NUMUPDAT_T1")*NUMUPDAT_T2
CENEXEC_T3 =~ .46*NBACKAUD_T3 + equal("CENEXEC_T1=~NBACKVIS_T1")*NBACKVIS_T3 + equal("CENEXEC_T1=~NUMUPDAT_T1")*NUMUPDAT_T3
#CENEXEC_T4 =~ .46*NBACKAUD_T4 + equal("CENEXEC_T1=~NBACKVIS_T1")*NBACKVIS_T4 + equal("CENEXEC_T1=~NUMUPDAT_T1")*NUMUPDAT_T4
#CENEXEC_T5 =~ .46*NBACKAUD_T5 + equal("CENEXEC_T1=~NBACKVIS_T1")*NBACKVIS_T5 + equal("CENEXEC_T1=~NUMUPDAT_T1")*NUMUPDAT_T5

CENEXEC_T2 ~ 1*CENEXEC_T1     # Fixed regression of CENEXEC_T2 on CENEXEC_T1
dCENEXEC1 =~ 1*CENEXEC_T2     # Fixed regression of dCENEXEC1 on CENEXEC_T2
CENEXEC_T2 ~ 0*1          # This line constrains the intercept of CENEXEC_T2 to 0
CENEXEC_T2 ~~ 0*CENEXEC_T2    # This fixes the variance of the CENEXEC_T2 to 0 

CENEXEC_T3 ~ 1*CENEXEC_T2           # Fixed regression of CENEXEC_T3 on CENEXEC_T2
dCENEXEC2 =~ 1*CENEXEC_T3           # Fixed regression of dCENEXEC2 on CENEXEC_T3
CENEXEC_T3 ~ 0*1                # This line constrains the intercept of CENEXEC_T3 to 0
CENEXEC_T3 ~~ 0*CENEXEC_T3          # This fixes the variance of the CENEXEC_T3 to 0  

#CENEXEC_T4 ~ 1*CENEXEC_T3           # Fixed regression of CENEXEC_T4 on CENEXEC_T3
#dCENEXEC3 =~ 1*CENEXEC_T4           # Fixed regression of dCENEXEC3 on CENEXEC_T4
#CENEXEC_T4 ~ 0*1                # This line constrains the intercept of CENEXEC_T4 to 0
#CENEXEC_T4 ~~ 0*CENEXEC_T4          # This fixes the variance of the CENEXEC_T4 to 0  

#CENEXEC_T5 ~ 1*CENEXEC_T4           # Fixed regression of CENEXEC_T5 on CENEXEC_T4
#dCENEXEC4 =~ 1*CENEXEC_T5           # Fixed regression of dCENEXEC4 on CENEXEC_T5
#CENEXEC_T5 ~ 0*1                # This line constrains the intercept of CENEXEC_T5 to 0
#CENEXEC_T5 ~~ 0*CENEXEC_T5          # This fixes the variance of the CENEXEC_T5 to 0  




# This is changed
dCENEXEC1 ~ 0*1            # This fixes the intercept of the change score to 0 
dCENEXEC2 ~ 0*1            # This fixes the intercept of the change score to 0 
#dCENEXEC3 ~ 0*1            # This fixes the intercept of the change score to 0 
#dCENEXEC4 ~ 0*1            # This fixes the intercept of the change score to 0 

dCENEXEC1 ~~  dCENEXEC1       # This estimates the residual variance of the change scores 
dCENEXEC2 ~~  equal("dCENEXEC1~~dCENEXEC1")*dCENEXEC2       # This estimates the residual variance of the change scores 
#dCENEXEC3 ~~  equal("dCENEXEC1~~dCENEXEC1")*dCENEXEC3       # This estimates the residual variance of the change scores 
#dCENEXEC4 ~~  equal("dCENEXEC1~~dCENEXEC1")*dCENEXEC4       # This estimates the residual variance of the change scores 

CENEXEC_T1 ~ 0*1              # This fixes the intercept of CENEXEC_T1 to 0. 
CENEXEC_T1 ~~ 0*CENEXEC_T1        # This fixes the residual variance of the CENEXEC_T1 to 0. 



# This is new
iCENEXEC=~1*CENEXEC_T1                   # This defines the CENEXEC intercept measurement model
sCENEXEC=~1*dCENEXEC1+1*dCENEXEC2            # This defines the CENEXEC slope measurement model
iCENEXEC~1                           # This specifies the CENEXEC intercept intercept (mean)
iCENEXEC~~iCENEXEC                       # This specifies the CENEXEC intercept variance
sCENEXEC~1                           # This specifies the CENEXEC slope intercept
sCENEXEC~~sCENEXEC                       # This specifies the CENEXEC slope variance


# Error variances for indicators
NBACKAUD_T1~~NBACKAUD_T1          # This specifies the indicators residual variances 
NBACKVIS_T1~~NBACKVIS_T1          # This specifies the indicators residual variances 
NUMUPDAT_T1~~NUMUPDAT_T1          # This specifies the indicators residual variances 

# Error variances for indicators
NBACKAUD_T2~~equal("NBACKAUD_T1~~NBACKAUD_T1")*NBACKAUD_T2          # This specifies the indicators residual variances 
NBACKVIS_T2~~equal("NBACKVIS_T1~~NBACKVIS_T1")*NBACKVIS_T2          # This specifies the indicators residual variances 
NUMUPDAT_T2~~equal("NUMUPDAT_T1~~NUMUPDAT_T1")*NUMUPDAT_T2          # This specifies the indicators residual variances 

# Error variances for indicators
NBACKAUD_T3~~equal("NBACKAUD_T1~~NBACKAUD_T1")*NBACKAUD_T3          # This specifies the indicators residual variances 
NBACKVIS_T3~~equal("NBACKVIS_T1~~NBACKVIS_T1")*NBACKVIS_T3          # This specifies the indicators residual variances 
NUMUPDAT_T3~~equal("NUMUPDAT_T1~~NUMUPDAT_T1")*NUMUPDAT_T3          # This specifies the indicators residual variances 

# Error variances for indicators
#NBACKAUD_T4~~equal("NBACKAUD_T1~~NBACKAUD_T1")*NBACKAUD_T4          # This specifies the indicators residual variances 
#NBACKVIS_T4~~equal("NBACKVIS_T1~~NBACKVIS_T1")*NBACKVIS_T4          # This specifies the indicators residual variances 
#NUMUPDAT_T4~~equal("NUMUPDAT_T1~~NUMUPDAT_T1")*NUMUPDAT_T4          # This specifies the indicators residual variances 

# Error variances for indicators
#NBACKAUD_T5~~equal("NBACKAUD_T1~~NBACKAUD_T1")*NBACKAUD_T5          # This specifies the indicators residual variances 
#NBACKVIS_T5~~equal("NBACKVIS_T1~~NBACKVIS_T1")*NBACKVIS_T5          # This specifies the indicators residual variances 
#NUMUPDAT_T5~~equal("NUMUPDAT_T1~~NUMUPDAT_T1")*NUMUPDAT_T5          # This specifies the indicators residual variances 


# Measurement model intercepts
NBACKAUD_T1~0*1                  # This fixes the intercept to 0
NBACKVIS_T1~1
NUMUPDAT_T1~1

NBACKAUD_T2~0*1                  # This fixes the intercept to 0
NBACKVIS_T2~equal("NBACKVIS_T1~1")*1
NUMUPDAT_T2~equal("NUMUPDAT_T1~1")*1

NBACKAUD_T3~0*1                  # This fixes the intercept to 0
NBACKVIS_T3~equal("NBACKVIS_T1~1")*1
NUMUPDAT_T3~equal("NUMUPDAT_T1~1")*1

#NBACKAUD_T4~0*1                  # This fixes the intercept to 0
#NBACKVIS_T4~equal("NBACKVIS_T1~1")*1
#NUMUPDAT_T4~equal("NUMUPDAT_T1~1")*1

#NBACKAUD_T5~0*1                  # This fixes the intercept to 0
#NBACKVIS_T5~equal("NBACKVIS_T1~1")*1
#NUMUPDAT_T5~equal("NUMUPDAT_T1~1")*1



'

# Fit the model-----
fitted.model <-try( 
  lavaan(MILCS, data=simdatMILCS, estimator='mlr',fixed.x=FALSE,missing='fiml'),
  silent=TRUE
)



  


# inspect(fitMILCS, "cov.lv")

# summary(fitted.model, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


if(1==0){
#Visualize the raw data if so inclined
theme_set(theme_grey(base_size = 18)) #increase text size
id=factor(1:samplesize)
plotdattemp=data.frame(c(simdatMILCS$T1X1,simdatMILCS$T1X2,simdatMILCS$T1X3),
                       c(simdatMILCS$T2X1,simdatMILCS$T2X2,simdatMILCS$T2X3),as.factor(c(id,id,id)),
                       c(rep('X1',times=samplesize),rep('X2',times=samplesize),rep('X3',times=samplesize)))
colnames(plotdattemp)<-c('CENEXEC_T1', 'CENEXEC_T2','id','Indicator')
plotdat<-melt(plotdattemp,by='id')
ggplot(plotdat,aes(variable,value,group=id,col=Indicator))+geom_point(size=3,alpha=.7)+geom_line(alpha=.7)+ggtitle('Multiple indicator Latent Change Score model')+ylab('Cognitive scores')+xlab('Time points')+facet_grid(~Indicator)
}




