# Power Analysis for Latent Change Score Model for Central Executive ---------------------

# n.reps should be previously defined
# sample.size should be previously defined




# Initialize vector to store p values
p.test.of.mean.of.slope <- rep(NA, n.reps)

# Define alpha level for the test
alpha = .05

# Choose to print or not print warnings at each replication by setting warn=1 or warn=-1
options(warn=-1) 


which.rep=1
total.tried.reps=0
#for(which.rep in 1:n.reps){
  
repeat{
  if(which.rep > n.reps) break
  
  total.tried.reps=total.tried.reps+1
  
  # print(paste0("Conducting rep ", which.rep, ", Total reps tried = ", total.tried.reps))

    # Source the file to generate the data and fit the model
  
    # 5/8/2023
    # Now referencing the folder with the R code as 'code.folder'
    # And code as "R.code.file
  
    # source.code.file <- "Univariate_Latent_Variable_3TimePoints_LatentGrowth_CE.R"
    R.code.file <- "Univariate_Latent_Variable_3TimePoints_LatentGrowth_CE.R"
    
    #source(paste0(R.folder, source.code.file))
    source(paste0(code.folder, R.code.file))
    
    # If there's an error 'try' returns and object with class 'try-error'. 
    # So check that and see if we should ditch this rep
    
    # Try replication again if an error
    if ('try-error' %in% class(fitted.model)) next
    
    # Try replication again if not converged
    if (lavInspect(fitted.model, what="converged")==FALSE) next
  
    # Try replication again if not admissable solution
    if ( 
      #!is.null(tryCatch(lavInspect(fitted.model, what="post.check"),error=function(e) e, warning=function(w) w))
      lavInspect(fitted.model, what="post.check")==FALSE
    ) next
    

    
  # save p-value from the test of the mean of the slope
  p.test.of.mean.of.slope[which.rep] <- 
      as.numeric(parameterEstimates(fitted.model) %>%
        select(lhs, op, pvalue) %>%           # select for these columns
        filter(lhs=="sCENEXEC", op=="~1") %>% # select for this row
        select(pvalue)                        # now select this column
      )  

 
  which.rep=which.rep+1 # increment the replication       
  
  
} # closes repeat loop over reps

  


# Calculate power of individual test
power.test.of.mean.of.slope <- mean(p.test.of.mean.of.slope < alpha, na.rm=TRUE)
