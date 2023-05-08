# R code to conduct sample size planning analyses for the latent change score modeling


# simsem references
# https://github.com/simsem/simsem/wiki/Vignette

# Yan also did a simulation using lavaan and his own simulations 
# See 'LGM power mini.r'



rm(list=ls())


# Load the packages ---------------
library(lavaan)
library(tidyverse)

# Define the seed for reproducibility ---------------
set.seed(4392)


# Define the folders ------

#   Drive letter
#   main analysis folder
#   R Code folder and functions subfolder
#   Conditions folder

main.folder <- getwd()

# library(rprojroot)
# file.name.in.root.directory <- "MUPPET CFA IIS (Testing).Rproj"
# main.folder <- find_root(has_file(file.name.in.root.directory))

code.folder <- paste(main.folder, "/Code/", sep="")
functions.folder <- paste(code.folder, "Functions/", sep="")
data.folder <- paste(main.folder, "/Data/", sep="")

latent.change.score.folder <- paste0(main.folder, "/Latent Change Score/")
dir.create(latent.change.score.folder)
#setwd(latent.change.score.folder)


# Define folders with locations ---------------
# main.folder <- "E:/Profiles of Working Memory and Word Learning (Shelley Gray)/"
# analyses.folder <- paste0(main.folder, "Analyses/")
# R.folder <- paste0(analyses.folder, "R/R Code for POWER 3.0 Proposal Work/")
# dir.create(R.folder)
# latent.change.score.folder <- paste0(analyses.folder, "Latent Change Score/")
# dir.create(latent.change.score.folder)
# setwd(latent.change.score.folder)




# Set parameters for Monte Carlo simulations ---------------------
# Define the number of Monte Carlo replications
n.reps = 500


# Define the sample sizes to examine ---------------------
sample.sizes.to.examine = seq(70, 150, 5)

# Set up the table to save the results for the power analyses ---------------------
power.test.of.mean.of.slope.for.CE <- rep(NA, length(sample.sizes.to.examine))
power.test.of.mean.of.slope.for.FOA <- rep(NA, length(sample.sizes.to.examine))
power.test.of.mean.of.slope.for.PHON <- rep(NA, length(sample.sizes.to.examine))


power.results <- as.data.frame(cbind(
  sample.sizes.to.examine, 
  power.test.of.mean.of.slope.for.CE,
  power.test.of.mean.of.slope.for.FOA,
  power.test.of.mean.of.slope.for.PHON
))



# sample sizes from original proposal
# designed for older code when sample size was set, and power was calculated
# not using here where we are looping to calculate power at different sample sizes
# and not splitting sample by multiple subgroups
# keeping here as a legacy
if(1==0){
  sample.size <- 131  # TD K-5 in cohort 1
  sample.size <- 56   # Dys K-5 in cohort 1
}

# sample sizes from revised proposal, Spring 2020
# designed for older code when sample size was set, and power was calculated
# not using here where we are looping to calculate power at different sample sizes
# and not splitting sample by multiple subgroups
# keeping here as a legacy
if(1==0){
  sample.size <- 138  # TD 1-3 in cohort 1
  sample.size <- 65   # Dys 1-3 in cohort 1
}

# sample sizes from POWER 3.0 proposal, Spring 2023
# designed for older code when sample size was set, and power was calculated
# not using here where we are looping to calculate power at different sample sizes
# and not splitting sample by multiple subgroups
# keeping here as a legacy
if(1==0){
  sample.size <- 114  # TD 3-5 in cohorts 1-3
  sample.size <- 114   # Dys Grade 3 pooling over cohorts 1-3 which will have scores Grades 3-5
  sample.size <- 118   # DLD/Dys Grade 3 pooling over cohorts 1-3 which will have scores Grades 3-5
}



# Loop over sample sizes and calculate power for each ---------------------
which.sample.size=0
which.sample.size=which.sample.size+1
for(which.sample.size in 1:(length(sample.sizes.to.examine))){
  
  
# * Print out which sample size currently on ---------------------
print(paste0("Conducting analysis for sample size ", which.sample.size, " of ", length(sample.sizes.to.examine)))
  
  
# * Define the sample size ---------------------
sample.size = sample.sizes.to.examine[which.sample.size]  

# * Power Analysis for Latent Change Score Model for Central Executive ---------------------

# Run the source code to generate the data, fit the model, save the test result, and tabulate power
# 5/8/2023
# Now referencing the folder with the R code as 'code.folder'
# And code as "R.code.file
# source(paste0(R.folder, R.code.file))
R.code.file <- "Power Analysis for Latent Change Score Model for Central Executive.R"
source(paste0(code.folder, R.code.file))

# * * Print power of the test for the mean of the slopes -----
# print(paste0("Power of test of the mean of the slopes = ", power.test.of.mean.of.slope))

# * * Store value for the power of the test for the mean of the slopes  -----
power.results[which.sample.size, ]$power.test.of.mean.of.slope.for.CE <- power.test.of.mean.of.slope




# * Power Analysis for Latent Change Score Model for Focus of Attention/Visuospatial Sketch Pad ------------------

# Run the source code to generate the data, fit the model, save the test result, and tabulate power
# 5/8/2023
# Now referencing the folder with the R code as 'code.folder'
# And code as "R.code.file
# source(paste0(R.folder, R.code.file))
R.code.file <- "Power Analysis for Latent Change Score Model for Focus of Attention.R"
source(paste0(code.folder, R.code.file))

# * * Print power of the test for the mean of the slopes -----
# print(paste0("Power of test of the mean of the slopes = ", power.test.of.mean.of.slope))

# * * Store value for the power of the test for the mean of the slopes  -----
power.results[which.sample.size, ]$power.test.of.mean.of.slope.for.FOA <- power.test.of.mean.of.slope




# * Power Analysis for Latent Change Score Model for Phonological Storage & Rehearsal/ Phonological Loop ------------------

# Run the source code to generate the data, fit the model, save the test result, and tabulate power
# 5/8/2023
# Now referencing the folder with the R code as 'code.folder'
# And code as "R.code.file
# source(paste0(R.folder, R.code.file))
R.code.file <- "Power Analysis for Latent Change Score Model for Phonological.R"
source(paste0(code.folder, R.code.file))

# * * Print power of the test for the mean of the slopes -----
# print(paste0("Power of test of the mean of the slopes = ", power.test.of.mean.of.slope))

# * * Store value for the power of the test for the mean of the slopes  -----
power.results[which.sample.size, ]$power.test.of.mean.of.slope.for.PHON <- power.test.of.mean.of.slope

} # closes loop over sample sizes

# Write out results -----

# * Define the output folder ----
output.folder <- latent.change.score.folder

# * Write out the table of power values ----
file.name <- "Power Cacluations LCS Models.csv"
write_csv(power.results, paste0(output.folder, file.name))

