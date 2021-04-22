#########################################################
#########################################################
#vii.	Structural Equation Models (SEM)
#########################################################
#########################################################

#This module will demonstrate how to use two packages for fitting structural equation models: lavaan and OpenMx

#lavaan is somewhat easier to use and handles some aspects of the model fitting process automatically 
#but is less flexible than OpenMx

#OpenMx model syntax is generally more verbose than lavaan and requires explicit specification of all model identification constraints
#but is flexible enough to fit practically any model and has great capabilities for working with ordinal outcome data

#both are useful packages and, in this example, will produce the same results. 

#it can be valuable to experiment in OpenMx to become more familiar with the default settings that are assigned behind the scenes in other software (like lavaan)
#for example, lavaan automatically identifies latent variables with mean=0, freely estimated variance, and the first factor loading fixed to 1
#in this example, the latent variable in our OpenMx model will also be identified like this; 
#however, another option would be to fix the mean (0) and variance (1) and estimate all factor loadings. 
#similar discrepancies are illustrated in the differences between how the two packages handle calculation of fit statistics
#we will cover RAM path specification in OpenMx here, but also note that specification via matrix algebra is supported as well

#these demonstrations both implement estimation by full information maximum likelihood; however other estimators are available in both packages

#remove the hashtag and run only once to install.
#OpenMx may take some time to install
#install.packages(c("lavaan", "OpenMx", "semPlot"), dependencies = TRUE)

#########################################################
library(psych)
library(lavaan)
library(OpenMx)
library(semPlot)
library(stargazer)

options(stringsAsFactors = FALSE)
getwd() # this function can be used to find your current working directory
#this is where R is looking to find files

#this directory contains the data we will work with
setwd("/Users/thomasns/Documents/0DevInternship/Modules/Modules") 

#########################################################
#########################################################
#reading in data 
#https://www.cdc.gov/nchs/nhis/2019nhis.htm
#the data can be downloaded from the link above
#the downloaded file should be saved in the directory specified above in setwd()
#########################################################
####################################################
df<-read.table("adult19.csv", #the dataset file in the current working directory
               sep=",", #the character that separates values in the file (a comma here, because the file is csv)
               header=TRUE) #does the file have column names as the first row?

#Descriptions of the 5 variables we will use in this demonstration are provided below

####################################################
#Highest level of education of all the adults in SA's family 
table(df$MAXEDUC_A)
#00 Never attended/kindergarten only
#01 Grade 1-11
#02 12th grade, no diploma
#03 GED or equivalent
#04 High School Graduate
#05 Some college, no degree
#06 Associate degree: occupational, technical, or vocational program
#07 Associate degree: academic program
#08 Bachelor's degree (Example: BA, AB, BS, BBA)
#09 Master's degree (Example: MA, MS, MEng, MEd, MBA)
#10 Professional School degree (Example: MD, DDS, DVM, JD)
#11 Doctoral degree (Example: PhD, EdD)

####################################################
#Food insecurity items

#Worry food would run out 
table(df$FDSRUNOUT_A)
#1 Often true
#2 Sometimes true
#3 Never true
#7 Refused
#8 Not Ascertained
#9 Don't Know

#Food didn't last 
table(df$FDSLAST_A)
#1 Often true
#2 Sometimes true
#3 Never true
#7 Refused
#8 Not Ascertained
#9 Don't Know

#Couldn't afford to eat balanced meals 
table(df$FDSBALANCE_A)
#1 Often true
#2 Sometimes true
#3 Never true
#7 Refused
#8 Not Ascertained
#9 Don't Know

#Would you say your health in general is excellent, very good, good, fair, or poor?
table(df$PHSTAT_A)
#1 Excellent
#2 Very Good
#3 Good
#4 Fair
#5 Poor

#########################################################
#########################################################
#1.	Preliminary Steps, Data Cleaning
#########################################################
#########################################################

#########################################################
#Subset the 5 variables of interest
df_subset<-df[,c("MAXEDUC_A", 
                 "FDSRUNOUT_A", "FDSLAST_A", "FDSBALANCE_A", 
                 "PHSTAT_A")]

#########################################################
#examine frequencies of variables to determine what recoding should be done
table(df_subset$MAXEDUC_A) #looks fine
table(df_subset$FDSRUNOUT_A) # >6 is NA and need to reverse code so higher number = greater food insecurity
table(df_subset$FDSLAST_A)  # >6 is NA and need to reverse code so higher number = greater food insecurity
table(df_subset$FDSBALANCE_A) # >6 is NA and need to reverse code so higher number = greater food insecurity
table(df_subset$PHSTAT_A)# >6 is NA and need to reverse code so higher number = better health

#########################################################
#Assign missing value codes to NA so that they are excluded from analysis
df_subset$FDSRUNOUT_A[df_subset$FDSRUNOUT_A>6] <- NA
df_subset$FDSLAST_A[df_subset$FDSLAST_A>6] <- NA
df_subset$FDSBALANCE_A[df_subset$FDSBALANCE_A>6] <- NA
df_subset$PHSTAT_A[df_subset$PHSTAT_A>6] <- NA

#reverse coding can be set up by subtracting the max value from the variable, adding 1, and taking the absolute value
#absolute value of ( var - (max of variable +1) )
df_subset$FDSRUNOUT_A_Reverse<-abs(df_subset$FDSRUNOUT_A - (max(df_subset$FDSRUNOUT_A, na.rm=T)+1))
table(df_subset$FDSRUNOUT_A, df_subset$FDSRUNOUT_A_Reverse)#check reverse coding

df_subset$FDSLAST_A_Reverse<-abs(df_subset$FDSLAST_A - (max(df_subset$FDSLAST_A, na.rm=T)+1))
table(df_subset$FDSLAST_A, df_subset$FDSLAST_A_Reverse)#check reverse coding

df_subset$FDSBALANCE_A_Reverse<-abs(df_subset$FDSBALANCE_A - (max(df_subset$FDSBALANCE_A, na.rm=T)+1))
table(df_subset$FDSBALANCE_A, df_subset$FDSBALANCE_A_Reverse)#check reverse coding

df_subset$PHSTAT_A_Reverse<-abs(df_subset$PHSTAT_A - (max(df_subset$PHSTAT_A, na.rm=T)+1))
table(df_subset$PHSTAT_A, df_subset$PHSTAT_A_Reverse, exclude=NULL)#check reverse coding

describe(df_subset)

#########################################################
#########################################################
#2.	SEM in Lavaan
#########################################################
#########################################################

#first, define the SEM using lavaan's syntax
#recall that the ~ symbol means "estimated by"
#lavaan models are defined as a series of regression equations
# the operator =~ indicates that the equation is for a measurement model of a latent variable
# the operator ~ indicates that the equation is a series of regression paths

#Latent_FoodInsec is measured by 3 indicator variables (FDSRUNOUT_A_Reverse + FDSLAST_A_Reverse + FDSBALANCE_A_Reverse)
#we will test if
#MAXEDUC_A predicts Latent_FoodInsec
#Latent_FoodInsec and MAXEDUC_A predict PHSTAT_A_Reverse

#########################################################
#Model Syntax
LavaanModel <- '
# measurement model
Latent_FoodInsec =~ FDSRUNOUT_A_Reverse + FDSLAST_A_Reverse + FDSBALANCE_A_Reverse 

# regressions
Latent_FoodInsec ~ MAXEDUC_A
PHSTAT_A_Reverse ~ Latent_FoodInsec + MAXEDUC_A
'
#########################################################
#fit the model 
Lavaanfit <- sem(LavaanModel, #the model syntax object
           missing = "fiml.x", #how to handle missing data? full-information maximum likelihood in this example
           data= df_subset) #the source of data

summary(Lavaanfit) # generate a summary of the results
standardizedSolution(Lavaanfit) #generate summary of standardized results. 
#Standardized results can be compared to each other directly so are generally preferred to the raw estimates 

#various fit statistics can be extracted easily to describe how well the model fits the data
fitMeasures(Lavaanfit) #you would likely only pick a few of these to report, minimally the chisq, df, and p.

#########################################################
#we can export these tables using stargazer
stargazer(standardizedSolution(Lavaanfit),
          type="html",
          summary=FALSE, 
          title="Lavaan Standardized",
          out="LavaanStandardized.doc")

stargazer(data.frame(fitMeasures(Lavaanfit)),
          type="html",
          summary=FALSE, 
          title="Lavaan Fit Stats",
          out="LavaanFitStats.doc")

#########################################################
#plot the model
#the semPlot package includes convenient functions to plot structural equation models

semPaths(Lavaanfit, #the model to plot
         "std", #standardized path coefficients
         intercept=FALSE, # do not include the intercept
         residuals=FALSE, #do not include the variances
         what="est", #plot path estimates from the model
         edge.label.cex=1.3, #include numeric coefficient for paths
         fade=FALSE) # plot all paths, even if near 0

#run again with filetype="png" to export the plot
semPaths(Lavaanfit, #the model to plot
         "std", #standardized path coefficients
         intercept=FALSE, # do not include the intercept
         residuals=FALSE, #do not include the variances
         what="est", #plot path estimates from the model
         edge.label.cex=1.3, #include numeric coefficient for paths
         fade=FALSE,# plot all paths, even if near 0
         filetype="png",#the file will be named qgraph.png.
         filename="LavaanModel") #The name of the export file, without file extension

#Maximum years of education education, a proxy for SES, predicts less food insecurity and better self-reported health.
#Food insecurity predicts worse self-reported health. 

#########################################################
#########################################################
#3.	SEM in OpenMx
#########################################################
#########################################################

#It can be useful to define groups of variables that will be handled in similar ways for use in your model syntax
#this can make for model syntax that is easier to read
#for example, below we create an object that lists all observed variables in the model 
#and another that lists all of the items that measure food insecurity
#Defining these here means we can just enter "observed" rather than the full list in the model syntax
observed <- c("MAXEDUC_A", 
              "FDSRUNOUT_A_Reverse", 
              "FDSLAST_A_Reverse", 
              "FDSBALANCE_A_Reverse",
              "PHSTAT_A_Reverse")
FoodInsecItems <- c( "FDSRUNOUT_A_Reverse", 
                     "FDSLAST_A_Reverse", 
                     "FDSBALANCE_A_Reverse")

#########################################################
#Path Specification 
#Models in OpenMx can be built by specifying a series of single and double-headed arrows using the MxPath() function
#For example, the syntax below specifies a single headed arrow from Latent Food Insecurity to Self Reported Health.
#Single-headed arrows indicate a directional relationship (X predicts Y)
#Double-headed arrows indicate a non-directional relationship (X and Y are correlated)
#Double-headed arrows from a variable to itself indicate its variance
#Single-headed arrows from 1 (a constant) to a variable indicate its mean

#This format allows for variances and covariances to be calculated from a path diagram, given the relevant path coefficients 
#More information can be found below
#https://en.wikipedia.org/wiki/Path_analysis_(statistics)#Path_tracing_rules

#try running the commands below to see a summary of the path that it would add to the model  

#Single-headed arrow from one variable to another
mxPath(from="Latent_FoodInsec",  #build a path from the latent variable
       to="PHSTAT_A_Reverse", #to self reported health
       arrows=1, #the arrow has one point, towards self reported health
       free=TRUE, #the path is freely estimated
       values=0.1,#the optimizer needs a starting value
       labels="Latent_FoodInsec_To_OverallHealth")#label the series of coefficients

#Double-headed arrow from variable to itself (for each observed variables)
mxPath(from=observed, ##build a path from the variable to itself (since there is no "to" argument)
       arrows=2, #double headed arrow, for variances (or correlations)
       free=TRUE, #freely estimate observed variances
       values=0.1, #the optimizer needs a starting value,
       labels=paste0("variance_", observed)) #label the series of coefficients

#########################################################
#Model Syntax
OpenMxModel<-mxModel("OpenMx FIML Model", #specify a title for the mode
        type="RAM", #RAM stands for reticular action model. This allows specifying the model by paths. OpenMx also supports specifications based on matrix algebra
        manifestVars=observed, #the observed variables in the model
        latentVars="Latent_FoodInsec", #the latent variables in the model
        
        #Specify the Means in the Model
        mxPath(from="one", #build a path from the constant 1 (for means)
               to=observed, #to each observed variable
               arrows=1,#the arrow has one point, towards the variable
               free=TRUE, #the parameter is freely estimated
               values=0.1,#the optimizer needs a starting value, anything close to 0 works fine here, but can require more thought in complex models
               labels=paste0("mean_", observed)), #label the series of coefficients
        
        mxPath(from="one",  #build a path from the constant 1 (for means)
               to="Latent_FoodInsec", #to our latent variable for food insecurity
               arrows=1, #the arrow has one point, towards the variable
               free=FALSE, #the parameter is FIXED. Identifying latent variables requires parameter constraints to define the scale of the values
               values=0, #the mean is fixed at 0
               labels=paste0("mean_", "Latent_FoodInsec")), #label 

        #Specify the Variances in the Model
        mxPath(from=observed, ##build a path from the variable to itself (since there is no "to" argument)
               arrows=2, #double headed arrow, for variances (or correlations)
               free=TRUE, #freely estimate observed variances
               values=0.1, #the optimizer needs a starting value,
               labels=paste0("variance_", observed)), #label the series of coefficients
        
        mxPath(from="Latent_FoodInsec", ##build a path from the variable to itself (since there is no "to" argument)
               arrows=2, #double headed arrow, for variances (or correlations)
               free=TRUE,#freely estimate the latent variance. We can estimate this because we will later fix the first factor loading to 1, like the lavaan identification default
               values=0.1, #the optimizer needs a starting value,
               labels=paste0("variance_", "Latent_FoodInsec")),#label the series of coefficients
        
        #Specify the Measurement Model for Food Insecurity
        mxPath(from="Latent_FoodInsec", #build a path from the latent variable
               to=FoodInsecItems, #to each food insecurity item
               arrows=1, #the arrow has one point, towards the food insecurity items
               free=c(FALSE, TRUE, TRUE), #the first path is fixed, others are freely estimated
               values=c(1, 0.1, 0.1), #the first path is fixed at 1, other 2 have starting values of 0.1 for the optimizer
               labels=paste0("loading_", FoodInsecItems)),#label the series of coefficients
        
        #Specify the Regression Paths
        mxPath(from="Latent_FoodInsec",  #build a path from the latent variable
               to="PHSTAT_A_Reverse", #to self reported health
               arrows=1, #the arrow has one point, towards self reported health
               free=TRUE, #the path is freely estimated
               values=0.1,#the optimizer needs a starting value,
               labels="Latent_FoodInsec_To_OverallHealth"),#label the series of coefficients
     
        mxPath(from="MAXEDUC_A", #build 2 paths from max education 
               to=c("Latent_FoodInsec", "PHSTAT_A_Reverse"),#to self reported health and latent food insecurity
               arrows=1, #each arrow has one point, towards self reported health / latent food insecurity
               free=TRUE,#the path is freely estimated
               values=0.1,#the optimizer needs a starting value,
               labels=c("EDUC_To_LatentFoodInsec", "EDUC_To_OverallHealth")),#label the series of coefficients
       
         #The Data Source for the Model
        mxData(observed=df_subset, #identify the dataset to draw variables from
               type="raw")) #we are using raw data as input. covariance matrices can also be used as input too

#########################################################
#run the model specified above
OpenMxFit<-mxRun(OpenMxModel) #FIML is the default for raw data inputs with RAM specification
OpenMxFit$fitfunction #note here that the fit function is MxFitFunctionML, confirming FIML estimation
summary(OpenMxFit) #generate a summary of the model. In the A matrix, path coefficients are stored as paths FROM column variable TO row variable
mxStandardizeRAMpaths(OpenMxFit, SE=TRUE) # generate a standardized summary of the model, again in the A matrix path coefficients are stored as paths FROM column variable TO row variable

#########################################################
#to estimate fit statistics with OpenMx, we must first construct a reference model to compare to with mxRefModels(), and supply this as an argument in our call to mxRun()
#This step is implicit in lavaan
OpenMxFit_saturated <-mxRefModels(OpenMxFit, run=T)
summary(OpenMxFit, refModels=OpenMxFit_saturated)

#########################################################
#we can export these tables using stargazer
stargazer(mxStandardizeRAMpaths(OpenMxFit, SE=TRUE),
          type="html",
          summary=FALSE, 
          title="OpenMxStandardizedCoefs_withSE",
          out="OpenMxStandardized.doc")

#the fit statistics are stored in positions 8 - 34 in the summary object. It is recommended to select which are needed before exporting
temp<-summary(OpenMxFit, refModels=OpenMxFit_saturated) #save summary as an object
temp[8:34] # examine available fit statistics
temp<-data.frame(temp$Chi, temp$ChiDoF, temp$p, temp$CFI, temp$RMSEA, temp$RMSEACI[1],temp$RMSEACI[2]) #extract chi square, df, p, CFI, and RMSEA with CI
colnames(temp) <- c("ChiSquare", "ChiSquare_DF", "ChiSquare_P", "CFI", "RMSEA", "RMSEA_lower", "RMSEA_upper") #set clearer names

#export
stargazer(temp,
          type="html",
          summary=FALSE, 
          title="OpenMx Fit Stats",
          out="OpenMxFitStats.doc")

#########################################################
#plot the model
semPaths(OpenMxFit, #the model to plot
         "std", #standardized path coefficients
         intercept=FALSE, # do not include the intercept
         residuals=FALSE, #do not include the variances
         what="est", #plot path estimates from the model
         edge.label.cex=1.3, #include numeric coefficient for paths
         fade=FALSE) # plot all paths, even if near 0
#Maximum years of education education, a proxy for SES, predicts less food insecurity and better self-reported health.
#Food insecurity predicts worse self-reported health. 

#########################################################
#Likelihood-based confidence intervals for standardized estimates can be retrieved with the commands below, which are also available in the OpenMx package documentation
#this is a unique feature of OpenMx and is a method of inference that is generally more robust than calculating confidence intervals from the SE
#this can also lead to some longer run times when requesting many likelihood-based CI in complex models
#here, run time will still be quick

#if adapting this command for a different analysis, only 4 spots need to be changed. These are flagged below
OpenMxModel2 <- mxModel(model=OpenMxModel, # to adapt, fill in model name here (in place of OpenMxModel)
                        mxMatrix(type="Iden",nrow=nrow(OpenMxFit$A),name="I"), # to adapt for a different analysis, fill in model name here (in place of OpenMxModel)
                        mxAlgebra( vec2diag(diag2vec( solve(I-A)%*%S%*%t(solve(I-A)) )%^%-0.5) ,
                                   name="InvSD"),
                        mxAlgebra( InvSD %*% A %*% solve(InvSD),
                                   name="Az",dimnames=dimnames(OpenMxFit$A)), # to adapt for a different analysis, fill in model name here (in place of OpenMxModel)
                        mxAlgebra( InvSD %*% S %*% InvSD, 
                                   name="Sz",dimnames=dimnames(OpenMxFit$S)), # to adapt for a different analysis, fill in model name here (in place of OpenMxModel)
                        mxCI(c("Az","Sz")))
OpenMxFit2<-mxRun(OpenMxModel2, intervals=TRUE) #run model with likelihood-based CI for all path estimates, variances, and covariances

#the coefficients are stored in two matrices (Az and Sz)
#the rows and columns of these matrices are the variables in your model
#in the Az matrix standardized path coefficients are stored as paths FROM column variable TO row variable
OpenMxFit2$Az$result

#Sz is structured like a variance-covariance matrix
#note that the off diagonal is 0 because we didn't specify any correlations, only variances and single-headed arrows
OpenMxFit2$Sz$result

#the likelihood-based confidence intervals are stored under $output$confidenceIntervals
LikelihoodBasedCI<-OpenMxFit2$output$confidenceIntervals #parameters are only labeled by their position in Az and Sz
#but we can extract names from the dimensions of the original A and S matrices

#expand.grid generates all combinations of the two columns listed as arguments
#in this case, we can use expand.grid to generate a list of names that align with the dimensions of the Az and Sz matrices, since they include all variables in the model as row/column names
Vars<-rbind(
  expand.grid(
    row.names(OpenMxFit2$A), colnames(OpenMxFit2$A)), # to adapt for a different analysis, fill in model name here (in place of OpenMxModel)
  expand.grid(
    row.names(OpenMxFit2$S), colnames(OpenMxFit2$S))) # to adapt for a different analysis, fill in model name here (in place of OpenMxModel)

Vars<-paste("from", Vars$Var2, "to", Vars$Var1, sep=" ") #add in descriptive text "from" and "to" to assist interpretation of Az parameters
row.names(LikelihoodBasedCI)<-Vars #assign row names to the likelihood-based CI for labels
LikelihoodBasedCI # it can be useful to remove rows where likelihood-based CI were not estimated (listing NA / 0 / NA)
LikelihoodBasedCI<-LikelihoodBasedCI[LikelihoodBasedCI[,"estimate"]!=0,] #here we retain only the rows where the estimate column is not equal to 0
LikelihoodBasedCI
#export likelihood-based CI with stargazer
stargazer(LikelihoodBasedCI,
          type="html",
          summary=FALSE, 
          title="OpenMx Likelihood-based CI",
          out="OpenMxLikelihoodBasedCI.doc")

#citations for the packages used in this module can be retrieved below
citation("psych")
citation("lavaan")
citation("OpenMx")
citation("semPlot")
citation("stargazer")
