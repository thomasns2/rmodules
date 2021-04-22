#########################################################
#########################################################
#v.	Linear Models
#########################################################
#########################################################

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

#Descriptions of the 4 variables we will use in this demonstration are provided below

####################################################
#Continuous Outcome: 
#About how long has it been since you last saw a doctor or other health care professional about your health?
table(df$LASTDR_A)
#0 Never
#1 Within the past year (anytime less than 12 months ago)
#2 Within the last 2 years (1 year but less than 2 years ago)
#3 Within the last 3 years (2 years but less than 3 years ago)
#4 Within the last 5 years (3 years but less than 5 years ago)
#5 Within the last 10 years (5 years but less than 10 years ago)
#6 10 years ago or more

####################################################
#Binary Outcome:
#During the past 12 months, have you DELAYED getting medical care because of the cost?
table(df$MEDDL12M_A) 
# 1 yes, 2 no

####################################################
#Predictors :
#Would you say your health in general is excellent, very good, good, fair, or poor?
table(df$PHSTAT_A)
#1 Excellent
#2 Very Good
#3 Good
#4 Fair
#5 Poor

#Coverage status as used in Health United States
table(df$NOTCOV_A) 
#1 not covered, 2 covered

#########################################################
#########################################################
#1.	Preliminary Steps, Data Cleaning
#########################################################
#########################################################

#########################################################
#Subset the 4 variables of interest
df_subset<-df[,c("LASTDR_A", "MEDDL12M_A", "PHSTAT_A", "NOTCOV_A")]

#########################################################
#examine frequencies of variables to determine what recoding should be done
table(df_subset$LASTDR_A)
table(df_subset$MEDDL12M_A)
table(df_subset$PHSTAT_A)
table(df_subset$NOTCOV_A)

#########################################################
#Assign missing value codes to NA so that they are excluded from analysis
df_subset$MEDDL12M_A[df_subset$LASTDR_A>6] <- NA #values greater than 6 are missing codes
df_subset$MEDDL12M_A[df_subset$MEDDL12M_A>2] <- NA #values greater than 2 are missing codes
df_subset$PHSTAT_A[df_subset$PHSTAT_A>5] <- NA #values greater than 5 are missing codes
df_subset$NOTCOV_A[df_subset$NOTCOV_A>2] <- NA #values greater than 2s are missing codes

#recode into binary coding scheme, 0=No instead of 2=No 
#this sort of coding scheme is preferable in most cases, but not technically required
#many statistical models have unique properties for comparisons between 0 and 1. Using this coding scheme for binary variables is a good habit 
df_subset$MEDDL12M_A[df_subset$MEDDL12M_A==2]<-0 #1 yes delay, 0 no delay
df_subset$NOTCOV_A[df_subset$NOTCOV_A==2]<-0# 1 no coverage, 0 yes coverage

#reverse code self-rated health so higher values are better self-rated health
#reverse coding can be set up by subtracting the max value from the variable, adding 1, and taking the absolute value
#absolute value of ( var - (max of variable +1) )
#this could also be accomplished by manually recoding each level i.e. : 
#df_subset$PHSTAT_A_Reverse[df_subset$PHSTAT_A_Reverse==1]<-4 
df_subset$PHSTAT_A_Reverse<-abs(df_subset$PHSTAT_A - (max(df_subset$PHSTAT_A, na.rm=T)+1))
table(df_subset$PHSTAT_A, df_subset$PHSTAT_A_Reverse, exclude=NULL)#check reverse coding by running crosstabs between the old and new variable

#LASTDR_A is an ordinal variable, but in this demonstration we will treat it as continuous
#to better approximate the continuous variable that underlies the ordinal scale, we assign numeric codes that are scaled in years
#since each ordinal level represents a range of years, we assign the mid point of the range as the numeric value for each ordinal level
#recode LASTDR_A to a pseudo continuous scale by recoding to mid-point of ranges
df_subset$LASTDR_A[df_subset$LASTDR_A==0] <- 0 #Never
df_subset$LASTDR_A[df_subset$LASTDR_A==1] <- 0.5 #Less than 1 Year
df_subset$LASTDR_A[df_subset$LASTDR_A==2] <- 1.5 #1 -2 Years
df_subset$LASTDR_A[df_subset$LASTDR_A==3] <- 2.5 #2 - 3 Years
df_subset$LASTDR_A[df_subset$LASTDR_A==4] <- 4 #3 - 5 Years
df_subset$LASTDR_A[df_subset$LASTDR_A==5] <- 7.5 #5 - 10 Years 
df_subset$LASTDR_A[df_subset$LASTDR_A==6] <- 10 #10 + years

#examine frequencies again after cleaning variables
table(df_subset$LASTDR_A)
table(df_subset$MEDDL12M_A)
table(df_subset$PHSTAT_A)
table(df_subset$PHSTAT_A_Reverse)
table(df_subset$NOTCOV_A)

#########################################################
#########################################################
#2.	Ordinary Least Squares Regression with lm()
#########################################################
#########################################################

#Ordinary least squares regression is appropriate for continuous outcome variables
#here, we conduct simple linear regression with a single predictor and then multiple linear regression with two predictors
#the lm function is used to specify ordinary least regression models in R
#lm stands for linear model

#########################################################
#Simple linear regression predicting 
#About how long has it been since you last saw a doctor or other health care professional about your health?
#as a function of #Would you say your health in general is excellent, very good, good, fair, or poor?
SimpleLM_Model<-lm(LASTDR_A~ #the outcome variable. The ~ symbol means "estimated by"
                     PHSTAT_A_Reverse, #the predictor variable
                   data=df_subset) #the data.frame that contains the outcome and predictor variables
summary(SimpleLM_Model)#generate summary of the model
#a 1 unit increase in self-reported health status is associated with 0.075 higher years since last doctor appointment

#########################################################
#You can add an additional predictor to the model with the + symbol
MultipleLM_Model<-lm(LASTDR_A~ #the outcome variable. The ~ symbol means "estimated by"
                       PHSTAT_A_Reverse+ #predictor variable 1
                       NOTCOV_A, #predictor variable 2
                     data=df_subset)#the data.frame that contains the outcome and predictor variables
summary(MultipleLM_Model)#generate summary of model
#a 1 unit increase in self-reported health status is associated with 0.072 higher years since last doctor appointment after controlling for health insurance status
#not having health insurance is associated with 1.17 higher years since last doctor appointment after controlling for self-reported health status

#########################################################
#The lm model object stores all of the information displayed in the summary output
#we can access the results by interacting with the model object using the $ operator
#here we focus on the elements of the object that are most relevant to the user-- there are others that are used internally by various functions
MultipleLM_Model$coefficients# the intercept and b (slope) coefficients for each predictor
MultipleLM_Model$residuals #the residual for each subject. defined as observed outcome value minus predicted outcome value
MultipleLM_Model$fitted.values #the predicted values of the outcome for each subject based on the predictors
MultipleLM_Model$df.residual #the residual degrees of freedom of the model
MultipleLM_Model$call #the formula used to conduct the analysis
MultipleLM_Model$model #the data.frame that was used in the analysis. 
#Notably, MultipleLM_Model$model may be different than the data.frame that is included in the original call to lm (df_subset). 
#By default, lm() will drop subjects that have missing values on the outcome or any predictors included in the analysis

#see here that the number of rows (subjects) between MultipleLM_Model$model and df_subset is different
nrow(MultipleLM_Model$model) #analytic sample
nrow(df_subset)#overall sample

#########################################################
#########################################################
#3.	Logistic Regression with glm()
#########################################################
#########################################################

#Logistic regression is appropriate for binary outcome variables
#here, we conduct simple logistic regression with a single predictor and then multiple logistic regression with two predictors
#the glm function is used to specify logistic regression models in R
#glm stands for 'generalized linear model'. Logistic regression is one example of a generalized linear model, glm() can be used to fit several others as well

#########################################################
#Simple logistic regression predicting
##During the past 12 months, have you DELAYED getting medical care because of the cost?
#as a function of #Would you say your health in general is excellent, very good, good, fair, or poor?
SimpleGLM_Model <- glm(MEDDL12M_A ~ #the outcome variable. The ~ symbol means "estimated by"
                         PHSTAT_A_Reverse, #the predictor variable
                       data = df_subset, #the data.frame that contains the outcome and predictor variables
                       family = binomial(link = "logit")) #the family and link function used to model the outcome. Changing this allows for glm to fit other generalized linear models. For logistic regression, use binomial family with a logit link function
summary(SimpleGLM_Model)#generate summary of model
#a 1 unit increase in self-reported health status is associated with 0.41 lower log-odds (logit) of having delayed medical care

#########################################################
#log odds slopes are not easily interpreted, and so it is a common practice to convert these values to odds ratios. 
#this can be accomplished by exponentiating the log-odds b with the exp() function
exp(-0.41308) 
#a 1 unit increase in self-reported health status is associated with 0.66 times lower odds of having delayed medical care

#########################################################
#You can add an additional predictor to the model with the + symbol
MultipleGLM_Model <- glm(MEDDL12M_A ~ #the outcome variable. The ~ symbol means "estimated by"
                           PHSTAT_A_Reverse+ #predictor variable 1
                           NOTCOV_A, #predictor variable 2
                         data = df_subset, #the data.frame that contains the outcome and predictor variables
                         family = "binomial")#the family and link function used to model the outcome. Changing this allows for glm to fit other generalized linear models. For logistic regression, use binomial family with a logit link function
summary(MultipleGLM_Model)#generate summary of model
#a 1 unit increase in self-reported health status is associated with 0.46 lower log-odds of having delayed medical care after controlling for health insurance status
#not having health insurance is associated with 2.14 higher log-odds of having delayed medical care after controlling for self-reported health status

#########################################################
#exponentiating the log-odds b with the exp() function for a more interpretable result
exp(-0.46198) 
#a 1 unit increase in self-reported health status is associated with 0.66 times lower odds of having delayed medical care

exp(2.14086)
#not having health insurance is associated with 8.5 times higher odds of having delayed medical care

#########################################################
#The glm model object stores all of the information displayed in the summary output
#we can access the results by interacting with the model object using the $ operator
#again we focus on the elements of the object that are most relevant to the user
MultipleGLM_Model$coefficients# the intercept and log odds scale b (slope) coefficients for each predictor
MultipleGLM_Model$residuals #the log odds scale residual for each subject. defined as observed outcome value minus predicted outcome value
MultipleGLM_Model$fitted.values #the log odds scale predicted values of the outcome for each subject based on the predictors
MultipleGLM_Model$df.residual #the residual degrees of freedom of the model
MultipleGLM_Model$call #the formula used to conduct the analysis
MultipleGLM_Model$model #the data.frame that was used in the analysis. 
#Again, MultipleGLM_Model$model may be different than the data.frame that is included in the original call to glm (df_subset). 
#By default, glm() will drop subjects that have missing values on the outcome or any predictors included in the analysis

#see here that the number of rows (subjects) between MultipleLM_Model$model and df_subset is different
nrow(MultipleGLM_Model$model) #analytic sample
nrow(df_subset)#overall sample


#########################################################
#4. Generate Tables
#########################################################
library(stargazer) #we will use stargazer to export the tables

#########################################################
#Linear regression

#extract the coefficients table that is generated by summary() and save as a new object
MultipleLM_Model_Table<-summary(MultipleLM_Model)$coef

#print the unformatted coefficients tables
MultipleLM_Model_Table

#Change column names of the table
colnames(MultipleLM_Model_Table) <- c("B", "SE", "t", "p")

#Summary does not generate 95% confidence intervals by default, but these are easily calculated using the standard error
#The formula for a 95% CI is B +/- 1.96*SE
#The scaling factor 1.96 is the z critical value associated with alpha 0.05 for a two tailed hypothesis test
Lower95<-MultipleLM_Model_Table[,"B"] - 1.96*MultipleLM_Model_Table[,"SE"]#calculate the lower boundary of the 95% CI
Upper95<-MultipleLM_Model_Table[,"B"] + 1.96*MultipleLM_Model_Table[,"SE"]#calculate the upper boundary of the 95% CI
MultipleLM_Model_Table<-data.frame(MultipleLM_Model_Table, Lower95, Upper95) #Add the lower and upper boundary of the 95% CI as new columns to the table

#By default, variable names are stored in the row.names of the table, see below
row.names(MultipleLM_Model_Table)
#we will not export the row.names of the table to make sure that the alignment of columns and columns headers is maintained in export
#so save the names as their own column in the table
MultipleLM_Model_Table<-data.frame(row.names(MultipleLM_Model_Table), MultipleLM_Model_Table)
colnames(MultipleLM_Model_Table)[1]<-"VariableNames"#rename the new column
MultipleLM_Model_Table$VariableNames #check the names, we will want to rename them to be more descriptive

#Rename variable names to be more descriptive
MultipleLM_Model_Table$VariableNames[MultipleLM_Model_Table$VariableNames%in%"PHSTAT_A_Reverse"] <- "Self Reported Health"
MultipleLM_Model_Table$VariableNames[MultipleLM_Model_Table$VariableNames%in%"NOTCOV_A"] <- "No Health Insurance"

#Extract analytic sample size 
#Model objects contain analytic dataframe, extract object and calculate number of rows
nrow(MultipleLM_Model$model)

#export the custom table with stargazer
stargazer(MultipleLM_Model_Table, #the object to create a table from
          type="html", #html format to export into a Word document
          summary=FALSE, #export the data.frame as-is. There are a variety of summary functions that can also be performed instead
          out="MultipleLM_Model_Table.doc")#the output file

#while it's worth knowing how to put a table together from scratch, sjPlot also provides functions to generate tables 
library(sjPlot) # 
#the table will be displayed in the viewer in the lower right corner
tab_model(MultipleLM_Model,
          show.intercept = TRUE, #include intercept
          show.est= TRUE, #include b
          show.ci=0.95, #include 95% CI
          show.stat=TRUE, #include t statistic
          show.se=TRUE, #include SE
          show.p=TRUE) #include p value

#run again with "file=" argument to export
tab_model(MultipleLM_Model,
          show.intercept = TRUE, #include intercept
          show.est= TRUE, #include b
          show.ci=0.95, #include 95% CI
          show.stat=TRUE, #include t statistic
          show.se=TRUE, #include SE
          show.p=TRUE,
          file="MultipleLM_Model_Table2.doc") #include p value

#########################################################
#Logistic regression

#extract the coefficients table that is generated by summary() and save as a new object
MultipleGLM_Model_Table<-summary(MultipleGLM_Model)$coef

#print the unformatted coefficients tables
MultipleGLM_Model_Table

#Change column names of the table
colnames(MultipleGLM_Model_Table) <- c("B", "SE", "z", "p")

#recall that summary() returns log-odds scale b, we will want to calculate odds ratios
#Calculate Odds Ratios and 95% CI for logistic regression
OR <- exp(MultipleGLM_Model_Table[,"B"])

#confidence intervals for the odds ratio can be calculated by applying the same procedure we used for lm(), and then using exp() afterwards
Lower95<- exp(MultipleGLM_Model_Table[,"B"] - (MultipleGLM_Model_Table[,"SE"]* 1.96))
Upper95<- exp(MultipleGLM_Model_Table[,"B"] + (MultipleGLM_Model_Table[,"SE"]* 1.96))

#save the OR, 95%CI, and p value as a new table
MultipleGLM_Model_Table<-data.frame(OR, Lower95, Upper95, MultipleGLM_Model_Table[,"p"])
rm(OR,Lower95,Upper95)#remove the objects since we now saved them in a table
colnames(MultipleGLM_Model_Table)[4] <- "p"#rename the pvalue column of the table

#Set row names as their own column in the table, variable names
MultipleGLM_Model_Table<-data.frame(row.names(MultipleGLM_Model_Table), MultipleGLM_Model_Table)
colnames(MultipleGLM_Model_Table)[1]<-"VariableNames"

#Rename variables
MultipleGLM_Model_Table$VariableNames[MultipleGLM_Model_Table$VariableNames%in%"PHSTAT_A_Reverse"] <- "Self Reported Health"
MultipleGLM_Model_Table$VariableNames[MultipleGLM_Model_Table$VariableNames%in%"NOTCOV_A"] <- "No Health Insurance"

#Extract analytic sample size 
#Model objects contain analytic dataframe, extract object and calculate number of rows
nrow(MultipleGLM_Model$model)

#export the custom table with stargazer
stargazer(MultipleGLM_Model_Table, #the object to create a table from
          type="html", #html format to export into a Word document
          summary=FALSE, #export the data.frame as-is. There are a variety of summary functions that can also be performed instead
          out="MultipleGLM_Model_Table.doc")#the output file

#the sjPlot version of the table
#the table will be displayed in the viewer in the lower right corner
tab_model(MultipleGLM_Model,
          show.intercept = TRUE, #include intercept
          show.est= TRUE, #include b
          show.ci=0.95, #include 95% CI
          show.stat=TRUE, #include z statistic
          show.se=TRUE, #include SE
          show.p=TRUE) #include p value

#run again with "file=" argument to export
tab_model(MultipleGLM_Model,
          show.intercept = TRUE, #include intercept
          show.est= TRUE, #include b
          show.ci=0.95, #include 95% CI
          show.stat=TRUE, #include z statistic
          show.se=TRUE, #include SE
          show.p=TRUE,#include p value
          file="MultipleGLM_Model_Table2.doc") #the output file

#########################################################
#4. Visualizing results with ggplot2 
#########################################################
#NOTE, sjPlot also has some functionality for plotting linear models
#we focus on ggplot2 in this section to provide more direct control over what is plotted and how
#to review sjPlot options for plotting models quickly, see the function plot_model()
?plot_model
library(ggplot2)

#########################################################
#Linear Regression Plots

#we will use the results that are stored in our tables to generate plots
#first, remove intercept from table for plotting
#one axis of the plot will display slope coefficients. Plotting the intercept on the same plot would require a clunky axis name (B or Intercept)
#the intercept also does not always have a meaningful interpretation
#maybe there are some contexts where plotting the intercept is important, in which case this section of code should be ignored
MultipleLM_Model_Table_NoIntercept<-MultipleLM_Model_Table[ 
  (MultipleLM_Model_Table$VariableNames%in%"(Intercept)")==FALSE,
  ]

#########################################################
#Forest plots can be useful to display a summary of the model coefficients
LM_Forest<-ggplot(data=MultipleLM_Model_Table_NoIntercept, #identify source of data to plot 
       aes(x=VariableNames, #x axis is variable names
           y=B, #y axis is effect size
           ymin=Lower95, #bars extending out from B are lower and upper 95% CI
           ymax=Upper95)) + #bars extending out from B are lower and upper 95% CI
  geom_pointrange() + #use the point range geom for dot with bars extending outwards
  coord_flip() +  # flip coordinates (puts labels on y axis)
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=0 after flip for null effect
  xlab("Predictor") + ylab(paste("B")) + #Label axis
  ggtitle("Years Since Last Doctor Appt")+ #title for the plot
  theme(#control text size with these arguments below
    title= element_text(size = 16), 
    axis.title.x = element_text(size = 16), 
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size=14))
LM_Forest #print the saved plot


#########################################################
#Plot Predicted Values
#plotting predicted values of a regression equation can be useful to give a visual summary of the model

#recall that a multiple linear regression follow the form
# y = b0 + b1*x1 + b1*x2

#where 
#y is the outcome
#b0 is the intercept
#b1 is the b (slope) coefficient for variable x1
#x1 is the first predictor
#b2 is the b (slope) coefficient for variable x2
#x2 is the second predictor

#predicted values of y are generated by using the b0, b1, and b2 we estimated from our model, along with user-specified values of x1 and x2

#below, we generate predicted values for the following two sets of individuals:

#1. someone with mean self reported health and health insurance
  #intercept + (self-reported health b * mean self reported health) + (No health insurance b * 0)

#2. someone with mean self reported health and no health insurance
  #intercept + (self-reported health b * mean self reported health) + (No health insurance b * 1)

#we accomplish this by generating a data.frame with the two hypothetical people we want to generate predictions for
#the predict() function can then use this fake data to generate predictions

#First, the effect of No Health Insurance at mean level of Self Reported Health
PredictionSet<-data.frame(matrix(nrow=2, ncol=2))#initialize empty object
colnames(PredictionSet)<-c("PHSTAT_A_Reverse", "NOTCOV_A")#column names corresponding to the predictors of interest
PredictionSet[,"NOTCOV_A"] <- c(0, 1) #assign values for health insurance, 0= insured, 1=not insured
PredictionSet[,"PHSTAT_A_Reverse"] <- mean(MultipleLM_Model$model$PHSTAT_A_Reverse) #set value of self-reported health to the mean

#Generate predictions for these 2 observations
PredictedTimeSinceDoctorAppt<-predict.lm(MultipleLM_Model, newdata=PredictionSet)

#add predictions into the data.frame 
PredictionSet<-data.frame(PredictionSet, PredictedTimeSinceDoctorAppt)
PredictionSet

#apply text labels to the levels of 'health insurance' for the plot
PredictionSet$NOTCOV_A[PredictionSet$NOTCOV_A==0] <- "Yes"
PredictionSet$NOTCOV_A[PredictionSet$NOTCOV_A==1] <- "No"

#plot predictions 
LM_Insurance<-ggplot(PredictionSet,#identify source of data to plot 
       aes(x=NOTCOV_A, #Health insurance on the x axis
           y=PredictedTimeSinceDoctorAppt))+#Predicted time since Dr Appt on the Y axis
  geom_bar(stat="identity")+ #plot bars 
  labs(title="", #add a title here
       x ="Health Insurance", y = "Years Since Last Doctor Appt")+ #axis labels
  theme(axis.text=element_text(size=20), #text size
        axis.title=element_text(size=20),
        legend.title = element_text(size=20),
        legend.text=element_text(size=20),
        title = element_text(size=20),
        legend.position="bottom")
LM_Insurance

#########################################################
#Next, we generate predicted values for 10 combinations of values:
#the effect of Self Reported Health (1,2,3,4,5) at different levels of Health Insurance (0,1)

PredictionSet<-data.frame(matrix(nrow=10, ncol=2))#initialize empty object
colnames(PredictionSet)<-c("PHSTAT_A_Reverse", "NOTCOV_A")#column names corresponding to variables of interest
PredictionSet[,"PHSTAT_A_Reverse"] <-  rep(c(1,2,3,4,5), 2) #repeat sequence of 1-5 twice 
PredictionSet[,"NOTCOV_A"] <- c(rep(0, 5), rep(1, 5)) #repeat 0 (5 times) and repeat 1 (5 times)
PredictionSet#here, we can see that our 10 rows contain every combination of Self Reported Health and Health Insurance

PredictedTimeSinceDoctorAppt<-predict.lm(MultipleLM_Model, newdata=PredictionSet)#Generate predictions for these 10 observations
PredictionSet<-data.frame(PredictionSet, PredictedTimeSinceDoctorAppt)#add predictions into df
PredictionSet#the fake data now contains predicted values of Y

#apply text labels for plot
PredictionSet$NOTCOV_A[PredictionSet$NOTCOV_A==0] <- "Yes"
PredictionSet$NOTCOV_A[PredictionSet$NOTCOV_A==1] <- "No"
colnames(PredictionSet)

#plot predictions 
LM_SelfReportHealth<-ggplot(PredictionSet,aes(x=PHSTAT_A_Reverse, #Self reported health status on the x axis
                         y=PredictedTimeSinceDoctorAppt , #Predicted time since Dr Appt on the Y axis
                         color=NOTCOV_A))+ #color lines by Health insurance status
  geom_line(#use lines to plot predicted values
    aes(group=NOTCOV_A), #group lines by health insurance status
            size=1.3)+ # set line size here
  labs(title="", #add a title
       x ="Self Reported Health", y = "Years Since Last Doctor Appt", color = "Health Insurance")+ #axis and legend labels
  theme(axis.text=element_text(size=20),#text size
        axis.title=element_text(size=20),
        legend.title = element_text(size=20),
        legend.text=element_text(size=20),
        title = element_text(size=20),
        legend.position="bottom")
LM_SelfReportHealth

#########################################################
#4 Logistic Regression Plots

#remove intercept from table for plotting
MultipleGLM_Model_Table_NoIntercept<-MultipleGLM_Model_Table[
  (MultipleGLM_Model_Table$VariableNames%in%"(Intercept)")==FALSE,
  ]

#Forest plot of coefficients
GLM_Forest<-ggplot(data=MultipleGLM_Model_Table_NoIntercept, #identify source of data to plot 
       aes(x=VariableNames, #x axis is variable names
           y=OR, #y axis is effect size
           ymin=Lower95, #bars extending out from B are lower and upper 95% CI
           ymax=Upper95)) + #bars extending out from B are lower and upper 95% CI
  geom_pointrange() + #point range geom for dot with bars extending outwards
  coord_flip() +  # flip coordinates (puts labels on y axis)
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=0 after flip for null effect
  xlab("Predictor") + ylab(paste("OR")) + #Label axis
  ggtitle("Delayed Medical Care")+
  theme(#control text size with these arguments below
    title= element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size=14))
GLM_Forest

#Plot, the effect of No Health Insurance at mean level of Self Reported Health
#Generate fake data to predict for
PredictionSet<-data.frame(matrix(nrow=2, ncol=2))#initialize empty object
colnames(PredictionSet)<-c("PHSTAT_A_Reverse", "NOTCOV_A")#column names corresponding to variables of interest
PredictionSet[,"NOTCOV_A"] <- c(0, 1) #0= insured, 1=not insured
PredictionSet[,"PHSTAT_A_Reverse"] <- mean(MultipleGLM_Model$model$PHSTAT_A_Reverse) #set value of self-reported health to the mean

LogitDelayedMedCare<-predict.glm(MultipleGLM_Model, newdata=PredictionSet)#Generate predictions for these 2 rows
PredictionSet<-data.frame(PredictionSet, LogitDelayedMedCare)#add predictions into df
PredictionSet#note here that the predicted values are log-odds scale. these values can be converted to probabilities
#the formula to convert log-odds (logit) to probability (%) is
#100 * (exp(x) / 1+exp(x)), where x is log-odds scale
#below, we apply the exp() function as a preliminary step to simplify the formula to 
#100 * (x/1+x), where x is odds scale

#convert log-odds to probability for interpretability
PredictionSet$OddsDelayedMedCare <- exp(PredictionSet$LogitDelayedMedCare)
PredictionSet$ProbabilityDelayedMedCare<-100* (PredictionSet$OddsDelayedMedCare / (1 + PredictionSet$OddsDelayedMedCare))
PredictionSet#see here, predicted values in log odds, odds, and probability (%) scales
#probability has an intuitive interpretation, so we will plot those values

#apply text labels for plot
PredictionSet$NOTCOV_A[PredictionSet$NOTCOV_A==0] <- "Yes"
PredictionSet$NOTCOV_A[PredictionSet$NOTCOV_A==1] <- "No"

#plot predictions 
GLM_Insurance<-ggplot(PredictionSet,#identify source of data to plot 
       aes(x=NOTCOV_A, #Health insurance status on the X axis
           y=ProbabilityDelayedMedCare))+ #probability of delaying medical care on the y axis
  geom_bar(stat="identity")+ #plot bars
  labs(title="",# add a title here
       x ="Health Insurance", y = "Pr (%) Delayed Med Care")+#axis labels
  theme(axis.text=element_text(size=20),#text size
        axis.title=element_text(size=20),
        legend.title = element_text(size=20),
        legend.text=element_text(size=20),
        title = element_text(size=20),
        legend.position="bottom")
GLM_Insurance

#########################################################
#Next, the effect of Self Reported Health at different levels of Health Insurance
#Generate fake data to predict for
PredictionSet<-data.frame(matrix(nrow=10, ncol=2))#initialize empty object
colnames(PredictionSet)<-c("PHSTAT_A_Reverse", "NOTCOV_A")#column names corresponding to variables of interest
PredictionSet[,"PHSTAT_A_Reverse"] <-  rep(c(1,2,3,4,5), 2) #repeat sequence of 1-5 twice
PredictionSet[,"NOTCOV_A"] <- c(rep(0, 5), rep(1, 5)) #repeat 0 5 times and repeate 1 5 times

LogitDelayedMedCare<-predict.glm(MultipleGLM_Model, newdata=PredictionSet)#Generate predictions for these 10 rows
PredictionSet<-data.frame(PredictionSet, LogitDelayedMedCare)#add predictions into df
PredictionSet

#convert logit to probability for interpretability
PredictionSet$OddsDelayedMedCare <- exp(PredictionSet$LogitDelayedMedCare)
PredictionSet$ProbabilityDelayedMedCare<-100* (PredictionSet$OddsDelayedMedCare / (1 + PredictionSet$OddsDelayedMedCare))
PredictionSet

#apply text labels for plot
PredictionSet$NOTCOV_A[PredictionSet$NOTCOV_A==0] <- "Yes"
PredictionSet$NOTCOV_A[PredictionSet$NOTCOV_A==1] <- "No"
colnames(PredictionSet)

#plot predictions 
GLM_SelfReportHealth<-ggplot(PredictionSet,#identify source of data to plot 
                             aes(x=PHSTAT_A_Reverse, #self reported health on the x axis
                                 y=ProbabilityDelayedMedCare , #probability of delaying medical care on the y axis
                                 color=NOTCOV_A))+#color lines by Health insurance status
  geom_line(aes(group=NOTCOV_A), #group lines by Health insurance status
            size=1.3)+#set size of lines
  labs(title="",#add a title
       x ="Self Reported Health", y = "Pr (%) Delayed Med Care", color = "Health Insurance")+ #axis and legend labels
  theme(axis.text=element_text(size=20),#text size
        axis.title=element_text(size=20),
        legend.title = element_text(size=20),
        legend.text=element_text(size=20),
        title = element_text(size=20),
        legend.position="bottom")
GLM_SelfReportHealth

#########################################################
#########################################################
#Take a look at final outputs before exporting

#OLS Regression
MultipleLM_Model_Table
LM_Forest
LM_Insurance
LM_SelfReportHealth

#Logistic Regression
MultipleGLM_Model_Table
GLM_Forest
GLM_Insurance
GLM_SelfReportHealth

#########################################################
#########################################################
#Export results
#LM#########################################################
write.table(MultipleGLM_Model_Table, "MultipleGLM_Model_Table.csv", sep=",", row.names=FALSE)

tiff("LM_Forest.tiff", width = 2250, height = 2550, units = 'px', res = 300)
LM_Forest 
dev.off()

tiff("LM_Insurance.tiff", width = 2250, height = 2550, units = 'px', res = 300)
LM_Insurance 
dev.off()

tiff("LM_SelfReportHealth.tiff", width = 2250, height = 2550, units = 'px', res = 300)
LM_SelfReportHealth
dev.off()

#GLM#########################################################
write.table(MultipleLM_Model_Table, "MultipleLM_Model_Table.csv", sep=",", row.names=FALSE)

tiff("GLM_Forest.tiff", width = 2250, height = 2550, units = 'px', res = 300)
GLM_Forest 
dev.off()

tiff("GLM_Insurance.tiff", width = 2250, height = 2550, units = 'px', res = 300)
GLM_Insurance 
dev.off()

tiff("GLM_SelfReportHealth.tiff", width = 2250, height = 2550, units = 'px', res = 300)
GLM_SelfReportHealth
dev.off()

citation("stargazer")
citation("sjPlot")
citation("ggplot2")

