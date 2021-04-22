#########################################################
#########################################################
#iv.	Comparing Means and Frequencies
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
#WEIGHTLBTC_A
#Weight without shoes (pounds),
table(df$WEIGHTLBTC_A) #table() prints the frequency distribution of a variable
#100-299 100-299 pounds
#996 Not available
#997 Refused
#998 Not Ascertained
#999 Don't Know

####################################################
#BMICAT_A
#Categorical body mass index
table(df$BMICAT_A)
#1 Underweight
#2 Healthy weight
#3 Overweight
#4 Obese
#9 Unknown

####################################################
#HYPEV_A
#Have you EVER been told by a doctor or other health professional that you had hypertension, also called high blood pressure?
table(df$HYPEV_A)
#1 Yes
#2 No
#7 Refused
#8 Not Ascertained
#9 Don't Know

####################################################
#HYPMED_A
#Are you NOW taking any medication prescribed by a doctor for your high blood pressure?
table(df$HYPMED_A)
#1 Yes
#2 No
#7 Refused
#8 Not Ascertained
#9 Don't Know

#########################################################
#########################################################
#1.	Preliminary Steps, Data Cleaning
#########################################################
#########################################################

#########################################################
#Subset the 4 variables of interest
df_subset<-df[,c("WEIGHTLBTC_A", "BMICAT_A", "HYPEV_A", "HYPMED_A")]

#########################################################
#examine frequencies of variables to determine what recoding should be done
table(df_subset$WEIGHTLBTC_A)
table(df_subset$BMICAT_A)
table(df_subset$HYPEV_A)
table(df_subset$HYPMED_A)

#########################################################
#Assign missing value codes to NA so that they are excluded from analysis
df_subset$WEIGHTLBTC_A[df_subset$WEIGHTLBTC_A>=996] <- NA #values greater than 996 are missing codes
df_subset$BMICAT_A[df_subset$BMICAT_A>=7] <- NA #values greater than 7 are missing codes
df_subset$HYPEV_A[df_subset$HYPEV_A>=7] <- NA #values greater than 7 are missing codes
df_subset$HYPMED_A[df_subset$HYPMED_A>=7] <- NA #values greater than 7 are missing codes

#########################################################
#recode into binary coding scheme, 0=No instead of 2=No 
#this sort of coding scheme is preferable in most cases, but not technically required
#many statistical models have unique properties for comparisons between 0 and 1. Using this coding scheme for binary variables is a good habit 
df_subset$HYPEV_A[df_subset$HYPEV_A==2]<-0 #1 yes delay, 0 no delay
df_subset$HYPMED_A[df_subset$HYPMED_A==2]<-0# 1 no coverage, 0 yes coverage

#########################################################
#generate a new 3 category variable from HYPEV_A and HYPMED_A
#this variable will distinguish three groups: 
#those who have never had hypertension (0), 
#those who have had hypertension in their lifetime (1)
#and those who have had hypertension in their lifetime and are currently taking hypertension medication (2)

table(df_subset$HYPEV_A, df_subset$HYPMED_A, exclude=NULL)#examine the overlap of the two measures
#subjects with no lifetime hypertension (HYPEV_A) are coded missing for the hypertension medication item (HYPMED_A)
#refer to practice questions after module 3 for more practice interpreting tables

#we can infer that if someone has never had hypertension, they are not currently taking medication for hypertension
df_subset$HYPMED_A[df_subset$HYPEV_A==0] <- 0 
table(df_subset$HYPEV_A, df_subset$HYPMED_A, exclude=NULL)#we can't infer if someone who has had lifetime hypertension has taken medication, so we leave those subjects as NA on HYPMED_A

#create a new variable that incorporates information from both items using a nested ifelse statement
#here, the script will demonstrate two equivalent approaches to creating this variable

#approach 1: nested ifelse statement
#recall that ifelse statements are structured as such : ifelse( CONDITION , YES , NO)
#The NO argument of the ifelse statement can be an additional ifelse statement
#in this framework, the code will test the first condition, assign the corresponding value, then test the second condition and assign the corresponding value, etc
#the nested ifelse statement must end with a final NO condition which is used if none of the conditions are met
df_subset$HYPEV_MED<-ifelse(df_subset$HYPEV_A==0, 0, #condition 1, if HYPEV_A is 0, assign 0
       ifelse(df_subset$HYPEV_A==1 & df_subset$HYPMED_A==0, 1, #condition 2, if HYPEV_A is 1 and HYPMED_A is 0, assign 1
              ifelse(df_subset$HYPMED_A==1, 2, #condition 3, if HYPMED_A is 1, assign 2
                     NA))) #final NO condition, assign NA if no other conditions are met
table(df_subset$HYPEV_MED)

#approach 2: addition and recode
#the same recoding procedure can be set up by adding the two variables together and then recoding 3 to 2
df_subset$HYPEV_MED2<-df_subset$HYPMED_A + df_subset$HYPEV_MED
df_subset$HYPEV_MED2[df_subset$HYPEV_MED2==3] <- 2
table(df_subset$HYPEV_MED, df_subset$HYPEV_MED2, exclude=NULL)# note the two approaches to recoding produce identical variables

#remove one of the variable copies
df_subset$HYPEV_MED2 <- NULL

#########################################################
#########################################################
#1.	Two sample t tests with t.test()
#########################################################
#########################################################
#A two-sample t-test can be used to compare means between two groups
TtestOutput<-t.test(WEIGHTLBTC_A~HYPEV_A, #test for differences in the mean of WEIGHTLBTC_A, grouped by the 2 levels of HYPEV_A and save results as TtestOutput
       alternative="two.sided", #conduct a two-tailed hypothesis test. One-tailed tests are also possible with "less" or "greater"
       mu=0, #the value of the mean difference between the two groups under the null hypothesis. This should almost always be 0
       paired=FALSE, #conduct a paired t-test for repeated measures data? 
       var.equal=TRUE, #are variances equal between groups? this is an assumption of the t-test. R will apply a correction if FALSE. We assume TRUE here, but this can be verifed with the leveneTest() function in the car package
       conf.level=0.95, #conduct the hypothesis test at what level of confidence? This should almost always be 0.95 for alpha of 0.05
       data=df_subset, #the dataset
       na.action=na.omit) #missing values are removed from the test
TtestOutput #print a summary the results of the t-test
#mean weight is higher among subjects who report lifetime high blood pressure

#########################################################
#The t-test model object stores all of the information displayed in the summary output
#we can access the results by interacting with the model object using the $ operator
TtestOutput$statistic #the t statistic of the test
TtestOutput$parameter #the degrees of freedom of the test
TtestOutput$p.value #the p-value of the test
TtestOutput$conf.int #a confidence interval for the mean difference between groups. confidence level determined above by the user
TtestOutput$estimate #the means in the two groups
TtestOutput$null.value #the value of the mean difference between the two groups under the null hypothesis. This should almost always be 0
TtestOutput$stderr #the standard error of the mean difference between groups 
TtestOutput$alternative #two-tailed hypothesis test or one-tailed hypothesis test. determined above by the user
TtestOutput$method #the method used to conduct the t-test. determined above by the user 
TtestOutput$data.name #the variables used in the t-test

#########################################################
#########################################################
#2.	ANOVA with aov()
#########################################################
#########################################################
#ANOVA can be used to compare means between more than two groups
#note that there is a function called anova() that compares SEM models, rather than comparing means between groups. here we use aov()
aovOutput<-aov(WEIGHTLBTC_A~as.factor(HYPEV_MED), #test for differences in the mean of WEIGHTLBTC_A, grouped by the 3 levels of HYPEV_MED and save results as aovOutput. We set HYPEV_MED to be a factor for compatability with the tukeyHSD() function
                    data=df_subset, #the dataset
                    na.action=na.omit) #missing values are removed from the test
aovOutputSummary<-summary(aovOutput) #save a summary the results of ANOVA
aovOutputSummary#print a summary the results of ANOVA. aov is designed specifically to work with summary()
aovOutputSummary<-data.frame(aovOutputSummary[[1]])#remove the list structure from the object for easier use later
colnames(aovOutputSummary)<-c("df", "Sum.Sq", "Mean.Sq", "F", "p")#set column names
aovOutputSummary
#note that the hypothesis test for ANOVA is that there is a significant mean difference between ANY of the groups

#########################################################
#a posthoc test is required to check which two groups are significantly different, after correcting for multiple tests
aovOutput_Tukey<-TukeyHSD(aovOutput)
aovOutput_Tukey#print a summary the results
#group 1 is significantly higher than group 0
#group 2 is significantly higher than group 0
#groups 1 and 2 are not significantly different
#Mean weight is higher among subjects with lifetime high blood pressure and those who are currently using high blood pressure medication 
#Subjects with lifetime high blood pressure and those currently using medication are not significantly different
aovOutput_Tukey<-data.frame(aovOutput_Tukey[[1]])#remove the list structure from the object for easier use later
aovOutput_Tukey
######################################################### 
#NOTE: 
str(aovOutput)#examine the structure of the aovOutput object
#the aov model object stores all of the information displayed in the summary output
#in R aov() is run as a wrapper for the linear regression function lm(), meaning that it performs its computations using lm(), but restructures them for the user
#ANOVA (comparing mean y among groups of x) and linear regression (y regressed on dummy codes for groups of x) are equivalent 
#aov() restructures some of the output from lm when passed to summary()
#some of the raw output that is stored in aovOutput corresponds to the lm() way of thinking about the analysis

#########################################################
#########################################################
#3.	Chi Square test with chisq.test()
#########################################################
#########################################################
#the Chi Square can be used to compare the proportion of subjects falling into each cell of a crosstabulation of two variables
#Specifically, the test compares the observed count in each cell to the count that would be expected if there was no relationship between the two variables
chisqOutput<-chisq.test(x=as.factor(df_subset$BMICAT_A),
           y=as.factor(df_subset$HYPEV_A))
chisqOutput#print a summary the results of the chi square test
#there is an association between categorical BMI and lifetime high blood pressure
#to find out where the significant difference in cell proportions lies, additional testing would be necessary 

chisqOutput$statistic #the chi-square statistics
chisqOutput$parameter #the degrees of freedom of the test
chisqOutput$p.value #the p-value of the test
chisqOutput$method #the method used to conduct the t-test
chisqOutput$data.name #the variables in the chisquare test
chisqOutput$observed #the observed contigency table
chisqOutput$expected #the contingency table expected under the null hypothesis
chisqOutput$residuals #pearson residuals : (observed - expected) / sqrt(expected)
chisqOutput$stdres #standardized residuals :  (observed - expected) / sqrt(V), where V is the residual cell variance 

#########################################################
#########################################################
#4.	Generating tables 
#########################################################
#########################################################
library(psych)
library(stargazer)
library(sjPlot)
library(ggplot2)

#t-test
######################################################### 
#first, we'll pull some descriptive information from describe()
#the t-test output includes group means, but not the standard deviation or SE of those means
TtestTable<-rbind(#combine the following arguments by row and save as new object TtestTable
describe(df_subset$WEIGHTLBTC_A[df_subset$HYPEV_A==0]), # run describe on WEIGHTLBTC_A where HYPEV_A==0
describe(df_subset$WEIGHTLBTC_A[df_subset$HYPEV_A==1])) # run describe on WEIGHTLBTC_A where HYPEV_A==1
TtestTable#print the full output of describe

#########################################################
#add a column for grouping variable name
TtestTable<-data.frame(
  c("Weight, No Lifetime HBP", "Weight, Yes Lifetime HBP"), #values of the grouping variable column
  TtestTable) #the existing table to append the new column to
colnames(TtestTable)[1] <-c("GroupingVar") #rename the new column

#elements of the table can be extracted if all descriptive statistics are not needed
TtestTable<-TtestTable[,c("GroupingVar","n", "mean", "sd", "se")]
TtestTable#print the new output

#########################################################
#next, extract the results from the t-test. 
#here we will create a tabl that combines the means table (TtestTable) with inferential statistics from the t-test (TtestTable_hypothTest)
#the means table has two rows, so the inferential statistics table also needs to have two rows
#R can't combine two tables if they dont have the same number of rows
#we add a blank space next to each value in TtestTable_hypothTest so that the table will have 2 rows, like TtestTable
#note that we also use round(,digits=) to determine the number of digits to display for each value
TtestTable_hypothTest<-data.frame(c(round(TtestOutput$statistic,digits=2), ""), #the t statistic of the test
                                  c(round(TtestOutput$parameter, digits=2), ""), #the degrees of freedom of the test
                                  c(round(TtestOutput$p.value, digits=3), "")) #the p value of the test
colnames(TtestTable_hypothTest) <-c("t", "df", "p") #rename the new columns

TtestTable<-data.frame(TtestTable, TtestTable_hypothTest) #combine descriptive means with the hypothesis test results
#remove the temporary object TtestTable_hypothTest
rm(TtestTable_hypothTest)

#########################################################
#display the table in the viewer before exporting
tab_df(TtestTable) #note that a p-value of 0 here indicates that it is <0.001 (the number of digits we round to). p-values are never actually 0 

#export the custom table with stargazer
stargazer(TtestTable, #the object to create a table from
          type="html", #html format to export into a Word document
          summary=FALSE, #export the data.frame as-is. There are a variety of summary functions that can also be performed instead
          out="TtestTable.doc")#the output file

#ANOVA
######################################################### 
#The approach we used to generate a table for the t-test can be repeated with more groups to generate a similar table for the ANOVA
ANOVATable1<-rbind(#combine the following arguments by row and save as new object TtestTable
  describe(df_subset$WEIGHTLBTC_A[df_subset$HYPEV_MED==0]), # run describe on WEIGHTLBTC_A where HYPEV_A==0
  describe(df_subset$WEIGHTLBTC_A[df_subset$HYPEV_MED==1]), # run describe on WEIGHTLBTC_A where HYPEV_A==1
  describe(df_subset$WEIGHTLBTC_A[df_subset$HYPEV_MED==2])) # run describe on WEIGHTLBTC_A where HYPEV_A==2
ANOVATable1#print the full output of describe

#########################################################
#add a column for grouping variable name
ANOVATable1<-data.frame(
  c("Weight, No HBP", "Weight, HBP", "Weight, HBP Med"), #values of the grouping variable column
  ANOVATable1) #the existing table to append the new column to
colnames(ANOVATable1)[1] <-c("GroupingVar") #rename the new column
#elements of the table can be extracted if all descriptive statistics are not needed
ANOVATable1<-ANOVATable1[,c("GroupingVar","n", "mean", "sd", "se")]
ANOVATable1#print the new output

#########################################################
#the results of the hypothesis test are stored in aovOutputSummary, which we extracted using summary() above
aovOutputSummary
#if all columns in the data.frame are numeric, we can use round() on all columns of a data.frame simulataneously
aovOutputSummary<-round(aovOutputSummary, digits=2)
#we can also replace the NA in columns F and p with blank cells 
aovOutputSummary[2,"F"] <- "" #replace row 2 column F with blank
aovOutputSummary[2,"p"] <- "" #replace row 2 column p with blank
#labels for the rows are currently stored as row.names, which will not be included in the table on export
row.names(aovOutputSummary)
#add a new column to label the rows
aovOutputSummary<-data.frame(c("HBP", "Residual"),
                             aovOutputSummary)
colnames(aovOutputSummary)[1] <- c("Variable Name")#rename the new column

#########################################################
#finally, we format the results of the tukey test for export
aovOutput_Tukey
aovOutput_Tukey<-round(aovOutput_Tukey, digits=2) #round the values in the table
#create a new column that labels the comparisons in the tukey table
Comparison<-c("HBP VS No HBP", 
               "HBP Med VS No HBP", 
               "HBP VS HBP Med")
aovOutput_Tukey<-data.frame(Comparison,
                            aovOutput_Tukey)
#########################################################
#display the table in the viewer before exporting
#note that a p-value of 0 here indicates that it is <0.001 (the number of digits we round to). p-values are never actually 0 
tab_df(ANOVATable1) 
tab_df(aovOutputSummary)
tab_df(aovOutput_Tukey)

#########################################################
#export the custom table with stargazer
stargazer(ANOVATable1, #the object to create a table from
          type="html", #html format to export into a Word document
          summary=FALSE, #export the data.frame as-is. There are a variety of summary functions that can also be performed instead
          out="ANOVATable1.doc")#the output file
stargazer(aovOutputSummary, #the object to create a table from
          type="html", #html format to export into a Word document
          summary=FALSE, #export the data.frame as-is. There are a variety of summary functions that can also be performed instead
          out="aovOutputSummary.doc")#the output file
stargazer(aovOutput_Tukey, #the object to create a table from
          type="html", #html format to export into a Word document
          summary=FALSE, #export the data.frame as-is. There are a variety of summary functions that can also be performed instead
          out="aovOutput_Tukey.doc")#the output file


######################################################### 
#Chi square
#there is a convenient function in sjPlot to generate crosstabulations
tab_xtab(var.row=df_subset$BMICAT_A,  #variable on rows
          var.col=df_subset$HYPEV_A, #variable on columns
         var.labels=c("Categorical BMI", "Lifetime HBP"), #labels for the two variables
         value.labels = list(
           c("Underweight","Healthy","Overweight","Obese"), #value labels for variable on rows
           c("No", "Yes")), #value labels for variable on columns
         show.obs=TRUE, #show observed frequencies
         show.exp = TRUE, #show expected frequencies under the null hypothesis
         show.col.prc = TRUE, #show column percentage
         statistics = "pearson", #changes the correlation statistic for summary. Pearson is not the best choice here because items are not continuous, but used to speed up computation. Kendall or spearman is common for ordinal data
         show.summary=TRUE) #display chi square statistic, df, and p along the bottom row

#run again with "file=" argument to export
tab_xtab(var.row=df_subset$BMICAT_A,  #variable on rows
         var.col=df_subset$HYPEV_A, #variable on columns
         var.labels=c("Categorical BMI", "Lifetime HBP"), #labels for the two variables
         value.labels = list(
           c("Underweight","Healthy","Overweight","Obese"), #value labels for variable on rows
           c("No", "Yes")), #value labels for variable on columns
         show.obs=TRUE, #show observed frequencies
         show.exp = TRUE, #show expected frequencies under the null hypothesiss
         show.col.prc = TRUE, #show column percentage
         statistics = "pearson", #changes the correlation statistic for summary. Pearson is not the best choice here because items are not continuous, but used to speed up computation. Kendall or spearman is common for ordinal data
         show.summary=TRUE, #display chi square statistic, df, and p along the bottom row
         file="ChiSquareTable.doc")

#########################################################
#########################################################
#5.	Visualizing results with sjPlot 
#########################################################
#########################################################
#########################################################
#Error messages in this section are harmless ****

######################################################### 
#T-test
TtestPlot<-plot_grpfrq(var.cnt=df_subset$WEIGHTLBTC_A,  #the outcome
            var.grp=as.factor(df_subset$HYPEV_A), #the grouping variable
            type="boxplot", #generate a box plot
            axis.labels = c("No HBP", "HBP"), #labels for the levels of the grouping variable
            show.legend = FALSE, #do not display the legend
            title="Weight by HBP Status") #plot title
TtestPlot

######################################################### 
#ANOVA
ANOVAplot<-plot_grpfrq(var.cnt=df_subset$WEIGHTLBTC_A,  #the outcome
            var.grp=as.factor(df_subset$HYPEV_MED), #the grouping variable
            type="boxplot", #generate a box plot
            axis.labels = c("No HBP", "HBP", "HBP Med"),#labels for the levels of the grouping variable
            show.legend = FALSE, #do not display the legend
            title="Weight by HBP Medication Status") #plot title
ANOVAplot

######################################################### 
#Chi-square
ChisquarePlot<-plot_xtab(x=df_subset$BMICAT_A, #Variable 1 of the contingency table (x axis)
          grp=df_subset$HYPEV_A, #Variable 2 of the contingency table (legend)
          type="line", #line plot, comment out this line with # and remove the # in front of the next two lines to switch to bar plot
          #type="bar",
          #bar.pos="dodge", #position dodge for non-overlapping bars in bar plot
          show.values = FALSE, #print percentages on plot
          show.n=FALSE, #print n on plot (requires show.values=T)
          show.total=FALSE, #plot an additional line for total (ungrouped) sample
          axis.titles = c("Categorical BMI"), #x axis title
          axis.labels = c("Underweight", "Healthy", "Over", "Obese"), #labels for level of variable x
          legend.labels = c("No HBP", "Yes HBP")) #labels for level of variable grp
ChisquarePlot

######################################################### 
#export the plots
tiff("TtestPlot.tiff", width = 2250, height = 2550, units = 'px', res = 300)# TIFF image device is initialized for export to the current working directory. The width, height, units, and res argument can be adjusted to produce plots of varying size and resolution. These specifications match the journal Drug and Alcohol Dependence, but other journals may require other specifications. 
TtestPlot #Once the image device is initialized, printing the plot with send the plot to the image device. 
dev.off()#close out the device with the dev.off() function.

tiff("ANOVAPlot.tiff", width = 2250, height = 2550, units = 'px', res = 300)# TIFF image device is initialized for export to the current working directory. The width, height, units, and res argument can be adjusted to produce plots of varying size and resolution. These specifications match the journal Drug and Alcohol Dependence, but other journals may require other specifications. 
ANOVAplot #Once the image device is initialized, printing the plot with send the plot to the image device. 
dev.off()#close out the device with the dev.off() function.

tiff("ChiSquarePlot.tiff", width = 2250, height = 2550, units = 'px', res = 300)# TIFF image device is initialized for export to the current working directory. The width, height, units, and res argument can be adjusted to produce plots of varying size and resolution. These specifications match the journal Drug and Alcohol Dependence, but other journals may require other specifications. 
ChisquarePlot #Once the image device is initialized, printing the plot with send the plot to the image device. 
dev.off()#close out the device with the dev.off() function.

#citations for the packages used in this module can be retrieved below
citation("psych")
citation("stargazer")
citation("sjPlot")
citation("ggplot2")
