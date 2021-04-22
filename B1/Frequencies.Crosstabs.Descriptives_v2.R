#########################################################
#########################################################
#iii. Frequencies, Crosstabs and Descriptives
#########################################################
#########################################################

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

#Descriptions of the 3 variables we will use in this demonstration are provided below

####################################################
#WEARGLSS_A 
#Wear glasses/contact lenses 
#1 Yes
#2 No
#7 Refused
#8 Not Ascertained
#9 Don't Know

#VISIONDF_A 
#Do you have difficulty seeing, even when wearing glasses or contact lenses?
#1 No difficulty
#2 Some difficulty
#3 A lot of difficulty
#4 Cannot do at all
#7 Refused
#8 Not Ascertained
#9 Don't Know

#HEARAID_A  
#Use hearing aid 
#1 Yes
#2 No
#7 Refused
#8 Not Ascertained
#9 Don't Know

#HEARINGDF_A 
#Do you have difficulty hearing even when using your hearing aid(s)?
#1 No difficulty
#2 Some difficulty
#3 A lot of difficulty
#4 Cannot do at all
#7 Refused
#8 Not Ascertained
#9 Don't Know

#First, we subset our 4 variables of interest
df2 <- df[,c("WEARGLSS_A", "VISIONDF_A", "HEARAID_A", "HEARINGDF_A")]

####################################################
#1.	table(), describe(), and count() 
####################################################
#Here we will examine three basic functions for exploring your data

#########################################################
#table()
#table() can be used to create a frequency distribution of a single column
#this may be familiar, as we have used table() in earlier modules as well
#be sure to use the "exclude=NULL" operator to include missing values in the frequency distribution
table(df2$WEARGLSS_A, exclude=NULL)
table(df2$VISIONDF_A, exclude=NULL)
table(df2$HEARAID_A, exclude=NULL)
table(df2$HEARINGDF_A, exclude=NULL)

#we can also save the output of table as a data.frame for later use
#column 1 of the table will list values, column 2 lists frequencies
WEARGLSS_A_Table<-data.frame(table(df2$WEARGLSS_A, exclude=NULL))


#referring to the coding scheme above, we see that these variables include missing values under the codes 7,8, and 9
#we set these to NA and then check the frequency distributions again
df2$WEARGLSS_A[df2$WEARGLSS_A>=7] <- NA
df2$VISIONDF_A[df2$VISIONDF_A>=7] <- NA
df2$HEARAID_A[df2$HEARAID_A>=7] <- NA
df2$HEARINGDF_A[df2$HEARINGDF_A>=7] <- NA

#Note that the missing codes are now grouped under NA
table(df2$WEARGLSS_A, exclude=NULL)
table(df2$VISIONDF_A, exclude=NULL)
table(df2$HEARAID_A, exclude=NULL)
table(df2$HEARINGDF_A, exclude=NULL)

#we can use table() with two variables to generate a crosstab or crosstabulation of two variables
#crosstabs are useful for examining the overlap between the categories of two variables
#significance tests of the differences between the number of people in each cell will be discussed in a later module 

#values of WEARGLSS_A are positioned on the y axis of the table 
#values of VISIONDF_A are positioned on the x axis of the table
#the number of subjects overlapping under each combination of categories is shown in the body of the table
table(df2$WEARGLSS_A, df2$VISIONDF_A, exclude=NULL)
#for example 17450 subjects wear glasses (WEARGLSS_A==1) and do not have difficulty seeing with glasses (VISIONDF_A==1)

#values of HEARAID_A are positioned on the y axis of the table 
#values of HEARINGDF_A are positioned on the x axis of the table
#the number of subjects overlapping under each combination of categories is shown in the body of the table
table(df2$HEARAID_A, df2$HEARINGDF_A, exclude=NULL)
#for example 734 subjects use a hearing aid (HEARAID_A==1) and do not have hearing with a hearing aid (HEARINGDF_A==1)

#Sometimes it is useful to save the frequencies from table() for the purposes of creating a plot
#If a table() object is saved as a data.frame, it will display values of the variable in one column and the associated frequencies in another
WEARGLSS_A_Table<-data.frame(table(df2$WEARGLSS_A, exclude=NULL))
WEARGLSS_A_Table#the values of WEARGLSS_A are in column 1, the associated frequencies are in column 2

#Frequencies associated with combinations of values can also be saved in this format by creating a data.frame from crosstabs
WEARGLSS_A_by_WEARGLSS_A_Table<-data.frame(table(df2$WEARGLSS_A, df2$VISIONDF_A, exclude=NULL))
WEARGLSS_A_by_WEARGLSS_A_Table#Values of WEARGLSS_A are in column 1, values of VISIONDF_A are in column 2, the associated frequencies are in column 3

#########################################################
#count()
#count() from the plyr package produces similar output as data.frame(table())
#however, count() is written to be more efficient that table() in some cases
#it is recommended to use this format if constructing tables to display conditional overlap between many variables

#remove the hashtag and run the line below only once
#install.packages("plyr", dependencies = TRUE)#install plyr and the packages that it depends on

library(plyr)#load plyr

WEARGLSS_A_by_WEARGLSS_A_Table2<-count(df2, #where the variables are stored
                                      vars=c("WEARGLSS_A", "VISIONDF_A")#the variables to pull from df2
                                      )#close the command

#count() also produces output that is labeled better than data.frame(table()) and excludes combinations where the frequency is 0
#see below
WEARGLSS_A_by_WEARGLSS_A_Table
WEARGLSS_A_by_WEARGLSS_A_Table2

#this procedure also works for tables of a single variable
WEARGLSS_A_Table2<-count(df2, vars=c("WEARGLSS_A"))
WEARGLSS_A_Table2

#we will return to these tables later when generating data visualizations
#for now, we will keep the output of count() and delete the output of data.frame(table())
rm(WEARGLSS_A_by_WEARGLSS_A_Table)
rm(WEARGLSS_A_Table)

#########################################################
#install.packages("psych", dependencies = TRUE)#install psych and the packages that it depends on
library(psych)#load psych

#describe()
#describe() from the psych package can be used to calculate descriptive statistics for many variables at once
#describe() will generate descriptive statistics for any variable, it is recommended to be cautious to use this function for numeric, rather than categorical, variables
#by default, describe will output mean, sd, median, trimmed mean, median absolute deviation from the median, minimum, maximum, skew, kurtosis, and standard error
describe(df2)#generate descriptives for the variables in df2

#note that WEARGLSS_A and HEARAID_A are binary and should be excluded from calculation of descriptive statistics
#VISIONDF_A and HEARINGDF_A are ordinal, but for the purposes of this demonstration we will treat them as continuous variables
DescriptivesTable<-describe(df2[,c("VISIONDF_A", "HEARINGDF_A")])
DescriptivesTable#describe() automatically generates output in data.frame form

#columns from the descriptives table can be extracted using the brackets operator
DescriptivesTable<-DescriptivesTable[,c("n", "mean", "sd")]
DescriptivesTable

####################################################
#2.	sjPlot and stargazer packages for creating and exporting tables and plots
####################################################
#sjPlot is a convenient package for creating nicely formatted tables and plots
#sjPlot relies on ggplot2 to perform its functions, and so we will install that here as well
#sjPlot functions are designed with very specific kinds of tables or plots in mind
#there is limited flexibility with this workflow, but the functions are more user-friendly than some alternatives

#remove the hashtag and run this only once
#install.packages(c("sjPlot", "ggplot2"), dependencies = T)
#

library(sjPlot)
library(ggplot2)

#########################################################
#tab_stackfrq() can be used to generate frequency tables for multiple variables that have the same response options

#frequency table for variables with the same response options
APA_freqTable<-tab_stackfrq(df2[,c("VISIONDF_A", "HEARINGDF_A")], #identify the variables to be tabelled
             var.labels = c("Vision Difficulty", "Hearing Difficulty"), #provide labels for the tables
             value.labels = c("No difficulty","Some difficulty","A lot of difficulty","Cannot do at all"), #provide labels for the values of the variables
             show.n=TRUE)#show N, in addition to percentage
APA_freqTable#the table is shown in the "Viewer" window in the bottom right corner

#if we add the "file=" argument, we can export the table out of R as an html file
#the file can be opened with a word processor like Word
APA_freqTable<-tab_stackfrq(df2[,c("VISIONDF_A", "HEARINGDF_A")], #identify the variables to be tabelled
                            var.labels = c("Vision Difficulty", "Hearing Difficulty"), #provide labels for the tables
                            value.labels = c("No difficulty","Some difficulty","A lot of difficulty","Cannot do at all"), #provide labels for the values of the variables
                            show.n=TRUE,#show N, in addition to percentage
                            file="APA_freqTable.doc")#a file name for the exported table object, to be opened with a word processor like Word

#########################################################
#sjPlot is also a convenient package for creating data visualizations
#there are many functions to quickly generate plots
#plot_frq() can be used to produce bar charts
#sjPlot uses the package ggplot2 to create visualizations, which we will discuss further in the next section
#generate a histogram of HEARINGDF_A
BarPlot<-plot_frq(df2$HEARINGDF_A,
         title="Hearing Difficulty", #the title of the plot
         type="bar", #the type of plot. other options include "bar", "dot", "histogram", "line", "density", "boxplot", "violin"
         axis.title = "Category",#the axis title
         axis.labels = c("No difficulty","Some difficulty","A lot of difficulty","Cannot do at all"))#the labels of the values of HEARINGDF
BarPlot#entering the plot object into the console will display it in the Plots tab in the lower right corner

#the bar plot can be exported by calling a graphics function
#TIFF files are lossless, meaning that they do not compress the image but maintain the highest image quality. This results in the larger file size. 
#Journals will often request TIFF files. PNG and JPG are other options that will result in smaller file size, but lower image quality. 
tiff("BarPlot1.tiff", width = 2250, height = 2550, units = 'px', res = 300)# TIFF image device is initialized for export to the current working directory. The width, height, units, and res argument can be adjusted to produce plots of varying size and resolution. These specifications match the journal Drug and Alcohol Dependence, but other journals may require other specifications. 
BarPlot #Once the image device is initialized, printing the plot with send the plot to the image device. 
dev.off()#close out the device with the dev.off() function.


#tab_stackfrq() and plot_frq() are just two examples of sjPlot functions
#often, there will be a function designed to match the table or plot you are looking for
#more information about these functions and the sjPlot package as a whole can be obtained using the following commands
?sjPlot
?plot_frq
?tab_stackfrq

?plot_model()

#########################################################
#stargazer
#stargazer is a more flexible package for producing tables
#any data.frame object can be converted into an APA format table using stargazer

#remove the hashtag and run this only once
#install.packages("stargazer", dependencies=TRUE)
#

library(stargazer)
stargazer(DescriptivesTable, #the object to create a table from
          type="html", #html format to export into a Word document
          summary=FALSE, #export the data.frame as-is. There are a variety of summary functions that can also be performed instead
          out="DescriptivesTable.doc")#the output file

#open DescriptivesTable.doc in your working directory to see the table

####################################################
#3.	Introduction to ggplot2 for visualizing data
####################################################

#sjPlot generates plots by calling the package ggplot2
#using ggplot2 directly allows for more flexibility in plot generation


#ggplot uses the following general structure to form commands 

#ggplot(DATA, aes(AESTHETICS)) + GEOMETRY_FUNCTION()
#DATA is the data to plot. ggplot2 generally works better with summaries of your data than the raw data itself
#AESTHETICS are used to map the x axis, y axis, and legend of your plot
#GEOMETRY_FUCTION determines the kind of plot that ggplot will generate. There are different geometry functions for different plots 

#########################################################
#first, we will generate a bar plot that is similar to the one we generated with sjPlot
#first, create a summary of your data to use as input

HEARINGDF_A_Frequencies<-count(df2$HEARINGDF_A)
HEARINGDF_A_Frequencies
#if you want to include the missing values, this value must be changed from NA or ggplot will remove the row
HEARINGDF_A_Frequencies[5,1] <- "Missing" #Change row 5 column 1 from NA to "Missing"
HEARINGDF_A_Frequencies

#########################################################
#A basic bar plot
ggplot(HEARINGDF_A_Frequencies, #the data to plot
       aes(x=x, y=freq))+ #plot column "x" on the x axis and column "freq" on the y axis
       geom_bar(stat="identity") #use a bar plot and plot the data without transformation or aggregation

#raw data can be supplied to ggplot2 if stat="identity" is changed to stat="bin" and the argument for the y axis in aes() is removed
#most ggplot2 functions work easier with summary data though, so for the sake of illustration all plots will be generated from summaries

#########################################################
#adding labels and colors to the bar plot

#this defines a new object "apatheme" that contains a series of options to generate a plot in APA format
#run these commands and add this to any plot
#from https://stackoverflow.com/questions/60591014/r-add-tweaks-to-interaction-plot-with-ggplot
apatheme<-theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.title=element_blank())

#ggplot2 indexes colors with numeric codes
#below is a sequence of color codes that are colorblind friendly, recommended for use in publications
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#E69F00", "#999999")


#to plot labels instead of numeric codes on the x axis, we edit the data that we are plotting
#first, convert numeric values to text labels

HEARINGDF_A_Frequencies[1, "x"] <- "No difficulty" #row 1, column x
HEARINGDF_A_Frequencies[2, "x"] <- "Some difficulty" #row 2, column x
HEARINGDF_A_Frequencies[3, "x"] <- "A lot of difficulty" #row 3, #column x 
HEARINGDF_A_Frequencies[4, "x"] <- "Cannot do at all" #row 4, column x

#next, convert the x column to a factor. 
#By default, ggplot2 would reorder the bars in the plot by alphabetical order if it were a character vector. 
#To keep an order that aligns with the labels numeric value, specify the levels of the factor in the desired order
HEARINGDF_A_Frequencies$x<-factor(HEARINGDF_A_Frequencies$x, 
                                  levels=c("No difficulty","Some difficulty","A lot of difficulty","Cannot do at all", "Missing"))


#other options can be defined in the call to ggplot()
BarPlot2<-ggplot(HEARINGDF_A_Frequencies, #the data to plot
       aes(x=x, y=freq, fill=x))+ #plot column "x" on the x axis and column "freq" on the y axis, fill color of bars by levels of x
  geom_bar(stat="identity")+#use a bar plot and plot the data without transformation or aggregation
  ggtitle("Frequency Distribution")+ #main title
  xlab("Difficulty Hearing")+ #x axis label
  ylab("Frequency")+#y axis label
  scale_fill_manual(values=cbPalette)+#use cbPalette to define colors associated with the levels of x
  apatheme #apatheme options, from above
BarPlot2

tiff("BarPlot2.tiff", width = 2250, height = 2550, units = 'px', res = 300)# TIFF image device is initialized for export to the current working directory. The width, height, units, and res argument can be adjusted to produce plots of varying size and resolution. These specifications match the journal Drug and Alcohol Dependence, but other journals may require other specifications. 
BarPlot2 #Once the image device is initialized, printing the plot with send the plot to the image device. 
dev.off()#close out the device with the dev.off() function.


#a quick guide to many different options in ggplot can be found here 
#https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

#citations for the packages used in this module can be retrieved below
citation("plyr")
citation("psych")
citation("sjPlot")
citation("ggplot2")
citation("stargazer")
