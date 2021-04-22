#*Warning : This module discusses depression and focuses on related variables for practice examples * 

#########################################################
#########################################################
#ii.	Manipulating and recoding data in R 
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


#Before proceeding, we will generate a unique id number for each row in the data.frame
#this column will be used later in the "Merging data sets" section
#often, datasets will already have a unique identifier for each row
#this dataset does not, so we will use the row names as the ID for each row
as.numeric(row.names(df)) #print row names and convert to numeric
df$ID<-as.numeric(row.names(df))# save numeric row names as a new column in df, ID
df$ID#print the new ID column

#Descriptions of the 3 variables we will use in this demonstration are provided below

####################################################
#DEPFREQ_A
#How often do you feel depressed? Would you say daily, weekly, monthly, a few times a year, or never?
table(df$DEPFREQ_A) #table() prints the frequency distribution of a variable
#1 Daily
#2 Weekly
#3 Monthly
#4 A few times a year
#5 Never
#7 Refused
#8 Not Ascertained
#9 Don't Know

####################################################
#DEPMED_A
#Do you take prescription medication for depression?
table(df$DEPMED_A)
#1 Yes
#2 No
#7 Refused
#8 Not Ascertained
#9 Don't Know

####################################################
#DEPLEVEL_A
#Thinking about the last time you felt depressed, how depressed did you feel?
#Would you say a little, a lot, or somewhere in between?
table(df$DEPLEVEL_A)
#1 A little
#2 A lot
#3 Somewhere in between a little and a lot
#7 Refused
#8 Not Ascertained
#9 Don't Know
#Sample adults 18+ who feel depressed daily, weekly, monthly, a few times a year or 
#refused or don't know how often they feel depressed OR 
#who do take medication or refused or don't know if they take medication for depression.

#########################################################
#########################################################
#1.	using the brackets operator
#########################################################
#########################################################

#Recall this example of the basic format of the brackets operator from the previous module 
#########################################################
#the rows and columns of a data.frame can be accessed by using the brackets operator
df[1,]#prints the first row of the data.frame
df[,1]#prints the first column of the data.frame (URBRRL)
df[1,1] #[1,1] prints the cell located at the first row and first column
df[,"URBRRL"]#columns can also be called by name, rather than number. URBRRL is the first column of the data.frame
#########################################################

#Our variables of interest are DEPFREQ_A, DEPMED_A, DEPLEVEL_A
#Not knowing the column number associated with each variable, we can use column names to call each variable from the df
df[,"DEPFREQ_A"]
df[,"DEPMED_A"]
df[,"DEPLEVEL_A"]

#For this example, we will save a new data.frame that includes only these three variables
#save a new object, df2, that is a dataframe containing the element in parentheses separated by commas
df2<-data.frame( 
df[,"ID"],#or equivalently df2$ID
df[,"DEPFREQ_A"], #or equivalently df2$DEPFREQ_A
df[,"DEPMED_A"], #or equivalently df2$DEPMED_A
df[,"DEPLEVEL_A"]) #or equivalently df2$DEPLEVEL_A
colnames(df2) <-c("ID", "DEPFREQ_A", "DEPMED_A", "DEPLEVEL_A")#reassign column names to df 2
head(df2)#print the first 6 rows of df2

#########################################################
#first, we will use the brackets operator to recode DEPFREQ_A
#5=Never in the initial coding scheme, we will set these codes to 0 (0=Never)
table(df2$DEPFREQ_A) #check before recoding
df2$DEPFREQ_A[df2$DEPFREQ_A==5] <- 0 #extract DEPFREQ_A from df2 with $ operator and identify the rows that are equal to 5
#note here that the brackets operator does not use a comma since df2$DEPFREQ_A has only one dimension
table(df2$DEPFREQ_A) #check after recoding

#########################################################
#next, we will use the brackets operator to assign NA to the missing value codes in our three variables
#NA is a unique code in R which is used to indicate missing data
#generally, NA values are excluded from the operations of a function

#instead of "==NA" R uses the following syntax to index missing values in an existing column: is.na()
is.na(df2$DEPFREQ_A)#for example
table(is.na(df2$DEPFREQ_A))#note that there are no NA values in this column. Currently, missings are assigned a numeric code
table(is.na(df2$DEPMED_A))#also no missing codes here
table(is.na(df2$DEPLEVEL_A))#17781 missings on this variable because of the survey structure: 
  #Sample adults 18+ who feel depressed daily, weekly, monthly, a few times a year or 
  #refused or don't know how often they feel depressed OR 
  #who do take medication or refused or don't know if they take medication for depression.

#On each variable, values 7,8, and 9 indicate different kinds of missingness 
#Here, we assign these codes to missing with the greater than (>=) operator
#when filling in NA for existing values, use "NA"
df2$DEPFREQ_A[df2$DEPFREQ_A>=7] <- NA
df2$DEPMED_A[df2$DEPMED_A>=7] <- NA
df2$DEPLEVEL_A[df2$DEPLEVEL_A>=7] <- NA

#examine our newly recoded variables
#NA codes can be included in a frequency distribution by specifying exclude=NULL as an argument to table()
table(df2$DEPFREQ_A, exclude=NULL) #636 set to missing
table(df2$DEPMED_A, exclude=NULL) #522 set to missing
table(df2$DEPLEVEL_A, exclude=NULL) #(17991-17781)=210 set to missing 

#We can check that our recoding worked as intended by checking the original data
table(df$DEPFREQ_A >= 7) #636
table(df$DEPMED_A >= 7) #522 
table(df$DEPLEVEL_A >= 7)#210

#########################################################
#finally, we can use the brackets operator to create subsets of our data
#For example, here imagine that we want to create a new data.frame that only includes subjects who 
#report "Yes" for "Do you take prescription medication for depression?" (DEPMED_A==1)
df2_OnlyRxMeds<-df2[df2$DEPMED_A==1,] #create new data.frame (df2_OnlyRxMeds), taking only the rows of df2 where DEPMED_A==1
#or, those who report "No" for "Do you take prescription medication for depression?" (DEPMED_A==2)
df2_NoRxMeds<-df2[df2$DEPMED_A==2,] #create new data.frame (df2_NoRxMeds), taking only the rows of df2 where DEPMED_A==2
#recall that entries on the left side of the comma in brackets indexes "rows"
#note that this new data.frame has fewer rows, since only subjects who report "Yes" for "Do you take prescription medication for depression?" are included
nrow(df2)
nrow(df2_OnlyRxMeds)
nrow(df2_NoRxMeds)

#########################################################
#########################################################
#2.	ifelse() and conditional logic/operators
#########################################################
#########################################################
#ifelse() is a useful function for recoding data and can be used as an alternative to the brackets operator
#ifelse() has three arguments
#ifelse( CONDITION , YES , NO)
#CONDITION is the circumstance you want to investigate in the data
#YES is what ifelse returns if the CONDITION is TRUE
#NO is what ifelse returns if the CONDITION is FALSE


#########################################################
#Many functions in R are vectorized
#Vectorized functions automatically carry out their procedures across all values of a column, rather than just a single value
#The vectorized functionality of ifelse can be used to manipulate data
#In this example, we will used data columns to define the condition

#per survey design, most subjects who report "Never" being depressed (DEPFREQ_A==0)
#are  not asked about their level of Depression DEPLEVEL_A
#In this example, we reassign these subjects to be 0 for level of depression
df2$DEPLEVEL_A_recode1<-ifelse(df2$DEPFREQ_A==0, #CONDITION: if depression frequency is 0 (Never) 
                              0, #YES: Pass a 0 to the new object
                              df2$DEPLEVEL_A)#NO: otherwise, keep the current value of depression level
table(df2$DEPLEVEL_A, exclude=NULL)
table(df2$DEPLEVEL_A_recode1,exclude=NULL)#many subjects that were NA are now 0
#but we also have moved people from categories 1, 2, and 3 to 0

#we can use two conditions at once for better control over the behavior of the ifelse statetment
#in this case, we specify that we only want to pass a 0 to the new object if depression frequency is never AND the existing depression level item is missing so that we only overwrite NA values
#Subjects that choose not to answer depression level may still
df2$DEPLEVEL_A_recode2<-ifelse(df2$DEPFREQ_A==0 & is.na(df2$DEPLEVEL_A), #CONDITION: if depression frequency is 0 (Never) and the existing depression level item is NA
                              0, #YES: Pass a 0 to the new object
                              df2$DEPLEVEL_A)#NO: otherwise, keep the current value of depression level
table(df2$DEPLEVEL_A_recode2, exclude=NULL)
table(df2$DEPLEVEL_A, exclude=NULL)#note that we only convert NAs to 0s here, leaving the 1s, 2s, and 3s unaffected 

#########################################################
#########################################################
#3. Merging data sets 
#########################################################
#########################################################
#datasets that have a common index (same subjects, same families, etc) can be merged using the merge() command
#merge will line up the rows of your data.frames by the index provided to "by="

#########################################################
#first, we make sure there are no empty rows in the data.frames we use for this demonstration
df2_OnlyRxMeds<-df2_OnlyRxMeds[!is.na(df2_OnlyRxMeds$ID),]#retain only the rows that are NOT missing for ID
df2_NoRxMeds<-df2_NoRxMeds[!is.na(df2_NoRxMeds$ID),]#retain only the rows that are NOT missing for ID
nrow(df2_OnlyRxMeds)
nrow(df2_NoRxMeds)
#########################################################
#merge the two data.frames we created earlier: df2_OnlyRxMeds, df2_NoRxMeds
MergedDF_allXallY<-merge(x=df2_OnlyRxMeds, #First data.frame, x
                         y=df2_NoRxMeds, #second data.frame, y
                         by="ID", #index to merge by, uniquely identifies rows
                         all.x=TRUE, #keep all rows of x, regardless of whether there is a match in y
                         all.y = TRUE) #keep all rows of y, regardless of whether there is a match in x
nrow(MergedDF_allXallY)#full sample that has data for RX meds item

#########################################################
#if we use "all.x = TRUE" and "all.y = FALSE" we only retain subjects that are included in df x
#these two data.frames are mutually exclusive, i.e. they do not share any rows, so "all.y=FALSE" will cause ALL rows of data.frame y to be dropped
MergedDF_allX<-merge(x=df2_OnlyRxMeds, #First data.frame, x
                         y=df2_NoRxMeds, #second data.frame, y
                         by="ID", #index to merge by, uniquely identifies rows
                         all.x=TRUE, #keep all rows of x, regardless of whether there is a match in y
                         all.y = FALSE) #Only keep rows of y if there is a match in x

#Since the two data.frames we are merging do not overlap, our merged file with "all.y=FALSE" is the same as the original x data.frame
nrow(MergedDF_allX) == nrow(df2_OnlyRxMeds)

#########################################################
#This can also be demonstrated with all.x=FALSE
MergedDF_allY<-merge(x=df2_OnlyRxMeds, #First data.frame, x
                     y=df2_NoRxMeds, #second data.frame, y
                     by="ID", #index to merge by, uniquely identifies rows
                     all.x=FALSE, #Only keep rows of x if there is a match in y
                     all.y = TRUE) #keep all rows of y, regardless of whether there is a match in x

#Since the two data.frames we are merging do not overlap, our merged file with "all.y=FALSE" is the same as the original x data.frame
nrow(MergedDF_allY) == nrow(df2_NoRxMeds)

#########################################################
#In summary
#all.x=TRUE, all.y = TRUE : Keep all rows, regardless of whether they are shared between the two data.frames
#all.x=TRUE, all.y = FALSE : Keep all of x and the rows of y that are also included in x
#all.x=FALSE, all.y = TRUE : Keep all of y and the rows of x that are also included in y
#all.x=FALSE, all.y = FALSE : Keep only the rows of x and y that are shared between the data.frames

#########################################################
#########################################################
#4. Saving data  Write.table
#########################################################
#########################################################
#data.frames can be saved to an external directory using write.table()
getwd()#recall from the previous module that getwd() will print your current working directory
#this is where R will write files to by default

write.table(testDF, #the object to be exported
            "testDF.csv", #the file name
            sep=",", #the character that separates values in the file. We are exporting a .csv, so we use a comma here
            row.names=FALSE)#Adds the row.names of the data.frame as a column. This will usually offset column names from their corresponding column. It is recommended to set this to FALSE when exporting data

list.files() #this function lists the files that are included in your current working directory. Your new file should be listed here after export



