#########################################################
#########################################################
#vi.	Character Strings, Pattern Matching, and Loops
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

#Descriptions of the 8 variables we will use in this demonstration are provided below
#These variables were used in previous modules

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

####################################################
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
#During the past 12 months, have you DELAYED getting medical care because of the cost?
table(df$MEDDL12M_A) 
# 1 yes, 2 no

####################################################
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
#Subset the 8 variables of interest
df_subset<-df[,c("WEIGHTLBTC_A", "BMICAT_A", "HYPEV_A", "HYPMED_A",
                 "LASTDR_A", "MEDDL12M_A", "PHSTAT_A", "NOTCOV_A")]

#########################################################
#########################################################
#1.	str_split_fixed() and paste() for manipulating strings 
#########################################################
#########################################################
#install.packages("stringr", dependencies = TRUE) #run only once
library(stringr)# str_split_fixed is a function from the stringr package

#str_split_fixed is a useful function for splitting up existing character strings
#str_split_fixed has 3 arguments to take note of:
  #string : the character vector to apply to split to
  #pattern : the character to split the character vector
  #n : the number of times to split the character vector

#########################################################
#as an example, consider the following sequence of characters
"GCTA_GCTA_GCTA"

#we can use str_split_fixed to convert this sequence into 3 elements by splitting on the "_" character
str_split_fixed(string="GCTA_GCTA_GCTA", #the string to split
                pattern="_", #the character to split on
                n=3) # the number of times to split

#if we changed n to 2, we would only split on the first instance of "_" (to create 2 elements)
str_split_fixed(string="GCTA_GCTA_GCTA", #the string to split
                pattern="_", #the character to split on
                n=2) # the number of times to split

#we can split a sequence on any character. for example here, we split on "A" instead of "_". Note that the "A" is removed
str_split_fixed(string="GCTA_GCTA_GCTA", #the string to split
                pattern="A", #the character to split on
                n=3) # the number of times to split

#str_split_fixed is vectorized, such that it can apply the split to multiple strings at once
str_split_fixed(string=c("GCTA_GCTA_GCTA","ATCG_ATCG_ATCG"), #the strings to split, now two strings
                pattern="_", #the character to split on
                n=3) # the number of times to split

#########################################################
#str_split_fixed can be used to quickly changes column names for an entire dataset, if there is a common element to alter
#here, we use str_split_fixed on the column names of df_subset
colnames(df_subset) #a series of strings
#str_split_fixed is vectorized, such that it can apply the split to multiple strings at once
split_colnames<-str_split_fixed(string=colnames(df_subset), #the strings to split (now two strings)
                pattern="_", #the character to split on
                n=2) # the number of times to split
split_colnames #we split on "_", so that A is in its own column
split_colnames[,1] #we can access the first column with the brackets operator

#use the first column of the split as new column names
colnames(df_subset) <- split_colnames[,1]
colnames(df_subset) #the new column names do not include "A"

#########################################################
#the paste0() function can be used to combine strings.
#this can be useful for renaming multiple columns at once, particularly if the column names should be changed in the same way (i.e. adding a string to all columns)
# here, we will add a string to the first four columns to identify that they were previously used in module 4 
colnames(df_subset)[1:4] <- paste0(colnames(df_subset)[1:4], "_Module4")

#and the last 4 columns to identify that they were used in module 5
colnames(df_subset)[5:8] <- paste0(colnames(df_subset)[5:8], "_Module5")

colnames(df_subset)
#########################################################
#########################################################
#2.	Pattern matching with grep() and pattern replacement with gsub()
#########################################################
#########################################################
#the grep() function can be used to identify character elements that match a given pattern
#we will consider 3 arguments to grep() here
#pattern : the pattern to match. Patterns are specified as 'regular expressions'
  #regular expressions are a standard feature in many programming languages. 
    #we will examine exact string matching here, but more information about more nuanced approaches to matching can be found here: https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html
#x : the object to search 
#value : TRUE or FALSE. TRUE to return the actual match, FALSE to return the number corresponding to the match (i.e. the column number)
#########################################################
#Matching variables from Module 4
grep(pattern="_Module4", #pattern to match, any column names including the string "_Module4"
     x=colnames(df_subset), #the object to check for matches, column names of df_subset
     value=TRUE)#retrieve the column name rather than the column number

#Matching variables from Module 5
grep(pattern="_Module5", #pattern to match, any column names including the string "_Module5"
     x=colnames(df_subset), #the object to check for matches, column names of df_subset
     value=TRUE)#retrieve the column name rather than the column number

#Multiple matching arguments can be specified using the | operator to specify "or"
grep(pattern="_Module5|_Module4", #pattern to match, any column names including the string "_Module5"
     x=colnames(df_subset), #the object to check for matches, column names of df_subset
     value=TRUE)#retrieve the column name rather than the column number

#grep() can be entered as an argument to the brackets operator to retrieve a subset of columns without typing out each individual column name
#recall that the brackets operator functions as [ROWS,COLUMNS]
#since we are retrieving columns, we enter the grep() command to the right of the comma

#retrieve all the columns from Module 4 and save as a new object
df_subset_Module4<-df_subset[,grep(pattern="_Module4", x=colnames(df_subset), value=TRUE)]
colnames(df_subset_Module4)

#retrieve all the columns from Module 5 and save as a new object
df_subset_Module5<-df_subset[,grep(pattern="_Module5", x=colnames(df_subset), value=TRUE)]
colnames(df_subset_Module5)

#########################################################
#the gsub() function has similar arguments as grep(), but replaces the pattern with a new string instead of extracting all strings with matching elements
#we will consider 3 arguments to gsub() here
#pattern : the pattern to match. Patterns are specified as 'regular expressions'
#x : the object to search 
#replacement : the string to replace "pattern" with

#replace "Module5" with "5" , and assign as the new column names for df_subset
colnames(df_subset)<-gsub(pattern="Module5", #the pattern to replace
                         x=colnames(df_subset),#the object to search for matches
                         replacement="5") #the replacement for "pattern"
colnames(df_subset)

#replace "Module4" with "4" , and assign as the new column names for df_subset
colnames(df_subset)<-gsub(pattern="Module4", #the pattern to replace
                          x=colnames(df_subset),#the object to search for matches
                          replacement="4") #the replacement for "pattern"
colnames(df_subset)
#########################################################
#########################################################
#3.	for(){} loops
#########################################################
#########################################################

#for loops are a highly flexible way to automate procedures in R
#the general syntax of a for loop is
#for (VARIABLE in RANGE) {PROCEDURE}
#VARIABLE is a value that changes in PROCEDURE. i and j are commonly used, but any character string works fine
#RANGE is the range of values that VARIABLE takes, also defining the number of times the loop repeats
#PROCEDURE is the task that you specify

#########################################################
#consider an example of three equivalent ways to print each individual element of a random sequence of 5 digits

RandomSequence <- sample(1:999, size=5)#generate a sequence of 5 random numbers

#########################################################
#1. without a loop
print(RandomSequence[1])
print(RandomSequence[2])
print(RandomSequence[3])
print(RandomSequence[4])
print(RandomSequence[5])

#########################################################
#2. with a loop
for (i in 1:5){ #variable i will take values from 1 to 5
  print(RandomSequence[i]) #print element i of RandomSequence at each iteration of the loop
  }#close the loop

#########################################################
#3. this loop can also be thought of as an abbreviated way of writing the following sequence of commands
i<-1 #iteration 1
print(RandomSequence[i])
i<-2 #iteration 2
print(RandomSequence[i])
i<-3 #iteration 3
print(RandomSequence[i])
i<-4 #iteration 4
print(RandomSequence[i])
i<-5 #iteration 5
print(RandomSequence[i])

#########################################################
#The range that a loop iterates over can be defined flexibly to make your code adapt to different situations more easily
#for example here we define the range of the loop depending on the number of columns in df_subset 1:ncol(df_subset)
#and then print a table for every column
for ( i in 1:ncol(df_subset)){ #replace i with values 1 through the number of columns in df_subset
  print(colnames(df_subset)[i]) # print the ith column name of df_subset
  print(table(df_subset[,i])) #print a table() of the ith column of df_subset
  print("#####")#sometimes it can be useful to print a constant after each iteration ends for nicer looking output
}

#########################################################
#Aligning columns between data.frames

#for loops can contain multiple data.frames
#it is important to make sure that the columns of multiple data.frames align if using a single variable for multiple data.frames

#########################################################
#first, we create a new data.frame that is a transformation of df_subset
df_subsetPlus10<-df_subset + 10

#imagine that you want to create crosstabs of each variable in df_subset by the transformed version of the variable in df_SubsetPlus10
#first, make sure that the ith column of df_subset corresponds to the correct ith column of df_subsetPlus10

#paste the column names of the two data.frames together
cbind(colnames(df_subset), colnames(df_subsetPlus10)) #check for correspondence along the rows of the output

#knowing that our columns are lined up as we expect, we can proceed to loop over both data.frames in tandem
for ( i in 1:ncol(df_subset)){ #replace i with values 1 through the number of columns in df_subset
  print(colnames(df_subset)[i]) # print the ith column name of df_subset
  print(colnames(df_subsetPlus10)[i]) # print the ith column name of df_subsetPlus10
  print(table(df_subset[,i], df_subsetPlus10[,i])) #print a table() of the ith column of df_subset by the ith column of df_subsetPlus10
  print("#####")#sometimes it can be useful to print a constant after each iteration ends for nicer looking output
}

#########################################################
#if our columns were misaligned, our output would look quite different since i would index different variables in the two data.frames 
#mix up column order
df_subsetPlus10<-df_subsetPlus10[,abs(seq(1:8)-9)]

#try the loop again and we can see that our variables are misaligned, as there are non-zero off-diagonal elements in the crosstabs
for ( i in 1:ncol(df_subset)){ #replace i with values 1 through the number of columns in df_subset
  print(colnames(df_subset)[i]) # print the ith column name of df_subset
  print(colnames(df_subsetPlus10)[i]) # print the ith column name of df_subsetPlus10
  print(table(df_subset[,i], df_subsetPlus10[,i])) #print a table() of the ith column of df_subset by the ith column of df_subsetPlus10
  print("#####")#sometimes it can be useful to print a constant after each iteration ends for nicer looking output
}

#misalignment could be diagnosed by this command
cbind(colnames(df_subset), colnames(df_subsetPlus10)) #check for correspondence along the rows of the output

#this can be a source of coding errors and is something worth checking before iterating over multiple data.frames at once

#########################################################
#Here is a link to some notes about writing efficient loops
#https://www.r-bloggers.com/2018/08/growing-objects-and-loop-memory-pre-allocation/

#and some information about when alternatives to looping might be faster
#https://stackoverflow.com/questions/30240573/are-for-loops-evil-in-r

#citations for the packages used in this module can be retrieved below
citation("stringr")
