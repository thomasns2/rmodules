#R Programming Basics

#########################################################
#The R studio interface
#########################################################
#Scripts are in the upper left window, This is a record of your code and can (should!) be saved as its own file

#The console is in the lower left window. Code entered here is run by R. 
##There are other options for using this window, but we will mostly use the console. The terminal is used for establishing remote connections. 

#The upper right window displays your global environment. Data objects that you read in or create can be seen here.
##There are other options for using this window, but we will always use the "Environment" tab in these tutorials

#The lower right window can display a variety of different things that we will use throughout the tutorials: 
##Files displays the current working directory. These are the files that are immediately available to R for reading in
##Plots will display graphs or plots that we generate in the tutorials
##Packages provides a list of the packages that are installed, and also an easy interface for installing additional packages
##Help can be used to identify the documentation for the various functions and packages that you use

#########################################################
#########################################################
#working directories
#########################################################
getwd() # this function can be used to find your current working directory
#this is where R is looking to find files

list.files() #this function lists the files that are included in your current working directory
#this information can also be found under Files in the bottom right window of Rstudio

#note, that your working directory will be different. Refer to the output of getwd() for your current directory
#The error message "Cannot change working directory" indicates that the directory path is not correct
#the command that was entered last can be recalled and edited by pressing the "up" arrow in the bottom left window (console)
setwd("/Users/thomasns/Documents/0DevInternship/Modules") #this function changes your working directory
list.files() #note here, the list of files is different after changing the directory

setwd("/Users/thomasns/Documents/0DevInternship/Modules/Modules") #this directory contains the data we will work with

#complete file paths can also be used to identify files
"/Users/thomasns/Documents/0DevInternship/Modules/Modules/adult19.csv" #this would identify our dataset from any working directory
"adult19.csv"# since we have set a working directory, we can use an abbreviated file name

#########################################################
#########################################################
#reading in data 
#https://www.cdc.gov/nchs/nhis/2019nhis.htm
#the data can be downloaded from the link above
#the downloaded file should be saved in the directory specified above in setwd()
#########################################################

#Commands can be spread across multiple lines to make scripts easier to read
#Below, the read.table() command is spread across 3 lines, 1 line for each argument
#The command is closed by the closing parentheses ")" following "header=TRUE"
####################################################
#R is an object-based programming language. An R session will generally consist of reading, manipulating, and saving various data objects
#Here, we read in the dataset as a new object called "df"
#after running the three lines of code, refer to the upper right window to see your new data object
df<-read.table("adult19.csv", #the dataset file in the current working directory
               sep=",", #the character that separates values in the file (a comma here, because the file is csv)
               header=TRUE) #does the file have column names as the first row?

####################################################
#it is often a good idea to check a few things about your dataset after reading it in
nrow(df)#the number of rows (usually subjects) 31997
ncol(df)#the number of columns (usually variables) 534
head(df) #the first row of the dataset (or first several rows, depending on number of columns)
colnames(df) #the column names of the dataset

#########################################################
#########################################################
#installing/loading packages
#########################################################

####################################################
#packages provide additional functionality and are an important part of working in R
#useful packages can often be identified by searching online 
#R is open source and many researchers are involved in package development, there will often be an R package already written that is designed to assist with whatever specific tasks or analyses are of interest

install.packages("psych", dependencies = TRUE)#this function installs the package psych, along with any other packages that are required for psych to work
#installation can also be done via the Packages tab in the bottom right window

####################################################
library(psych)#this function loads the psych package
#after loading psych, all of the functions included in the package are available to the user

####################################################
#a detailed description of the package's functionality can be called with ? or help()
?psych
help(psych)

####################################################
#these commands can also call a help page for specific functions 
?describe

#Functions are pre-written commands like in excel that are included in the various packages you can install
#The input for different functions varies widely depending on the purpose of the function
#The general format for entering information into a function is as follows: 
#FUNCTION (argument1, argument2, [...])
#For example, the read.table() function that we used to read in df
#df<-read.table("adult19.csv", #argument 1
#               sep=",", #argument 2
#               header=TRUE) #argument 3

#########################################################
#########################################################
#data objects and class
#########################################################

####################################################
#datasets are often stored as data.frame class objects. A data.frame is a matrix with 3 attributes: column names, class, and row names
attributes(df)#attributes displays the attributes of the data.frame we read in
#these elements can also be accessed individually
colnames(df)
rownames(df)
class(df)#object class determines how some functions interact with a data object. Many functions check object class before conducting their operations
####################################################

#More information about classes of objects can be found on datacamp
#https://www.datacamp.com/community/tutorials/r-objects-and-classes

####################################################
#we can access the columns of our data.frame with the $ operator
df$LASTDR_A # this command extracts the column LASTDR_A from the data.frame

####################################################
#columns also have a class
class(df$LASTDR_A) #integer class, R recognizes that these are numbers 
class(df$HHX) # character class, R recognizes that these are not numbers
df$HHX#note that each entry has quotes around it, indicating that R recognizes it as a character class object

####################################################
#object class determines what operations R will perform on a column 
LASTDR_A_plusone<-(df$LASTDR_A + 1) #we can perform mathematical operations with integer class objects. Here we add 1 to every row of LASTDR_A
#numeric class objects behave the same
#note that when we create a new object, it is displayed in the upper right window
#mathematical operations can be used to transform variables for analysis to be better aligned with the statistical assumptions of a model
#for example log transformation is often used to make a variables distribution more similar to a normal distribution

class(LASTDR_A_plusone) <- "character"#we can change the class of our object and R will no longer recognize the values as numbers
LASTDR_A_plusone#note that each entry has quotes around it, indicating that R recognizes it as a character class object

LASTDR_A_plusone - df$LASTDR_A #the mathematical operation fails because LASTDR_A_plusone is recognized as a character class object
LASTDR_A_plusone<-as.numeric(LASTDR_A_plusone)#convert LASTDR_A_plusone back to a numeric class object

####################################################
#factors are a unique object class that have additional attributes
LASTDR_A_plusone_FACTOR<-factor(LASTDR_A_plusone) #create a factor version of LASTDR_A_plusone
class(LASTDR_A_plusone_FACTOR)#class is factor
attributes(LASTDR_A_plusone_FACTOR)#factors have an additional "levels" attribute

#levels can be used converted to text labels
#text labels can be useful when constructing plots
#here we use c() the concatenate function to combine a series of text entries and assign them as the level() of our factor
levels(LASTDR_A_plusone_FACTOR)<-c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
attributes(LASTDR_A_plusone_FACTOR)
LASTDR_A_plusone_FACTOR

#factors can store text data and interact with R functions differently than character class objects
#use of factors or character class columns for text data is often a matter of preference
#it is important to know if R has read character data as a factor or character class object, as they will often behave much differently
#these tutorials will not use factor class objects for most lessons
#to prevent R from reading character class objects as factors, we set a global option in each session
options(stringsAsFactors = FALSE)

rm(LASTDR_A_plusone)#delete LASTDR_A_plusone
rm(LASTDR_A_plusone_FACTOR)#delete LASTDR_A_plusone_FACTOR

#########################################################
#########################################################
#the brackets [row,column] operator 
#########################################################

#the rows and columns of a data.frame can be accessed by using the brackets operator
df[1,]#prints the first row of the data.frame
df[,1]#prints the first column of the data.frame (URBRRL)
df[1,1] #[1,1] prints the cell located at the first row and first column
df[,"URBRRL"]#columns can also be called by name, rather than number. URBRRL is the first column of the data.frame

#brackets can be used to interact with most objects. if the object has 2 dimensions (rows and columns) the brackets must have a column [,]
#if the object has 1 dimension, there is no comma in the brackets
df$URBRRL[1]#here, we use the $ to extract a column from df and then use the brackets to extract the first element of the column

#brackets are very flexible and can be used to interact with other object types
colnames(df)# prints the column names of the data.frame
colnames(df)[1] #prints the first column name of the data.frame
colnames(df)[2] #prints the second column name of the data.frame

#selecting the first element of colnames is equivalent to entering the actual name of the first column
#note the use of "==" rather than "=" to indicate equivalence
#the "=" operator function the same way as the "<-" operator that we have used earlier in the module
colnames(df)[1] == "URBRRL"# these 2 specifications are interchangable in your code
#for example
df[1, "URBRRL"]  == df[1, colnames(df)[1]] 

#the brackets operator can be used to interact with any object, as long as your usage of the comma aligns with the number of dimensions of the object
attributes(df)#prints the attributes of df
attributes(df)[1] #prints the first attribute of df
attributes(df)[,1] #this fails, because attributes(df) has only one dimension
attributes(df)$names #the first element of attributes(df) is "names". The $ can be used in place of [1] if the name of the first element is known

#the brackets operator is important for constructing loops. This will be the topic of a later module

#########################################################
#########################################################
#Lists
#########################################################
#Lists are a basic way to combine things in R
#If R can't find another way to put objects together, it will combine them in a list
#Any objects can be assembled in a list
ListObject<-list(df, "a", 1)#this list object is comprised of our data.frame, the letter "a", and the number 1
class(ListObject)#the class of the object is "list"
#elements of the list can be accessed with double brackets
ListObject[[1]] #element 1, df
ListObject[[2]] #element 2, the letter "a"
ListObject[[3]] #element 3, the number 1

rm(ListObject)#delete ListObject

#Lists are very flexible and can be used to store any series of data objects 
#This can be useful for streamlining certain tasks in R, and will be discussed further in later modules

#You can save your script by clicking the small floppy disc icon in the upper left hand corner
#Exit R
q()

#citations for the packages used in this module can be retrieved below
citation("psych")
