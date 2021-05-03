#initialize Data
#############################################################
df<-data.frame(
c(23,21,345,2),
c("Blue", "Brown", "Hazel", "Brown"),
c("Yes","Yes","Yes","No"),
c(6,10,3,8))
colnames(df) <- c("ID", "EyeColor", "Pets", "Var001")
df
#############################################################


#QUESTIONS (answers below)
#############################################################

#1. How would you tell R to extract the second column?


#2. Which of the following notations is NOT equivalent to df[1,2]?
#Multiple choice : 
df$EyeColor[1]
df[1, "EyeColor"] 
df["EyeColor", 1] 

#3. How would you determine the class of column 2, "Eyecolor"? 


#4. How would you add 1 to column 4, "Var001" and save it as a new object named Var001PlusOne?



#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################









#ANSWERS 
#############################################################
#1. How would you tell R to extract the second column?
df[,2]

#2. Which of the following notations is NOT equivalent to df[1,2]?
#Multiple choice : 
df$EyeColor[1]
df[1, "EyeColor"] 
df["EyeColor", 1]   #CORRECT ANSWER

#3. How would you determine the class of column 2, "Eyecolor"? 
class(df$EyeColor)
class(df[,"Eyecolor"])
class(df[,2])

#4. How would you add 1 to column 4, "Var001" and save it as a new object named Var001PlusOne?
Var001PlusOne<-df$Var001+1
Var001PlusOne<-df[,4]+1



