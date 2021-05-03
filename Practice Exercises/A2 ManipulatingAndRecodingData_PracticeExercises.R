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

#1. How would you tell R to rename column 4, “Wellbeing”?

#2. Suppose that you want to create a dataset including only people with VAR001 values less than 10. What is the syntax? 

#3. Which value does the following ifelse statement return, 1 or 0? ifelse(1+1==2, 0, 1)

#4. Which value does the following ifelse statement return, 1 or 0? ifelse(1+1==3, 1, 0)

#5. Which value does the following ifelse statement return, 1 or 0? ifelse(1+1==3, 0, 1)

#5. What sequence of values does the following ifelse statement return? ifelse(df$EyeColor=="Brown", 0, 1)

#6. What sequence of values does the following ifelse statement return? ifelse(df$Pets=="Yes", 1, 0)


#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################









#ANSWERS 
#############################################################
#1. How would you tell R to rename column 4, “Wellbeing”?
colnames(df)[4]<-"Wellbeing"

#2. Suppose that you want to create a dataset including only people with VAR001 values less than 10. What is the syntax? 
New_df<-df[df$Var001<10,]

#3. Which value does the following ifelse statement return, 1 or 0? ifelse(1+1==2, 0, 1)
ifelse(1+1==2, 0, 1)# 1+1==2 is TRUE, so the function returns the value in the YES argument (0)

#4. Which value does the following ifelse statement return, 1 or 0? ifelse(1+1==3, 1, 0)
ifelse(1+1==3, 1, 0)# 1+1==3 is FALSE, so the function returns the value in the NO argument (0)

#5. Which value does the following ifelse statement return, 1 or 0? ifelse(1+1==3, 0, 1)
ifelse(1+1==3, 0, 1)# 1+1==3 is FALSE, so the function returns the value in the NO argument (1)

#5. What sequence of values does the following ifelse statement return? ifelse(df$EyeColor=="Brown", 0, 1)
ifelse(df$EyeColor=="Brown", 0, 1) #df$EyeColor=="Brown" is TRUE in rows 1 and 3 and FALSE in rows 2 and 4. The sequence is 1,0,1,0 

#6. What sequence of values does the following ifelse statement return? ifelse(df$Pets=="Yes", 1, 0)
ifelse(df$Pets=="Yes", 1, 0) #df$Pets=="Yes" is TRUE in rows 1, 2, and 3 and FALSE in row 4. The sequence is 1,1,1,0 



