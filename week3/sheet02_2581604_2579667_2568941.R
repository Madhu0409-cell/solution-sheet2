###############
### Cleaning Data
###############

# Please do the "Cleaning Data with R" exercise that was assigned in DataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercises below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on how to further work with this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory. 
install.packages("rstudioapi")
currentwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(currentwd)

# 2. Read in the data into a variable called "dat".
dat <- read.csv('digsym.csv')


# 3. Load the libraries languageR, stringr, dplyr and tidyr.
Packages <- c("languageR", "stringr", "dplyr", "tidyr")
install.packages(Packages)
lapply(Packages, library, character.only = TRUE)

# 4. How many rows, how many columns does that data have?
# Answer: 3700 rows, 11 columns
nrow(dat)
ncol(dat)

# 5. Take a look at the structure of the data frame using "glimpse".
glimpse(dat)

# 6. View the first 20 rows, view the last 20 rows.
head(dat, 20)
tail(dat, 20)

# 7. Is there any missing data in any of the columns?
any(is.na(dat)) # shows us if there is any missing data
is.na(dat)
# Answer: Yes, for example in StimulDS1.RT

# 8. Get rid of the row number column.
row.names(dat) <- NULL
head(dat)

# 9. Put the Sub_Age column second.
dat <- dat[, c(1, 11, 2:10)]

# 10. Replace the values of the "ExperimentName" column with something shorter, more legible.
dat$ExperimentName <- as.factor(str_replace(dat$ExperimentName,"Digit Symbol - Kopie","Dsk"))


# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.
data2 <- subset(dat, List == "Trial:2")
dat <- data2
rm(data2)

# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate".
dat <- separate(dat,col=Sub_Age,into = c("Subject","Age"),sep=" _ ")
dat



# 13. Make subject a factor.
dat$Subject <- as.factor(dat$Subject)
dat

# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".

dat <- separate(dat, col = File, into = c("file","condition" ), sep = "_")
dat


# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 at the end  (i.e., 
# same number of+ characters, such that "1_right" should be replaced by "1_right0" etc).
dat$File <- str_pad(dat$File,width=8,side="right",pad="0")
dat

# 16. Remove the column "List".
dat$List <- NULL
dat

# 17. Change the data type of "Age" to integer.
dat$Age  <- as.integer(dat$Age)
str(dat)


# 18. Missing values, outliers:
# Do we have any NAs in the data, and if so, how many and where are they?

any(is.na(dat)) # check if there is any NAs
sum(is.na(dat)) # if there is, how many
complete.cases(dat) # where are they: find rows with no missing values

# We find  no missing values in the Output.

# 19. Create an "accuracy" column using ifelse-statement.
# If actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0.

# def function for defining corret response
my_function <- function(i, data_df=dat){
  if (data_df[i, "StimulDS1.RESP"]==data_df[i, "StimulDS1.CRESP"]){
    return(1)
  } else {
    return(0)
  }
}

# create accruacy
accuracy <- c()
for (i in 1:nrow(dat)){
  accuracy[i]<-my_function(i, dat)
}

# add new column to dataframe
dat <- cbind(dat, accuracy) 

head(dat)



# 20. How many wrong answers do we have in total?
sum(dat$accuracy)
# answer: 3145

# 21. What's the percentage of wrong responses?
100*sum(dat$accuracy)/length(dat$accuracy)
# answer: 94.44444


# 22. Create a subset "correctResponses" that only contains those data points where subjects 
# responded correctly. 
correctResponses <- dat %>% 
  subset(accuracy == 1)

head(correctResponses)



# 23. Create a boxplot of StimulDS1.RT - any outliers?

# answer: outliner can be observed. Mean of the distribution lies around 1000, 
# distribution's right tail ends around 5000, however there a single point which lies around 14000. 
# Given how this data point significantly differs from the rest, it is clearly an outliner.

boxplot(dat$StimulDS1.RT, horizontal = TRUE)


# 24. Create a histogram of StimulDS1.RT with bins set to 50.
hist(dat$StimulDS1.RT, col = 'skyblue3', breaks = 50)


# 25. Describe the two plots - any tails? any suspiciously large values?

# answer: in both plots, we can see that there is one outliner on the right side which is 
# located far away from the general data distribution.


# 26. View summary of correct_RT.

summary(dat$StimulDS1.RT)

# output
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 486     894    1091    1219    1399   13852 


# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named 
# "cleaned".

cleaned <- dat[dat$StimulDS1.RT < max(dat$StimulDS1.RT), ]
summary(cleaned$StimulDS1.RT)

# output:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 486     894    1091    1215    1399    7902 


## EXTRA Exercises:
##You can stop here for your submission of this week's assignment,
##but you are encouraged to try it now. 
##All these exercises will be discussed and solved in the tutorial!

# 28. Dealing with the tail of the distribution: outlier removal
# Now we want to define a cutoff value for the StimulDS1.RT variable in the correctResp dataset.
# Values should not differ more than 2.5 standard deviations from the grand mean of this variable.
# This condition should be applied in a new variable called "correct_RT_2.5sd", which prints NA 
# if an RT value is below/above the cutoff. 


# 29. Take a look at the outlier observations.
# Any subjects who performed especially poorly?


# 30. How many RT outliers are there in total?


# 31. Plot a histogram and boxplot of the correct_RT_2.5sd column again - nice and clean eh?


# 32. Next, we'd like to take a look at the average accuracy per subject.
# Using the "cast" function from the library "reshape", create a new data.frame which shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".


# 33. Sort in ascending order or plot of the average accuracies per subject.


# 34. Would you exclude any subjects, based on their avrg_accuracy performance?


# 35. Congrats! Your data are now ready for analysis. Please save the data frame you created 
# into a new file called "digsym_clean.csv".


