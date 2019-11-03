### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 4. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates') name and matriculation number. 
## Anar Amirli 2581604, Madhumitha Mohanram 2579667, Ga Yeon Roﬂ 2568941

## Change the name of the file by adding your matriculation numbers
## (sheet01_firstID_secondID_thirdID.R)



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## moodle discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
getwd()
##Output[1] "C:/Users/firem/Desktop

## b) Get help with this function.
help(getwd)

## c) Change your working directory to another directory.
setwd("C:\Users\StatR")


###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages("languageR")
require(languageR)
library(languageR)

## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?

##  The head command shows the top 6 entries in the Dataset.
##  The tail command shows the last 6 entries in the Dataset.

head(dutchSpeakersDistMeta)
##OUTPUT[2B]:head
#      Speaker    Sex AgeYear  AgeGroup ConversationType EduLevel
# 410   N01001 female    1952 age45to55             <NA>     high
# 409   N01002   male    1952 age45to55       maleFemale     high
# 1157  N01003   male    1949 age45to55       maleFemale     high
# 252   N01004 female    1971 age25to34             <NA>     high
# 251   N01005 female    1944   age56up       femaleOnly      mid
# 254   N01006   male    1969 age25to34       maleFemale     high

tail(dutchSpeakersDistMeta)
##OUTPUT[2B]:TAIL
#      Speaker    Sex AgeYear  AgeGroup ConversationType EduLevel
# 163   N01216   male    1981 age18to24             <NA>      mid
# 162   N01217   male    1976 age25to34             <NA>      mid
# 164   N01218 female    1980 age18to24       maleFemale     high
# 1049  N01219   male    1980 age18to24             <NA>      mid
# 1051  N01220 female    1955 age35to44             <NA>      mid
# 1050  N01221 female    1983 age18to24       maleFemale     high

summary(dutchSpeakersDistMeta)
##OUTPUT[2B]:summary
# Speaker        Sex        AgeYear          AgeGroup    ConversationType EduLevel  
# N01001 :  1   female:90   Min.   :1923   age18to24:71   femaleOnly:26      high:117  
# N01002 :  1   male  :73   1st Qu.:1956   age25to34:38   maleFemale:55      low :  1  
# N01003 :  1   NA's  : 2   Median :1974   age35to44:12   maleOnly  :10      mid : 44  
# N01004 :  1               Mean   :1967   age45to55:20   NA's      :74      NA's:  3  
# N01005 :  1               3rd Qu.:1979   age56up  :22                                
# (Other):158               Max.   :1987   NA's     : 2                                
# NA's   :  2               NA's   :2 


## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.
## There are 165 speakers included in the dataset
nrow(dutchSpeakersDistMeta)
##OUTPUT[2C]:nrow : [1] 165



## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.
boxplot(dutchSpeakersDistMeta$AgeYear~dutchSpeakersDistMeta$Sex, main=toupper("BoxPlot for Sex and AgeYear"),col=c("red","Yellow"))
##OUTPUT[2D]:boxplot for for Sex(red for women,yellow for men) and AgeYear

## e) Does it seem as if either of the two groups has more variability in age?
## Yes we have more variability in age of female than age of males as the red box denoting them has more coverage than yellow box denoting males
data <- dutchSpeakersDistMeta[,c("AgeYear", "Sex")] 
## f) Do you see any outliers in either of the two groups?
## From the plot,it is observed that there are 2 outliers in group of males slightly prior and after 1930

## g) Now calculate the mean and standard deviation of the AgeYear per group. 
##    Do this by creating a subset for each group.
data <- dutchSpeakersDistMeta[,c("AgeYear", "Sex")]

##  Mean of Age Year  for females

FemaleAgegrp<- subset(data, Sex =='female')
FemaleAgegrp$Sex <- droplevels(FemaleAgegrp$Sex) # drop the unused level

fMean <- tapply(FemaleAgegrp$AgeYear,FemaleAgegrp$Sex, mean)
fMean

##OUTPUT[2g]:Mean of Age Year  for females
##  female
##  1966.889

##  Mean of Age Year  for males

MaleAgegrp<- subset(data, Sex =='male')
MaleAgegrp$Sex <- droplevels(MaleAgegrp$Sex) # drop the unused level

mMean <- tapply(MaleAgegrp$AgeYear,MaleAgegrp$Sex, mean)
mMean

##OUTPUT[2g]:Mean of Age Year  for males
##  male
##  1967.301

## Standard Deviation for females over AgeYear

FstdDev<- tapply(FemaleAgegrp$AgeYear,FemaleAgegrp$Sex, sd)
FstdDev  
##Output: 
##  femal
##  15.87411

## Standard Deviation for males over AgeYear

MstdDev<- tapply(MaleAgegrp$AgeYear,MaleAgegrp$Sex, sd)
MstdDev  
##Output: 
##  male 
##  14.66258 



## h) What do the whiskers of a boxplot mean?
##The two horizontal lines above and below the actual plot which denotes the extreme 0f data(highest and lowest observations) are called as whiskers. And the points beyond those whiskers are plotted as outliers.


###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)

#Answer: It is discrete measurement because e.g. there exists no value between 16 and 17. 


## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?

# Answer: Dataframe is better if you want to store data of different classes.

## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25

pps <- c(1:25)

## d) Next, create a vector containing all the observations. Name this vector 'obs'.

obs <- c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20)

## e) Create a dataframe for this data. Assign this to 'stories'. 

stories <- data.frame(pps, obs)
stories

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?

# Answer: Numeric.

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?

stories$pps <- factor(stories$pps)
#Answer: We make sure that the values of pps are categorial variables wich cannot be ordered like integers.

## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.

hist(stories$obs, breaks=8)

## i) Create a kernel density plot using density().

plot(density(stories$obs))

## j) What is the difference between a histogram and a kernel density plot?

# Answer: A density plot shows smooth distribution in a continous interval.
# With a clear shape we see where the values are most concentrated at one glance. 
# A histogram helps to inspect exact values of each variable. 

## This is a difficult one, remember you just need to provide a serious attempt at solving each 
## exercise in order to pass. 
## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)

hist(stories$obs, 
     breaks=8, 
     prob= TRUE, 
     col= "gray")


lines(density(stories$obs), col="red")


###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.

x <- seq(from=-5, to=5, by=0.1)


## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.


y<-dnorm(x, mean=mean(x),sd=sd(x))
plot(y)



## c) Now use plot() to plot the normal distribution for z values of "x". 

plot(x, y, xlab="x value", ylab="Density", main="normal distribution for z values of x")


## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.

plot(x, y, ylim=c(0, 0.8), type="l",  xlab="x value", ylab="Density", main="normal distribution for z values of x")

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.

abline(v=c(mean(x), median(x)), col=c("blue", "red"), lty=2)


## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".

b1temp <- beaver1[, "temp"]

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.

b1temp_mean <- mean(b1temp)
b1temp_sd <- sd(b1temp)

plot(b1temp, dnorm(b1temp, mean=b1temp_mean, sd=b1temp_sd), xlab="temp", ylab="density", type="l", main="normal distribution")


## h) We observe two temparatures (36.91 and 38.13). What's the likelihood that
##    these temperatures (or more extreme ones) respectively come 
##    from the normal distribution from g)?

# P(temp<=36.91)
round(pnorm(q=36.91, mean=b1temp_mean, sd=b1temp_sd, lower.tail = TRUE), 4)
# output: 0.5976

# P(temp>=38.13)
round(pnorm(q=38.13, mean=b1temp_mean, sd=b1temp_sd, lower.tail = FALSE), 4)
# output: 0

## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. What do you observe?

for (i in 0:5){
  
  random_samples<-rnorm(n=20,m=b1temp_mean,sd=b1temp_sd)
  hist(random_samples,
       main=paste("Histogram of random sample", i))
  
}

# Answer: Histograms mostly didn't do well on interpreting the denisty distribution
# from g. It showed rather random trends than the normal distribution. 
# There were significant variations in the distribution of the histograms.
