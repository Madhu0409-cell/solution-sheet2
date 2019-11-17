# 32. Next, we'd like to take a look at the average accuracy per subject.
# Using the "cast" function from the library "reshape", create a new data.frame whisch shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".

install.packages("reshape2") 
library(reshape2)
help(cast)
avrg_accuracy <- dcast(data,Subject~accuracy,mean)

# 33. Sort in ascending order or plot of the average accuracies per subject.
help(order)
order(avrg_accuracy)

# 34. Would you exclude any subjects, based on their avrg_accuracy performance?
# No we shouldn't exclude any subjects as it might lead to inaccurate results

# 35. Congrats! Your data are now ready for analysis. Please save the data frame you created into a new file called "digsym_clean.csv".
digsym_clean.csv <- write.csv(data)
