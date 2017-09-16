library(RcmdrPlugin.IPSUR)
data(RcmdrTestDrive)
attach(RcmdrTestDrive)
# shows names of variables/columns
names(RcmdrTestDrive)

# summary of all variables
summary(RcmdrTestDrive)

# make a table with one variable
race <- table(RcmdrTestDrive$race)

# make a bar char from a table with one variable
barplot(race)

# calculate agg mean (salary) group by (gender) from table - 2 methods (tapply or by)
aggMean <- tapply(RcmdrTestDrive$salary, RcmdrTestDrive$gender, mean)
by(RcmdrTestDrive$salary, RcmdrTestDrive$gender, mean, na.rm = TRUE)

# get the agg mean of a variable group by another variable
mean(RcmdrTestDrive$salary[RcmdrTestDrive$gender == 'Male'])

# get the max/min agg from a variable
aggMean[which(aggMean == max(aggMean))]

# calculate spread of two variables using standard deviation
sdSalaryVsGender <- tapply(RcmdrTestDrive$salary, RcmdrTestDrive$gender, sd)

# boxplot of two variables (y~x)
boxplot(RcmdrTestDrive$salary~RcmdrTestDrive$gender, data=RcmdrTestDrive)

# sort a variable
reduction <- sort(RcmdrTestDrive$reduction)

# get value of variable by index 137th
reduction[137]

# find the Inner Quartile Range (IQR) of a variable
IQR(reduction)

# five number summary of a variable
fivenum(reduction)

# calculate (approx) IQR from five number summary
fivenum(reduction)[4] - fivenum(reduction)[2]

# check for outliers
# potential
temp <- fivenum(reduction)
1.5 * (temp[4] - temp[2]) + temp[4]
# suspected
3 * (temp[4] - temp[2]) + temp[4]

# boxplot shows outliers (variable has a ton so use median for measure of central tendency and MAD or scales IQR for spread)
boxplot(RcmdrTestDrive$after, data = RcmdrTestDrive)

# measures of center (mean vs median)
c(mean(RcmdrTestDrive$after), median(RcmdrTestDrive$after))

# measures of spread (sd vs mad vs rescaled IQR)
c(sd(RcmdrTestDrive$after), mad(RcmdrTestDrive$after), IQR(RcmdrTestDrive$after)/1.349)

# measures of shape (skewness vs 2*SQRT(6/n) and kurtosis vs 2*SQRT(24/n))
library(e1071)
# not sure why do this
2*sqrt(6/nrow(RcmdrTestDrive))
# 0 is expected value, the whale is in the tail (pos or neg)
skewness(RcmdrTestDrive$after)
# not sure why do thi
2*sqrt(24/nrow(RcmdrTestDrive))
# 3 is expected value, 3 types in R (excess is type 1)
kurtosis(RcmdrTestDrive$after, type = 1)

# histogram
hist(RcmdrTestDrive$after)
