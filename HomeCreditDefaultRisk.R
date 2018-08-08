#July 13, 2018 KDD Class UNC Charlotte
#Team Members:
#Aaradhya Mehta
#Lavanya Kanagaraj
#Ryan Scolaro
#Vamsi Veginati
#EDA and Data Preprocessing for Kaggle Competition
#examples of preparing data for regression analysis
#https://www.kaggle.com/c/home-credit-default-risk
#application_train.csv is a modified form of the
#application_train file available on the competition site
#on Kaggle - many attributes have been removed for the
#purpose of this exercise
install.packages('plyr')
library(plyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("e1071")
library(e1071)
install.packages("mice")
library(mice)
# 1. set working directory and load the application_train.csv file
loan.data <- read.csv(file="application_train.csv", header=TRUE, sep=",")
# 2. use summary to get information on the 122 attributes
summary(loan.data)
attach(loan.data)
# 3. Missing Values - analyze missing values in the data
sapply(loan.data, function(x) sum(is.na(x)))
# 4. Create a box plot comparing AMT_INCOME_TOTAL and AMT_CREDIT
boxplot(AMT_INCOME_TOTAL,AMT_CREDIT)
#chart of amt credit grouped by family status
fs <- ggplot(loan.data, aes(x="", y=AMT_CREDIT, group=NAME_FAMILY_STATUS)) + 
  geom_boxplot(fill="slateblue", alpha=0.2,outlier.color="blue") + 
  xlab("FAMILY STATUS")+ylab("AMT CREDIT")
fs
#boxplot(loan.data$AMT_CREDIT)
# are there outliers that should be removed based on the resulting box plot?
#Yes there are outliers in AMT_INCOME_TOTAL and AMT_CREDIT that should be removed.
#We can use this script to remove them, although we do not do so at this time.
source("outlierscript.R")
outlierKD(loan.data, AMT_INCOME_TOTAL) 
outlierKD(loan.data, AMT_CREDIT) 
# 5. Create flag variables for the NAME_FAMILY_STATUS categorical attribute
#We can use the below code to create a single variable and assign different values for different types of family status
#Below code will assign values 1 to 6 for each type of family status
#BEGIN CODE TO ASSIGN VALUES TO A NEW VARIABLE(can be used for clustering)
#NAME_FAMILY_STATUS_CATEGORIAL_ATTR <- as.factor(NAME_FAMILY_STATUS)
#levels(NAME_FAMILY_STATUS_CATEGORIAL_ATTR) <- 1:length(levels(NAME_FAMILY_STATUS_CATEGORIAL_ATTR))
#loan.data['NAME_FAMILY_STATUS_CATEGORIAL_ATTR'] <- as.numeric(NAME_FAMILY_STATUS_CATEGORIAL_ATTR)
#END CODE FOR CRAETING SINGLE VARIABLE
#add new attributes to loan.data and set equal to 0 in each case
#example shows one
loan.data$flag_s_not_m <- 0
loan.data$flag_m <- 0
loan.data$flag_c_m <- 0
loan.data$flag_w <- 0
loan.data$flag_s <- 0
#unknown will be if all flags are 0
#first single not married - change new field to 1 depending on
#flag family status, this can be used for logistic regression
loan.data$flag_s_not_m[loan.data$NAME_FAMILY_STATUS == 'Single / not married'] <- 1
#check field
loan.data$flag_s_not_m
#continue for all other fields
loan.data$flag_m[loan.data$NAME_FAMILY_STATUS == 'Married'] <- 1
#check field
loan.data$flag_m
loan.data$flag_c_m[loan.data$NAME_FAMILY_STATUS == 'Civil Marriage'] <- 1
#check field
loan.data$flag_c_m
loan.data$flag_w[loan.data$NAME_FAMILY_STATUS == 'Widow'] <- 1
#check field
loan.data$flag_w
loan.data$flag_s[loan.data$NAME_FAMILY_STATUS == 'Separated'] <- 1
#check field
loan.data$flag_s
#COMMENT
#oounts (dplyr)
count(loan.data, 'flag_s_not_m')
count(loan.data, 'flag_m')
count(loan.data, 'flag_c_m')
count(loan.data, 'flag_w')
count(loan.data, 'flag_s')
#
# 6. Standardize AMT_INCOME_TOTAL using Z-Score Standardization. Calculate skewness. Is the
#attribute normally distributed?

loan.data$zscore.AMT_INCOME_TOTAL <- (AMT_INCOME_TOTAL - mean(AMT_INCOME_TOTAL))/sd(AMT_INCOME_TOTAL)
loan.data$zscore.AMT_INCOME_TOTAL
hist(loan.data$zscore.AMT_INCOME_TOTAL, breaks=11,xlim=c(-5,5), main="histogram of Z-score",
     xlab="Z score of AMT_INCOME_TOTAL",ylab="Counts")
box(which = "plot", lty="solid", col="black")

AMT_INCOME_TOTAL_skew <- (3*(mean(AMT_INCOME_TOTAL)-median(AMT_INCOME_TOTAL))) / sd(AMT_INCOME_TOTAL)
AMT_INCOME_TOTAL_skew
#Answer : AMT_INCOME_TOTAL is not normally distributed
# we will use e1071 library for skewness
#does it matter if you use the standardized values or actual data?
skewincome = loan.data$zscore.AMT_INCOME_TOTAL    
skewa = skewness(skewincome)                # apply the skewness function 
skewa
###SKEWNESS VALUE DIFFER WHILE USING THE FIRST FORMULA AND THE skewness FUNCTION OF e1071 library


# 7. Analyze AMT_INCOME_TOTAL for Outliers using the Z-Score values. show
#both lower range and upper range outliers.
# show the lower range outliers
loan.data$zscore.AMT_INCOME_TOTAL[loan.data$zscore.AMT_INCOME_TOTAL < -3]
# show the upper range outliers
loan.data$zscore.AMT_INCOME_TOTAL[loan.data$zscore.AMT_INCOME_TOTAL > 3]
maxincome_outliers <- loan.data$zscore.AMT_INCOME_TOTAL[loan.data$zscore.AMT_INCOME_TOTAL > 3]
length(maxincome_outliers)
minincome_outliers <- loan.data$zscore.AMT_INCOME_TOTAL[loan.data$zscore.AMT_INCOME_TOTAL < -3]
length(minincome_outliers)


# 8. Standardize AMT_CREDIT using Z-Score Standardization. Calculate skewness.  Is
#the attribute normally distributed?
#hist(AMT_CREDIT)
loan.data$zscore.AMT_CREDIT <- (AMT_CREDIT - mean(AMT_CREDIT))/sd(AMT_CREDIT)
loan.data$zscore.AMT_CREDIT
hist(loan.data$zscore.AMT_CREDIT, breaks=11,xlim=c(-5,5), main="histogram of Z-score",
     xlab="Z score of AMT_CREDIT",ylab="Counts")
box(which = "plot", lty="solid", col="black")

AMT_CREDIT_skew <- (3*(mean(AMT_CREDIT)-median(AMT_CREDIT))) / sd(AMT_CREDIT)
AMT_CREDIT_skew
# Skewness is 0.6372444
#Answer : Value 0 is considered as normally distributed where as AMT_CREDIT is not symmentric as skewness is more than 0.5
skewcred = loan.data$zscore.AMT_CREDIT    
skewb = skewness(skewcred)                # apply the skewness function 
skewb
###SKEWNESS VALUE DIFFER WHILE USING THE FIRST FORMULA AND THE skewness FUNCTION OF e1071 library

# 9. Analyze AMT_CREDIT for Outliers using the Z-Score values. Show
#both the lower range and upper range outliers
loan.data$zscore.AMT_CREDIT[loan.data$zscore.AMT_CREDIT < -3]
# show the upper range outliers
loan.data$zscore.AMT_CREDIT[loan.data$zscore.AMT_CREDIT > 3]
maxcredit_outliers <- loan.data$zscore.AMT_CREDIT[loan.data$zscore.AMT_CREDIT > 3]
length(maxcredit_outliers)
mincredit_outliers <- loan.data$zscore.AMT_CREDIT[loan.data$zscore.AMT_CREDIT < -3]
length(mincredit_outliers)

#10. Using knowledge from 4 through 9, make a decision on 
#removing outliers and transforming for normality in the two attributes and document your decision
uif_AMT_INCOME_TOTAL<-quantile(AMT_INCOME_TOTAL,.75)+1.5*IQR(AMT_INCOME_TOTAL)
uif_AMT_INCOME_TOTAL
amt_income_frame <- subset(loan.data, AMT_INCOME_TOTAL<uif_AMT_INCOME_TOTAL)
amt_income_frame

uif_AMT_CREDIT<-quantile(AMT_CREDIT,.75)+1.5*IQR(AMT_CREDIT)
uif_AMT_CREDIT
amt_credit_frame <- subset(loan.data, AMT_CREDIT<uif_AMT_CREDIT)
amt_credit_frame
#11. Use binning to discretize AMT_TOTAL_INCOME
# into five bins named A through E with A being the lowest
# and E being the highest - name the new attribute CAT_AMT_TOTAL_INCOME
#CAT_AMT_TOTAL_INCOME <- discretize(AMT_TOTAL_INCOME,cuts=5)
#could not find function "discretize"
# into five bins named A through E with A being the lowest
# and E being the highest - name the new attribute CAT_AMT_TOTAL_INCOME
loan.data$CAT_AMT_TOTAL_INCOME[loan.data$AMT_INCOME_TOTAL > 0 & loan.data$AMT_INCOME_TOTAL <= 100000 ] <- "A"
loan.data$CAT_AMT_TOTAL_INCOME[loan.data$AMT_INCOME_TOTAL > 100000 & loan.data$AMT_INCOME_TOTAL <= 150000 ] <- "B"
loan.data$CAT_AMT_TOTAL_INCOME[loan.data$AMT_INCOME_TOTAL > 150000 & loan.data$AMT_INCOME_TOTAL <= 200000 ] <- "C"
loan.data$CAT_AMT_TOTAL_INCOME[loan.data$AMT_INCOME_TOTAL > 200000 & loan.data$AMT_INCOME_TOTAL <= 250000 ] <- "D"
loan.data$CAT_AMT_TOTAL_INCOME[loan.data$AMT_INCOME_TOTAL > 250000 ] <- "E"

amt_income_frame$CAT_AMT_TOTAL_INCOME[amt_income_frame$AMT_INCOME_TOTAL > 0 & amt_income_frame$AMT_INCOME_TOTAL <= 100000 ] <- "A"
amt_income_frame$CAT_AMT_TOTAL_INCOME[amt_income_frame$AMT_INCOME_TOTAL > 100000 & amt_income_frame$AMT_INCOME_TOTAL <= 150000 ] <- "B"
amt_income_frame$CAT_AMT_TOTAL_INCOME[amt_income_frame$AMT_INCOME_TOTAL > 150000 & amt_income_frame$AMT_INCOME_TOTAL <= 200000 ] <- "C"
amt_income_frame$CAT_AMT_TOTAL_INCOME[amt_income_frame$AMT_INCOME_TOTAL > 200000 & amt_income_frame$AMT_INCOME_TOTAL <= 250000 ] <- "D"
amt_income_frame$CAT_AMT_TOTAL_INCOME[amt_income_frame$AMT_INCOME_TOTAL > 250000 ] <- "E"

#12. Use MICE to impute appropriate values for
#the missing values in CNT_CHILDREN (note:  the actual value in 
#each case was 0)
#install.packages("mice")
#library(mice)
md.pattern(loan.data)
dataframe_children <- as.data.frame(CNT_CHILDREN)
#note maxit is low due to time for processing can be higher
imputed_Data <- mice(loan.data, m=1, maxit = 2, method = 'cart', seed = 500)
imputed_Data$imp$CNT_CHILDREN
summary(imputed_Data)
densityplot(imputed_Data)
completedData <- complete(imputed_Data,1)
sapply(completedData, function(x) sum(is.na(x)))

#13 use the table command to create a contingency table
#of your choice
table(loan.data$NAME_FAMILY_STATUS,loan.data$NAME_HOUSING_TYPE)
#count gives you n way frequency for pairs
#14 get a count of the n way frequency for pairs
library(plyr)
count(loan.data,vars = c("NAME_FAMILY_STATUS","NAME_HOUSING_TYPE"))
#15 is the data realistic for DAYS_BIRTH and DAYS_EMPLOYED?
#why or why not?
#No, the data is not realistic as it has negative values, but we can convert them using date functions to look realistic
attach(loan.data)
loan.data$DOB_NEW <- as.Date(DAYS_BIRTH, origin = "1970-01-01")
loan.data$DOB_NEW
loan.data$DATE_EMPLOYED_NEW <- as.Date(DAYS_EMPLOYED, origin = "1970-01-01")
loan.data$DATE_EMPLOYED_NEW
#loan.data <- subset(loan.data, select = -c(D_EMPLOYED_NEW)) - To drop a redundant column
#16 what else would you like to do with the data in order to
#complete EDA and Pre-processing?  What have you learned?
#
dataset=data.frame(TARGET,CODE_GENDER,CNT_CHILDREN,AMT_CREDIT,AMT_INCOME_TOTAL,NAME_FAMILY_STATUS)
pairs(dataset) # graphically shows the relationship between variables
dataset
#Note this final step does not need to be executed, you can just show
#a plan and document what you have learned so far.
#TURN IN:
#zipped folder
#one per group