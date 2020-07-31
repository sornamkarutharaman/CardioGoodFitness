#======================================================
#     Cardio Fitness - Descriptive Statistics(Nov19)
#======================================================

#Set the Working directory 
setwd("<Proje Directory>")

#Verify the Working Directory
getwd()

#Install necessary Packages 
install.packages("pacman")
library(pacman)
p_load(readr,readxl,dplyr,ggplot2)

#Load the Dataset&View
CardioFitness <- read_csv("<Dataset>")
View(CardioFitness)


# DataSummary 
dim(CardioFitness) # 180 rows and 9 variables
#[1] 180   9
str(CardioFitness)


# Change Datatype of the Variables
CardioFitness$Gender = as.factor(CardioFitness$Gender)
CardioFitness$Product = as.factor(CardioFitness$Product)
CardioFitness$MaritalStatus = as.factor(CardioFitness$MaritalStatus)

# Verify the changes of the Datatype 
str(CardioFitness) 

# check summary statistics
summary(CardioFitness) 

#summary by Product
by(CardioFitness, INDICES = Product, FUN = summary)
by(CardioFitness, INDICES = MaritalStatus, FUN = summary)
by(CardioFitness, INDICES = Gender, FUN = summary)

# checking for missing values in dataset
anyNA(CardioFitness) 
#[1] FALSE

#Univariate Analysis

#partition to get 2 graphs in a row
par(mfrow = c(1,1))
#Age
hist(CardioFitness$Age)
boxplot(CardioFitness$Age)

#Age
hist(CardioFitness$Education)
boxplot(CardioFitness$Education)

#Fitness
hist(CardioFitness$Fitness)
boxplot(CardioFitness$Fitness)

#Usage
hist(CardioFitness$Usage)
boxplot(CardioFitness$Usage)

#Income
hist(CardioFitness$Income)
boxplot(CardioFitness$Income)

#Miles
hist(CardioFitness$Miles)
boxplot(CardioFitness$Miles)


#bivariate,multivariate analysis

# for correlation graph
install.packages("corrplot") 
library(corrplot) 

#Age vs Gender
boxplot(Age~Gender,data = CardioFitness,horizontal = TRUE,col = c("blue","red"))

#Age vs education - plots panelled by product
qplot(Age,Education,data=CardioFitness,facets = .~Product)

# Age vs Usage for three products
qplot(Age,Usage,data=CardioFitness,color = Product)

#Age vs Income for products
qplot(Age,Income,data=CardioFitness,color = Product,geom = c("point","smooth"))

#Usage vs Fitness for products
qplot(Usage,Fitness,data = CardioFitness, geom = c("point","smooth"),method = lm,facets = .~Product)

#Products vs marital status
qplot(MaritalStatus,data = CardioFitness,fill = Product)


#Multivariate Analysis - Usage,Fitness,Income,Miles
corrplot(cor(CardioFitness[,6:9]))


#========================================
#             THE - END
#========================================

