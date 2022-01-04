###### Interpretability in ML - Chapter 4. The Data                           ######
## Reproduces all results including tables and figures from chapter 4. The Data   ##
## Working directories need to be changed to the respective working directory     ##
## Directories of csv's need to be changed to where the abalone data set is saved ##
## Last revision: 17/07/2020                                                      ##

##### 1. General commands #####

library(dplyr)
library(ggplot2)
library(stargazer)

# Clear environment before starting the analysis
rm(list=ls())
# Set a seed to enable the reproduction of the exact results of the analysis
set.seed(1)

# Adjust the working directory
# Change working directory here
input.folder <- "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\"
setwd(input.folder)

##### 2. Read CSV file into a single dataframe and pre-processing #####
# For reproducing the resu  lts change the directory here
df <- read.csv(paste("C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\" ,"abalone",".csv", sep =""), sep=",", 
                   header=TRUE, quote = "",row.names = NULL)

# Get an overview of the data
str(df)

# Create categorical variable
df$AgeCat <- 0
age_mean <- mean(df$Rings) 
df$AgeCat[df$Rings > age_mean] = "1" # we separate by the mean here which is 9.933684 to show that we 
df$AgeCat[df$Rings < age_mean] = "0" # divide the sample in similar halfs, using 9 as a threshold 
df$AgeCat <- as.numeric(df$AgeCat)   # since Rings-values can be only whole numbers.
                                     # This yields effectively the same result 

# Reverse previous pre-processing to obtain interpretable data values
df$Length <- df$Length*200
df$Diameter <- df$Diameter*200
df$Height <- df$Height*200
df$WholeWeight <- df$WholeWeight*200
df$ShuckedWeight <- df$ShuckedWeight*200
df$VisceraWeight <- df$VisceraWeight*200
df$ShellWeight <- df$ShellWeight*200

# Get an overview of the data again to check if preprocessing was successful
str(df)

##### 3. Descriptive statistics calculation and latex output ##### 
desc_statistics <- stargazer(df, type = "latex")
desc_statistics

##### 4. Correlation matrix calculation and latex output #####
# exclude Sex since we calculate numerical correlations
df$Sex <- NULL
Correlation_matrix <- cor(df, method = "pearson")
Correlation_matrix_latex <- stargazer(Correlation_matrix)


##### End of this document #####