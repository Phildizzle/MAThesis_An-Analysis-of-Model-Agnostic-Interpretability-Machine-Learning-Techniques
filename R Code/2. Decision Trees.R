###### Interpretability in ML - Chapter 5.2 Decision Trees                         ######
## Reproduces all results including tables and figures from chapter 5.2 Decision Trees ##
## Directories of csv's need to be changed to where the abalone data set is saved      ##
## Directories for individual chart plots need to be changed when reproducing the plots##
## Last revision: 17/07/2020                                                           ##

##### 1. General commands #####

library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)	# Color selection for fancy tree plot

# Clear environment before starting the analysis
rm(list=ls())
# Set a seed to enable the reproduction of the exact results of the analysis
set.seed(1)

# Adjust the working directory
# Change working directory here
input.folder <- "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\"
setwd(input.folder)

# Create folder for the decision tree plots
dir.create("Plots - Decision Tree")

##### 2. Read CSV file into a single dataframe #####
# Change csv directory to where the abalone data set is saved
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

# Remove Rings column since it serves no purpose for classification
df$Rings <- NULL

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

##### 3. Calculate decision tree model (Rings on all eight covariates #####
tree_model <- rpart(AgeCat~., data=df, method = "class")
summary(tree_model)

##### 4.  Make predictions to check performance of the decision tree #####
# Subset dataframe for making predictions of tree_model
df_wo_AgeCat <- df %>% select(Sex, Length, Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight)
tree_prediction <- predict(tree_model, df_wo_AgeCat, type = "class")

# Calculate confusion matrix and accuracy for text part
confMat <- table(df$AgeCat,tree_prediction)
confMat

accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

##### 5. Create a decision tree plot and save the png file #####
# rpart.plot plots a first version of the decision tree in tree_model
# fancyRpartPlot of the rattle package produces a similar but aesthetically more pleasing version of the same plot
rpart.plot(tree_model, box.palette="auto", shadow.col="gray", nn=TRUE)

tree_model_plot <- fancyRpartPlot(tree_model)
tree_model_plot

ggsave(
  "DT1_abalone_plot.png",
  plot = tree_model_plot,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Decision Tree\\",
  height = 19,
  width = 12
)

# The note in tree_model_plot of the rattle package was manually removed via an image editing software


##### End of this document #####