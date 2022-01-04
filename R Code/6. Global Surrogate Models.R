##### Interpretability in ML - Chapter 6.4 Global Surrogate Models                          ######
## Reproduces all results including tables and figures from chapter 6.4 Global Surrogate Models ##
## Directories of csv's need to be changed to where the abalone data set is saved               ##
## Directories for individual chart plots need to be changed when reproducing the plots         ##
## Last revision: 17/07/2020

##### 1. General commands #####

library(dplyr)
library(e1071)
library(ggplot2)
library(lattice)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)	# Color selection for fancy tree plot
library(stargazer)

# Clear environment before starting the analysis
rm(list=ls())
# Set a seed to enable the reproduction of the exact results of the analysis
set.seed(1)

# Adjust the working directory
# Change working directory here
input.folder <- "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\"
setwd(input.folder)

# Create folder for the linear regression plots
dir.create("Plots - Global Surrogate Models")

##### 2. Read CSV file into a single dataframe and pre-processing
# Change csv directory to where the abalone data set is saved
df <- read.csv(paste("C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\" ,"abalone",".csv", sep =""), sep=",", 
               header=TRUE, quote = "",row.names = NULL)

# Reverse previous pre-processing to obtain interpretable data values
df$Length <- df$Length*200
df$Diameter <- df$Diameter*200
df$Height <- df$Height*200
df$WholeWeight <- df$WholeWeight*200
df$ShuckedWeight <- df$ShuckedWeight*200
df$VisceraWeight <- df$VisceraWeight*200
df$ShellWeight <- df$ShellWeight*200
df$Sex <- as.factor(df$Sex)

##### 3. Calculate random forest model (Rings on all eight covariates) #####
rf_model <- randomForest(Rings ~ ., data = df, importance = TRUE)
summary(rf_model)

# Subset dataframe for making predictions of tree_model
df_wo_Rings <- df %>% select(Sex, Length, Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight)
tree_prediction <- predict(rf_model, df_wo_Rings, type = "Response")

X <- cbind(df[which(names(df) != "Rings")], tree_prediction)

##### 4. Calculate Global Surrogate Model (linear regression) for all covariates #####
linear_model <- lm(tree_prediction ~ ., data = X)
summary(linear_model)

# Calculate the linear model from chapter 5.1 for comparison purposes
linear_model_ch5 <- lm(Rings ~ ., data = df)
linear_model_sum <- summary(linear_model)

linear_model_sum <- summary(linear_model)
latex_output_linear_model <- stargazer(linear_model,linear_model_ch5, type = "latex")
latex_output_linear_model

##### 5. Calculate Global Surrogate Model for classification task #####

# Create categorical variable
df$AgeCat <- 0
age_mean <- mean(df$Rings) 
df$AgeCat[df$Rings > age_mean] = "1" # we separate by the mean here which is 9.933684 to prove that we 
df$AgeCat[df$Rings < age_mean] = "0" # divide the sample in similar halfs, using 9 as a threshold
df$AgeCat <- as.factor(df$AgeCat)   # yields effectively the same result 

# Remove Rings column since it serves no purpoes for classification
df$Rings <- NULL

# Calculate SVM model (Rings on all eight covariates) #####
svm_model_AgeCat <- svm(AgeCat ~ ., data = df, kernel = "radial", gamma = 0.75, cost = 0.25, probability = TRUE)

summary(svm_model_AgeCat)

df_wo_AgeCat <- df %>% select(Sex, Length, Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight)

svm_prediction <- predict(svm_model_AgeCat, df_wo_AgeCat, type = "Class")

X <- cbind(df[which(names(df) != "AgeCat")], svm_prediction)

##### 3. Calculate decision tree model (Rings on all eight covariates #####
tree_model <- rpart(svm_prediction~., data=X, method = "class")
summary(tree_model)
tree_prediction <- predict(tree_model, df_wo_AgeCat, type = "class")

# Calculate confusion matrix and accuracy for text part
confMat <- table(df$AgeCat,svm_prediction)
confMat

accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

##### 5. Create a decision tree plot and save the png file #####
# rpart.plot plots a first version of the decision tree in tree_model
# fancyRpartPlot of the rattle package produces a similar but aesthetically more pleasing version of the same plot

rpart_tree_plot <- rpart.plot(tree_model, box.palette="auto", shadow.col="gray", nn=TRUE)

tree_model_plot <- fancyRpartPlot(tree_model)
tree_model_plot

setwd("C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Global Surrogate Models")


png("fig_18_surrogate_tree.png", width = 800, height = 600)
tree_model_plot <- fancyRpartPlot(tree_model)
dev.off()


ggsave(
  "fig_18_surrogate_tree.png",
  plot = rpart_tree_plot,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Global Surrogate Models",
  height = 19,
  width = 12
)


##### End of this document #####