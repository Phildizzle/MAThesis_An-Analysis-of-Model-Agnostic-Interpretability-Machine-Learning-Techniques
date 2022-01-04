###### Interpretability in ML - Chapter 6.5 Local surrogate models (LIME)  ######
## Reproduces all results including tables and figures from chapter 6.5 Local surrogate models (LIME) ##
##### Interpretability in ML - Chapter 6.5 Local surrogate models (LIME)                          ######
## Reproduces all results including tables and figures from chapter 6.5 Local surrogate models (LIME) ##
## Directories of csv's need to be changed to where the abalone data set is saved               ##
## Directories for individual chart plots need to be changed when reproducing the plots         ##
## Last revision: 17/07/2020

##### 1. General commands #####

library(dplyr)
library(e1071)
library(ggplot2)
library(kernlab)
library(lattice)
library(lime)
library(mlr)
library(randomForest)

# Clear environment before starting the analysis
rm(list=ls())
# Set a seed to enable the reproduction of the exact results of the analysis
set.seed(1)

# Adjust the working directory
# Change working directory here
input.folder <- "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\"
setwd(input.folder)

# Create folder for the linear regression plots
dir.create("Plots - Local surrogate models (LIME)")

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
##### creating regression task with the mlr package since LIME         #####
##### can only handle specific input functions                         #####

obs_28 <- df[28,which(names(df) != "Rings")]
train_set_wo__obs_28 <- df[-28, ]

# make regression task and calculate random forest model
task_abalone <- makeRegrTask(data = train_set_wo__obs_28, 
                              target = "Rings")
learner <- makeLearner("regr.randomForest", predict.type = "response")
black_box <- train(learner, task_abalone)

##### 4. Calculate Local surrogate models (LIME) for regression task #####
lime_obs_28 <- lime(train_set_wo__obs_28[, 1:8], black_box)
explanation <- explain(obs_28, lime_obs_28, n_features = 8)

lime_Rings_28 <- plot_features(explanation)

ggsave(
  "fig_19_lime_Rings.png",
  plot = lime_Rings_28,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Local surrogate models (LIME)",
  height = 7,
  width = 7
)

##### 5. Calculate support vector machine model (Agecat on all eight covariates) #####
##### by creating regression task with the mlr package since LIME                #####
##### an only handle specific input functions                                    #####

# Create categorical variable
df$AgeCat <- 0
age_mean <- mean(df$Rings) 
df$AgeCat[df$Rings > age_mean] = "1" # we separate by the mean here which is 9.933684 to prove that we 
df$AgeCat[df$Rings < age_mean] = "0" # divide the sample in similar halfs, using 9 as a threshold
df$AgeCat <- as.factor(df$AgeCat)   # yields effectively the same result 

# Remove Rings column since it serves no purpoes for classification
df$Rings <- NULL

obs_28 <- df[28,which(names(df) != "AgeCat")]
train_set_wo__obs_28 <- df[-28, ]

# make regression task and calculate random forest model
# unfortunately we have to use a different package to calculate the SVM model 
# both models are however identical and the difference between them is negligible
task_abalone <- makeClassifTask(data = train_set_wo__obs_28, 
                             target = "AgeCat")
learner <- makeLearner("classif.ksvm", predict.type = "prob")
black_box <- train(learner, task_abalone)

##### 6. Calculate Local surrogate models (LIME) for classification task #####
lime_obs_28 <- lime(train_set_wo__obs_28[, 1:8], black_box)
explanation <- explain(obs_28, lime_obs_28, n_features = 7, n_labels = 1)

lime_AgeCat_28 <- plot_features(explanation)

ggsave(
  "fig_20_lime_AgeCat.png",
  plot = lime_AgeCat_28,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Local surrogate models (LIME)",
  height = 7,
  width = 7
)


##### End of this document #####