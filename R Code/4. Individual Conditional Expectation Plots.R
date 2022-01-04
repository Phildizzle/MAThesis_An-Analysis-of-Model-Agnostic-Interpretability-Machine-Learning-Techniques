###### Interpretability in ML - Chapter 6.2 Individual Conditional Expectation Plots  ######
## Reproduces all results including tables and figures from chapter 6.2 ICE Plots         ##
## Directories of csv's need to be changed to where the abalone data set is saved         ##
## Directories for individual chart plots need to be changed when reproducing the plots   ##
## Last revision: 17/07/2020                                                              ##

##### 1. General commands #####

library(e1071)
library(dplyr)
library(doParallel)
library(ggplot2)
library(lattice)
library(ICEbox)
library(parallel)
library(pdp)
library(randomForest)

# Clear environment before starting the analysis
rm(list=ls())
# Set a seed to enable the reproduction of the exact results of the analysis
set.seed(1)

# Adjust the working directory
# Change working directory here
input.folder <- "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\"
setwd(input.folder)

# Create folder for the ICE plots
dir.create("Plots - Individual Conditional Expectation Plots")

##### 2. Read CSV file into a single dataframe and pre-processing
# Change csv directory to where the abalone data set is saved
df <- read.csv(paste("C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\" ,"abalone",".csv", sep =""), sep=",", 
               header=TRUE, quote = "",row.names = NULL)

# Get an overview of the data
str(df)

# Reverse previous pre-processing to obtain interpretable data values
df$Length <- df$Length*200
df$Diameter <- df$Diameter*200
df$Height <- df$Height*200
df$WholeWeight <- df$WholeWeight*200
df$ShuckedWeight <- df$ShuckedWeight*200
df$VisceraWeight <- df$VisceraWeight*200
df$ShellWeight <- df$ShellWeight*200
df$Sex <- as.factor(df$Sex)

# Get an overview of the data again to check if preprocessing was successful
str(df)

##### 3. Calculate random forest model (Rings on all eight covariates) #####
rf_model <- randomForest(Rings ~ ., data = df, importance = TRUE)
summary(rf_model)

# Subset dataframe for making predictions of tree_model
df_wo_Rings <- df %>% select(Sex, Length, Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight)
tree_prediction <- predict(rf_model, df_wo_Rings, type = "Response")

# Calculate MSE
MSE_RF <- mean((df$Rings - tree_prediction)^2)
MSE_RF

##### 4. Calculate Individual Conditional Expectation Plots for Rings on WholeWeight, ShuckedWeight, Length, and Sex #####
# we can still use the pdp-package to obtain ICE curves

pred.ice <- function(object, newdata) predict(object, newdata)

ICE_WW <- partial(rf_model, pred.var = "WholeWeight", plot = TRUE, rug = TRUE, 
                  pred.fun = pred.ice, alpha = 0.08, plot.engine = "ggplot2")

ICE_SW <- partial(rf_model, pred.var = "ShuckedWeight", plot = TRUE, rug = TRUE,
                  pred.fun = pred.ice, alpha = 0.08, plot.engine = "ggplot2")

ICE_Length <- partial(rf_model, pred.var = "Length", plot = TRUE, rug = TRUE,
                      pred.fun = pred.ice, alpha = 0.08, plot.engine = "ggplot2")

ICE_SEX <- partial(rf_model, pred.var = "Sex", plot = TRUE, rug = TRUE,
                   pred.fun = pred.ice, alpha = 0.08, plot.engine = "ggplot2")


PDP_grid <- grid.arrange(ICE_WW, ICE_SW, ICE_Length, ICE_SEX , ncol = 2, nrow = 2)

ggsave(
  "fig_10_ICE_WW_SW_LE_SE_Overview.png",
  plot = PDP_grid,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Individual Conditional Expectation Plots",
  height = 10,
  width =10
)

##### 5. Calculate Individual Conditional Expectation Plots for AgeCat #####

# Create categorical variable
df$AgeCat <- 0
age_mean <- mean(df$Rings) 
df$AgeCat[df$Rings > age_mean] = "1" # we separate by the mean here which is 9.933684 to prove that we 
df$AgeCat[df$Rings < age_mean] = "0" # divide the sample in similar halfs, using 9 as a threshold
df$AgeCat <- as.factor(df$AgeCat)   # yields effectively the same result 

# Remove Rings column since it serves no purpoes for classification
df$Rings <- NULL

# Calculate SVM model (Rings on all eight covariates)
svm_model_AgeCat <- svm(AgeCat ~ ., data = df, kernel = "radial", gamma = 0.75, cost = 0.25, probability = TRUE)

summary(svm_model_AgeCat)

# Subset dataframe for making predictions of tree_model
df_wo_AgeCat <- df %>% select(Sex, Length, Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight)
svm_prediction_AgeCat <- predict(svm_model_AgeCat, df_wo_AgeCat, type = "Probability")

# Calculate confusion matrix and accuracy for text part
confMat <- table(df$AgeCat,svm_prediction_AgeCat)
confMat

accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

cluster <- makeCluster(6)  # use X processors 
registerDoParallel(cluster)  # register the parallel backend

ICE_WW <- partial(svm_model_AgeCat, pred.var = "WholeWeight", plot = TRUE, rug = TRUE,
                  ice = TRUE, alpha = 0.08, plot.engine = "ggplot2", type = "auto")

ICE_SW <- partial(svm_model_AgeCat, pred.var = "ShuckedWeight", plot = TRUE, rug = TRUE,
                  ice = TRUE, alpha = 0.08, plot.engine = "ggplot2", type = "classification")

ICE_Length <- partial(svm_model_AgeCat, pred.var = "Length", plot = TRUE, rug = TRUE,
                      ice = TRUE, alpha = 0.08, plot.engine = "ggplot2", type = "classification")

ICE_SEX <- partial(svm_model_AgeCat, pred.var = "Sex", plot = TRUE, rug = TRUE,
                   ice = TRUE, alpha = 0.08, plot.engine = "ggplot2", type = "classification")

ICE_grid <- grid.arrange(ICE_WW, ICE_SW, ICE_Length, ICE_SEX , ncol = 2, nrow = 2)

ggsave(
  "fig_11_ICE_WW_SW_LE_SE_Overview_AgeCat.png",
  plot = ICE_grid,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Individual Conditional Expectation Plots",
  height = 10,
  width =10
)

stopCluster(cluster)  # good practice to shut off the cluster otherwise it's operating in the back

##### 6. Calculate centered Individual Conditional Expectation Plots for AgeCat on ShuckedWeight#####
cICE_SW <- partial(svm_model_AgeCat, pred.var = "ShuckedWeight", plot = TRUE, rug = TRUE, 
                   ice = TRUE, alpha = 0.08, plot.engine = "ggplot2", center = TRUE)

ggsave(
  "fig_12_cICE_SW.png",
  plot = cICE_WW,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Individual Conditional Expectation Plots",
  height = 10,
  width = 10
)
cICE_SW <- partial(svm_model_AgeCat, pred.var = "ShuckedWeight", plot = TRUE, rug = TRUE, 
                   ice = TRUE, alpha = 0.08, plot.engine = "ggplot2", center = TRUE)
cICE_WW

##### 7. Calculate derivative ICE Plots for WholeWeight on Rings #####
# Re-read data set
# Change csv directory to where the abalone data set is saved
df <- read.csv(paste("C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\" ,"abalone",".csv", sep =""), sep=",", 
               header=TRUE, quote = "",row.names = NULL)

# Get an overview of the data
str(df)

# Reverse previous pre-processing to obtain interpretable data values
df$Length <- df$Length*200
df$Diameter <- df$Diameter*200
df$Height <- df$Height*200
df$WholeWeight <- df$WholeWeight*200
df$ShuckedWeight <- df$ShuckedWeight*200
df$VisceraWeight <- df$VisceraWeight*200
df$ShellWeight <- df$ShellWeight*200
df$Sex <- as.factor(df$Sex)

# Get an overview of the data again to check if preprocessing was successful
str(df)

##### re-calculate random forest model (Rings on all eight covariates) #####
rf_model <- randomForest(Rings ~ ., data = df, importance = TRUE)
summary(rf_model)

# Subset dataframe for making predictions of tree_model
df_wo_Rings <- df %>% select(Sex, Length, Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight)

aba_ice <- ice(object = rf_model, X = df_wo_Rings, y = df$Rings, predictor = "WholeWeight", frac_to_build = .1)

aba_dice <- dice(aba.ice)
plot_dice <- plot(aba_dice)
png("Plots - Individual Conditional Expectation Plots\\fig_13_dICE_WW.png", width = 500, height = 500)
plot(aba_dice)
dev.off()


##### End of this document #####