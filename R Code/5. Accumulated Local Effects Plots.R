##### Interpretability in ML - Chapter 6.3 Accumulated Local Effects Plots             ######
## Reproduces all results including tables and figures from chapter 6.3 ALE Plots          ##
## Directories of csv's need to be changed to where the abalone data set is saved          ##
## Directories for individual chart plots need to be changed when reproducing the plots    ##
## Last revision: 17/07/2020                                                               ##

##### 1. General commands #####

library(e1071)
library(ALEPlot)
library(ggplot2)
library(gridExtra)
library(lattice)
library(iml)
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
dir.create("Plots - Accumulated Local Efects Plots")

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

X <- cbind(df[which(names(df) != "Rings")])

##### 4. Calculate Accoumulated Local Effect Plots for WholeWeight, ShuckedWeight, Length, and Sex for Rings #####
ale_WW <- FeatureEffect$new(predictor, method = "ale", feature = "WholeWeight")
ale_WW_plot <- ale_WW$plot()

ale_SW <- FeatureEffect$new(predictor, method = "ale", feature = "ShuckedWeight")
ale_SW_plot <- ale_SW$plot()

ale_LE <- FeatureEffect$new(predictor, method = "ale", feature = "Length")
ale_LE_plot <- ale_LE$plot()

ale_SE <- FeatureEffect$new(predictor, method = "ale", feature = "Sex")
ale_SE_plot <- ale_SE$plot()

ALE_grid_RINGS <- grid.arrange(ale_WW_plot, ale_SW_plot, ale_LE_plot, ale_SE_plot , ncol = 2, nrow = 2)

ggsave(
  "fig_14_ALE_OV_RINGS.png",
  plot = ALE_grid_RINGS,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Accumulated Local Efects Plots",
  height = 10,
  width =10
)

##### 5. Calculate second-order ALE for WholeWeight and ShuckedWeight for Rings #####

ale_WW_SW <- FeatureEffect$new(predictor, method = "ale", feature = c("WholeWeight", "ShuckedWeight"))
ale_WW_SW_plot <- ale_WW_SW$plot()

gmggsave(
  "fig_15_ALE_SO_WW_SW.png",
  plot = ale_WW_SW_plot,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Accumulated Local Efects Plots",
  height = 10,
  width =10
)

##### 6. Calculate Accoumulated Local Effect Plots for WholeWeight, ShuckedWeight, Length, and Sex for AgeCat #####
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

X <- df[which(names(df) != "AgeCat")]

predictor <- Predictor$new(svm_model_AgeCat, data = X, y = df$AgeCat)


ale_WW <- FeatureEffect$new(predictor, method = "ale", feature = "WholeWeight")
ale_WW_plot <- ale_WW$plot()

ale_SW <- FeatureEffect$new(predictor, method = "ale", feature = "ShuckedWeight")
ale_SW_plot <- ale_SW$plot()

ale_LE <- FeatureEffect$new(predictor, method = "ale", feature = "Length")
ale_LE_plot <- ale_LE$plot()

ale_SE <- FeatureEffect$new(predictor, method = "ale", feature = "Sex")
ale_SE_plot <- ale_SE$plot()

ALE_grid_AGECAT <- grid.arrange(ale_WW_plot, ale_SW_plot, ale_LE_plot, ale_SE_plot , ncol = 2, nrow = 2)

ggsave(
  "fig_16_ALE_OV_AGECAT.png",
  plot = ALE_grid_AGECAT,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Accumulated Local Efects Plots",
  height = 10,
  width = 10
)

##### 7. Calculate second-order ALE for WholeWeight and ShuckedWeight for Rings #####

ale_WW_SW <- FeatureEffect$new(predictor, method = "ale", feature = c("WholeWeight", "ShuckedWeight"))
ale_WW_SW_plot <- ale_WW_SW$plot()

ggsave(
  "fig_17_ALE_SO_WW_SW_AGECAT.png",
  plot = ale_WW_SW_plot,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Accumulated Local Efects Plots",
  height = 10,
  width =10
)

############################################
# not part of the plots of this thesis but for the interested reader:
# the imlplots package builds a shiny application with a dynamic interface
# to compute PDPs, ICEs, c-ICEs, and ALEs in a configurable environment
# the plotting environment is less nice as with the individual packags themselves
# but the dynamic interface is interesting for explorimentation
# 
# devtools::install_github('compstat-lmu/imlplots')
# library(imlplots)
# 
# abalone.task = makeRegrTask(data = df, target = "Rings")
# 
# rf.mod = train("regr.randomForest", abalone.task)
# glm.mod = train("regr.glm", abalone.task)
# mod.list = list(rf.mod, glm.mod)
# 
# imlplots(data = df, task = abalone.task, models = mod.list)
# 
# 
# ALE_RF <- ALEPlot(df[,2:4], rf_model, pred.fun = yhat, J = 1, K = 50, NA.plot = TRUE)


##### End of this document #####