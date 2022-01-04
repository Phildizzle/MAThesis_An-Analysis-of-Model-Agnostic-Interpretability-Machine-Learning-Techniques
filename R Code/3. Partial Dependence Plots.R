###### Interpretability in ML - Chapter 6.1 Partial Dependence Plots                ######
## Reproduces all results including tables and figures from chapter 6.1 PDPs            ##
## Directories of csv's need to be changed to where the abalone data set is saved       ##
## Directories for individual chart plots need to be changed when reproducing the plots ##
## Last revision: 17/07/2020                                                            ##

##### 1. General commands #####

library(e1071)
library(dplyr)
library(doParallel)
library(ggplot2)
library(lattice)
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

# Create folder for the PDPs
dir.create("Plots - Partial Dependence Plots")

##### 2. Read CSV file into a single dataframe and pre-processing #####
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

##### 4. Calculate PDPs for Rings on WholeWeight, ShuckedWeight, Length, and Sex #####
PDP_WW <- partial(rf_model, pred.var = "WholeWeight", plot = TRUE, rug = TRUE,
                  plot.engine = "ggplot2")

PDP_SW <- partial(rf_model, pred.var = "ShuckedWeight", plot = TRUE, rug = TRUE,
                  plot.engine = "ggplot2")

PDP_Length <- partial(rf_model, pred.var = "Length", plot = TRUE, rug = TRUE,
                        plot.engine = "ggplot2")

PDP_SEX <- partial(rf_model, pred.var = "Sex", plot = TRUE, rug = TRUE,
                   plot.engine = "ggplot2")

PDP_grid <- grid.arrange(PDP_WW, PDP_SW, PDP_Length,PDP_SEX , ncol = 2, nrow = 2)

ggsave(
  "fig_5_PDP_1_Overview_WW_SW_LE_SEX_Rings.png",
  plot = PDP_grid,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Partial Dependence Plots",
  height = 10,
  width =10
)

##### 5. Calculate two-dimensional PDP for Rings #####
# We use the trellis-format of the lattice package to plot the two-dimensional PDPs
# Note each partial dependence with two features takes approximately 5 minutes when calculated with 1 processor
# Parallelization of the process is thus highly recommended
cluster <- makeCluster(6)    # use X processors 
registerDoParallel(cluster)  # register the parallel backend

PDP_WW_SW <- partial(rf_model, pred.var = c("WholeWeight", "ShuckedWeight"))
PDP_PLOT_WW_SW <- plotPartial(PDP_WW_SW)
png("Plots - Partial Dependence Plots\\fig_6_PDP_2_TWODIM_WW_SW.png", width = 500, height = 500)
PDP_PLOT_WW_SW
dev.off()

PDP_WW_SEX <- partial(rf_model, pred.var = c("WholeWeight", "Sex"))
PDP_PLOT_WW_SEX <- plotPartial(PDP_WW_SEX)
png("Plots - Partial Dependence Plots\\appendix_fig_1_PDP_3_TWODIM_WW_SEX.png", width = 600, height = 400)
PDP_PLOT_WW_SEX
dev.off()

PDP_WW_LE <- partial(rf_model, pred.var = c("WholeWeight", "Length"))
PDP_PLOT_WW_LE <- plotPartial(PDP_WW_LE)
png("Plots - Partial Dependence Plots\\appendix_fig_2_PDP_4_TWODIM_WW_LE.png", width = 500, height = 500)
PDP_PLOT_WW_LE
dev.off()

stopCluster(cluster)  # good practice to shut off the cluster otherwise it remains operating in the back

##### 6. Calculate PDPs for AgeCat on WholeWeight, ShuckedWeight, Length, and Sex #####
# Create categorical variable
df$AgeCat <- 0
age_mean <- mean(df$Rings) 
df$AgeCat[df$Rings > age_mean] = "1" # we separate by the mean here which is 9.933684 to show that we 
df$AgeCat[df$Rings < age_mean] = "0" # divide the sample in similar halfs, using 9 as a threshold 
df$AgeCat <- as.factor(df$AgeCat)   # since Rings-values can be only whole numbers.
                                     # This yields effectively the same result 

# Remove Rings column since it serves no purpose for classification
df$Rings <- NULL

# Calculate SVM model (Rings on all eight covariates) #####
svm_model_AgeCat <- svm(AgeCat ~ ., data = df, kernel = "radial", gamma = 0.75,cost = 0.25, probability = TRUE)
  
summary(svm_model_AgeCat)

# Subset dataframe for making predictions of tree_model
df_wo_AgeCat <- df %>% select(Sex, Length, Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight)
svm_prediction_AgeCat <- predict(svm_model_AgeCat, df_wo_AgeCat, type="class")

# Calculate confusion matrix and accuracy for text part
confMat <- table(df$AgeCat,svm_prediction_AgeCat)
confMat

accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

PDP_WW <- partial(svm_model_AgeCat, pred.var = "WholeWeight", plot = TRUE, rug = TRUE,
                  plot.engine = "ggplot2", type = "classification")

PDP_SW <- partial(svm_model_AgeCat, pred.var = "ShuckedWeight", plot = TRUE, rug = TRUE,
                  plot.engine = "ggplot2", type = "classification")

PDP_Length <- partial(svm_model_AgeCat, pred.var = "Length", plot = TRUE, rug = TRUE,
                      plot.engine = "ggplot2", type = "classification")

PDP_SEX <- partial(svm_model_AgeCat, pred.var = "Sex", plot = TRUE, rug = TRUE,
                   plot.engine = "ggplot2", type = "classification")


PDP_grid <- grid.arrange(PDP_WW, PDP_SW, PDP_Length,PDP_SEX , ncol = 2, nrow = 2)

ggsave(
  "fig_7_PDP_1_Overview_WW_SW_LE_SEX_AgeCat.png",
  plot = PDP_grid,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Partial Dependence Plots\\",
  height = 10,
  width =10
)

##### 7. Calculate two-dimensional PDP for AgeCat #####
# We use the trellis-format of the lattice package to plot the two-dimensional PDPs
# Note each partial dependence with two features takes approximately 5 minutes when calculated with 1 processor
# Parallelization of the process is thus highly recommended

cluster <- makeCluster(6)    # use X processors 
registerDoParallel(cluster)  # register the parallel backend

PDP_WW_SW <- partial(svm_model_AgeCat, pred.var = c("WholeWeight", "ShuckedWeight"), type = "classification")
PDP_PLOT_WW_SW <- plotPartial(PDP_WW_SW)
png("Plots - Partial Dependence Plots\\fig_8_PDP_2_TWODIM_WW_SW_AgeCat.png", width = 800, height = 800)
PDP_PLOT_WW_SW
dev.off()

PDP_WW_SEX <- partial(svm_model_AgeCat, pred.var = c("WholeWeight", "Sex"), type = "classification")
PDP_PLOT_WW_SEX <- plotPartial(PDP_WW_SEX)
png("Plots - Partial Dependence Plots\\appendix_fig_3_PDP_3_TWODIM_WW_SEX_AgeCat.png", width = 800, height = 800)
PDP_PLOT_WW_SEX
dev.off()

PDP_WW_LE <- partial(svm_model_AgeCat, pred.var = c("WholeWeight", "Length"), type = "classification")
PDP_PLOT_WW_LE <- plotPartial(PDP_WW_LE)
png("Plots - Partial Dependence Plots\\appendix_fig_4_PDP_4_TWODIM_WW_LE_AgeCat.png", width = 800, height = 800)
PDP_PLOT_WW_LE
dev.off()

stopCluster(cluster)  # good practice to shut off the cluster otherwise it remains operating in the back

##### 8. Calculate three-dimensional PDP for Rings on WholeWeight, ShuckedWeight, Length #####
# We use the trellis-format of the lattice package to plot the three-dimensional PDPs
# Note that this problem may take up to 8h to solve on a single processor
# Parallelization is thus recommended

cluster <- makeCluster(7)  # use 7 workers 
registerDoParallel(cluster)  # register the parallel backend

PDP_WW_SW <- partial(rf_model, pred.var = c("WholeWeight", "ShuckedWeight", "Length"))

stopCluster(cluster)  # good practice to shut off the cluster otherwise it's operating in the back

PDP_PLOT_WW_SW_LE <- plotPartial(PDP_WW_SW)
PDP_WW_SW_PLOT <- plotPartial(PDP_WW_SW, levelplot = FALSE, zlab = "Length", drape = TRUE,colorkey = TRUE, screen = list(z = 315, x = 250, y = 180))
PDP_WW_SW_PLOT

png("Plots - Partial Dependence Plots\\fig_9_PDP_THREE_DIM_WW_SW_LE_AgeCat.png", width = 1000, height = 1000)
PDP_WW_SW_PLOT
dev.off()

##### 9. Simulated example, when PDP fail #####
set.seed(1) # Seed 1 happens to be a good example
x_1 <- runif(10000, min=-1, max=1)
x_2 <- runif(10000, min=-1, max=1)
epsilon <- rnorm(10000, mean = 0, sd = 0.3)
y <- (x_1)^2 - 100*x_1*x_2 + epsilon

data <- as.data.frame(cbind(y,x_1,x_2))

scatter_plot <- ggplot(data, aes(x_2,y)) +
  geom_point()
scatter_plot

tree_model_example <- rpart(y ~ x_1 + x_2, data = data)

PDP_example <- partial(tree_model_example, pred.var = "x_2", plot = TRUE, smooth = TRUE,
                  plot.engine = "ggplot2")
PDP_example

PDP_example_grid <- grid.arrange(scatter_plot, PDP_example, ncol = 2, nrow = 1)
ggsave(
  "app2_1_PDP_example.png",
  plot = PDP_example_grid,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Partial Dependence Plots\\",
  height = 4,
  width =10
)


##### End of this document #####