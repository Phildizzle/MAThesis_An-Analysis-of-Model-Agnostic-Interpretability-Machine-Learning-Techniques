###### Interpretability in ML - Chapter 5.1 Linear Regression                         ######
## Reproduces all results including tables and figures from chapter 5.1 Linear regression ##
## Needs "Helper functions.R" for the effect and weight plots                             ##
## Directories of csv's need to be changed to where the abalone data set is saved         ##
## Source directory needs to be changed to where the helper functions are located         ##
## Directories for individual chart plots need to be changed when reproducing the plots   ##
## Last revision: 17/07/2020                                                              ##

##### 1. General commands #####

library(dplyr)
library(ggplot2)
library(stargazer)

# Clear environment before starting the analysis
rm(list=ls())
# Set a seed to enable the reproduction of the exact results of the analysis
set.seed(1)

# Change source directory for helper functions here
# Import helper functions needed for effect and coefficient plot
source("C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Helper functions.R")

# Adjust the working directory
# Change working directory here
input.folder <- "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\"
setwd(input.folder)

# Create folder for the linear regression plots
dir.create("Plots - Linear Regression")

##### 2. Read CSV file into a single dataframe #####
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

##### 3. Calculate linear model (Rings on all eight covariates) and create latex output #####
linear_model <- lm(Rings ~ ., data = df)
summary(linear_model)

linear_model_sum <- summary(linear_model)
latex_output_linear_model <- stargazer(linear_model, type = "latex")
latex_output_linear_model

##### 4. Create a weight plot of the coefficients and save the png file #####
# the coef_plot function is part of the 'Helper functions.R'-script in the source working directory
# calculate normalized linear model
linear_model_norm <- lm(scale(Rings) ~ Sex + scale(Length) + scale(Diameter) + scale(Height) + scale(WholeWeight) + scale(ShuckedWeight) + scale(VisceraWeight) + scale(ShellWeight), data = df)

coef_plot_lm <- coef_plot(linear_model_norm)
coef_plot_lm

ggsave(
  "fig_1_abalone_coeff_plot_lm.png",
  plot = last_plot(),
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Linear Regression\\",
  height = 6,
  width = 8.5
)

##### 5. Create an effect plot of the coefficients and save the png file #####
# the effect_plot function is part of the 'Helper functions.R'-script in the source working directory

effect_plot_lm <- effect_plot(linear_model, df)
effect_plot_lm

ggsave(
  "fig_2_abalone_effect_plot_lm.png",
  plot = last_plot(),
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Linear Regression\\",
  height = 6,
  width = 8.5
)

##### Effects for obs. 28 were manually calculated and plotted in the png of effect_plot_lm

##### 6. Feature plot for non-normalized coefficients #####

# Reread data-set without processing
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

linear_model <- lm(Rings ~ ., data = df)
coef_plot_lm <- coef_plot(linear_model)

ggsave(
  "app_0_fig_abalone_effect_plot_lm_norm.png",
  plot = coef_plot_lm,
  path = "C:\\Users\\PJ2892!\\Desktop\\MA Data\\Abalone Data\\Plots - Linear Regression\\",
  height = 6,
  width = 8.5
)


##### End of this document #####