#-----Imports-----
library(randomForest)
library(tree)
library(caret)

setwd("C:\\Users\\NEWUSER\\Documents\\Classes\\Graduate\\DS_501\\Case_Study_3\\Fast_Food_Predictor")
list.files(path='.')


#-----Splitting Data-----
fast_food_raw = read.csv("fastfood.csv")

fast_food_subset = subset(fast_food_raw, select = -c(item, salad, vit_a, vit_c, calcium, calories, total_fat, sat_fat, trans_fat, cholesterol))
fast_food_subset = na.omit(fast_food_subset)

#-----Creation of Model-----
gbm_model <- gbm(factor(restaurant) ~ sodium + cal_fat + total_carb + fiber + sugar + protein, data = fast_food_subset, distribution = "multinomial", n.trees = 10000, shrinkage = 0.001, interaction.depth = 8)

#-----Saving Model-----
saveRDS(gbm_model, "gbm_model.rds")

