# Edupristine | CART - Regression Tree | Insurance Case Study
install.packages("forecast")
install.packages("rattle")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(forecast)
library(rattle)

# Set the working directory to folder where you have placed the Input Data
# setwd(dir = )
setwd("D:/BAContent/CaseStudiesAdvanced/CART-Regression Tree (Insurance Industry)");

  InsData = read.csv(file = "Insurance_Dataset.csv", header = T)

View(x = InsData)

# Summarize the dataset
summary(object = InsData)

# Look at the average Losses()
for(i in 2:ncol(InsData))
{
  if(length(unique(InsData[,i])) <= 5)
  {
    AverageLoss = aggregate(x = InsData$Losses, by = list(InsData[,i]), FUN = mean)
    print(colnames(InsData)[i])
    print(AverageLoss)
    print("************************************************************")
  
    }
}

# Boxplot/ Plot
InsuranceBoxPlot = boxplot(x = InsData[,-1]) # Look for outliers

# Print Five-Number summary for each variable obtained from boxplot
colnames(InsuranceBoxPlot$stats) = colnames(InsData[,-1])
InsuranceBoxPlot$stats

# Divide the analysis into 2 parts. Part A will include building a model with the Dependent Variable (Losses) as is and 
# on the other hand, Part B will include building a model after "capping" the Dependent Variable to a certain upper limit value

###################### PART A Starts ######################

# Make a copy of the Original Dataset
InsDataUncapped = InsData

# Random Sampling
set.seed(777) # To ensure reproducibility
Index = sample(x = 1:nrow(InsDataUncapped), size = 0.7*nrow(InsDataUncapped))

# Create Train dataset
InsDataTrainUncapped = InsDataUncapped[Index, ]
nrow(InsDataTrainUncapped)

# Create Test dataset
InsDataTestUncapped = InsDataUncapped[-Index, ]
nrow(InsDataTestUncapped)

# Modeling

# Build a full model with default settings
set.seed(123) # To ensure reproducibility of xerrors (cross validated errors while estimating complexity paramter for tree pruning)
CartFullModel = rpart(formula = Losses ~ . , data = InsDataTrainUncapped[,-1], method = "anova")

CartFullModel

summary(object = CartFullModel)


# Plot the Regression Tree
rpart.plot(x = CartFullModel, type = 4,fallen.leaves = T, cex = 0.6)
title("CartFullModel") # Enlarge the plot by clicking on Zoom button in Plots Tab on R Studio

# fancyRpartPlot() function to plot the same model
# Expand the plot window in R Studio to see a presentable output
fancyRpartPlot(model = CartFullModel, main = "CartFullModel", cex = 0.6) 

# The following code also produces the same output, but in a windowed form
# windows()
# fancyRpartPlot(model = CartFullModel, main = "CartFullModel", cex = 0.6)

printcp(x = CartFullModel)

rsq.rpart(x = CartFullModel)
# This produces a plot which may help particpants to look for a model depending on R-Square values produced at various splits

# Lets change rpart.control() to specify certain attributes for tree building
RpartControl = rpart.control(cp = 0.005)
set.seed(123)
CartModel_1 = rpart(formula = Losses ~ . , data = InsDataTrainUncapped[,-1], method = "anova", control = RpartControl)

CartModel_1
summary(CartModel_1)
rpart.plot(x = CartModel_1, type = 4,fallen.leaves = T, cex = 0.6)
printcp(x = CartModel_1)
rsq.rpart(x = CartModel_1)


# CartModel_2
RpartControl = rpart.control(cp = 0.015) # Increase cp to 0.015
set.seed(123)
CartModel_2 = rpart(formula = Losses ~ . , data = InsDataTrainUncapped[,-1], method = "anova", control = RpartControl)

CartModel_2
summary(CartModel_2)
rpart.plot(x = CartModel_2, type = 4,fallen.leaves = T, cex = 0.6)
printcp(x = CartModel_2)
rsq.rpart(x = CartModel_2)

# CartModel_3
RpartControl = rpart.control(cp = 0.02) # Increase cp to 0.2
set.seed(123)
CartModel_3 = rpart(formula = Losses ~ . , data = InsDataTrainUncapped[,-1], method = "anova", control = RpartControl)

CartModel_3
summary(CartModel_3)
rpart.plot(x = CartModel_3, type = 4,fallen.leaves = T, cex = 0.6)
printcp(x = CartModel_3)
rsq.rpart(x = CartModel_3)

# CartModel_4
RpartControl = rpart.control(cp = 0.02) # Increase cp to 0.015
set.seed(123)
CartModel_4 = rpart(formula = Losses ~ Age + Fuel.Type + Vehicle.Age, 
                    data = InsDataTrainUncapped[,-1], method = "anova", control = RpartControl)

CartModel_4
summary(CartModel_4)
rpart.plot(x = CartModel_4, type = 4,fallen.leaves = T, cex = 0.6)
printcp(x = CartModel_4)
rsq.rpart(x = CartModel_4)

##########################################################################################################
# The following code for pruning a tree is only required to be run for the purpose of understanding how pruning is done
# Lets change rpart.control() to specify certain attributes for tree building
RpartControl = rpart.control(cp = 0.0005)
set.seed(123)
CartModel_5 = rpart(formula = Losses ~ . , data = InsDataTrainUncapped[,-1], method = "anova", control = RpartControl)

CartModel_5
summary(CartModel_5)
rpart.plot(x = CartModel_5, type = 4,fallen.leaves = T, cex = 0.6)
printcp(x = CartModel_5)
rsq.rpart(x = CartModel_5)

printcp(x = CartModel_5)

set.seed(123)
CartPrunedModel = prune(tree = CartModel_5, cp = 0.0045508)
printcp(CartPrunedModel) # Validate pruned tree by seeing the printcp result
##########################################################################################################

# Model Evaluation Measures on test dataset using the finalized (pruned model)
# Use predict() the get the predicted values for the testset using the finalized model

# Intermediate Model: Finalize CartFullModel (Based on Tree size i.e. Depth, Variables included as well as the R-Square produced)
# Predict on testset
CartFullModelPredictTest = predict(object = CartFullModel, newdata = InsDataTestUncapped, type = "vector")

# Calculate RMSE and MAPE manually

# Participants can calculate RMSE and MAPE using various available functions in R, but that may not
# communicate effectively the mathematical aspect behind the calculations

# RMSE
Act_vs_Pred = CartFullModelPredictTest - InsDataTestUncapped$Losses # Differnce
Act_vs_Pred_Square = Act_vs_Pred^2 # Square
Act_vs_Pred_Square_Mean = mean(Act_vs_Pred_Square) # Mean
Act_vs_Pred_Square_Mean_SqRoot = sqrt(Act_vs_Pred_Square_Mean) # Square Root
Act_vs_Pred_Square_Mean_SqRoot

# MAPE
Act_vs_Pred_Abs = abs(CartFullModelPredictTest - InsDataTestUncapped$Losses) # Absolute Differnce
Act_vs_Pred_Abs_Percent = Act_vs_Pred_Abs/InsDataTestUncapped$Losses # Percent Error
Act_vs_Pred_Abs_Percent_Mean = mean(Act_vs_Pred_Abs_Percent)*100 # Mean
Act_vs_Pred_Abs_Percent_Mean

# Validate RMSE and MAPE calculation with a function in R
UncappedModelAccuarcy = accuracy(f = CartFullModelPredictTest, x = InsDataTestUncapped$Losses)

###################### PART A Ends ######################



###################### PART B Starts ######################

# Look at quantiles to cap the Losses Column
quantile(x = InsData$Losses, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.85, 0.95, 0.99, 1))

# Make a copy
InsDataCapped = InsData

# Capping the Losses column
InsDataCapped$CappedLosses = ifelse(test = InsData$Losses > 1200, yes = 1200, no = InsData$Losses)

summary(object = InsDataCapped)

# Check Column Names
colnames(x = InsDataCapped)

# Remove Losses column
InsDataCapped = InsDataCapped[,-9]

# Boxplot/ Plot
boxplot(x = InsDataCapped[,-1]) # Look for outliers

# Average Capped Losses()
for(i in 2:ncol(InsDataCapped))
{
  if(length(unique(InsDataCapped[,i])) <= 5)
  {
    AverageLoss = aggregate(x = InsDataCapped$CappedLosses, by = list(InsDataCapped[,i]), FUN = mean)
    print(colnames(InsDataCapped)[i])
    print(AverageLoss)
    print("************************************************************")
  }
}

# Random Sampling
set.seed(777) # To ensure reproducibility
Index = sample(x = 1:nrow(InsDataCapped), size = 0.7*nrow(InsDataCapped))

# Create Train dataset
InsDataTrainCapped = InsDataCapped[Index, ]
nrow(InsDataTrainCapped)

# Create Test dataset
InsDataTestCapped = InsDataCapped[-Index, ]
nrow(InsDataTestCapped)

# Build a full model with default settings
set.seed(123) # To ensure reproducibility of xerrors (cross validated errors while estimating complexity paramter)
CartCappedFullModel = rpart(formula = CappedLosses ~ . , data = InsDataTrainCapped[,-1], method = "anova")

CartCappedFullModel
summary(object = CartCappedFullModel)
rpart.plot(x = CartCappedFullModel, type = 4,fallen.leaves = T, cex = 0.6)
printcp(x = CartCappedFullModel)
rsq.rpart(x = CartCappedFullModel)

# Lets change rpart.control() to specify certain attributes for tree building
# CartModel_11
RpartControl = rpart.control(cp = 0.005)
set.seed(123)
CartModel_11 = rpart(formula = CappedLosses ~ . , data = InsDataTrainCapped[,-1], method = "anova", control = RpartControl)

CartModel_11
summary(CartModel_11)
rpart.plot(x = CartModel_11, type = 4,fallen.leaves = T, cex = 0.6)
printcp(x = CartModel_11)
rsq.rpart(x = CartModel_11)


# CartModel_12
RpartControl = rpart.control(cp = 0.015) # Increase cp to 0.015
set.seed(123)
CartModel_12 = rpart(formula = CappedLosses ~ . , data = InsDataTrainCapped[,-1], method = "anova", control = RpartControl)

CartModel_12
summary(CartModel_12)
rpart.plot(x = CartModel_12, type = 4,fallen.leaves = T, cex = 0.6)
printcp(x = CartModel_12)
rsq.rpart(x = CartModel_12)

# CartModel_13
RpartControl = rpart.control(cp = 0.02) # Increase cp to 0.2
set.seed(123)
CartModel_13 = rpart(formula = CappedLosses ~ . , data = InsDataTrainCapped[,-1], method = "anova", control = RpartControl)

CartModel_13
summary(CartModel_13)
rpart.plot(x = CartModel_13, type = 4,fallen.leaves = T, cex = 0.6)
printcp(x = CartModel_13)
rsq.rpart(x = CartModel_13)

# Finalize CartModel_12 (Based on Tree size - Depth, Variables included as well as the R-Square produced)
# Predict on testset
CartModel_12PredictTest = predict(object = CartModel_12, newdata = InsDataTestCapped, type = "vector")

# Calculate RMSE and MAPE manually
# RMSE
CappedModel_Act_vs_Pred = CartModel_12PredictTest - InsDataTestCapped$CappedLosses # Differnce
CappedModel_Act_vs_Pred_Square = CappedModel_Act_vs_Pred^2 # Square
CappedModel_Act_vs_Pred_Square_Mean = mean(CappedModel_Act_vs_Pred_Square) # Mean
CappedModel_Act_vs_Pred_Square_Mean_SqRoot = sqrt(CappedModel_Act_vs_Pred_Square_Mean) # Square Root
CappedModel_Act_vs_Pred_Square_Mean_SqRoot

# MAPE
CappedModel_Act_vs_Pred_Abs = abs(CartModel_12PredictTest - InsDataTestCapped$CappedLosses) # Absolute Differnce
CappedModel_Act_vs_Pred_Abs_Percent = CappedModel_Act_vs_Pred_Abs/InsDataTestCapped$CappedLosses # Percent Error
CappedModel_Act_vs_Pred_Abs_Percent_Mean = mean(CappedModel_Act_vs_Pred_Abs_Percent)*100 # Mean
CappedModel_Act_vs_Pred_Abs_Percent_Mean

# Validate the same with a function in R
CappedModelAccuarcy = accuracy(f = CartModel_12PredictTest, x = InsDataTestCapped$CappedLosses)

# Select one final model from two intermediate finalized models - With the help of RMSE and MAPE
# Although, MAPE for the two finalized models, namely, CartFullModel and CartModel_12 happen to be very close,
# but there is quite a difference in RMSE of the two models. Based on RMSE, CartModel_12 is the finalized model
windows()
fancyRpartPlot(model = CartModel_12, main = "Final CART Regression Tree", cex = 0.6, sub = "Model 12")





install.packages('randomForest')
library(randomForest)
set.seed(415)

train <-InsDataTrainUncapped[,-1]

fit <- randomForest(Losses ~ . ,
                    data=train, 
                    importance=TRUE, 
                    ntree=2)

varImpPlot(fit)
test <- InsDataTestCapped[,-1]
Prediction <- predict(fit, test)
Prediction
colnames(test)
 submit <- data.frame(CappedLosses = test$CappedLosses, Survived = Prediction)
 write.csv(submit, file = "firstforest.csv", row.names = FALSE)


