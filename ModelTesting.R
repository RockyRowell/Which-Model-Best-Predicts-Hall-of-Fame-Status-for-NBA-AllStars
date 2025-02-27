## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE, include=FALSE----------------------------------------------------------------------------------------------------------------------------------
library(ggplot2); library(ggthemes); library(car); library(tidyverse); library(ggcorrplot)
library(mgcv); library(gratia); library(gridExtra); library(rpart); library(rpart.plot)
library(randomForest); library(caret); library(pROC); library(knitr)


## ----include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------
# load in data and prepare for data analysis

# load in data
hof <- read.csv("Data/hof_final.csv", stringsAsFactors = T)

# make rowname the player's name
rownames(hof) <- hof$Player

# remove all variables that are not stat categories or have other issues
hof <- hof %>% select(-c(X, Player, GS, MP, Three, Three_Attempt, ORB, DRB, STL, BLK, TOV, position, AllStarSelections, height, weight, born, MVP, FMVP, DPOY, championship))

# remove NA values
hof <- hof[!is.na(hof$TRB), ]


## ----include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------
# fit model
hof.gam <- gam(HallOfFame~s(G)+s(OWS)+s(DWS)+s(WS)+s(FG)+s(FGA)+s(FT)+s(FTA)+s(TRB)+s(AST)+s(PF)+s(PTS), data=hof, family = binomial)

# plot smooth terms
# save plots
smooth_plots <- draw(hof.gam)


## ----echo=FALSE, fig.height=3.5, fig.width=6, fig.align="center"------------------------------------------------------------------------------------------------------------------
# plots 1-4
grid.arrange(
  smooth_plots[[1]], smooth_plots[[2]],
  smooth_plots[[3]], smooth_plots[[4]], 
  ncol = 2, nrow = 2
)

# plots 5-8
grid.arrange(
  smooth_plots[[5]], smooth_plots[[6]],
  smooth_plots[[7]], smooth_plots[[8]], 
  ncol = 2, nrow = 2
)

# plots 9-12
grid.arrange(
  smooth_plots[[9]], smooth_plots[[10]],
  smooth_plots[[11]], smooth_plots[[12]], 
  ncol = 2, nrow = 2
)


## ----warning=FALSE, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
# set seed
set.seed(112233)

# 10 folds
n.fold = 10
folds <- createFolds(hof$HallOfFame, k = n.fold, list = TRUE, returnTrain = FALSE)

# initialize vectors to store auc, sensitivity, and specificity
auc.lr <- rep(0, n.fold); auc.gam <- rep(0, n.fold)
auc.tree <- rep(0, n.fold); auc.rf <- rep(0, n.fold)

sens.lr <- rep(0, n.fold); sens.gam <- rep(0, n.fold)
sens.tree <- rep(0, n.fold); sens.rf <- rep(0, n.fold)

spec.lr <- rep(0, n.fold); spec.gam <- rep(0, n.fold)
spec.tree <- rep(0, n.fold); spec.rf <- rep(0, n.fold)

# loop through 10 folds
for(k in 1:n.fold){
  # split data
  test.indices <- folds[[k]]
  test.data <- hof[test.indices, ]
  train.data <- hof[-test.indices, ]
  
  # logistic regression
  mod.lr <- glm(HallOfFame~., data=train.data, family=binomial)

  # generalized additive model
  mod.gam <- gam(HallOfFame~s(G)+s(OWS)+s(DWS)+s(WS)+s(FG)+s(FGA)+s(FT)+s(FTA)+s(TRB)+s(AST)+s(PF)+s(PTS), data=train.data, family = binomial)
  
  # regression tree
  tree <- rpart(HallOfFame ~., data=train.data, cp=0.001)
  cp.table <- printcp(tree)
  which.min.RMSE <- which.min(cp.table[,'xerror'])
  cp.opt <- cp.table[which.min.RMSE,'CP']
  mod.tree <- prune.rpart(tree, cp.opt)
  
  # random forest
  mtry.table <- tuneRF(train.data[,c(1:12)], train.data[,13], ntreeTry=150, stepFactor=0.5)
  opt.mtry.ind <- which.min(mtry.table[,2])
  opt.mtry <- mtry.table[opt.mtry.ind,"mtry"]
  mod.rf <- randomForest(HallOfFame~., data=train.data, mtry=opt.mtry, ntree=500)
  
  # obtain predictions
  lr.preds <- predict(mod.lr, newdata=test.data)
  gam.preds <- predict(mod.gam, newdata=test.data)
  tree.preds <- predict(mod.tree, newdata=test.data)
  rf.preds <- predict(mod.rf, newdata=test.data)
  
  # compute auc
  roc.lr <- roc(test.data$HallOfFame, lr.preds)
  auc.lr[k] <- roc.lr$auc
  
  roc.gam <- roc(test.data$HallOfFame, gam.preds)
  auc.gam[k] <- roc.gam$auc
    
  roc.tree <- roc(test.data$HallOfFame, tree.preds)
  auc.tree[k] <- roc.tree$auc
    
  roc.rf <- roc(test.data$HallOfFame, rf.preds)
  auc.rf[k] <- roc.rf$auc
  
  # since it is binary we can just round to closest number for predicted class
  lr.pred.class <- ifelse(lr.preds > 0.5, 1, 0)
  gam.pred.class <- ifelse(gam.preds > 0.5, 1, 0)
  tree.pred.class <- ifelse(tree.preds > 0.5, 1, 0)
  rf.pred.class <- ifelse(rf.preds > 0.5, 1, 0)
  
  # compute confusion matrices (convert to factors)
  cm.lr <- confusionMatrix(as.factor(lr.pred.class), as.factor(test.data$HallOfFame))
  cm.gam <- confusionMatrix(as.factor(gam.pred.class), as.factor(test.data$HallOfFame))
  cm.tree <- confusionMatrix(as.factor(tree.pred.class), as.factor(test.data$HallOfFame))
  cm.rf <- confusionMatrix(as.factor(rf.pred.class), as.factor(test.data$HallOfFame))
  
  # save sensitivity and specificity values
  sens.lr[k] <- cm.lr$byClass['Sensitivity']
  spec.lr[k] <- cm.lr$byClass['Specificity']
  
  sens.gam[k] <- cm.gam$byClass['Sensitivity']
  spec.gam[k] <- cm.gam$byClass['Specificity']
  
  sens.tree[k] <- cm.tree$byClass['Sensitivity']
  spec.tree[k] <- cm.tree$byClass['Specificity']
  
  sens.rf[k] <- cm.rf$byClass['Sensitivity']
  spec.rf[k] <- cm.rf$byClass['Specificity']
}

# create matrix of mean values
table <- matrix(
  c(mean(auc.lr), mean(auc.gam), mean(auc.tree), mean(auc.rf),
    mean(sens.lr), mean(sens.gam), mean(sens.tree), mean(sens.rf),
    mean(spec.lr), mean(spec.gam), mean(spec.tree), mean(spec.rf)),
  nrow = 4, byrow=F
)

# add names
rownames(table) <- c("Logistic Regression", "Additive Model", "Regression Tree", "Random Forest")
colnames(table) <- c("AUC", "Sensitivity", "Specificity")

# round values
table <- round(table, 3)


## ----Table, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(table, caption = "Model Performance Metrics")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fit lr model and use gam model from earlier
hof.lr <- glm(HallOfFame~., data=hof, family=binomial)

# aic
AIC.lr <- AIC(hof.lr)
AIC.gam <- AIC(hof.gam)

# round values
AIC.lr <- round(AIC.lr, 3)
AIC.gam <- round(AIC.gam, 3)

# print output
print(paste("The AIC for Logistic Regression is:", AIC.lr))
print(paste("The AIC for Additive Model is:", AIC.gam))

