setwd("E:/March 1st Extracted")
library(randomForest)
library(ggplot2)
library(caret)
library(xgboost)
library(dplyr)
library(readr)
library(nnet)
library(ROCR)
library(pROC)
library(gridExtra)
library(cowplot)
library(InformationValue)
firstmonth <- read.csv("First Month Subset Final.csv")[,2:159]
sixweek <- read.csv("Six Week Subset Final.csv")[,2:159]
####Splitting#####

set.seed(1234)
inTrainfirst <- createDataPartition(y=firstmonth$drop, p=0.7, list=FALSE)  
trainfirst <- firstmonth[inTrainfirst,]
testfirst <- firstmonth[-inTrainfirst,]
set.seed(123)
sixind <- createDataPartition(y=sixweek$drop, p=.7, list=FALSE)
trainsix <- sixweek[sixind,]
testsix <- sixweek[-sixind,]
rm(inTrainfirst, sixind)
####Split####

#####First Month Model####
#Multinom Model#
mnfirst <- lm(drop ~., data = firstmonth)
mnpredfirst <- as.numeric(predict(mnfirst, firstmonth))
mnpredfirst <- prediction(mnpredfirst, firstmonth$drop)
evalmn <- performance(mnpredfirst, "acc")
rocmn <- performance(mnpredfirst, "tpr", "fpr")
aucmn <- performance(mnpredfirst, "auc")
aucmn <- unlist(slot(aucmn, "y.values"))


#Random Forest Modeling#
load("rffirst.RData")
plot(rffirst)
rfpredfirst <- predict(rffirst, testfirst)
confusionMatrix(testfirst$drop, rfpredfirst)
rfpred <- prediction(rfpredfirst, testfirst$drop)
rfeval <- performance(rfpred, "acc")
rfroc <- performance(rfpred, "tpr", "fpr")
rfauc <- performance(rfpred, "auc")
rfauc <- unlist(slot(rfauc, "y.values"))



#XGBoost#
trainm <- data.matrix(select(trainfirst,-drop))
train_label<- trainfirst$drop
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)
testm <- data.matrix(select(testfirst,-drop))
test_label <- testfirst$drop
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

#List Parameters for XGBoost#
xgb_params <- list("objective" = "binary:logistic")
watchlist <- list(train = train_matrix, test = test_matrix)
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 83,
                       watchlist = watchlist,
                       eta = .2,
                       print_every_n = 1)
xgb.importance(model = bst_model)
xgbpredfirst <- predict(bst_model, as.matrix(testfirst))
confusionMatrix(testfirst$drop, xgbpredfirst)
xgbpred <- prediction(xgbpredfirst, testfirst$drop)
xgbeval <- performance(xgbpred, "acc")
xgbroc <- performance(xgbpred, "tpr", "fpr")
xgbauc <- performance(xgbpred, "auc")
xgbauc <- unlist(slot(xgbauc, "y.values"))
####First month Plotting and Selection####
#Eval Plots#
plot(rfeval, col = "green")
plot(evalmn, add = T, col = "red")
plot(xgbeval, add = T, col = "blue")
#ROC Plots#
plot(rfroc, col = "green")
plot(rocmn, col = "red", add = T)
plot(xgbroc, col = "blue", add = T)
#AUC Comparisons#
aucmn
rfauc
xgbauc



#####Six Month Model####
mnsix <- lm(drop ~., data = sixweek)
mnpredsix <- as.numeric(predict(mnsix, sixweek))
mnpredsix <- prediction(mnpredsix, sixweek$drop)
evalmnsix <- performance(mnpredsix, "acc")
rocmnsix <- performance(mnpredsix, "tpr", "fpr")
aucmnsix <- performance(mnpredsix, "auc")
aucmnsix <- unlist(slot(aucmnsix, "y.values"))
#Random Forest Modeling#
load("rfsix.RData")
plot(rfsix)
rfpredsix <- predict(rfsix, testsix)
confusionMatrix(testsix$drop, rfpredsix)
rfpredsix <- prediction(rfpredsix, testsix$drop)
rfevalsix <- performance(rfpredsix, "acc")
rfrocsix <- performance(rfpredsix, "tpr", "fpr")
rfaucsix <- performance(rfpredsix, "auc")
rfaucsix <- unlist(slot(rfaucsix, "y.values"))

#XGBoost#
trainmsix <- data.matrix(select(trainsix,-drop))
train_labelsix<- trainsix$drop
train_matrixsix <- xgb.DMatrix(data = as.matrix(trainmsix), label = train_labelsix)
testmsix <- data.matrix(select(testsix,-drop))
test_labelsix <- testsix$drop
test_matrixsix <- xgb.DMatrix(data = as.matrix(testmsix), label = test_labelsix)

#List Parameters for XGBoost#
xgb_paramssix <- list("objective" = "binary:logistic")
watchlistsix <- list(train = train_matrixsix, test = test_matrixsix)
bst_modelsix <- xgb.train(params = xgb_paramssix,
                       data = train_matrixsix,
                       nrounds = 500,
                       watchlist = watchlistsix,
                       eta = .2,
                       print_every_n = 1)
xgb.importance(model = bst_modelsix)
xgbpredsix <- predict(bst_modelsix, as.matrix(testsix))
confusionMatrix(testsix$drop, xgbpredsix)
xgbpredsix <- prediction(xgbpredsix, testsix$drop)
xgbevalsix <- performance(xgbpredsix, "acc")
xgbrocsix <- performance(xgbpredsix, "tpr", "fpr")
xgbaucsix <- performance(xgbpredsix, "auc")
xgbaucsix <- unlist(slot(xgbaucsix, "y.values"))
####First month Plotting and Selection####
#Eval Plots#
p1 <- ggplot() + geom_line(aes(x=unlist(rfeval@x.values), y=unlist(rfeval@y.values)), colour = "green") + 
  geom_line(aes(x=unlist(evalmn@x.values), y=unlist(evalmn@y.values)), colour = "red") +
  geom_line(aes(x=unlist(xgbeval@x.values), y=unlist(xgbeval@y.values)), colour = "blue") +
  labs(title = "Four Week Evaluation") + xlab("Cutoff") + ylab("Accuracy") +
  scale_x_continuous(limits = c(0, 1))
p1
p2 <- ggplot() + geom_line(aes(x=unlist(rfevalsix@x.values), y=unlist(rfevalsix@y.values), colour = "green")) + 
  geom_line(aes(x=unlist(evalmnsix@x.values), y=unlist(evalmnsix@y.values), colour = "red")) +
  geom_line(aes(x=unlist(xgbevalsix@x.values), y=unlist(xgbevalsix@y.values), colour = "blue")) +
  labs(title = "Six Week Evaluation") + xlab("Cutoff") + ylab(element_blank()) +
  scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(breaks = NULL) +
  theme(axis.text.y=element_blank(), axis.ticks.y = element_blank())  + scale_color_discrete(name = "Models", labels = c("Linear", "Random Forest", "XGBoost"))
p2
p <- plot_grid(p1, p2)
title <- ggdraw() + 
  draw_label("Comparison of Performance Evaluation Between Models",
             fontface = 'bold')
evalgrid <- plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1)) 
evalgrid

#ROC Plots#
p3 <- ggplot() + geom_line(aes(x=unlist(rfroc@x.values), y=unlist(rfroc@y.values), colour = "green"), show.legend = F) + 
  geom_line(aes(x=unlist(rocmn@x.values), y=unlist(rocmn@y.values), colour = "blue"), show.legend = F) +
  geom_line(aes(x=unlist(xgbroc@x.values), y=unlist(xgbroc@y.values), colour = "red"), show.legend = F) +
  labs(title = "Four Week AUC") + xlab("False Positive Rate") + ylab("True Positive Rate") +
  scale_x_continuous(limits = c(0, 1))
p3
p4 <- ggplot() + geom_line(aes(x=unlist(rfrocsix@x.values), y=unlist(rfrocsix@y.values), colour = "green")) + 
  geom_line(aes(x=unlist(rocmnsix@x.values), y=unlist(rocmnsix@y.values), colour = "blue")) +
  geom_line(aes(x=unlist(xgbrocsix@x.values), y=unlist(xgbrocsix@y.values), colour = "red")) +
  labs(title = "Six Week AUC") + xlab("False Positive Rate") + ylab(element_blank()) +
  scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(breaks = NULL) +
  theme(axis.text.y=element_blank(), axis.ticks.y = element_blank())+ scale_color_discrete(name = "Models", labels = c("Linear", "Random Forest", "XGBoost"))
p4
p2 <- plot_grid(p3, p4)
title <- ggdraw() + 
  draw_label("Area Under Curve Analysis Between Four Week and Six Week Subsets",
             fontface = 'bold')
rocgrid <- plot_grid(title, p2, ncol = 1, rel_heights = c(0.1, 1)) 
rocgrid
aucmn

aucmnsix
rfaucsix
xgbaucsix

rfaucsix - rfauc
xgbaucsix - xgbauc