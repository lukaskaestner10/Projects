library(tidyverse)

# 1. Logistic Regression

rm(list=ls())

rexnord <- read.csv("Rexnord data.csv")
rexnord$CustNum = as.factor(rexnord$CustNum)

rexnord_corr <- rexnord

# Let's check correlations. First, create a data frame that contains just our quantitive X variables. Then
# compute correlations.

rexnord_corr[] <- lapply(rexnord,as.integer)
library(sjPlot)
sjp.corr(rexnord_corr)
sjt.corr(rexnord_corr)

rexnord_corr <- select(rexnord_corr, -c("Ind"))
sjp.corr(rexnord_corr)

rexnord_corr <- select(rexnord_corr, -c("Type"))
sjp.corr(rexnord_corr)

rexnord <- select(rexnord, -c("Ind", "Type"))


# Remove low counts

rexnord %>% 
  group_by(Prod) %>%
  summarise(count = length(Prod))
# Remove Ocelot (1), Black Bear (9), Zebra (1)

rexnord <- rexnord %>%
  filter(Prod != "Ocelot") %>%
  filter(Prod != "Black Bear") %>%
  filter(Prod != "Zebra")

rexnord %>% 
  group_by(InqOrProj) %>%
  summarise(count = length(InqOrProj))

rexnord %>% 
  group_by(ProjType) %>%
  summarise(count = length(ProjType))

rexnord %>% 
  group_by(IndSub) %>%
  summarise(count = length(IndSub))
# Remove less than 2
# Paperboard & Containers (1), Marine (1), Paper Converting Equipment (1), Pumps - API (1), Rail (1)

rexnord <- rexnord %>%
  filter(IndSub != "Paperboard & Containers") %>%
  filter(IndSub != "Marine") %>%
  filter(IndSub != "Paper Converting Equipment") %>%
  filter(IndSub != "Pumps - API") %>%
  filter(IndSub != "Rail")

rexnord %>% 
  group_by(Replacement) %>%
  summarise(count = length(Replacement))

rexnord <- select(rexnord, -c("Date"))
rexnord <- select(rexnord, -c("CustNum"))

## Set a random number seed to make your training/test partition reproductible
set.seed(1)

# Split data - 75% Training, 25% Testing
train_ind <- sample.int(n = nrow(rexnord), size = floor(.75*nrow(rexnord)), replace = FALSE)

rexnord_train <- rexnord[train_ind, ]
rexnord_test  <- rexnord[-train_ind, ]






## Log Fit 1


log_fit1 <- glm(formula = Outcome ~., data=rexnord_train, family=binomial())
summary(log_fit1)

rexnord_train$predict_signif<-ifelse(predict(log_fit1,rexnord_train, type="response")>0.5, 1, 0)

# Confusion Matrix - Captures the strength of the performance in the test data set

Pred_train<-rexnord_train$predict_signif
Actual_train<-rexnord_train$Outcome

confusion_signif <- table(Pred_train, Actual_train)
confusion_signif

# Overall accuracy is 1 minus the error rate (percent wrong)
# 84.20% for .5
# 84.11% for .4
# 83.19% for .3
# 75.16% for .2
overall_accuracy_signif <- 1 - (confusion_signif[1,2]+confusion_signif[2,1])/sum(confusion_signif)
overall_accuracy_signif

# Specificity is the probability of getting a negative classification given that the true class is negative
# Reflects the model's ability to detect negatives
sensitivity_signif <- confusion_signif[2,2]/sum(confusion_signif[,2])
specificity_signif <- confusion_signif[1,1]/sum(confusion_signif[,1])
sensitivity_signif
specificity_signif


rexnord_train <- select(rexnord_train, -c(predict_signif))


## Log Fit 2

log_fit2 <- glm(formula = Outcome ~ . + Prod*LeadTime + Prod*Replacement + Prod*NetValue + Prod*ProjType
                + Prod*InqOrProj, data=rexnord_train, family=binomial())
summary(log_fit2)

rexnord_train$predict_signif<-ifelse(predict(log_fit2,rexnord_train, type="response")>0.5, 1, 0)

# Confusion Matrix - Captures the strength of the performance in the test data set

Pred_train<-rexnord_train$predict_signif
Actual_train<-rexnord_train$Outcome

confusion_signif <- table(Pred_train, Actual_train)
confusion_signif

# Overall accuracy is 1 minus the error rate (percent wrong)
# 84.27% for .5
# 84.11% for .4
# 83.06% for .3
# 69.43% for .2
overall_accuracy_signif <- 1 - (confusion_signif[1,2]+confusion_signif[2,1])/sum(confusion_signif)
overall_accuracy_signif

# Specificity is the probability of getting a negative classification given that the true class is negative
# Reflects the model's ability to detect negatives
sensitivity_signif <- confusion_signif[2,2]/sum(confusion_signif[,2])
specificity_signif <- confusion_signif[1,1]/sum(confusion_signif[,1])
sensitivity_signif
specificity_signif



# Picking one as our final model
log_fit2_final <- log_fit2

predict(log_fit2,rexnord_test)

# Check the accuracy on test data
rexnord_test$predict_final<-ifelse(predict(log_fit2_final,rexnord_test, type="response")>0.5, 1, 0)

confusion_final  <- table(rexnord_test$predict_final, rexnord_test$Outcome)
confusion_final

overall_accuracy_final <- 1 - (confusion_final[1,2]+confusion_final[2,1])/sum(confusion_final)
overall_accuracy_final

sensitivity_final <- confusion_final[2,2]/sum(confusion_final[,2])
specificity_final <- confusion_final[1,1]/sum(confusion_final[,1])
sensitivity_final
specificity_final

# Final
# Accuracy: 85.04%
# Sensitivity: 1.86%
# Specificity: 99.81%
# Cut off: 0.5
# Used: Prod, InqOrProj, ProjType, IndSub, Replacement, NetValue, LeadTime, 
  # Prod*LeadTime, Prod*Replacement, Prod*NetValue, Prod*ProjType, Prod*InqOrProj
# Product and Product*LeadTime, depending on the value, had some significant values within them
# With imbalanced data, the model was more likely to predict 0 than 1 from the start since 
# it will always achieve a reasonable accuracy by just predicting that each observation is lost.











# 2. KNN
rm(list=ls())

rexnord <- read.csv("Rexnord data.csv")

rexnord <- select(rexnord, -c("Date", "CustNum"))

library(fastDummies)



rexnord2 <- dummy_cols(rexnord, select_columns = c("Prod","InqOrProj", "ProjType", "Type", "Ind", "IndSub", 
                                                   "Replacement", "Outcome"), remove_first_dummy = TRUE)

rexnord2 <- select(rexnord2, -c("Prod", "InqOrProj", "ProjType", "Type", "Ind", "IndSub", "Outcome", "Replacement"))

rexnord_scaled <- rexnord2
rexnord_scaled[, -c(103)] <- scale(rexnord_scaled[,-c(103)])



# Split data - 75% Training, 25% Testing
train_ind <- sample.int(n = nrow(rexnord_scaled), size = floor(.75*nrow(rexnord_scaled)), replace = FALSE)

rexnord_train <- rexnord_scaled[train_ind, ]
rexnord_test  <- rexnord_scaled[-train_ind, ]



train_class <- rexnord_train$Outcome_Won
test_class <- rexnord_test$Outcome_Won




library(class)

# Run the KNN algorithm. The arguments you feed it are 
# (1) the X variables from the training set,
# (2) the X variables from the validation set,
# (3) the classification variable from the training set, and
# (4) the k you want it to use.

knn_predict <- knn(rexnord_train,rexnord_test,train_class,k=3)

knnconfusion  <- table(knn_predict, test_class)
knnconfusion

# Calculate overall accuracy
overall_accuracy <- (knnconfusion[1,1]+knnconfusion[2,2])/sum(knnconfusion)
overall_accuracy

# Accuracy
# k = 3: 96.69%

# Used all variables
# Performed better than logistic regression at correctly predicting wins

