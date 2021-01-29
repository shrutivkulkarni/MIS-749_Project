library(caret) 
library(corrplot) 
library(dplyr) 
library(tidyverse) 
library(ggplot2) 
library(rattle) 
library(rpart)
library(rpart.plot)

data <- read.csv("bank-full.csv", sep=";")
str(data)
glimpse(data)
attach(data)

sum(is.na(data))
# [1] 0

summary(data)
colnames(data)[17] = "deposit"


#####Visualizations

ggplot(data = data) + geom_bar(aes (x = deposit), fill = "steel blue")

ggplot(data = data) + geom_bar(aes(x = education, fill = deposit), 
                               position = position_dodge(width = 0.9))

ggplot(data = data) + geom_boxplot(aes(y = balance, x = deposit, fill = deposit))+ 
                      ggtitle("balance wise subscription") + xlab("deposit") + ylab("balance")
summary(data$balance)


ggplot(data = data) + geom_boxplot(aes(y = balance, x = job, fill = deposit))+ 
                      xlab("job") + ylab("balance")
table(data$job)

ggplot(data = data) + geom_boxplot(aes(y = age, x = job, fill = deposit))+ 
                      xlab("job") + ylab("age") + theme_minimal() +
                      theme(legend.position = "right", axis.text.x.bottom = element_text(angle=40, hjust=0.9))


ggplot(data = data) + geom_boxplot(aes(y = balance, x = marital, fill = deposit))+ 
                      xlab("marital") + ylab("balance") + theme_minimal()

ggplot(data = data) + geom_boxplot(aes(y = balance, x = loan, fill = deposit))+ 
                      xlab("loan") + ylab("balance")

ggplot(data = data) + geom_boxplot(aes(y = balance, x = housing, fill = deposit))+ 
                      xlab("housing") + ylab("balance")

ggplot(data = data) + geom_boxplot(aes(y = balance, x = default, fill = deposit))+ 
                      xlab("default") + ylab("balance")

install.packages("reshape2")
library(reshape2)

marital_deposit <- table(data$marital, data$y)
mydata <- as.data.frame.matrix(marital_deposit)
mydata$marital <- rownames(mydata)
mydataLong <- melt(mydata, id.vars=c("marital"), value.name = "count")
names(mydataLong)[2] <- paste("deposit")
p <- ggplot(data=mydataLong, aes(x=marital,
                                 y=count,
                                 fill=deposit))
p + geom_bar(stat = "identity",
             width = 0.6,
             size = 0.5,
             color = "black")

myTable <- table(data$contact, data$y)
mydata <- as.data.frame.matrix(myTable)
mydata$contact <- rownames(mydata)
mydataLong <- melt(mydata, id.vars=c("contact"), value.name = "count")
names(mydataLong)[2] <- paste("deposit")
p <- ggplot(data=mydataLong, aes(x=contact,
                                 y=count,
                                 fill=deposit))
p + geom_bar(stat = "identity",
             width = 0.6,
             size = 0.5,
             color = "black")

#Visualizing continuous variables
data %>% ggplot(aes(x=deposit, y=campaign, fill=deposit)) + geom_boxplot()
data %>% ggplot(aes(x=deposit, y=previous, fill=deposit)) + geom_boxplot()

#Statistcal tests some categorical predictors 
#Our hypothesis is that people who do have loans, housing or default they will not subscribe for deposit account
table(data$default, data$deposit)
#           deposit
# default     no   yes
#       no   39159  5237
#       yes   763    52
chisq.test(table(data$default, data$deposit),correct=FALSE)
# data:  table(default, deposit)
# X-squared = 22.724, df = 1, p-value = 1.871e-06
#the p-value<0.05, proves that people who do default, they are less likely to subscribe for deposit account 


table(data$loan, data$deposit)
#          deposit
# loan     no     yes
#     no  33162   4805
#     yes  6760   484
chisq.test(table(data$loan, data$deposit))
# data:  table(loan, deposit)
# X-squared = 209.62, df = 1, p-value < 2.2e-16
#the p-value<0.05, proves that people who do have loan, they are less likely to subscribe for deposit account 


table(data$housing, data$deposit)
#            deposit
# housing    no   yes
#       no  16727  3354
#       yes 23195  1935
chisq.test(table(data$housing, data$deposit))
# data:  table(housing, deposit)
# X-squared = 874.82, df = 1, p-value < 2.2e-16
#the p-value<0.05, proves that people who do have housing, they are less likely to subscribe for deposit account 




#Data preprocessing, since we have a lot of factors, we might have to convert them

levels(data$poutcome)
# [1] "failure" "other"   "success" "unknown"

# Converting the other level to unknown and dropping the level other
other = which(data$poutcome=="other")
data$poutcome[other ] = "unknown"
table(data$poutcome)
data$poutcome <- droplevels(data$poutcome)
str(data)


# Identify correlated predictors
df_cor <- select_if(data, is.numeric) %>% cor()
corrplot(df_cor, method = "circle", order = "hclust")

#Pdays : "-1" means client was not previously contacted, this depicts the number of days sonce the customer was contacted,
#this is in a way explained using the previous variable too, since the previous is the number of conacts 
#made with the customer as part of the campaign, thus dropping pdays and instead keeping Previous 

#previous: number of contacts performed before this campaign and for this client 
data %>% group_by(previous) %>% 
         count() %>% 
         arrange(desc(n)) %>% 
         head()
# # A tibble: 6 x 2
# # Groups:   previous [6]
# previous     n
#       <int> <int>
# 1        0 36954
# 2        1  2772
# 3        2  2106
# 4        3  1142
# 5        4   714
# 6        5   459
# a large number of observations 0:36954 were not contaced ever

#Exploring pdays, pdays: -1 means client was not previously contacted
data %>% group_by(pdays) %>%
         count() %>%
         arrange(desc(n)) %>%
         head()
# # A tibble: 6 x 2
# pdays     n
#    <int> <int>
# 1    -1  36954
# 2   182   167
# 3    92   147
# 4    91   126
# 5   183   126
# 6   181   117

#the "pdays" value also suggests the same thing as the "previous" variable did, that is, 
#a large number of customers were not contacted and thus keeping one of these instead of both
#The similar count of 0 and -1 values in both previous and pdays variables 
#respectively suggest they are describing the same thing.

#correlation between the two
cor(data$pdays, data$previous)
# [1] 0.4548196    #slightly correlated in number but going by the pattern, it makes sense to drop one of these

#dropping pdays
data$pdays <- NULL


data %>% count(data$month, sort = TRUE)
# A tibble: 12 x 2
# `data$month`     n
#   <fct>           <int>
# 1  may          13766
# 2  jul           6895
# 3  aug           6247
# 4  jun           5341
# 5  nov           3970
# 6  apr           2932
# 7  feb           2649
# 8  jan           1403
# 9  oct            738
# 10 sep            579
# 11 mar            477
# 12 dec            214

# Removing duration predictor
ggplot(data= data) + geom_density(aes(x = duration, fill = deposit), alpha = 0.6)
#The plot clearly suggests that the more time or the call duration is 
#the more likely the person is going to subscribe for the deposit account, this predictor thus cannot be used
data$duration <- NULL


str(data)
summary(data)
# data$deposit <- ifelse(data$deposit=="yes",1, 0)
# data$deposit <- factor(data$deposit)#,levels = c(0,1), labels= c("no", "yes") )
# levels(data$deposit)

#relevel the deposit factor so Yes becomes the Positive class by default (Sensitivity)
data$deposit <- relevel(data$deposit, ref="yes")
levels(data$deposit)

#This data is clean enough for our analyses

#----Fitting models

#dividing the dataset into train and test using 80/20 split 
set.seed(123)
rows = sample(nrow(data))
shuffled_deposit = data[rows, ]
str(shuffled_deposit)
split = round(nrow(shuffled_deposit) * 0.80)
train_data = shuffled_deposit [1:split, ]
str(train_data)
test_data = shuffled_deposit [(split +1): nrow(shuffled_deposit), ]
str(test_data)

# Creating customized control for Cross Validation on training data only
set.seed(123)
control <- trainControl(method = "cv",
                        number = 10,
                        summaryFunction = twoClassSummary, 
                        classProbs = TRUE,
                        verboseIter = TRUE)

# Create grid
rfGrid <-  expand.grid(mtry = c(2, 4, 6, 8))

# Fit the models

#Decision tree
set.seed(123)
dt_train <- train(deposit~. ,
                  data = train_data,
                  method= "rpart",
                  trControl = control,
                  #metric = "ROC",
                  preProcess = "range",
                  tuneLength = 10)
plot(dt_train)
plot(dt_train$finalModel)
text(dt_train$finalModel)


prediction_dt <- predict(dt_train, test_data)
confusionMatrix(table(prediction_dt, test_data$deposit))

# Confusion Matrix and Statistics
# 
# 
# prediction_dt  yes   no
# yes            254  177
# no             796 7815
# 
# Accuracy : 0.8924          
# 95% CI : (0.8858, 0.8987)
# No Information Rate : 0.8839          
# P-Value [Acc > NIR] : 0.005635        
# 
# Kappa : 0.2954          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.24190         
#             Specificity : 0.97785         
#          Pos Pred Value : 0.58933         
#          Neg Pred Value : 0.90756         
#              Prevalence : 0.11612         
#          Detection Rate : 0.02809         
#    Detection Prevalence : 0.04767         
#       Balanced Accuracy : 0.60988         
#                                           
#        'Positive' Class : yes     



#fitting a GLM model
set.seed(123)
glm_model <- train(deposit ~ .,
                   data = train_data,
                   method = "glm",
                   family = "binomial",
                   metric = "ROC",
                   preProcess = c("range"),
                   trControl = control)
summary(glm_model)

prediction_glm <- predict(glm_model, test_data)
confusionMatrix(table(prediction_glm, test_data$deposit))

# Confusion Matrix and Statistics
# 
# 
# prediction_glm  yes   no
# yes  198  107
# no   852 7885
# 
# Accuracy : 0.8939          
# 95% CI : (0.8874, 0.9002)
# No Information Rate : 0.8839          
# P-Value [Acc > NIR] : 0.001328        
# 
# Kappa : 0.2532          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.18857         
#             Specificity : 0.98661         
#          Pos Pred Value : 0.64918         
#          Neg Pred Value : 0.90248         
#              Prevalence : 0.11612         
#          Detection Rate : 0.02190         
#    Detection Prevalence : 0.03373         
#       Balanced Accuracy : 0.58759         
#                                           
#        'Positive' Class : yes             
#                                 



#Random forests
set.seed(123)
rf_model <- train(deposit~. ,
                  data = train_data,
                  method= "rf",
                  trControl = control,
                  tuneGrid = rfGrid)

plot(rf_model)

prediction_rf <- predict(rf_model, test_data)
confusionMatrix(table(prediction_rf, test_data$deposit))

# Confusion Matrix and Statistics
# 
# 
# prediction_rf  yes   no
# yes  249  138
# no   801 7854
# 
# Accuracy : 0.8962          
# 95% CI : (0.8897, 0.9024)
# No Information Rate : 0.8839          
# P-Value [Acc > NIR] : 0.0001162       
# 
# Kappa : 0.303           
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.23714         
#             Specificity : 0.98273         
#          Pos Pred Value : 0.64341         
#          Neg Pred Value : 0.90745         
#              Prevalence : 0.11612         
#          Detection Rate : 0.02754         
#    Detection Prevalence : 0.04280         
#       Balanced Accuracy : 0.60994         
#                                           
#        'Positive' Class : yes           

#fitting a boosting model
set.seed(44)
gbm_model <- train(deposit ~., 
                   data=train_data, 
                   method="gbm",
                   tuneLength=8,
                   trControl=control)

summary(gbm_model)

prediction_gbm <- predict(gbm_model, test_data)
confusionMatrix(table(prediction_gbm, test_data$deposit))

# Confusion Matrix and Statistics
# 
# 
# prediction_gbm  yes   no
# yes  246  149
# no   804 7843
# 
# Accuracy : 0.8946          
# 95% CI : (0.8881, 0.9009)
# No Information Rate : 0.8839          
# P-Value [Acc > NIR] : 0.00067         
# 
# Kappa : 0.2958          
# 
# Mcnemar's Test P-Value : < 2e-16         
#                                           
#             Sensitivity : 0.23429         
#             Specificity : 0.98136         
#          Pos Pred Value : 0.62278         
#          Neg Pred Value : 0.90702         
#              Prevalence : 0.11612         
#          Detection Rate : 0.02721         
#    Detection Prevalence : 0.04369         
#       Balanced Accuracy : 0.60782         
#                                           
#        'Positive' Class : yes  


# Comparing model performance
models<- list("glm" = glm_model,
              "rf" = rf_model,
              "dt"=dt_train,
              "gbm"=gbm_model)

model.resamples<- resamples(models)
summary(model.resamples)

# Models: glm, rf, dt, gbm 
# Number of resamples: 10 
# 
# ROC 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# glm 0.7489504 0.7585601 0.7608315 0.7648752 0.7728313 0.7849076    0
# rf  0.7696985 0.7804623 0.7847816 0.7843115 0.7910837 0.7933410    0
# dt  0.7064691 0.7093583 0.7118115 0.7161127 0.7233412 0.7328114    0
# gbm 0.7643315 0.7816525 0.7939347 0.7930238 0.8061163 0.8176255    0
# 
# Sens 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# glm 0.1344340 0.1698113 0.1709906 0.1722155 0.1780660 0.1938534    0
# rf  0.1933962 0.2040094 0.2122642 0.2132600 0.2216981 0.2358491    0
# dt  0.1816038 0.1981132 0.2077869 0.2094819 0.2246462 0.2452830    0
# gbm 0.2028302 0.2057783 0.2216981 0.2257650 0.2417453 0.2688679    0
# 
# Spec 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# glm 0.9840276 0.9861416 0.9876292 0.9874100 0.9880990 0.9915440    0
# rf  0.9783902 0.9813655 0.9823050 0.9823364 0.9833229 0.9852803    0
# dt  0.9771375 0.9790166 0.9797996 0.9799562 0.9810523 0.9827748    0
# gbm 0.9740056 0.9784685 0.9796430 0.9806452 0.9830880 0.9890385    0

#plot performances
bwplot(model.resamples, metric="ROC")
bwplot(model.resamples, metric="Sens")
bwplot(model.resamples, metric="Spec")


#############################################################################################################################
# Data imbalance - Use SMOTE balance train data
install.packages("DMwR")
library(DMwR)
set.seed(9560)
train_data_smote <- SMOTE(deposit ~ ., perc.over=250, perc.under=150,
                          data  = train_data)

summary(train_data_smote)

# Fit the models

#Decision tree on the balanced data
set.seed(123)
dt_train_smote <- train(deposit~. ,
                  data = train_data_smote, # Balanced training data
                  method= "rpart",
                  trControl = control,
                  preProcess = "range",
                  tuneLength = 10)

plot(dt_train_smote)
plot(dt_train_smote$finalModel)
text(dt_train_smote$finalModel)


prediction_dt_smote <- predict(dt_train_smote, test_data)
confusionMatrix(table(prediction_dt_smote, test_data$deposit))

# Confusion Matrix and Statistics
# 
# 
# prediction_dt_smote  yes   no
# yes  391  509
# no   659 7483
# 
# Accuracy : 0.8708          
# 95% CI : (0.8637, 0.8777)
# No Information Rate : 0.8839          
# P-Value [Acc > NIR] : 0.9999          
# 
# Kappa : 0.3291          
# 
# Mcnemar's Test P-Value : 1.302e-05       
#                                           
#             Sensitivity : 0.37238         
#             Specificity : 0.93631         
#          Pos Pred Value : 0.43444         
#          Neg Pred Value : 0.91906         
#              Prevalence : 0.11612         
#          Detection Rate : 0.04324         
#    Detection Prevalence : 0.09954         
#       Balanced Accuracy : 0.65435         
#                                           
#        'Positive' Class : yes     


#fitting a GLM model on the balanced data
set.seed(123)
glm_model_smote <- train(deposit ~ .,
                   data = train_data_smote, # Balanced training data
                   method = "glm",
                   family = "binomial",
                   metric = "ROC",
                   preProcess = c("range"),
                   trControl = control)

summary(glm_model_smote)

prediction_glm_smote <- predict(glm_model_smote, test_data)
confusionMatrix(table(prediction_glm_smote, test_data$deposit))

# Confusion Matrix and Statistics
# 
# 
# prediction_glm_smote  yes   no
# yes  597 1689
# no   453 6303
# 
# Accuracy : 0.7631          
# 95% CI : (0.7542, 0.7718)
# No Information Rate : 0.8839          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.2364          
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.56857         
#             Specificity : 0.78866         
#          Pos Pred Value : 0.26115         
#          Neg Pred Value : 0.93295         
#              Prevalence : 0.11612         
#          Detection Rate : 0.06603         
#    Detection Prevalence : 0.25282         
#       Balanced Accuracy : 0.67862         
#                                           
#        'Positive' Class : yes        


#Random forests with balanced data
set.seed(123)
rf_model_smote <- train(deposit~. ,
                  data = train_data_smote, # Balanced training data
                  method= "rf",
                  trControl = control,
                  tuneGrid = rfGrid)

plot(rf_model_smote)

prediction_rf_smote <- predict(rf_model_smote, test_data)
confusionMatrix(table(prediction_rf_smote, test_data$deposit))

# Confusion Matrix and Statistics
# 
# 
# prediction_rf_smote  yes   no
# yes  575  896
# no   475 7096
# 
# Accuracy : 0.8484          
# 95% CI : (0.8408, 0.8557)
# No Information Rate : 0.8839          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.3709          
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.54762         
#             Specificity : 0.88789         
#          Pos Pred Value : 0.39089         
#          Neg Pred Value : 0.93726         
#              Prevalence : 0.11612         
#          Detection Rate : 0.06359         
#    Detection Prevalence : 0.16269         
#       Balanced Accuracy : 0.71775         
#                                           
#        'Positive' Class : yes      


#fitting a boosting model with balanced data
set.seed(44)
gbm_model_smote <- train(deposit ~., 
                   data=train_data_smote, # Balanced training data
                   method="gbm",
                   tuneLength=8,
                   trControl=control)

summary(gbm_model_smote)

prediction_gbm_smote <- predict(gbm_model_smote, test_data)
confusionMatrix(table(prediction_gbm_smote, test_data$deposit))

# Confusion Matrix and Statistics
# 
# 
# prediction_gbm_smote  yes   no
# yes  519  580
# no   531 7412
# 
# Accuracy : 0.8771          
# 95% CI : (0.8702, 0.8838)
# No Information Rate : 0.8839          
# P-Value [Acc > NIR] : 0.9776          
# 
# Kappa : 0.4133          
# 
# Mcnemar's Test P-Value : 0.1498          
#                                           
#             Sensitivity : 0.4943          
#             Specificity : 0.9274          
#          Pos Pred Value : 0.4722          
#          Neg Pred Value : 0.9331          
#              Prevalence : 0.1161          
#          Detection Rate : 0.0574          
#    Detection Prevalence : 0.1215          
#       Balanced Accuracy : 0.7109          
#                                           
#        'Positive' Class : yes   


# Comparing model performance
models_smote<- list("glm" = glm_model_smote,
              "rf" = rf_model_smote,
              "dt" = dt_train_smote,
              "gbm" = gbm_model_smote)

model.resamples_smote<- resamples(models_smote)
summary(model.resamples_smote)

# Models: glm, rf, dt, gbm 
# Number of resamples: 10 
# 
# ROC 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# glm 0.7777203 0.7821073 0.7904101 0.7897152 0.7957770 0.8003374    0
# rf  0.9226117 0.9273752 0.9290719 0.9300127 0.9318221 0.9374784    0
# dt  0.8248598 0.8358744 0.8394433 0.8407863 0.8458831 0.8587343    0
# gbm 0.9227209 0.9249813 0.9270228 0.9273514 0.9294836 0.9350002    0
# 
# Sens 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# glm 0.6155660 0.6439955 0.6527724 0.6507056 0.6603774 0.6726987    0
# rf  0.7885220 0.7981139 0.8073899 0.8064016 0.8099072 0.8294025    0
# dt  0.6391509 0.6502754 0.6661418 0.6618684 0.6719733 0.6784591    0
# gbm 0.7883556 0.8011408 0.8030660 0.8046691 0.8101415 0.8191824    0
# 
# Spec 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# glm 0.7537372 0.7797443 0.7841981 0.7826513 0.7913472 0.7932390    0
# rf  0.9118804 0.9184355 0.9213836 0.9216801 0.9266476 0.9307632    0
# dt  0.9221698 0.9257075 0.9331517 0.9351285 0.9461172 0.9488994    0
# gbm 0.9166667 0.9286134 0.9323630 0.9319811 0.9386435 0.9394654    0

#plot performances
bwplot(model.resamples_smote, metric="ROC")
bwplot(model.resamples_smote, metric="Sens")
bwplot(model.resamples_smote, metric="Spec")

# ROC plot
# Predict on test
glm_pred = predict(glm_model_smote, test_data)
rf_pred = predict(rf_model_smote, test_data)
dt_pred = predict(dt_train_smote, test_data)
gbm_pred = predict(gbm_model_smote, test_data)

# Make ROC curve
library(caTools)
colAUC(cbind(as.numeric(glm_pred), as.numeric(rf_pred), as.numeric(dt_pred), as.numeric(gbm_pred)), 
       test_data$deposit, plotROC = TRUE, alg="ROC")

