######################################
#                                    #
#        REGRESIÓN LOGÍSTICA         #
#                                    #
######################################

# Por Juanjo Beunza
# Agradecimiento a Brett Lantz y Hadley Wickham por su inspiración y código


rm(list=ls())

source("/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Frami analisis para REC/I- Preparacion datos tres modelos/Preparacion datos 3 modelos v8.R")



######################          MODELO A - BASE BRUTA          ########################################################################



# Creación del modelo

modelA <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiA_train)
summary(modelA)
prediction <- predict(modelA,newdata=subset(framiA_test,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiA_test$TenYearCHD,prediction_01)
#    prediction
#     0   1
# 0 644   6
# 1 116   6
#head(prediction, 20)
roc.curve(framiA_test$TenYearCHD, prediction_01)    # 




######################          MODELO B           ########################################################################


## Bruto
modelB <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiB_train)
summary(modelB)
prediction <- predict(modelB,newdata=subset(framiB_test,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiB_test$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 640   2
# 1 108  14
roc.curve(framiB_test$TenYearCHD, prediction_01)    # 0.556





## Normalizado y Balanceado
modelB_norm_bal_over <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiB_train_norm_bal_over)
summary(modelB_norm_bal_over)
prediction <- predict(modelB_norm_bal_over,newdata=subset(framiB_test_norm,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiB_test_norm$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 432   210
# 1 38    84  
roc.curve(framiB_test$TenYearCHD, prediction_01)    # 0.681 ***********

modelB_norm_bal_under <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiB_train_norm_bal_under)
summary(modelB_norm_bal_under)
prediction <- predict(modelB_norm_bal_under,newdata=subset(framiB_test_norm,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiB_test_norm$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 439   203
# 1 45    77  
roc.curve(framiB_test$TenYearCHD, prediction_01)    # 0.657

modelB_norm_bal_both <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiB_train_norm_bal_both)
summary(modelB_norm_bal_both)
prediction <- predict(modelB_norm_bal_both,newdata=subset(framiB_test_norm,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiB_test_norm$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 454   188
# 1 45    77  
roc.curve(framiB_test$TenYearCHD, prediction_01)    # 0.669

modelB_norm_bal_rose <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiB_train_norm_bal_rose)
summary(modelB_norm_bal_rose)
prediction <- predict(modelB_norm_bal_rose,newdata=subset(framiB_test_norm,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiB_test_norm$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 460   182
# 1 44    78  
roc.curve(framiB_test$TenYearCHD, prediction_01)    # 0.678




## Normalizado y Estandarizado
modelB_est_bal_over <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiB_train_est_bal_over)
summary(modelB_est_bal_over)
prediction <- predict(modelB_est_bal_over,newdata=subset(framiB_test_est,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiB_test_est$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 264   378
# 1 16    106  
roc.curve(framiB_test$TenYearCHD, prediction_01)    # 0.640

modelB_est_bal_under <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiB_train_est_bal_under)
summary(modelB_est_bal_under)
prediction <- predict(modelB_est_bal_under,newdata=subset(framiB_test_est,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiB_test_est$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 234   408
# 1 14    108  
roc.curve(framiB_test$TenYearCHD, prediction_01)    # 0.625

modelB_est_bal_both <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiB_train_est_bal_both)
summary(modelB_est_bal_both)
prediction <- predict(modelB_est_bal_both,newdata=subset(framiB_test_est,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiB_test_est$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 237   369
# 1 17    105  
roc.curve(framiB_test$TenYearCHD, prediction_01)    # 0.643

modelB_est_bal_rose <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiB_train_est_bal_rose)
summary(modelB_est_bal_rose)
prediction <- predict(modelB_est_bal_rose,newdata=subset(framiB_test_est,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiB_test_est$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 271   371
# 1 17    105 
roc.curve(framiB_test$TenYearCHD, prediction_01)    # 0.641








######################          MODELO C           ########################################################################


## Bruto
modelC <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiC_train)
summary(modelC)
prediction <- predict(modelC,newdata=subset(framiC_test,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiC_test$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 712   5
# 1 127  4
roc.curve(framiC_test$TenYearCHD, prediction_01)    # 0.512





## Normalizado y Balanceado
modelC_norm_bal_over <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiC_train_norm_bal_over)
summary(modelC_norm_bal_over)
prediction <- predict(modelC_norm_bal_over,newdata=subset(framiC_test_norm,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiC_test_norm$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 472   245
# 1 41    90  
roc.curve(framiC_test$TenYearCHD, prediction_01)    # 0.673

modelC_norm_bal_under <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiC_train_norm_bal_under)
summary(modelC_norm_bal_under)
prediction <- predict(modelC_norm_bal_under,newdata=subset(framiC_test_norm,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiC_test_norm$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 470   247
# 1 40    91  
roc.curve(framiC_test$TenYearCHD, prediction_01)    # 0.675 ************

modelC_norm_bal_both <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiC_train_norm_bal_both)
summary(modelC_norm_bal_both)
prediction <- predict(modelC_norm_bal_both,newdata=subset(framiC_test_norm,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiC_test_norm$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 493   224
# 1 48    83  
roc.curve(framiC_test$TenYearCHD, prediction_01)    # 0.661

modelC_norm_bal_rose <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiC_train_norm_bal_rose)
summary(modelC_norm_bal_rose)
prediction <- predict(modelC_norm_bal_rose,newdata=subset(framiC_test_norm,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiC_test_norm$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 499   218
# 1 46    85  
roc.curve(framiC_test$TenYearCHD, prediction_01)    # 0.672




## Normalizado y Estandarizado
modelC_est_bal_over <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiC_train_est_bal_over)
summary(modelC_est_bal_over)
prediction <- predict(modelC_est_bal_over,newdata=subset(framiC_test_est,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiC_test_est$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 337   380
# 1 20    111  
roc.curve(framiC_test$TenYearCHD, prediction_01)    # 0.659

modelC_est_bal_under <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiC_train_est_bal_under)
summary(modelC_est_bal_under)
prediction <- predict(modelC_est_bal_under,newdata=subset(framiC_test_est,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiC_test_est$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 276   441
# 1 14    117  
roc.curve(framiC_test$TenYearCHD, prediction_01)    # 0.639

modelC_est_bal_both <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiC_train_est_bal_both)
summary(modelC_est_bal_both)
prediction <- predict(modelC_est_bal_both,newdata=subset(framiC_test_est,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiC_test_est$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 338   379
# 1 25    106  
roc.curve(framiC_test$TenYearCHD, prediction_01)    # 0.640

modelC_est_bal_rose <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, family = binomial(link = "logit"), data = framiC_train_est_bal_rose)
summary(modelC_est_bal_rose)
prediction <- predict(modelC_est_bal_rose,newdata=subset(framiC_test_est,select=c(male, age, cigsPerDay, prevalentStroke, prevalentHyp, totChol, sysBP, glucose)),type='response')
prediction_01 <- ifelse(prediction > 0.5,1,0)
table(framiC_test_est$TenYearCHD,prediction_01)
#     prediction_01
#     0   1
# 0 346   371
# 1 24    107 
roc.curve(framiC_test$TenYearCHD, prediction_01)    # 0.650
