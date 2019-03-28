######################################
#                                    #
#          RANDOM FOREST             #
#                                    #
######################################

# Por Juanjo Beunza (ayuda conceptual de Enrique Puertas)
# Agradecimiento a Brett Lantz y Hadley Wickham por su inspiración

https://www.r-bloggers.com/how-to-implement-random-forests-in-r/
https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/randomForest
https://cran.r-project.org/web/packages/randomForest/randomForest.pdf


###### CLASIFICACIÓN CON RANDOM FOREST

rm(list = ls())
### PASO 1 - Recolección de datos
source("/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Frami analisis para REC/I- Preparacion datos tres modelos/Preparacion datos 3 modelos v8.R")

### PASO 2 - Exploratorio y preparación de datos
### PASO 3 - Entrenando el modelo con los datos RANDOM FOREST


# I- MODELO A
# Training random forest
library(randomForest)
set.seed(300)
rfA <- randomForest(TenYearCHD ~ ., data = framiA_train, importance=TRUE)
rfA

plot(rf)
# Error por valores missing. Paro modelo A.




# II- MODELO B

# Convertir evento de numérico a factor (en SVM, el outcome es siempre un factor... si no, sería una regresión, no una clasificación)
#str(frami)
str(framiB)
framiB$TenYearCHD <- as.factor(framiB$TenYearCHD)
framiB_train$TenYearCHD <- as.factor(framiB_train$TenYearCHD)
framiB_test$TenYearCHD <- as.factor(framiB_test$TenYearCHD)


# Training random forest modelo rfB1
library(randomForest)
set.seed(300)
rfB1 <- randomForest(TenYearCHD ~ ., data = framiB_train, importance=TRUE)
rfB1
#Predicción en testing set modelo rfB1
pred_framiB_test_1 <- predict(rfB1, framiB_test, type="class")
# Checking classification accuracy
table(pred_framiB_test_1, framiB_test$TenYearCHD)
#   pred_framiB_test_1   0   1
#                      0 637 113
#                      1 5   9
roc.curve(framiB_test$TenYearCHD, pred_framiB_test_1)    # 0.522


# Training random forest modelo rfB2
rfB2 <- randomForest(TenYearCHD ~ ., data = framiB_train, ntree=500,mtry=6, importance=TRUE)
rfB2
# # Predicción en training set modelo rfB2
# pred_framiB_train_2 <- predict(rfB2, framiB_train, type="class")
# # Checking classification accuracy
# table(pred_framiB_train_2, framiB_train$TenYearCHD)
# 
# #Predicción en testing set
pred_framiB_test_2 <- predict(rfB2, framiB_test, type="class")
# # Checking classification accuracy
table(pred_framiB_test_2, framiB_test$TenYearCHD)
# pred_framiB_test_2   0   1
#                    0 630 105
#                    1 12  17
roc.curve(framiB_test$TenYearCHD, pred_framiB_test_2)    # 0.560



# Frami B Balanceado

str(framiB_train_bal_over)
framiB_train_bal_over$TenYearCHD <- as.factor(framiB_train_bal_over$TenYearCHD)

# Training random forest modelo rfB1bal
#library(randomForest)
set.seed(300)
rfB1bal <- randomForest(TenYearCHD ~ ., data = framiB_train_bal_over, importance=TRUE)
rfB1bal
#Predicción en testing set modelo rfB1
pred_framiB_bal_over_test_1 <- predict(rfB1bal, framiB_test, type="class")
# Checking classification accuracy
table(pred_framiB_bal_over_test_1, framiB_test$TenYearCHD)
# pred_framiB_bal_over_test_1   0   1
#                             0 555  75
#                             1  87  47
roc.curve(framiB_test$TenYearCHD, pred_framiB_bal_over_test_1)    # 0.625


# III- MODELO C

# Convertir evento de numérico a factor (en SVM, el outcome es siempre un factor... si no, sería una regresión, no una clasificación)
#str(frami)
framiC$TenYearCHD <- as.factor(framiC$TenYearCHD)
framiC_train$TenYearCHD <- as.factor(framiC_train$TenYearCHD)
framiC_test$TenYearCHD <- as.factor(framiC_test$TenYearCHD)


# Training random forest modelo 1C
library(randomForest)
set.seed(300)
rfC1 <- randomForest(TenYearCHD ~ ., data = framiC_train, importance=TRUE)
rfC1

# Predicción en training set 1C
pred_framiC_train <- predict(rfC1, framiC_train, type="class")
# Checking classification accuracy
table(pred_framiC_train, framiC_train$TenYearCHD)
#Predicción en testing set
pred_framiC_test <- predict(rfC1, framiC_test, type="class")
# Checking classification accuracy
table(pred_framiC_test, framiC_test$TenYearCHD)
# pred_framiC_test   0   1
#                 0 711 127
#                 1   6   4
roc.curve(framiC_test$TenYearCHD, pred_framiC_test)    # 0.511

# Training random forest modelo 2C
rfC2 <- randomForest(TenYearCHD ~ ., data = framiC_train, ntree=500,mtry=6, importance=TRUE)
rfC2

# # Predicción en training set 2C
# pred_framiC_train <- predict(rfC2, framiC_train, type="class")
# # Checking classification accuracy
# table(pred_framiC_train, framiC_train$TenYearCHD)
# #Predicción en testing set
# pred_framiC_test <- predict(rfC2, framiC_test, type="class")
# # Checking classification accuracy
# table(pred_framiC_test, framiC_test$TenYearCHD)



# Frami C Balanceado

str(framiC_train_bal_over)
framiC_train_bal_over$TenYearCHD <- as.factor(framiC_train_bal_over$TenYearCHD)

# Training random forest modelo rfB1bal
#library(randomForest)
set.seed(300)
rfC1bal <- randomForest(TenYearCHD ~ ., data = framiC_train_bal_over, importance=TRUE)
rfC1bal
#Predicción en testing set modelo rfB1
pred_framiC_bal_over_test_1 <- predict(rfC1bal, framiC_test, type="class")
# Checking classification accuracy
table(pred_framiC_bal_over_test_1, framiC_test$TenYearCHD)
# pred_framiC_bal_over_test_1   0   1
#                            0 624  91
#                            1  93  40
roc.curve(framiC_test$TenYearCHD, pred_framiC_bal_over_test_1)    # 0.588



