######################################
#                                    #
#          DECISION TREE             #
#                                    #
######################################

# Por Juanjo Beunza (ayuda conceptual de Enrique Puertas)
# Agradecimiento a Brett Lantz y Hadley Wickham por su inspiración

#### CLASIFICACIÓN MEDIANTE DECISION TRESS

# Limpieza del environment
rm(list = ls())

## STEP 1 - Obtención de datos
source("/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Frami analisis para REC/I- Preparacion datos tres modelos/Preparacion datos 3 modelos v8.R")


## STEP 2 - Exploración y preparación de los datos
## STEP 3 - Entrenamiento del modelo en los datos del Train



###### I- MODELO A
library(C50)
str(framiA_train)
framiA$TenYearCHD <- as.factor(framiA$TenYearCHD)
framiA_train$TenYearCHD <- as.factor(framiA_train$TenYearCHD)
framiA_test$TenYearCHD <- as.factor(framiA_test$TenYearCHD)

str(framiA_train)

framiA_model <- C5.0(framiA_train[-9], framiA_train$TenYearCHD)
framiA_model
summary(framiA_model)

# Evaluating model performance (on test dataset)
framiA_pred <- predict(framiA_model, framiA_test)
library(gmodels)
CrossTable(framiA_test$TenYearCHD, framiA_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       705 |        12 |       717 | 
#                 |     0.831 |     0.014 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |       120 |        11 |       131 | 
#                 |     0.142 |     0.013 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       825 |        23 |       848 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiA_test$TenYearCHD, framiA_pred) # 0.534


# Improving model performance
framiA_boost10 <- C5.0(framiA_train[-9], framiA_train$TenYearCHD,
                       trials = 10)
framiA_boost10
summary(framiA_boost10)
# Let's check the testing dataset
framiA_boost_pred10 <- predict(framiA_boost10, framiA_test)
CrossTable(framiA_test$TenYearCHD, framiA_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10YearCHD', 'predicted 10YCHD'))
#                    | predicted 10YCHD 
#   actual 10YearCHD |         0 |         1 | Row Total | 
#   -----------------|-----------|-----------|-----------|
#                  0 |       711 |         6 |       717 | 
#                    |     0.838 |     0.007 |           | 
#   -----------------|-----------|-----------|-----------|
#                  1 |       123 |         8 |       131 | 
#                    |     0.145 |     0.009 |           | 
#   -----------------|-----------|-----------|-----------|
#       Column Total |       834 |        14 |       848 | 
#   -----------------|-----------|-----------|-----------|
roc.curve(framiA_test$TenYearCHD, framiA_boost_pred10)  # 0.526






###### II- MODELO B

# Convertir evento de numérico a factor (en SVM, el outcome es siempre un factor... si no, sería una regresión, no una clasificación)
str(framiB)
str(framiB_train)
str(framiB_test)
framiB$TenYearCHD <- as.factor(framiB$TenYearCHD)
framiB_train$TenYearCHD <- as.factor(framiB_train$TenYearCHD)
framiB_test$TenYearCHD <- as.factor(framiB_test$TenYearCHD)


library(C50)
framiB_model <- C5.0(framiB_train[-9], framiB_train$TenYearCHD)
framiB_model
summary(framiB_model)

# Evaluating model performance (on test dataset)
framiB_pred <- predict(framiB_model, framiB_test)
library(gmodels)
CrossTable(framiB_test$TenYearCHD, framiB_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                        | framiB_pred 
# framiB_test$TenYearCHD |         0 | Row Total | 
# -----------------------|-----------|-----------|
#                      0 |       642 |       642 | 
#                        |     0.840 |           | 
# -----------------------|-----------|-----------|
#                      1 |       122 |       122 | 
#                        |     0.160 |           | 
# -----------------------|-----------|-----------|
#           Column Total |       764 |       764 | 
# -----------------------|-----------|-----------|
roc.curve(framiB_test$TenYearCHD, framiB_pred)    # 0.5
  
# Improving model performance
framiB_boost10 <- C5.0(framiB_train[-9], framiB_train$TenYearCHD,
                         trials = 10)
framiB_boost10
summary(framiB_boost10)
# Let's check the testing dataset
framiB_boost_pred10 <- predict(framiB_boost10, framiB_test)
CrossTable(framiB_test$TenYearCHD, framiB_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10YearCHD', 'predicted 10YCHD'))
#                        | framiB_boost_pred10 
# framiB_test$TenYearCHD |         0 | Row Total | 
# -----------------------|-----------|-----------|
#                      0 |       642 |       642 | 
#                        |     0.840 |           | 
# -----------------------|-----------|-----------|
#                      1 |       122 |       122 | 
#                        |     0.160 |           | 
# -----------------------|-----------|-----------|
#           Column Total |       764 |       764 | 
# -----------------------|-----------|-----------|
roc.curve(framiB_test$TenYearCHD, framiB_boost_pred10)  # 0.500



# Modelo B balanceado  

framiB_train_bal_over$TenYearCHD <- as.factor(framiB_train_bal_over$TenYearCHD)
str(framiB_train_bal_over)
#framiB_test$TenYearCHD <- as.factor(framiB_test$TenYearCHD)
str(framiB_test)  
    
library(C50)
framiB_bal_over_model <- C5.0(framiB_train_bal_over[-9], framiB_train_bal_over$TenYearCHD)
framiB_bal_over_model
summary(framiB_bal_over_model)

# Evaluating model performance (on test dataset)
framiB_bal_over_pred <- predict(framiB_bal_over_model, framiB_test)
library(gmodels)
CrossTable(framiB_test$TenYearCHD, framiB_bal_over_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       462 |       180 |       642 | 
#                 |     0.605 |     0.236 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        75 |        47 |       122 | 
#                 |     0.098 |     0.062 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       537 |       227 |       764 | 
#   --------------|-----------|-----------|-----------|
  roc.curve(framiB_test$TenYearCHD, framiB_bal_over_pred)    # 0.552

# Improving model performance
framiB_bal_over_boost10 <- C5.0(framiB_train_bal_over[-9], framiB_train_bal_over$TenYearCHD, 
                       trials = 10)
framiB_bal_over_boost10
summary(framiB_bal_over_boost10)
# Let's check the testing dataset
framiB_bal_over_boost_pred10 <- predict(framiB_bal_over_boost10, framiB_test)
CrossTable(framiB_test$TenYearCHD, framiB_bal_over_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10YearCHD', 'predicted 10YCHD'))
#                    | predicted 10YCHD 
#   actual 10YearCHD |         0 |         1 | Row Total | 
#   -----------------|-----------|-----------|-----------|
#                  0 |       585 |        57 |       642 | 
#                    |     0.766 |     0.075 |           | 
#   -----------------|-----------|-----------|-----------|
#                  1 |        88 |        34 |       122 | 
#                    |     0.115 |     0.045 |           | 
#   -----------------|-----------|-----------|-----------|
#       Column Total |       673 |        91 |       764 | 
#   -----------------|-----------|-----------|-----------|
roc.curve(framiB_test$TenYearCHD, framiB_bal_over_boost_pred10)  # 0.595  
  
  




# III- MODELO C

# Convertir evento de numérico a factor (en SVM, el outcome es siempre un factor... si no, sería una regresión, no una clasificación)
#str(frami)
framiC$TenYearCHD <- as.factor(framiC$TenYearCHD)
framiC_train$TenYearCHD <- as.factor(framiC_train$TenYearCHD)
framiC_test$TenYearCHD <- as.factor(framiC_test$TenYearCHD)


library(C50)
framiC_model <- C5.0(framiC_train[-9], framiC_train$TenYearCHD)
framiC_model
summary(framiC_model)

# Evaluating model performance (on test dataset)
framiC_pred <- predict(framiC_model, framiC_test)
library(gmodels)
CrossTable(framiC_test$TenYearCHD, framiC_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       703 |        14 |       717 | 
#                 |     0.829 |     0.017 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |       120 |        11 |       131 | 
#                 |     0.142 |     0.013 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       823 |        25 |       848 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiC_test$TenYearCHD, framiC_pred)    # 0.532

# Improving model performance
framiC_boost10 <- C5.0(framiC_train[-9], framiC_train$TenYearCHD,
                       trials = 10)
framiC_boost10
summary(framiC_boost10)
# Let's check the testing dataset
framiC_boost_pred10 <- predict(framiC_boost10, framiC_test)
CrossTable(framiC_test$TenYearCHD, framiC_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10YearCHD', 'predicted 10YCHD'))
#                    | predicted 10YCHD 
#   actual 10YearCHD |         0 |         1 | Row Total | 
#   -----------------|-----------|-----------|-----------|
#                  0 |       708 |         9 |       717 | 
#                    |     0.835 |     0.011 |           | 
#   -----------------|-----------|-----------|-----------|
#                  1 |       124 |         7 |       131 | 
#                    |     0.146 |     0.008 |           | 
#   -----------------|-----------|-----------|-----------|
#       Column Total |       832 |        16 |       848 | 
#   -----------------|-----------|-----------|-----------|
roc.curve(framiC_test$TenYearCHD, framiC_boost_pred10)  # 0.520



  