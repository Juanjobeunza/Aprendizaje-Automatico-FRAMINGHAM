###########################################
#                                         #
#        SUPPORT VECTOR MACHINE           #
#       INBALANCED CLASSIFICATION         #
#                                         #
###########################################

# Juanjo Beunza y Enrique Puertas (idea y aplicación del balanceo)


NOTAS
- Es importantísimo normalizar o escalar los datos.
- El label o evento debe ser categórico (p.e. dicotómico) para un algoritmo de clasificación.



rm(list=ls())
library(ROSE)
library(rpart)
source("/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Frami analisis para REC/I- Preparacion datos tres modelos/Preparacion datos 3 modelos v8.R")




##################################################         MODELO B           #############################################



# Explorar data
str(framiB)         # 3817 obs 9 var
str(framiB_train)   # 3053 obs.9 var
str(framiB_test)    # 764 obs. 9 var
table(framiB_train$TenYearCHD)               # Train: 469 de 3053
# 0     1 
# 2584  469 
prop.table(table(framiB_train$TenYearCHD))   # 15%
table(framiB_test$TenYearCHD)                # Test: 122 de 764
# 0   1 
# 642 122 
prop.table(table(framiB_test$TenYearCHD))    # 15%


# Transformamos el evento o label en categorico

framiB$TenYearCHD <- as.factor(framiB$TenYearCHD)
framiB_train$TenYearCHD <- as.factor(framiB_train$TenYearCHD)
framiB_test$TenYearCHD <- as.factor(framiB_test$TenYearCHD)
framiB_train_norm$TenYearCHD <- as.factor(framiB_train_norm$TenYearCHD)
framiB_test_norm$TenYearCHD <- as.factor(framiB_test_norm$TenYearCHD)
framiB_train_est$TenYearCHD <- as.factor(framiB_train_est$TenYearCHD)
framiB_test_est$TenYearCHD <- as.factor(framiB_test_est$TenYearCHD)

framiB_train_norm_bal_over$TenYearCHD <- as.factor(framiB_train_norm_bal_over$TenYearCHD)
framiB_train_norm_bal_under$TenYearCHD <- as.factor(framiB_train_norm_bal_under$TenYearCHD)
framiB_train_norm_bal_both$TenYearCHD <- as.factor(framiB_train_norm_bal_both$TenYearCHD)
framiB_train_norm_bal_rose$TenYearCHD <- as.factor(framiB_train_norm_bal_rose$TenYearCHD)

framiB_train_est_bal_over$TenYearCHD <- as.factor(framiB_train_est_bal_over$TenYearCHD)
framiB_train_est_bal_under$TenYearCHD <- as.factor(framiB_train_est_bal_under$TenYearCHD)
framiB_train_est_bal_both$TenYearCHD <- as.factor(framiB_train_est_bal_both$TenYearCHD)
framiB_train_est_bal_rose$TenYearCHD <- as.factor(framiB_train_est_bal_rose$TenYearCHD)

framiC$TenYearCHD <- as.factor(framiC$TenYearCHD)
framiC_train$TenYearCHD <- as.factor(framiC_train$TenYearCHD)
framiC_test$TenYearCHD <- as.factor(framiC_test$TenYearCHD)
framiC_train_norm$TenYearCHD <- as.factor(framiC_train_norm$TenYearCHD)
framiC_test_norm$TenYearCHD <- as.factor(framiC_test_norm$TenYearCHD)
framiC_train_est$TenYearCHD <- as.factor(framiC_train_est$TenYearCHD)
framiC_test_est$TenYearCHD <- as.factor(framiC_test_est$TenYearCHD)

framiC_train_norm_bal_over$TenYearCHD <- as.factor(framiC_train_norm_bal_over$TenYearCHD)
framiC_train_norm_bal_under$TenYearCHD <- as.factor(framiC_train_norm_bal_under$TenYearCHD)
framiC_train_norm_bal_both$TenYearCHD <- as.factor(framiC_train_norm_bal_both$TenYearCHD)
framiC_train_norm_bal_rose$TenYearCHD <- as.factor(framiC_train_norm_bal_rose$TenYearCHD)

framiC_train_est_bal_over$TenYearCHD <- as.factor(framiC_train_est_bal_over$TenYearCHD)
framiC_train_est_bal_under$TenYearCHD <- as.factor(framiC_train_est_bal_under$TenYearCHD)
framiC_train_est_bal_both$TenYearCHD <- as.factor(framiC_train_est_bal_both$TenYearCHD)
framiC_train_est_bal_rose$TenYearCHD <- as.factor(framiC_train_est_bal_rose$TenYearCHD)

### PASO 3 - Entrenamiento del algoritmo (svm_model)

library(kernlab)

# Original
framiB_classifier <- ksvm(TenYearCHD ~ ., data = framiB_train,
                                       kernel = "vanilladot")

# Normalizado
framiB_norm_bal_over_classifier <- ksvm(TenYearCHD ~ ., data = framiB_train_norm_bal_over,
                           kernel = "vanilladot")
framiB_norm_bal_under_classifier <- ksvm(TenYearCHD ~ ., data = framiB_train_norm_bal_under,
                                kernel = "vanilladot")
framiB_norm_bal_both_classifier <- ksvm(TenYearCHD ~ ., data = framiB_train_norm_bal_both,
                                kernel = "vanilladot")
framiB_norm_bal_rose_classifier <- ksvm(TenYearCHD ~ ., data = framiB_train_norm_bal_rose,
                                kernel = "vanilladot")

# Estandarizada
framiB_est_bal_over_classifier <- ksvm(TenYearCHD ~ ., data = framiB_train_est_bal_over,
                                        kernel = "vanilladot")
framiB_est_bal_under_classifier <- ksvm(TenYearCHD ~ ., data = framiB_train_est_bal_under,
                                         kernel = "vanilladot")
framiB_est_bal_both_classifier <- ksvm(TenYearCHD ~ ., data = framiB_train_est_bal_both,
                                        kernel = "vanilladot")
framiB_est_bal_rose_classifier <- ksvm(TenYearCHD ~ ., data = framiB_train_est_bal_rose,
                                        kernel = "vanilladot")



########### No hay diferencia aparente en la training entre normalización y estandarización. ###################
########### Luego veremos que si hay diferencias en la testing ##############

# PASO 4 - Evaluar el rendimiento (performance) del modelo
# Prediction on the testing data set
framiB_predictions <- predict(framiB_classifier, framiB_test)

framiB_norm_bal_over_predictions <- predict(framiB_norm_bal_over_classifier, framiB_test_norm)
framiB_norm_bal_under_predictions <- predict(framiB_norm_bal_under_classifier, framiB_test_norm)
framiB_norm_bal_both_predictions <- predict(framiB_norm_bal_both_classifier, framiB_test_norm)
framiB_norm_bal_rose_predictions <- predict(framiB_norm_bal_rose_classifier, framiB_test_norm)

framiB_est_bal_over_predictions <- predict(framiB_est_bal_over_classifier, framiB_test_est)
framiB_est_bal_under_predictions <- predict(framiB_est_bal_under_classifier, framiB_test_est)
framiB_est_bal_both_predictions <- predict(framiB_est_bal_both_classifier, framiB_test_est)
framiB_est_bal_rose_predictions <- predict(framiB_est_bal_rose_classifier, framiB_test_est)


# Compare predicted vs real values
library(gmodels)

CrossTable(framiB_test$TenYearCHD, framiB_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))                  # Consigue accuracy 85% sin predicción=1
#                        | framiB_predictions 
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
roc.curve(framiB_test$TenYearCHD, framiB_predictions)    # 0.5

CrossTable(framiB_test_norm$TenYearCHD, framiB_norm_bal_over_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       414 |       228 |       642 | 
#                 |     0.542 |     0.298 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        35 |        87 |       122 | 
#                 |     0.046 |     0.114 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       449 |       315 |       764 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiB_test_norm$TenYearCHD, framiB_norm_bal_over_predictions)    # 0.679

CrossTable(framiB_test_norm$TenYearCHD, framiB_norm_bal_under_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       427 |       215 |       642 | 
#                 |     0.559 |     0.281 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        40 |        82 |       122 | 
#                 |     0.052 |     0.107 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       467 |       297 |       764 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiB_test_norm$TenYearCHD, framiB_norm_bal_under_predictions)    # 0.669

CrossTable(framiB_test_norm$TenYearCHD, framiB_norm_bal_both_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       440 |       202 |       642 | 
#                 |     0.576 |     0.264 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        39 |        83 |       122 | 
#                 |     0.051 |     0.109 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       479 |       285 |       764 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiB_test_norm$TenYearCHD, framiB_norm_bal_both_predictions)    # 0.683

CrossTable(framiB_test_norm$TenYearCHD, framiB_norm_bal_rose_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       446 |       196 |       642 | 
#                 |     0.584 |     0.257 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        40 |        82 |       122 | 
#                 |     0.052 |     0.107 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       486 |       278 |       764 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiB_test_norm$TenYearCHD, framiB_norm_bal_rose_predictions)    # 0.683

CrossTable(framiB_test_est$TenYearCHD, framiB_est_bal_over_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       278 |       364 |       642 | 
#                 |     0.364 |     0.476 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        15 |       107 |       122 | 
#                 |     0.020 |     0.140 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       293 |       471 |       764 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiB_test_est$TenYearCHD, framiB_est_bal_over_predictions)    # 0.655

CrossTable(framiB_test_est$TenYearCHD, framiB_est_bal_under_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       254 |       388 |       642 | 
#                 |     0.332 |     0.508 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        17 |       105 |       122 | 
#                 |     0.022 |     0.137 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       271 |       493 |       764 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiB_test_est$TenYearCHD, framiB_est_bal_under_predictions)    # 0.628

CrossTable(framiB_test_est$TenYearCHD, framiB_est_bal_both_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       303 |       339 |       642 | 
#                 |     0.397 |     0.444 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        21 |       101 |       122 | 
#                 |     0.027 |     0.132 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       324 |       440 |       764 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiB_test_est$TenYearCHD, framiB_est_bal_both_predictions)    # 0.650

CrossTable(framiB_test_est$TenYearCHD, framiB_est_bal_rose_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       292 |       350 |       642 | 
#                 |     0.382 |     0.458 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        19 |       103 |       122 | 
#                 |     0.025 |     0.135 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       311 |       453 |       764 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiB_test_est$TenYearCHD, framiB_est_bal_rose_predictions)    # 0.650









##################################################         MODELO C           #############################################


# Explorar data
str(framiC)         # 4240 obs 9 var
str(framiC_train)   # 3392 obs. 9 var
str(framiC_test)    # 848 obs. 9 var
table(framiC_train$TenYearCHD)               # Train: 513 de 3392
# 0     1
# 2879  513
prop.table(table(framiC_train$TenYearCHD))   # 15%
table(framiC_test$TenYearCHD)                # Test: 131 de 848
# 0   1 
# 717 131 
prop.table(table(framiC_test$TenYearCHD))    # 15%









### PASO 3 - Entrenamiento del algoritmo (svm_model)

library(kernlab)

# Original
framiC_classifier <- ksvm(TenYearCHD ~ ., data = framiC_train,
                          kernel = "vanilladot")

# Normalizado
framiC_norm_bal_over_classifier <- ksvm(TenYearCHD ~ ., data = framiC_train_norm_bal_over,
                                       kernel = "vanilladot")
framiC_norm_bal_under_classifier <- ksvm(TenYearCHD ~ ., data = framiC_train_norm_bal_under,
                                        kernel = "vanilladot")
framiC_norm_bal_both_classifier <- ksvm(TenYearCHD ~ ., data = framiC_train_norm_bal_both,
                                       kernel = "vanilladot")
framiC_norm_bal_rose_classifier <- ksvm(TenYearCHD ~ ., data = framiC_train_norm_bal_rose,
                                       kernel = "vanilladot")

# Estandarizado
framiC_est_bal_over_classifier <- ksvm(TenYearCHD ~ ., data = framiC_train_est_bal_over,
                                       kernel = "vanilladot")
framiC_est_bal_under_classifier <- ksvm(TenYearCHD ~ ., data = framiC_train_est_bal_under,
                                        kernel = "vanilladot")
framiC_est_bal_both_classifier <- ksvm(TenYearCHD ~ ., data = framiC_train_est_bal_both,
                                       kernel = "vanilladot")
framiC_est_bal_rose_classifier <- ksvm(TenYearCHD ~ ., data = framiC_train_est_bal_rose,
                                       kernel = "vanilladot")


########### Luego veremos que si hay diferencias en la testing ##############

# PASO 4 - Evaluar el rendimiento (performance) del modelo
# Prediction on the testing data set
framiC_predictions <- predict(framiC_classifier, framiC_test)

framiC_norm_bal_over_predictions <- predict(framiC_norm_bal_over_classifier, framiC_test_norm)
framiC_norm_bal_under_predictions <- predict(framiC_norm_bal_under_classifier, framiC_test_norm)
framiC_norm_bal_both_predictions <- predict(framiC_norm_bal_both_classifier, framiC_test_norm)
framiC_norm_bal_rose_predictions <- predict(framiC_norm_bal_rose_classifier, framiC_test_norm)

framiC_est_bal_over_predictions <- predict(framiC_est_bal_over_classifier, framiC_test_est)
framiC_est_bal_under_predictions <- predict(framiC_est_bal_under_classifier, framiC_test_est)
framiC_est_bal_both_predictions <- predict(framiC_est_bal_both_classifier, framiC_test_est)
framiC_est_bal_rose_predictions <- predict(framiC_est_bal_rose_classifier, framiC_test_est)


# Compare predicted vs real values
library(gmodels)

CrossTable(framiC_test$TenYearCHD, framiC_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))                  # Consigue accuracy 85% sin predicción=1
#                        | framiC_predictions 
# framiC_test$TenYearCHD |         0 | Row Total | 
# -----------------------|-----------|-----------|
#                      0 |       717 |       717 | 
#                        |     0.846 |           | 
# -----------------------|-----------|-----------|
#                      1 |       131 |       131 | 
#                        |     0.154 |           | 
# -----------------------|-----------|-----------|
#           Column Total |       848 |       848 | 
# -----------------------|-----------|-----------|
roc.curve(framiC_test$TenYearCHD, framiC_predictions)    # 0.500

CrossTable(framiC_test_norm$TenYearCHD, framiC_norm_bal_over_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       455 |       262 |       717 | 
#                 |     0.537 |     0.309 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        39 |        92 |       131 | 
#                 |     0.046 |     0.108 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       494 |       354 |       848 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiC_test_norm$TenYearCHD, framiC_norm_bal_over_predictions)    # 0.668

CrossTable(framiC_test_norm$TenYearCHD, framiC_norm_bal_under_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       445 |       272 |       717 | 
#                 |     0.525 |     0.321 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        38 |        93 |       131 | 
#                 |     0.045 |     0.110 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       483 |       365 |       848 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiC_test_norm$TenYearCHD, framiC_norm_bal_under_predictions)    # 0.665

CrossTable(framiC_test_norm$TenYearCHD, framiC_norm_bal_both_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       488 |       229 |       717 | 
#                 |     0.575 |     0.270 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        41 |        90 |       131 | 
#                 |     0.048 |     0.106 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       529 |       319 |       848 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiC_test_norm$TenYearCHD, framiC_norm_bal_both_predictions)    # 0.684

CrossTable(framiC_test_norm$TenYearCHD, framiC_norm_bal_rose_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       506 |       211 |       717 | 
#                 |     0.597 |     0.249 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        47 |        84 |       131 | 
#                 |     0.055 |     0.099 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       553 |       295 |       848 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiC_test_norm$TenYearCHD, framiC_norm_bal_rose_predictions)    # 0.673

CrossTable(framiC_test_est$TenYearCHD, framiC_est_bal_over_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       341 |       376 |       717 | 
#                 |     0.402 |     0.443 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        21 |       110 |       131 | 
#                 |     0.025 |     0.130 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       362 |       486 |       848 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiC_test_est$TenYearCHD, framiC_est_bal_over_predictions)    # 0.658

CrossTable(framiC_test_est$TenYearCHD, framiC_est_bal_under_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       288 |       429 |       717 | 
#                 |     0.340 |     0.506 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        16 |       115 |       131 | 
#                 |     0.019 |     0.136 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       304 |       544 |       848 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiC_test_est$TenYearCHD, framiC_est_bal_under_predictions)    # 0.640

CrossTable(framiC_test_est$TenYearCHD, framiC_est_bal_both_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       362 |       355 |       717 | 
#                 |     0.427 |     0.419 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        27 |       104 |       131 | 
#                 |     0.032 |     0.123 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       389 |       459 |       848 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiC_test_est$TenYearCHD, framiC_est_bal_both_predictions)    # 0.649

CrossTable(framiC_test_est$TenYearCHD, framiC_est_bal_rose_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual 10yCHD', 'predicted 10yCHD'))
#                 | predicted 10yCHD 
#   actual 10yCHD |         0 |         1 | Row Total | 
#   --------------|-----------|-----------|-----------|
#               0 |       364 |       353 |       717 | 
#                 |     0.429 |     0.416 |           | 
#   --------------|-----------|-----------|-----------|
#               1 |        27 |       104 |       131 | 
#                 |     0.032 |     0.123 |           | 
#   --------------|-----------|-----------|-----------|
#    Column Total |       391 |       457 |       848 | 
#   --------------|-----------|-----------|-----------|
roc.curve(framiC_test_est$TenYearCHD, framiC_est_bal_rose_predictions)    # 0.651


