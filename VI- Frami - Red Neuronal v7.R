######################################
#                                    #
#            RED NEURONAL            #
#                                    #
######################################

# Por Juanjo Beunza
# Agradecimiento a Brett Lantz y Hadley Wickham por su inspiración
# https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/ (regression)
# https://www.r-bloggers.com/neuralnet-train-and-test-neural-networks-using-r/
  

# NOTAS
# La red neuronal solo funciona con variables numéricas (num o int). Hay que modificar el label o evento.
# Es importante normalizar o escalar los datos antes de entrenar la red neuronal.


rm(list=ls())
getwd()
setwd("/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Frami analisis para REC/I- Preparación datos tres modelos")
source("/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Frami analisis para REC/I- Preparacion datos tres modelos/Preparacion datos 3 modelos v8.R")
library(neuralnet)
library(varhandle)


######################          MODELO A - BASE BRUTA          ########################################################################

### PASO 2 - Preparar los datos

### PASO 3 - Entrenando el modelo en los datos

# names(framiB_train)
framiA_train_model_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp +
                                  totChol + sysBP + glucose, data = framiB, hidden = 1, threshold = 0.01)
print(plot(framiA_train_model_1))
framiA_train_model_1$result.matrix       # Error 250

framiA_train_model_3 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp +
                                  totChol + sysBP + glucose, data = framiB, hidden = 3, threshold = 0.01)
print(plot(framiA_train_model_3))
framiA_train_model_3$result.matrix       # Error 246

framiA_train_model_5 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp +
                                    totChol + sysBP + glucose, data = framiB, hidden = 5, threshold = 0.01)
print(plot(framiA_train_model_5))
framiA_train_model_5$result.matrix       # Error 243

# framiA_train_model_8 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp +
#                                     totChol + sysBP + glucose, data = framiB, hidden = 8, threshold = 0.01)
# print(plot(framiA_train_model_8))
# framiA_train_model_8$result.matrix       # Error 249



### PASO 4 - Evaluar el rendimiento (performance) del modelo

temp_framiA_test <- subset(framiA_test, select = c("male","age", "cigsPerDay", "prevalentStroke", "prevalentHyp", "totChol", "sysBP", "glucose"))

framiA_model_1.results <- compute(framiA_train_model_1, temp_framiA_test)
results <- data.frame(actual = framiA_test$TenYearCHD, prediction = framiA_model_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#         prediction
# actual   0     1
# 0        650   
# 1        122   
roc.curve(actual, prediction)    # 0.5


framiA_model_3.results <- compute(framiA_train_model_3, temp_framiA_test)
results <- data.frame(actual = framiA_test$TenYearCHD, prediction = framiA_model_3.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#         prediction
# actual   0     1
# 0        649   1
# 1        119   3
roc.curve(actual, prediction)    # 


framiA_model_5.results <- compute(framiA_train_model_5, temp_framiA_test)
results <- data.frame(actual = framiA_test$TenYearCHD, prediction = framiA_model_5.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#         prediction
# actual   0     1
# 0        649   1
# 1        119   3
roc.curve(actual, prediction)    # 0,5 **********


# framiA_model_8.results <- compute(framiA_train_model_8, temp_framiA_test)
# results <- data.frame(actual = framiA_test$TenYearCHD, prediction = framiA_model_8.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# 
# roc.curve(actual, prediction)    #



######################          MODELO B - ANÁLISIS DE CASOS COMPLETOS          ###########################################################

### PASO 2 - Preparación de los datos
# 1. BASE
# summary(framiB_train)
# summary(framiB_test)
# 2a. Normalizada
# summary(framiB_train_norm)
# summary(framiB_test_norm)
# 2b. Estandarizada
# summary(framiB_train_est)
# summary(framiB_test_est)
# 3. Balanceada
# summary(framiB_train_bal_over)
# summary(framiB_train_bal_under)
# summary(framiB_train_bal_both)
# summary(framiB_train_bal_rose)
# 4. Normalizada y Balanceada
# summary(framiB_train_norm_bal_over)
# summary(framiB_train_norm_bal_under)
# summary(framiB_train_norm_bal_both)
# summary(framiB_train_norm_bal_rose)


### PASO 3 - Entrenamiento del algoritmo

# NORMALIZADO

framiB_norm_model_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = 1, threshold = 0.01)
# plot(framiB_norm_model_1)
framiB_norm_model_1$result.matrix       # Error 177
 
framiB_norm_model_2 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = 2, threshold = 0.01)
# plot(framiB_norm_model_2)
framiB_norm_model_2$result.matrix       # Error 175

framiB_norm_model_3 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = 3, threshold = 0.01)
# plot(framiB_norm_model_3)
framiB_norm_model_3$result.matrix       # Error 174

framiB_norm_model_4 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = 4, threshold = 0.01)
# plot(framiB_norm_model_4)
framiB_norm_model_4$result.matrix       # Error 170

framiB_norm_model_5 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = 5, threshold = 0.1)
# plot(framiB_norm_model_5)
framiB_norm_model_5$result.matrix       # Error 172

# framiB_norm_model_10 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = 10, threshold = 0.1)
# # plot(framiB_norm_model_10)
# framiB_norm_model_10$result.matrix      # Error 154
# 
# framiB_norm_model_13 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = 13, threshold = 0.1)
# # plot(framiB_norm_model_13)
# framiB_norm_model_13$result.matrix      # Error 146

framiB_norm_model_2_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(2,1), threshold = 0.01)
# plot(framiB_norm_model_2_1)
framiB_norm_model_2_1$result.matrix     # Error 175

framiB_norm_model_2_2 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(2,2), threshold = 0.01)
# plot(framiB_norm_model_2_2)
framiB_norm_model_2_2$result.matrix     # Error 175

framiB_norm_model_3_3 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(3,3), threshold = 0.1)
# plot(framiB_norm_model_3_3)
framiB_norm_model_3_3$result.matrix     # Error 171

framiB_norm_model_4_4 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(4,4), threshold = 0.1)
# plot(framiB_norm_model_4_4)
framiB_norm_model_4_4$result.matrix     # Error 165

framiB_norm_model_3_3_3 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(3,3,3), threshold = 0.1)
# plot(framiB_norm_model_3_3_3)
framiB_norm_model_3_3_3$result.matrix   # Error 173

framiB_norm_model_5_5 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(5,5), threshold = 0.1)
# plot(framiB_norm_model_5_5)
framiB_norm_model_5_5$result.matrix     # Error 158

framiB_norm_model_5_3 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(5,3), threshold = 0.1)
# plot(framiB_norm_model_5_3)
framiB_norm_model_5_3$result.matrix     # Error 172

framiB_norm_model_6_5 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(6,5), threshold = 0.1)
# plot(framiB_norm_model_6_5)
framiB_norm_model_6_5$result.matrix     # Error 155

framiB_norm_model_6_6 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(6,6), threshold = 0.1)
# plot(framiB_norm_model_6_6)
framiB_norm_model_6_6$result.matrix     # Error 161

framiB_norm_model_7_7 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(7,7), threshold = 0.1)
# plot(framiB_norm_model_7_7)
framiB_norm_model_7_7$result.matrix     # Error 149

# framiB_norm_model_9_9 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(9,9), threshold = 0.1)
# # plot(framiB_norm_model_9_9)
# framiB_norm_model_9_9$result.matrix     # Error 131
# 
# framiB_norm_model_11_11 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(11,11), threshold = 0.1)
# # plot(framiB_norm_model_11_11)
# framiB_norm_model_11_11$result.matrix   # Error 117
# 
# framiB_norm_model_9_9_9 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm, hidden = c(9,9,9), threshold = 0.5)
# plot(framiB_norm_model_9_9_9)
# framiB_norm_model_9_9_9$result.matrix   # Error 112





# ESTANDARIZADO

framiB_est_model_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_est, hidden = 1, threshold = 0.01)
# plot(framiB_est_model_1)
framiB_est_model_1$result.matrix       # 177

framiB_est_model_2 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_est, hidden = 2, threshold = 0.01)
# plot(framiB_est_model_2)
framiB_est_model_2$result.matrix       # 175

framiB_est_model_5 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_est, hidden = 5, threshold = 0.01)
# plot(framiB_est_model_5)
framiB_est_model_5$result.matrix       # 170

# framiB_est_model_13 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_est, hidden = 13, threshold = 0.1)
# # plot(framiB_est_model_13)
# framiB_est_model_13$result.matrix       # 144 

framiB_est_model_5_5 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_est, hidden = c(5,5), threshold = 0.1)
# plot(framiB_est_model_5_5)
framiB_est_model_5_5$result.matrix       # 169

framiB_est_model_7_7 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_est, hidden = c(7,7), threshold = 0.1)
# plot(framiB_est_model_7_7)
framiB_est_model_7_7$result.matrix       # 158

framiB_est_model_9_9 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_est, hidden = c(9,9), threshold = 0.1)
# plot(framiB_model_9_9)
framiB_est_model_9_9$result.matrix       # 143

# framiB_est_model_11_11 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_est, hidden = c(11,11), threshold = 0.1)
# plot(framiB_est_model_11_11)
# framiB_est_model_11_11$result.matrix     # 128
# 
# framiB_est_model_9_9_9 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_est, hidden = c(9,9,9), threshold = 0.5)
# plot(framiB_est_model_9_9_9)
# framiB_est_model_9_9_9$result.matrix     # 169







# NORMALIZADO Y BALANCEADO

framiB_norm_bal_over_model_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_over, hidden = 1, threshold = 0.01)
#plot(framiB_norm_bal_over_model_1)
framiB_norm_bal_over_model_1$result.matrix       # 533

framiB_norm_bal_under_model_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_under, hidden = 1, threshold = 0.01)
#plot(framiB_norm_bal_under_model_1)
framiB_norm_bal_under_model_1$result.matrix       # 98

framiB_norm_bal_both_model_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_both, hidden = 1, threshold = 0.01)
#plot(framiB_norm_bal_both_model_1)
framiB_norm_bal_both_model_1$result.matrix       # 325

framiB_norm_bal_rose_model_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_rose, hidden = 1, threshold = 0.01)
#plot(framiB_norm_bal_rose_model_1)
framiB_norm_bal_rose_model_1$result.matrix       # 325


framiB_norm_bal_over_model_2 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_over, hidden = 2, threshold = 0.01)
#plot(framiB_norm_bal_over_model_2)
framiB_norm_bal_over_model_2$result.matrix       # 522

framiB_norm_bal_under_model_2 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_under, hidden = 2, threshold = 0.01)
#plot(framiB_norm_bal_under_model_2)
framiB_norm_bal_under_model_2$result.matrix       # 95

framiB_norm_bal_both_model_2 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_both, hidden = 2, threshold = 0.01)
#plot(framiB_norm_bal_both_model_2)
framiB_norm_bal_both_model_2$result.matrix       # 304

framiB_norm_bal_rose_model_2 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_rose, hidden = 2, threshold = 0.01)
#plot(framiB_norm_bal_rose_model_2)
framiB_norm_bal_rose_model_2$result.matrix       # 316


framiB_norm_bal_over_model_3 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_over, hidden = 3, threshold = 0.01)
#plot(framiB_norm_bal_over_model_3)
framiB_norm_bal_over_model_3$result.matrix       # 509

framiB_norm_bal_under_model_3 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_under, hidden = 3, threshold = 0.01)
#plot(framiB_norm_bal_under_model_3)
framiB_norm_bal_under_model_3$result.matrix       # 92

framiB_norm_bal_both_model_3 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_both, hidden = 3, threshold = 0.01)
#plot(framiB_norm_bal_both_model_3)
framiB_norm_bal_both_model_3$result.matrix       # 295

framiB_norm_bal_rose_model_3 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_rose, hidden = 3, threshold = 0.01)
#plot(framiB_norm_bal_rose_model_3)
framiB_norm_bal_rose_model_3$result.matrix       # 235


# framiB_norm_bal_over_model_13 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_over, hidden = 13, threshold = 0.1)
# #plot(framiB_norm_bal_over_model_13)
# framiB_norm_bal_over_model_13$result.matrix       # 372
# 
# framiB_norm_bal_under_model_13 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_under, hidden = 13, threshold = 0.1)
# #plot(framiB_norm_bal_under_model_13)
# framiB_norm_bal_under_model_13$result.matrix       #  58
# 
# framiB_norm_bal_both_model_13 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_both, hidden = 13, threshold = 0.1)
# #plot(framiB_norm_bal_both_model_13)
# framiB_norm_bal_both_model_13$result.matrix       #  199
# 
# framiB_norm_bal_rose_model_13 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_rose, hidden = 13, threshold = 0.1)
# #plot(framiB_norm_bal_rose_model_13)
# framiB_norm_bal_rose_model_13$result.matrix       #  198



# framiB_norm_bal_over_model_9_9 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_over, hidden = c(9,9), threshold = 0.1)
# #plot(framiB_norm_bal_over_model_9_9)
# framiB_norm_bal_over_model_9_9$result.matrix       # 332
# 
# framiB_norm_bal_under_model_9_9 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_under, hidden = c(9,9), threshold = 0.1)
# #plot(framiB_norm_bal_under_model_9_9)
# framiB_norm_bal_under_model_9_9$result.matrix       # 45
# 
# framiB_norm_bal_both_model_9_9 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_both, hidden = c(9,9), threshold = 0.1)
# #plot(framiB_norm_bal_both_model_9_9)
# framiB_norm_bal_both_model_9_9$result.matrix       # 168
# 
# framiB_norm_bal_rose_model_9_9 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_rose, hidden = c(9,9), threshold = 0.1)
# #plot(framiB_norm_bal_rose_model_9_9)
# framiB_norm_bal_rose_model_9_9$result.matrix       # 178
# 


# framiB_norm_bal_over_model_11_11 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_over, hidden = c(11,11), threshold = 0.1)
# #plot(framiB_norm_bal_over_model_13)
# framiB_norm_bal_over_model_13$result.matrix       # 373
# 
# framiB_norm_bal_under_model_11_11 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_under, hidden = c(11,11), threshold = 0.1)
# #plot(framiB_norm_bal_under_model_13)
# framiB_norm_bal_under_model_13$result.matrix      # 58
# 
# framiB_norm_bal_both_model_11_11 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_both, hidden = c(11,11), threshold = 0.1)
# #plot(framiB_norm_bal_both_model_13)
# framiB_norm_bal_both_model_13$result.matrix       # 199
# 
# framiB_norm_bal_rose_model_11_11 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiB_train_norm_bal_rose, hidden = c(11,11), threshold = 0.1)
# #plot(framiB_norm_bal_rose_model_13)
# framiB_norm_bal_rose_model_13$result.matrix       # 198







### PASO 4 - Evaluar precisión en testing set (model performance). Tomado de https://www.r-bloggers.com/neuralnet-train-and-test-neural-networks-using-r/

# Test the resulting output 

framiB_norm_model_1


# NORMALIZADO                                             #################################################

temp_framiB_test_norm <- subset(framiB_test_norm, select = c("male","age", "cigsPerDay", "prevalentStroke", "prevalentHyp", "totChol", "sysBP", "glucose"))

framiB_norm_model_1.results <- compute(framiB_norm_model_1, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#         prediction
# actual   0     1
# 0        641   1
# 1        111   11
roc.curve(actual, prediction)    # 0.544


framiB_norm_model_2.results <- compute(framiB_norm_model_2, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_2.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#         prediction
# actual   0     1
# 0        640   2
# 1        109  13
roc.curve(actual, prediction)    # 0.552

framiB_norm_model_3.results <- compute(framiB_norm_model_3, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_3.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#        prediction
# actual   0   1
#       0 638  4
#       1 109  13
roc.curve(actual, prediction)    # 0.550

framiB_norm_model_4.results <- compute(framiB_norm_model_4, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_4.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#        prediction
# actual   0   1
#       0 632  10
#       1 108  14
roc.curve(actual, prediction)    # 0.550

framiB_norm_model_5.results <- compute(framiB_norm_model_5, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_5.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#         prediction
# actual   0   1
#        0 637  5
#        1 108  14
roc.curve(actual, prediction)    # 0.553

# framiB_norm_model_10.results <- compute(framiB_norm_model_10, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_10.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #          prediction
# # actual   0   1
# #        0 610  18
# #        1 104  10
# roc.curve(actual, prediction)    # 0.530
# 
# framiB_norm_model_13.results <- compute(framiB_norm_model_13, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_13.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #          prediction
# # actual   0   1
# #       0 616  12
# #       1 108   6
# roc.curve(actual, prediction)    # 0.517

framiB_norm_model_2_1.results <- compute(framiB_norm_model_2_1, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_2_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0     1
#        0 637   5
#        1 111   11
roc.curve(actual, prediction)    # 0.541

framiB_norm_model_2_2.results <- compute(framiB_norm_model_2_2, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_2_2.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#        0 637  5
#        1 111  11
roc.curve(actual, prediction)    # 0.541

framiB_norm_model_3_3.results <- compute(framiB_norm_model_3_3, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_3_3.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0    1
#        0 624  18
#        1 102  20
roc.curve(actual, prediction)    # 0.568

framiB_norm_model_4_4.results <- compute(framiB_norm_model_4_4, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_4_4.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#        0 631  11
#        1 107   15
roc.curve(actual, prediction)    # 0.553

framiB_norm_model_5_5.results <- compute(framiB_norm_model_5_5, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_5_5.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#        0 633  9
#        1 111  11
roc.curve(actual, prediction)    # 0.538

framiB_norm_model_3_3_3.results <- compute(framiB_norm_model_3_3_3, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_3_3_3.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#        0 637  5
#        1  110  12
roc.curve(actual, prediction)    # 0.545

framiB_norm_model_5_3.results <- compute(framiB_norm_model_5_3, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_5_3.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#         prediction
# actual   0   1
#        0 629  13
#        1 98   24
roc.curve(actual, prediction)    # 0.588

framiB_norm_model_6_5.results <- compute(framiB_norm_model_6_5, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_6_5.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#        0 620  21
#        1 103  18
roc.curve(actual, prediction)    # 0.561

framiB_norm_model_6_6.results <- compute(framiB_norm_model_6_6, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_6_6.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#        0 616  26
#        1 100  22
roc.curve(actual, prediction)    # 0.570

# framiB_norm_model_7_7.results <- compute(framiB_norm_model_7_7, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_7_7.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1   2
# #      0   1 584  40   3
# #      1   0  99  15   0
# roc.curve(actual, prediction)    # 0.532
# 
# framiB_norm_model_9_9.results <- compute(framiB_norm_model_9_9, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_9_9.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #          prediction
# # actual   0   1
# #        0 597  31
# #        1 100  14
# roc.curve(actual, prediction)    # 0.537
# 
# framiB_norm_model_11_11.results <- compute(framiB_norm_model_11_11, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_11_11.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #          prediction
# # actual   0   1
# #        0 582  46
# #        1  98  16
# roc.curve(actual, prediction)    # 0.534
# 
# framiB_norm_model_9_9_9.results <- compute(framiB_norm_model_9_9_9, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_model_9_9_9.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# # prediction
# # actual  -3  -1   0   1   2
# # 0   0   1 587  39   1
# # 1   1   0  98  15   0
# roc.curve(actual, prediction)    # 0.530





# ESTANDARIZADO                                    #####################################################
summary(framiB_test_est)

temp_framiB_test_est <- subset(framiB_test_est, select = c("male","age", "cigsPerDay", "prevalentStroke", "prevalentHyp", "totChol", "sysBP", "glucose"))
summary(temp_framiB_test_est)

framiB_est_model_1.results <- compute(framiB_est_model_1, temp_framiB_test_est)
results <- data.frame(actual = framiB_test_est$TenYearCHD, prediction = framiB_est_model_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
# prediction
# actual   0    1
#        0 595  47
#        1 83   39
roc.curve(actual, prediction)    # 0.623

framiB_est_model_2.results <- compute(framiB_est_model_2, temp_framiB_test_est)
results <- data.frame(actual = framiB_test_est$TenYearCHD, prediction = framiB_est_model_2.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#        0 579  63
#        1  77  45
roc.curve(actual, prediction)    # 0.635

framiB_est_model_5.results <- compute(framiB_est_model_5, temp_framiB_test_est)
results <- data.frame(actual = framiB_test_est$TenYearCHD, prediction = framiB_est_model_5.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#        0 599  43
#        1  99  23
roc.curve(actual, prediction)    # 0.561

# framiB_est_model_13.results <- compute(framiB_est_model_13, temp_framiB_test_est)
# results <- data.frame(actual = framiB_test_est$TenYearCHD, prediction = framiB_est_model_13.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1   2
# #      0   1 563  54  10
# #      1   0  90  22   2
# roc.curve(actual, prediction)    # 0.554

framiB_est_model_5_5.results <- compute(framiB_est_model_5_5, temp_framiB_test_est)
results <- data.frame(actual = framiB_test_est$TenYearCHD, prediction = framiB_est_model_5_5.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual     0   1   2
#        0 563  79   0
#        1  94  27   1
roc.curve(actual, prediction)    # 0.554

framiB_est_model_7_7.results <- compute(framiB_est_model_7_7, temp_framiB_test_est)
results <- data.frame(actual = framiB_test_est$TenYearCHD, prediction = framiB_est_model_7_7.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#         prediction
# actual   0   1   2
#       0 575 63   4
#       1  92  29 1
roc.curve(actual, prediction)    # 0.570

# framiB_est_model_9_9.results <- compute(framiB_est_model_9_9, temp_framiB_test_est)
# results <- data.frame(actual = framiB_test_est$TenYearCHD, prediction = framiB_est_model_9_9.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #          prediction
# # actual   0   1
# #      0 566  62
# #      1  83  31
# roc.curve(actual, prediction)    # 0.587
# 
# framiB_est_model_11_11.results <- compute(framiB_est_model_11_11, temp_framiB_test_est)
# results <- data.frame(actual = framiB_test_est$TenYearCHD, prediction = framiB_est_model_11_11.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1
# #      0   2 546  80
# #      1   1  95  18
# roc.curve(actual, prediction)    # 0.513
# 
# framiB_est_model_9_9_9.results <- compute(framiB_est_model_11_11, temp_framiB_test_est)
# results <- data.frame(actual = framiB_test_est$TenYearCHD, prediction = framiB_est_model_11_11.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1
# #      0   2 546  80
# #      1   1  95  18
# roc.curve(actual, prediction)    # 0.513




# NORMALIZADO Y BALANCEADO               #########################################################################

temp_framiB_test_norm <- subset(framiB_test_norm, select = c("male","age", "cigsPerDay", "prevalentStroke", "prevalentHyp", "totChol", "sysBP", "glucose"))
head(temp_framiB_test_est)

framiB_norm_bal_over_model_1.results <- compute(framiB_norm_bal_over_model_1, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_over_model_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 426 216
#      1  36  86
roc.curve(actual, prediction)    # 0.684 **************

framiB_norm_bal_under_model_1.results <- compute(framiB_norm_bal_under_model_1, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_under_model_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 444 198
#      1  43  79
roc.curve(actual, prediction)    # 0.670

framiB_norm_bal_both_model_1.results <- compute(framiB_norm_bal_both_model_1, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_both_model_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 445 197
#      1  41  81
roc.curve(actual, prediction)    # 0.679

framiB_norm_bal_rose_model_1.results <- compute(framiB_norm_bal_rose_model_1, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_rose_model_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 461 181
#      1  44  78
roc.curve(actual, prediction)    # 0.679


framiB_norm_bal_over_model_2.results <- compute(framiB_norm_bal_over_model_2, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_over_model_2.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 411 231
#      1  38  84
roc.curve(actual, prediction)    # 0.664

framiB_norm_bal_under_model_2.results <- compute(framiB_norm_bal_under_model_2, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_under_model_2.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 424 218
#      1  41  81
roc.curve(actual, prediction)    # 0.662

framiB_norm_bal_both_model_2.results <- compute(framiB_norm_bal_both_model_2, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_both_model_2.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 433 209
#      1  50  72
roc.curve(actual, prediction)    # 0.632

framiB_norm_bal_rose_model_2.results <- compute(framiB_norm_bal_rose_model_2, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_rose_model_2.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0  626 16
#      1  98  24
roc.curve(actual, prediction)    # 0.568


framiB_norm_bal_over_model_3.results <- compute(framiB_norm_bal_over_model_3, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_over_model_3.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 387 255
#      1  36  86
roc.curve(actual, prediction)    # 0.654

framiB_norm_bal_under_model_3.results <- compute(framiB_norm_bal_under_model_3, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_under_model_3.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 428 214
#      1  52  70
roc.curve(actual, prediction)    # 0.620

framiB_norm_bal_both_model_3.results <- compute(framiB_norm_bal_both_model_3, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_both_model_3.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 452 190
#      1  64  58
roc.curve(actual, prediction)    # 0.590

framiB_norm_bal_rose_model_3.results <- compute(framiB_norm_bal_rose_model_3, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_rose_model_3.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 598  44
#      1  93  29
roc.curve(actual, prediction)    # 0.585


# framiB_norm_bal_over_model_13.results <- compute(framiB_norm_bal_over_model_13, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_over_model_13.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual   0   1
# #      0  598 44
# #      1  93  29
# roc.curve(actual, prediction)    # 0.585
# 
# framiB_norm_bal_under_model_13.results <- compute(framiB_norm_bal_under_model_13, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_under_model_13.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual   0   1   
# #      0  598 44   
# #      1  93  29   
# roc.curve(actual, prediction)    # 0.585
# 
# framiB_norm_bal_both_model_13.results <- compute(framiB_norm_bal_both_model_13, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_both_model_13.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1   2
# #          0   2 439 187   0
# #          1   0  51  62   1
# roc.curve(actual, prediction)    # 0.629
# 
# framiB_norm_bal_rose_model_13.results <- compute(framiB_norm_bal_rose_model_13, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_rose_model_13.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1
# #      0   1 587  40
# #      1   0  91  23
# roc.curve(actual, prediction)    # 0.570
# 
# framiB_norm_bal_over_model_9_9.results <- compute(framiB_norm_bal_over_model_9_9, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_over_model_9_9.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1
# #      0   2 458 168
# #      1   1  52  61
# roc.curve(actual, prediction)    # 0.631
# 
# framiB_norm_bal_under_model_9_9.results <- compute(framiB_norm_bal_under_model_9_9, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_under_model_9_9.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1   2
# #      0   8 377 243   0
# #      1   3  51  58   2
# roc.curve(actual, prediction)    # 0.568
# 
# framiB_norm_bal_both_model_9_9.results <- compute(framiB_norm_bal_both_model_9_9, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_both_model_9_9.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1   2
# #      0   1 448 174   5
# #      1   0  59  54   1
# roc.curve(actual, prediction)    # 0.598
# 
# framiB_norm_bal_rose_model_9_9.results <- compute(framiB_norm_bal_rose_model_9_9, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_rose_model_9_9.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #          prediction
# # actual   0   1
# #      0 563  65
# #      1  93  21
# roc.curve(actual, prediction)    # 0.540
# 
# 
# framiB_norm_bal_over_model_11_11.results <- compute(framiB_norm_bal_over_model_11_11, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_over_model_11_11.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1
# #      0   4 458 166
# #      1   0  76  38
# roc.curve(actual, prediction)    # 0.537
# 
# framiB_norm_bal_under_model_11_11.results <- compute(framiB_norm_bal_under_model_11_11, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_under_model_11_11.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -2  -1   0   1   2   3
# #      0   2  14 365 230  16   1
# #      1   0   0  44  65   4   1
# roc.curve(actual, prediction)    # 0.616
# 
# framiB_norm_bal_both_model_11_11.results <- compute(framiB_norm_bal_both_model_11_11, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_both_model_11_11.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1   2   3
# #      0  12 421 191   3   1
# #      1   0  62  50   2   0
# roc.curve(actual, prediction)    # 0.579
# 
# framiB_norm_bal_rose_model_11_11.results <- compute(framiB_norm_bal_rose_model_11_11, temp_framiB_test_norm)
# results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_rose_model_11_11.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# attach(roundedresultsdf)
# table(actual,prediction)
# #         prediction
# # actual  -1   0   1
# #      0   5 520 103
# #      1   2  81  31
# roc.curve(actual, prediction)    # 0.550



# STEP 5 - Improving model performance



##### CÓDIGO PARA AJUSTAR LA SENSIBILIDAD Y LA ESPECIFICIDAD

datos <- seq(0, 1, by=0.01)
rnd <- function(x) trunc(x+sign(x)*0.7)
# 0.2 <- corte en p=0.8
# 0.3 <- corte en p=0.7
# 0.7 <- corte en p=0.3
# 0.8 <- corte en p=0.2
datos1 <- rnd(datos)
df <- cbind(datos, datos1)
df


# Model_1 de base frami_B normalizada y balanceada con OVER.

framiB_norm_bal_over_model_1.results <- compute(framiB_norm_bal_over_model_1, temp_framiB_test_norm)
results <- data.frame(actual = framiB_test_norm$TenYearCHD, prediction = framiB_norm_bal_over_model_1.results$net.result)
roundedresults<-sapply(results,rnd)      # Uso función rnd en lugar de round
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 203 439
#      1  10  112
head(results, 40)
roc.curve(actual, prediction)    # 0.617

results2 <- cbind(results, roundedresultsdf)
head(results2, 10)





######################          MODELO C - IMPUTACIONES          ##########################

# RESUMEN DE BASES MODELO C
# summary(framiC_train)
# summary(framiC_test)
# summary(framiC_train_norm)
# summary(framiC_test_norm)
# summary(framiC_train_est)
# summary(framiC_test_est)
# summary(framiC_train_bal_over)
# summary(framiC_train_bal_under)
# summary(framiC_train_bal_both)
# summary(framiC_train_bal_rose)
# summary(framiC_train_norm_bal_over)
# summary(framiC_train_norm_bal_under)
# summary(framiC_train_norm_bal_both)
# summary(framiC_train_norm_bal_rose)


### PASO 3 - Entrenamiento del algoritmo

# NORMALIZADO Y BALANCEADO

framiC_norm_bal_over_model_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiC_train_norm_bal_over, hidden = 1, threshold = 0.01)
#plot(framiC_norm_bal_over_model_1)
framiC_norm_bal_over_model_1$result.matrix       # 601

framiC_norm_bal_under_model_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiC_train_norm_bal_under, hidden = 1, threshold = 0.01)
#plot(framiC_norm_bal_under_model_1)
framiC_norm_bal_under_model_1$result.matrix       # 107

framiC_norm_bal_both_model_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiC_train_norm_bal_both, hidden = 1, threshold = 0.01)
#plot(framiC_norm_bal_both_model_1)
framiC_norm_bal_both_model_1$result.matrix       # 347

framiC_norm_bal_rose_model_1 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiC_train_norm_bal_rose, hidden = 1, threshold = 0.01)
#plot(framiC_norm_bal_rose_model_1)
framiC_norm_bal_rose_model_1$result.matrix       # 545


framiC_norm_bal_over_model_5 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiC_train_norm_bal_over, hidden = 5, threshold = 0.1)
#plot(framiC_norm_bal_over_model_5)
framiC_norm_bal_over_model_5$result.matrix       # 

framiC_norm_bal_under_model_5 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiC_train_norm_bal_under, hidden = 5, threshold = 0.1)
#plot(framiC_norm_bal_under_model_5)
framiC_norm_bal_under_model_5$result.matrix       # 

framiC_norm_bal_both_model_5 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiC_train_norm_bal_both, hidden = 5, threshold = 0.1)
#plot(framiC_norm_bal_both_model_5)
framiC_norm_bal_both_model_5$result.matrix       # 

framiC_norm_bal_rose_model_5 <- neuralnet(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, data = framiC_train_norm_bal_rose, hidden = 5, threshold = 0.1)
#plot(framiC_norm_bal_rose_model_5)
framiC_norm_bal_rose_model_5$result.matrix       # 

### PASO 4 - Evaluar precisión en testing set (model performance). 

# Test the resulting output 

framiB_norm_model_1


# NORMALIZADO Y BALANCEADO               #########################################################################

temp_framiC_test_norm <- subset(framiC_test_norm, select = c("male","age", "cigsPerDay", "prevalentStroke", "prevalentHyp", "totChol", "sysBP", "glucose"))
head(temp_framiC_test_norm)

framiC_norm_bal_over_model_1.results <- compute(framiC_norm_bal_over_model_1, temp_framiC_test_norm)
results <- data.frame(actual = framiC_test_norm$TenYearCHD, prediction = framiC_norm_bal_over_model_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 455 262
#      1  40  91
roc.curve(actual, prediction)    # 0.665

framiC_norm_bal_under_model_1.results <- compute(framiC_norm_bal_under_model_1, temp_framiC_test_norm)
results <- data.frame(actual = framiC_test_norm$TenYearCHD, prediction = framiC_norm_bal_under_model_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 441 276
#      1  35 96
roc.curve(actual, prediction)    # 0.674

framiC_norm_bal_both_model_1.results <- compute(framiC_norm_bal_both_model_1, temp_framiC_test_norm)
results <- data.frame(actual = framiC_test_norm$TenYearCHD, prediction = framiC_norm_bal_both_model_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 485 232
#      1  45  86
roc.curve(actual, prediction)    # 0.666

framiC_norm_bal_rose_model_1.results <- compute(framiC_norm_bal_rose_model_1, temp_framiC_test_norm)
results <- data.frame(actual = framiC_test_norm$TenYearCHD, prediction = framiC_norm_bal_rose_model_1.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 515 202
#      1  47  84
roc.curve(actual, prediction)    # 0.680 ***************************+



framiC_norm_bal_over_model_5.results <- compute(framiC_norm_bal_over_model_5, temp_framiC_test_norm)
results <- data.frame(actual = framiC_test_norm$TenYearCHD, prediction = framiC_norm_bal_over_model_5.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 485 232
#      1  57  74
roc.curve(actual, prediction)    # 0.621

framiC_norm_bal_under_model_5.results <- compute(framiC_norm_bal_under_model_5, temp_framiC_test_norm)
results <- data.frame(actual = framiC_test_norm$TenYearCHD, prediction = framiC_norm_bal_under_model_5.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 449 268
#      1  50  81
roc.curve(actual, prediction)    # 0.622

framiC_norm_bal_both_model_5.results <- compute(framiC_norm_bal_both_model_5, temp_framiC_test_norm)
results <- data.frame(actual = framiC_test_norm$TenYearCHD, prediction = framiC_norm_bal_both_model_5.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 503 214
#      1  54  77
roc.curve(actual, prediction)    # 0.645

framiC_norm_bal_rose_model_5.results <- compute(framiC_norm_bal_rose_model_5, temp_framiC_test_norm)
results <- data.frame(actual = framiC_test_norm$TenYearCHD, prediction = framiC_norm_bal_rose_model_5.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
#          prediction
# actual   0   1
#      0 676  41
#      1  109  22
roc.curve(actual, prediction)    # 0.555






################### Archivo con todo el Global Environment (Image, .RData)
save.image(file = "/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Frami analisis para REC/VI- Frami - Red Neuronal/Frami-Red Neuronal v4.RData")
load(file = "/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Frami analisis para REC/VI- Frami - Red Neuronal/Frami-Red Neuronal v4.RData")

save.image(file = "/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Frami analisis para REC/VI- Frami - Red Neuronal/Frami-Red Neuronal v6.RData")
load(file = "/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Frami analisis para REC/VI- Frami - Red Neuronal/Frami-Red Neuronal v6.RData")

  
  
  
  