######################################
#                                    #
#        PREPARACIÓN DATOS           #
#                                    #
######################################

# Por Juanjo Beunza
# Agradecimiento a Brett Lantz y Hadley Wickham por su inspiración y código
# Agradecimiento a Enrique Puertas por el marco conceptual, especialmente el del balanceo de "labels", todo mérito suyo. 


## Limpieza Environment
rm(list=ls())

## PASO 1 - Subir la base de datos
#getwd()
#setwd("/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Frami analisis para REC")
#getwd()

## Upload dataset
frami = read.csv("/Users/juanjosebeunzanuin/Documents/ALGORITMOS/Machine Learning Salud-UEM/Open Access Datasets/Kaggle/Framingham/framingham.csv")
# str(frami)


## PASO 2 - Preparación de la base de datos


## Selección de variables "stepwise"
#library(MASS)
# Modelo completo 
#frami1 <- na.omit(frami)          # Elimino missing para que pueda funcionar la función stepAIC.
#full.model <- glm(TenYearCHD ~ ., family = binomial(link = "logit"), data = frami1)
#coef(full.model)
#summary(full.model)
# Modelo "Stepwise Regression"
#step.model <- stepAIC(full.model, direction = "both", trace = FALSE, )
#summary(step.model)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -8.745885   0.522456 -16.740  < 2e-16 ***
# male             0.553297   0.107018   5.170 2.34e-07 ***
# age              0.065411   0.006442  10.153  < 2e-16 ***
# cigsPerDay       0.019579   0.004181   4.683 2.82e-06 ***
# prevalentStroke  0.751698   0.483585   1.554   0.1201    
# prevalentHyp     0.225762   0.135085   1.671   0.0947 .  
# totChol          0.002257   0.001122   2.011   0.0443 *  
# sysBP            0.014218   0.002857   4.976 6.50e-07 ***
# glucose          0.007317   0.001673   4.374 1.22e-05 ***

# Eliminar las variables no empleadas
#names(frami)
# [1] "male"            "age"             "education"       "currentSmoker"  
# [5] "cigsPerDay"      "BPMeds"          "prevalentStroke" "prevalentHyp"   
# [9] "diabetes"        "totChol"         "sysBP"           "diaBP"          
# [13] "BMI"             "heartRate"       "glucose"         "TenYearCHD" 
library(dplyr)
frami <- dplyr::select(frami, c(-education, -currentSmoker, -BPMeds, -diabetes, -diaBP, -BMI, -heartRate))
#names(frami)




#########################          A- MODELO A                           (bruto, según llega del repositorio Kaggle)
framiA <- frami
# Dividir la base en train y test
#nrow(framiA)
# 4240 observaciones
set.seed(1234)
train_sample <- sample(4240, 4240*0.8)
#str(train_sample)
framiA_train <- framiA[train_sample, ]
framiA_test <- framiA[-train_sample, ]
# prop.table(table(framiA_train$TenYearCHD))
# prop.table(table(framiA_test$TenYearCHD))
# nrow(framiA_train)
# nrow(framiA_test)






########################         B- MODELO B                  (Análisis del caso completo)
framiB <- frami
# Eliminando observaciones con valoresmissing
library(tidyr)
colSums(is.na(framiB))
# male             age      cigsPerDay prevalentStroke    prevalentHyp         totChol           sysBP         glucose      TenYearCHD 
# 0               0              29               0               0              50               0             388               0 
framiB <- framiB %>% drop_na()
table(frami$male)
nrow(framiB)
# table(framiB$male)
# str(framiB)
# summary(frami)


# Dividir la base en train y test
# nrow(framiB)
# 3817 observaciones
set.seed(1234)
train_sample <- sample(3817, 3817*0.8)
#str(train_sample)
framiB_train <- framiB[train_sample, ]
framiB_test <- framiB[-train_sample, ]
# prop.table(table(framiB_train$TenYearCHD))
# prop.table(table(framiB_test$TenYearCHD))
# nrow(framiB_train)
# nrow(framiB_test)


## HOMOGENEIZAR MEDIA Y RANGO

# Normalization (media = 0; rango variable). Better for normal distribution.
normalizar <- function(x) {
  return((x - mean(x)) / sd(x))
} 
framiB_train_norm <- as.data.frame(lapply(framiB_train[-9], normalizar))
framiB_train_norm$TenYearCHD <- framiB_train$TenYearCHD
summary(framiB_train_norm)

framiB_test_norm <- as.data.frame(lapply(framiB_test[-9], normalizar))
framiB_test_norm$TenYearCHD <- framiB_test$TenYearCHD
summary(framiB_test_norm)

# summary(framiB_train)
# summary(framiB_test)


# Estandarización o Max-min Normalization (rango de 0 a 1; media =/ 0). Better for non-normal distribution.
estandarizar <- function(x) {
  return((x - min(x)) / (max(x)-min(x)))
}
framiB_train_est <- as.data.frame(lapply(framiB_train[-9], estandarizar))
framiB_train_est$TenYearCHD <- framiB_train$TenYearCHD
summary(framiB_train_est)

framiB_test_est <- as.data.frame(lapply(framiB_test[-9], estandarizar))
framiB_test_est$TenYearCHD <- framiB_test$TenYearCHD
summary(framiB_test_est)

# summary(framiB_train)
# summary(framiB_test)



## BALANCEAR la base training con los 4 métodos disponibles sobre la base original y sobre la normalizada

# Balanceo sobre Base original
table(framiB_train$TenYearCHD)
# 0     1 
# 2584  469 
# I- Oversampling
library(ROSE)
framiB_train_bal_over <- ovun.sample(TenYearCHD~., data = framiB_train, method = "over", N=5168)$data #2584*2=5168
table(framiB_train_bal_over$TenYearCHD)
# 0    1 
# 2584 2584
# II- Undersampling             (undersampling is done without replacement)
framiB_train_bal_under <- ovun.sample(TenYearCHD~., data = framiB_train, method = "under", N = 938, seed = 1)$data #469*2=938
table(framiB_train_bal_under$TenYearCHD)
# 0  1 
# 469 469
# III- Both over and under sampling
framiB_train_bal_both <- ovun.sample(TenYearCHD ~ ., data = framiB_train, method = "both", p=0.5, N=3053, seed=1)$data
table(framiB_train_bal_both$TenYearCHD)
# 0    1 
# 1593 1460
# IV- ROSE: Mixed method with over and under sampling
framiB_train_bal_rose <- ROSE(TenYearCHD ~ ., data = framiB_train, seed = 1)$data
table(framiB_train_bal_rose$TenYearCHD)
# 0    1 
# 1593 1460


# Balanceo sobre base normalizada
table(framiB_train_norm$TenYearCHD)
# 0     1 
# 2584  469 
# I- Oversampling
framiB_train_norm_bal_over <- ovun.sample(TenYearCHD~., data = framiB_train_norm, method = "over", N=5168)$data #2584*2=5168
# II- Undersampling             (undersampling is done without replacement)
framiB_train_norm_bal_under <- ovun.sample(TenYearCHD~., data = framiB_train_norm, method = "under", N = 938, seed = 1)$data #469*2=938
# III- Both over and under sampling
framiB_train_norm_bal_both <- ovun.sample(TenYearCHD ~ ., data = framiB_train_norm, method = "both", p=0.5, N=3053, seed=1)$data
# IV- ROSE: Mixed method with over and under sampling
framiB_train_norm_bal_rose <- ROSE(TenYearCHD ~ ., data = framiB_train_norm, seed = 1)$data

# Balanceo sobre base estandarizada
table(framiB_train_est$TenYearCHD)
# 0     1 
# 2584  469 
# I- Oversampling
framiB_train_est_bal_over <- ovun.sample(TenYearCHD~., data = framiB_train_est, method = "over", N=5168)$data #2584*2=5168
# II- Undersampling             (undersampling is done without replacement)
framiB_train_est_bal_under <- ovun.sample(TenYearCHD~., data = framiB_train_est, method = "under", N = 938, seed = 1)$data #469*2=938
# III- Both over and under sampling
framiB_train_est_bal_both <- ovun.sample(TenYearCHD ~ ., data = framiB_train_est, method = "both", p=0.5, N=3053, seed=1)$data
# IV- ROSE: Mixed method with over and under sampling
framiB_train_est_bal_rose <- ROSE(TenYearCHD ~ ., data = framiB_train_est, seed = 1)$data



# RESUMEN DE BASES MODELO B
# summary(framiB_train)
# summary(framiB_test)
# summary(framiB_train_norm)
# summary(framiB_test_norm)
# summary(framiB_train_est)
# summary(framiB_test_est)
# summary(framiB_train_bal_over)
# summary(framiB_train_bal_under)
# summary(framiB_train_bal_both)
# summary(framiB_train_bal_rose)
# summary(framiB_train_norm_bal_over)
# summary(framiB_train_norm_bal_under)
# summary(framiB_train_norm_bal_both)
# summary(framiB_train_norm_bal_rose)



#########################          C- MODELO C                           (Imputando)
framiC <- frami

colSums(is.na(framiC))
# male             age      cigsPerDay prevalentStroke    prevalentHyp         totChol           sysBP         glucose      TenYearCHD 
# 0               0              29               0               0              50               0             388               0 

# Imputamos las medias de las variables continuas
framiC$cigsPerDay[is.na(framiC$cigsPerDay)] <- mean(framiC$cigsPerDay, na.rm = TRUE)
framiC$totChol[is.na(framiC$totChol)] <- mean(framiC$totChol, na.rm = TRUE)
framiC$glucose[is.na(framiC$glucose)] <- mean(framiC$glucose, na.rm = TRUE)
colSums(is.na(framiC))
# male             age      cigsPerDay prevalentStroke    prevalentHyp         totChol           sysBP         glucose      TenYearCHD 
# 0               0               0               0               0               0               0               0               0 


# Dividir la base en train y test
# nrow(framiC)
# 4240 observaciones
set.seed(1234)
train_sample <- sample(4240, 4240*0.8)
# str(train_sample)
framiC_train <- framiC[train_sample, ]
framiC_test <- framiC[-train_sample, ]
# prop.table(table(framiC_train$TenYearCHD))
# prop.table(table(framiC_test$TenYearCHD))
# nrow(framiC_train)
# nrow(framiC_test)



## HOMOGENEIZAR MEDIA Y RANGO

# Normalization (media = 0; rango variable). Better for normal distribution.
normalizar <- function(x) {
  return((x - mean(x)) / sd(x))
} 
framiC_train_norm <- as.data.frame(lapply(framiC_train[-9], normalizar))
framiC_train_norm$TenYearCHD <- framiC_train$TenYearCHD
summary(framiC_train_norm)

framiC_test_norm <- as.data.frame(lapply(framiC_test[-9], normalizar))
framiC_test_norm$TenYearCHD <- framiC_test$TenYearCHD
summary(framiC_test_norm)

#summary(framiC_train)
#summary(framiC_test)


# Estandarización o Max-min Normalization (rango de 0 a 1; media =/ 0). Better for non-normal distribution.
estandarizar <- function(x) {
  return((x - min(x)) / (max(x)-min(x)))
}
framiC_train_est <- as.data.frame(lapply(framiC_train[-9], estandarizar))
framiC_train_est$TenYearCHD <- framiC_train$TenYearCHD
summary(framiC_train_est)

framiC_test_est <- as.data.frame(lapply(framiC_test[-9], estandarizar))
framiC_test_est$TenYearCHD <- framiC_test$TenYearCHD
summary(framiC_test_est)

#summary(framiC_train)
#summary(framiC_test)





## BALANCEAR la base training con los 4 métodos disponibles sobre la base original y sobre la normalizada

# Balanceo sobre Base original
table(framiC_train$TenYearCHD)
# 0     1 
# 2874  518
nrow(framiC_train)
# [1] 3392

# I- Oversampling
library(ROSE)
framiC_train_bal_over <- ovun.sample(TenYearCHD~., data = framiC_train, method = "over", N=5748)$data #2874*2=5748
table(framiC_train_bal_over$TenYearCHD)
# 0    1 
# 2874 2874
# II- Undersampling             (undersampling is done without replacement)
framiC_train_bal_under <- ovun.sample(TenYearCHD~., data = framiC_train, method = "under", N = 1036, seed = 1)$data #518*2=1036
table(framiC_train_bal_under$TenYearCHD)
# 0   1 
# 518 518
# III- Both over and under sampling
framiC_train_bal_both <- ovun.sample(TenYearCHD ~ ., data = framiC_train, method = "both", p=0.5, N=3392, seed=1)$data
table(framiC_train_bal_both$TenYearCHD)
# 0    1 
# 1751 1641
# IV- ROSE: Mixed method with over and under sampling
framiC_train_bal_rose <- ROSE(TenYearCHD ~ ., data = framiC_train, seed = 1)$data
table(framiC_train_bal_rose$TenYearCHD)
# 0    1 
# 1751 1641


# Balanceo sobre base normalizada
table(framiC_train_norm$TenYearCHD)
# 0     1 
# 2874  518 
# I- Oversampling
framiC_train_norm_bal_over <- ovun.sample(TenYearCHD~., data = framiC_train_norm, method = "over", N=5748)$data #2874*2=5748
# II- Undersampling             (undersampling is done without replacement)
framiC_train_norm_bal_under <- ovun.sample(TenYearCHD~., data = framiC_train_norm, method = "under", N = 1036, seed = 1)$data #518*2=1036
# III- Both over and under sampling
framiC_train_norm_bal_both <- ovun.sample(TenYearCHD ~ ., data = framiC_train_norm, method = "both", p=0.5, N=3392, seed=1)$data
# IV- ROSE: Mixed method with over and under sampling
framiC_train_norm_bal_rose <- ROSE(TenYearCHD ~ ., data = framiC_train_norm, seed = 1)$data



# Balanceo sobre base estandarizada
table(framiC_train_est$TenYearCHD)
# 0     1 
# 2879  513 
# I- Oversampling
framiC_train_est_bal_over <- ovun.sample(TenYearCHD~., data = framiC_train_est, method = "over", N=5758)$data #2879*2=5758
# II- Undersampling             (undersampling is done without replacement)
framiC_train_est_bal_under <- ovun.sample(TenYearCHD~., data = framiC_train_est, method = "under", N = 938, seed = 1)$data #513*2=1026
# III- Both over and under sampling
framiC_train_est_bal_both <- ovun.sample(TenYearCHD ~ ., data = framiC_train_est, method = "both", p=0.5, N=3392, seed=1)$data
# IV- ROSE: Mixed method with over and under sampling
framiC_train_est_bal_rose <- ROSE(TenYearCHD ~ ., data = framiC_train_est, seed = 1)$data


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

