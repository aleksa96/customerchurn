# Avgust, 2019
# Telecom customer churn

# Varijable OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingTV and StreamingMovies
# zahtevaju internet konekciju i varijabla MultipleLines zahteva telefonsku uslugu tako da
# "No internet service" i "No phone service" zamenjujemo sa "No"
factorrenames <- names(churn[9:14])

churn <- churn %>%
  mutate_at(.vars=factorrenames,
            .funs=~recode_factor(., `No internet service`="No")) %>%
  mutate_at(.vars="MultipleLines",
            .funs=~recode_factor(., `No phone service`="No"))


str(churn)

# Podela dataset-a na train i test
library(caret)
set.seed(1) 
i <- createDataPartition(churn$Churn, p = 0.7, list = FALSE)
train <- churn[i, ]
test <- churn[-i, ]
dim(train)
dim(test)

# Kreiranje trainControl tako da ce svi modeli uzimati isti cross-validation(10 fold)
control <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = FALSE
)

###################################### LOGISTICKA REGRESIJA #############################################

str(churn)

# Kreiranje modela na osnovu train dataset-a
lg <- train(Churn ~ ., data = train, method="glm", trControl = control)
lg

# Kreiranje predikcija na osnovu test dataset-a
lg.pred <- predict(lg, newdata = test)
head(lg.pred)

# Kreiranje matrice konfuzije
lg.cm.1 <- table(True = test$Churn, Predicted = lg.pred)
lg.cm.1

lg.cm <- confusionMatrix(lg.pred, test[["Churn"]])
lg.accuracy <- lg.cm$overall[c(1,3,4)]
lg.cm

# Evaluacione metrike
source("Evaluacione metrike.R")
getEvaluationMetrics(lg.cm.1)

# Na osnovu evaluacionih metrike, primenjenih na test setu podataka, logisticka regresija daje accuracy 
# 81.3% gde mozemo reci da je nas model tacan ukoliko pretpostavimo da je u realnom svetu 80% tacnosti modela
# predstavlja idelan slucaj

########################### LINEARNI MODEL -  RIDGE I LASSO REGRESIJA ########################

# Train dataset koristimo kako bi nasli optimalnu vrednost alpha, mixing parametar
lmnet <- train(Churn ~ ., data = train, metric = "ROC", method = "glmnet", trControl = control, 
               preProcess = c("center","scale"))

plot(lmnet)
lmnet$bestTune$alpha
# 0.55 maksimizira AUC(area under the curve)
lmnetaccuracy <- lmnet$overall[c(1,3,4)]

# Kreiranje predikcija
lmnet.pred <- predict(lmnet, newdata = test)

# Kreiranje matrica knfuzije
lmnet.cm <- table(True = test$Churn, Predicted = lmnet.pred)
lmnet.cm

lmnet.cm.1 <- confusionMatrix(lmnet.pred, test[["Churn"]])
lmnet.accuracy <- lmnet.cm.1$overall[c(1,3,4)]
lmnet.cm.1

# Evaluacione metrike
getEvaluationMetrics(lmnet.cm)

# Lasso model tacno klasifikuje 81% od svih klijenata, sto je priblizno isto kao logisticka regresija

########################################## RANDOM FOREST ####################################

# Cross-validation na train setu bira broj varijabli koje ce dodati na model na svakoj grani
rf <- train(Churn ~ ., data=train, metric = "ROC", method = "ranger", trControl = control)
plot(rf)
# AUC vrednost je najveca za mtry = 2

# Kreiranje predikcija
rf.pred <- predict(rf, newdata = test)

# Kreiranje matrica knfuzije
rf.cm <- table(True = test$Churn, Predicted = rf.pred)
rf.cm

rf.cm.1 <- confusionMatrix(rf.pred, test[["Churn"]])
rf.accuracy <- rf.cm.1$overall[c(1,3,4)]
rf.cm.1

# Evaluacione metrike
getEvaluationMetrics(rf.cm)

# Slicno kao i prethodni modeli, RF daje priblizno jednake rezultate

########################################## KNN ####################################

# Cross-validacija kako bi se pronasla najbolja vrednost za k
knn <- train(Churn ~ ., data = train, 
                   method = "knn", trControl = control,
                   preProcess = c("center","scale"), tuneLength = 50)
knn

# Kreiranje predikcija
knn.pred <- predict(knn, newdata = test)

# Kreiranje matrica konfuzije
knn.cm <- table(True = test$Churn, Predicted = knn.pred)
knn.cm

knn.cm.1 <- confusionMatrix(knn.pred, test[["Churn"]])
knn.accuracy <- knn.cm.1$overall[c(1,3,4)]
knn.cm.1

# Evaluacione metrike
getEvaluationMetrics(knn.cm)

### Komentar:
 
########################################## SVM ####################################

grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 1))

svm <- train(Churn ~., data = train, method = "svmLinear",
                          trControl= control,
                          preProcess = c("center", "scale"),
                          tuneLength = 6,
                          tuneGrid = grid)

svm
plot(svm)

# Kreiranje predikcija
svm.pred <- predict(svm, newdata = test)

# Kreiranje matrica knfuzije
svm.cm <- table(True = test$Churn, Predicted = svm.pred)
svm.cm

svm.cm.1 <- confusionMatrix(svm.pred, test[["Churn"]])
svm.accuracy <- svm.cm.1$overall[c(1,3,4)]
svm.cm.1

# Evaluacione metrike
getEvaluationMetrics(svm.cm)

### Komentar:



################################ Uporedjivanje modela #########################################

models <- list("Logistic" = lg, "GLMnet" = lmnet, "Random Forest" = rf, "kNN" = knn, "SVM" = svm)
resamples <- resamples(models)
dotplot(resamples, metric="ROC", main = "AUC 95% CI")

summary(lg)

models.list <- c("Logistic", "GLMnet", "Random Forest", "SVM", "KNN")

accuracy.summ <- bind_rows(Logistic = lg.accuracy, GLMnet = lmnet.accuracy, RandomForest = rf.accuracy, kNN = knn.accuracy, SVM = svm.accuracy)

library(tibble)

accuracy.summ.2 <- add_column(accuracy.summ, "Model" = models.list, .before = "Accuracy")

accuracy.summ.2

install.packages("ggthemes")
library(ggthemes)

ggplot(accuracy.summ.2, aes(x = Model, y = Accuracy)) + geom_bar(stat = "identity") + 
  geom_errorbar(width = 0.2, aes(ymin = AccuracyLower, ymax = AccuracyUpper), color = "black") +
  coord_cartesian(ylim = c(0.7, 0.85)) +
  labs(y = "Accuracy %", x = "Model", title = "Model Prediction Accuracy with 95% CI") +
  theme_minimal()

summary(lg)
# Kada pogledamo vidimo da najvecu signifikantnost imaju PaymentMethod, PaperlessBilling,
# Contract, tenure i Dependents. Klijenti sa ovim atributima, najmanje napustaju kompaniju

levels(churn$Contract)
# Vidimo da su ContractTwo year i ContractOne year jaki indikatori koji pokazuju da klijenti koji imaju
# takve ugovore najmanje napustaju kompaniju, dok oni koji imaju ugovore Month-to-month jesu zapravo oni
# koji najcesce napustaju kompaniju

levels(churn$PaymentMethod)
# Klijenti koji placaju Electronic check nisu skloni napustanju kompanije

### Zakljucak:
#### Klijenti koji imaju month-to-month ugovore i koji imaju jednu telefonsku liniju i placaju metodama
#### koje ne podrazumevaju elektronske cekove najvise su skloni odlasku. Tako da bi kompanija
#### trebala da se okrene ka njima. Takodje bi trebala da obezbedi najveci akcenat na:
#### 1. Dvogodisnje ugovore
#### 2. Jednogodisnje ugovore
#### 3. DependentsYes 
#### 4. Placanje elektronskim cekovima



