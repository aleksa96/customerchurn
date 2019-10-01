# Avgust, 2019
# Telecom customer churn

# Ucitavanje dataset-a
churn <- read.csv(file = "telecom customer churn.csv")

# Pregled dataset-a kao i tipova varijabli
summary(churn) 
str(churn)

# Varijabla TotalCharges sadrzi 11 NA vrednosti koje je potrebno otkloniti
churn[is.na(churn$TotalCharges),1:6]
churn <- churn[complete.cases(churn),]
dim(churn)

# Varijablu SeniorCitizen je potrebno faktorisati buduci da ima vrednosti 0 ili 1
summary(churn$SeniorCitizen)
str(churn$SeniorCitizen)
unique(churn$SeniorCitizen)
churn$SeniorCitizen <- as.factor(ifelse(test = churn$SeniorCitizen == 1, yes = "Yes", no = "No"))
str(churn)
table(churn$SeniorCitizen)

# Cuvanje trenutnog dataset-a, kao prve, inicijalne verzije skupa podataka
saveRDS(object = churn, file = "telecom customer churn, v1.1.RData")

# Priprema podataka za modele klasifikacije
## Monthly charges i total charges su visoko korelisani tako da jednu izbacujemo(TotalCharges)
## Uklanjanje customerID koja nije relevantna za analiziranje

churn$customerID <- NULL
churn <- churn[,-c(19)]

################################################################################################

# Instaliranje i ucitavanje potrebnih biblioteka
install.packages("tidyverse")
install.packages("cowplot")
install.packages("caret")
install.packages("rpart")
install.packages("ROCR")
install.packages("rpart.plot")
library(tidyverse)
library(cowplot)
library(caret)
library(rpart)
library(ROCR)
library(rpart.plot)
install.packages("ggplot")
library(ggplot2)

## ggplot theme
theme <- theme(
  axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  legend.position="none" 
)

summary(churn)
str(churn)
saveRDS(object = churn, "telecom customer churn, selected features, v1.1.RData")
