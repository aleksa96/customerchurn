# Avgust, 2019
# Telecom customer churn

# Vizuelizacija podataka

attach(churn)
summary(churn)
data <- read.csv("telecom customer churn.csv")

# Najpre, analiziramo numericke varijable i njihove raspodele:
library(ggplot)
ggplot(data = churn, aes(MonthlyCharges, color = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)
# Broj tekucih korisnika ciji mesecni racun iznosi do 25$ je znacajno veliki, 
# dok je distribucija korisnika sa mesecnim racunom preko 30$ delimicno izjednacena u odnosu na churn

# MonthlyCharges u odnosu na churn
t.test(MonthlyCharges ~ Churn, var.equal = T)
wilcox.test(MonthlyCharges~Churn)
summary(churn)
?t.test

ggplot(data = data, aes(TotalCharges, color = Churn))+
  geom_freqpoly(binwidth = 200, size = 1)
# Sa grafika uocavamo izuzetnu pozitivnu asimetricnost (dugacak rep raspodele) kada 
# je rec o varijabli TotalCharges, bez obzira da li se radi o tekucim korisnicima ili 
# onima koj su napustili kompaniju.

ggplot(data = data, aes(tenure, color = Churn))+
  geom_freqpoly(binwidth = 200, size = 1)
# Tenure u odnosu na churn
churn$tenure <- as.numeric(churn$tenure)
t.test(tenure ~ Churn, var.equal = T)
levels(churn$Churn)
summary(churn)
?t.test

# Korelacija izmedju tenure, MC i TC, dijagram rasipanja
?cor
data$tenure <- as.numeric(data$tenure)
str(data)
corrplot::corrplot.mixed(cor(cor))
cor <- as.matrix(data[,c(6,19,20)])
cor

options(repr.plot.width = 4, repr.plot.height = 3)
library(tidyverse)
# Procentualni prikaz korisnika koji su napustili kompaniju (varijabla churn)
data %>%
  group_by(Churn) %>%
  summarize(n = n()) %>%
  mutate(
    percentage = round(n / sum(n), 3),
    n = NULL) %>%
  ggplot(aes(x = Churn, y = percentage)) + geom_col(aes(fill = Churn)) +
  theme +
  geom_text(
    aes(x = Churn, y = percentage, label = paste(percentage*100, "%", sep = ""))
  )
# 26.5% korisnika u ovom skupu podataka je napustilo kompaniju

###################################################################################################

# Smanjivanje velicine grafika
options(repr.plot.width = 4, repr.plot.height = 4)

# Prikaz varijable churn u odnosu na kategoricke varijable, graficki
## Kategoricke (faktorske) varijable za prikaz
function_columns <- churn %>%
  select(
    "gender", "SeniorCitizen", "Partner", "Dependents", "PhoneService", "MultipleLines", 
    "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport",
    "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod","Churn"
  )

for (i in 1:ncol(function_columns)) {
  # Get column names so dplyr group by works
  cname <- colnames(function_columns[c(i,17)])
  # Subset data frame by variable name selected
  a <- subset(
    function_columns, !is.na(function_columns[,i]) & function_columns[,i] != "",
    select = cname
  ) %>%
    # Create percentage statistics per variable
    group_by_at(vars(cname)) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round(n / sum(n), 2)
    )
  
  # Save plot in a variable so plots can be displayed sequentialy
  p <- ggplot(
    data = a, aes_string(
      x = colnames(a[1]), y = colnames(a[4]), fill = colnames(a[1])
    )
  ) +
    # Split each graph per Churn to see influence of variable
    facet_wrap("Churn") + 
    geom_bar(stat = "identity") +
    # Make graph a bit cleaner
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = 70, hjust = 1),
      legend.position="none"
    ) +
    geom_text(
      aes(y = Percentage, label = paste0(Percentage * 100,"%"))
    ) +
    labs(
      x = colnames(a[1]), y = "Churn", title = paste("Churn i", colnames(a[1]))
    )
  
  # Display graphs
  print(p)
  # Cleanup
  rm(cname)
  rm(i)
}

#######################################################################################################

# install.packages("plot_grid")
# install.packages("Rtools")
# 
# library(plot_grid)
options(repr.plot.width = 7, repr.plot.height = 3)
data %>%
    filter(Churn == "Yes") %>%
    group_by(tenure) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round(n / sum(n), 3)
    ) %>%
    # Create plot
    ggplot(
      aes(x = tenure, y = Percentage, color = tenure)
    ) +
    stat_smooth(method = "lm", col = "red") +
    geom_point(alpha = 2/3) +
    # Clean graph visual a bit
    theme +
    labs(
      x = "Tenure", y = "Churn (%)"
    )

ggplot(data = churn, aes(y = tenure, x = Churn, color = Churn)) +
  theme +
  geom_boxplot()
# Prikaz outlier-a za tenure varijablu odnosu na churn. Mogu se uociti 3 outlier-a kada je churn yes.

data %>%
  filter(Churn == "Yes") %>%
  group_by(MonthlyCharges) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    Percentage = round(n / sum(n), 3)
  ) %>%
  # Create plot
  ggplot(
    aes(x = MonthlyCharges, y = Percentage, color = MonthlyCharges)
  ) +
  stat_smooth(method = "lm", col = "red") +
  geom_point(alpha = 2/3) +
  # Clean graph visual a bit
  theme +
  labs(
    x = "MonthlyCharges", y = "Churn (%)"
  )

ggplot(data = churn, aes(y = MonthlyCharges, x = Churn, color = Churn)) +
  theme +
  geom_boxplot()
# MonthlyCharges nema outlier-a

data %>%
  filter(Churn == "Yes") %>%
  group_by(TotalCharges) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    Percentage = round(n / sum(n), 3)
  ) %>%
  # Create plot
  ggplot(
    aes(x = TotalCharges, y = Percentage, color = TotalCharges)
  ) +
  stat_smooth(method = "lm", col = "red") +
  geom_point(alpha = 2/3) +
  # Clean graph visual a bit
  theme +
  labs(
    x = "TotalCharges", y = "Churn (%)"
  )

ggplot(data = data, aes(y = TotalCharges, x = Churn, color = Churn)) +
  theme +
  geom_boxplot()
# Kada je churn yes, TotalCharges ima prilicno veliki broj outlier-a. Obratiti paznju.
str(churn)

#########################################################################################

