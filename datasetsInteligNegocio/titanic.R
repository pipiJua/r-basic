install.packages("mboost")
install.packages("TH.data")
library(grid)
library(gridExtra)
library(RODBC)

#setear el directorio de trabajo  
setwd("C:/Users/joaju/OneDrive/Documentos/datasets")

#################################
# titanic
install.packages("titanic")
library(titanic)
str(Titanic)
df <- as.data.frame(Titanic)
head(df)

str(titanic.raw)
summary(titanic.raw)

install.packages("arules")
library(arules)
# find association rules with default settings
rules.all <- apriori(titanic.raw)
rules.all
inspect(rules.all)

## series de tiempo... build an autoregressive integrated moving average (ARIMA) model 
a <- ts(1:30, frequency=12, start=c(2011,3))
print(a)
str(a)
attributes(a)

#Time Series Decomposition is to decompose a time series into trend, seasonal, 
# cyclical, and irregular components