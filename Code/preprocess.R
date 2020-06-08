library(ggplot2)
library(ggthemes)
library(corrplot)
library(reshape2)
library(tidyverse)
library(dplyr)
library(randomForest)
library(MASS)
library(naniar)
library(GGally)
library(e1071)
data=read.csv("trial.csv")

data=na.omit(data)

dim(data)
test_id = sample(1:nrow(data), 50, replace = F)
data_test = data[test_id,]
data = data[-test_id,]

library(ggparallel)

d = data[,c(-2)]
d$Risk = as.factor(d$Risk)


colnames(d) <- c(as.character(1:16),'Risk')

ggparcoord(d, 1:(ncol(d)-1), ncol(d), order = 'allClass')

temp = aov(PARA_A~Risk, data = data)           
summary(temp)


library(xtable)

xtable(cbind(sd1, sd2))
data_zero

test_id = sample(1:nrow(data), 100)

result_svm = svm(Risk~., data = d[-test_id,])

predict(result_svm, d[test_id,])
mean(d$Risk[test_id] != predict(result_svm, d[test_id,]))
