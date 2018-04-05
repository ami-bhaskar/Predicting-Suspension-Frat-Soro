rm(list=ls())

install.packages("gee")

library("geepack")
library("knitr")
library("lattice")
library("nlme")
library("ggplot2")
library("gee")

frat.data <- Final_Frat_data4

head(frat.data)

xyplot(Members ~ year, groups= name, data=frat.data, type = c("g", "l", "p"))

frat.data$`Current Status`  <- as.factor(frat.data$`Current Status`)
frat.data$`Council Recoded`  <- as.factor(frat.data$`Council Recoded`)
frat.data$'gender'  <- as.factor(frat.data$`gender`)
frat.data$'I'  <- as.integer(frat.data$`I`)
frat.data$'II'  <- as.integer(frat.data$`II`)
frat.data$'III'  <- as.integer(frat.data$`III`)
frat.data$'IV'  <- as.integer(frat.data$`IV`)
frat.data$'V'  <- as.integer(frat.data$`V`)
frat.data$'VI'  <- as.integer(frat.data$`VI`)
frat.data$'VII'  <- as.integer(frat.data$`VII`)
frat.data$'VIII'  <- as.integer(frat.data$`VIII`)
frat.data$'IX'  <- as.integer(frat.data$`IX`)
frat.data$'X'  <- as.integer(frat.data$`X`)

cor(frat.data)
cov(frat.data)

fform <- Failure ~ GPA + gender + TotComp + Members

gee1 <- geeglm(fform, data=frat.data, id=name, family= binomial, zcor=Null, corstr="exchangeable")
geefrat <- gee(fform, id=name,data=frat.data, family=binomial, corstr="exchangeable")
summary(geefrat)
summary(frat.gee)
