library(shiny)
library(shinyjs)
library(ggplot2)
library(GGally)
library(DT)
library(tidyverse)
library(lubridate)
library(shinycssloaders)
library(viridis)
library(visdat)
library(corrgram)
library(vcd)
library(summarytools)
library(car)
library(recipes)
library(caret)
library(rpart)
library(rpart.plot)
library(modeldata)
library(glmnet)


dat <- read.csv("Ass2Data.csv", header = TRUE, na.strings =
                  c("NA","N/A"), stringsAsFactors = TRUE)
cleandat <- dat
cleandat[cleandat == -99] <- NA

cleandat$POLITICS <- as.character(cleandat$POLITICS)
cleandat$POLITICS[is.na(cleandat$POLITICS )] <- "NONE"
cleandat$POLITICS[cleandat$POLITICS == "--"] <- "NONE"
cleandat$POLITICS <- as.factor(cleandat$POLITICS)


cleandat$HEALTHCARE_COST[is.na(cleandat$HEALTHCARE_COST)] <- 0



boxnames <- colnames(dat[,c(3:11, 13:14)])
boxchoices <- colnames(dat[,c(8, 10, 11, 14)])
namescorr <- colnames(dat[,c(3:11, 13:14)])
corrchoices <- colnames(dat[,c(3:11, 13:14)])
namesmosaic <- colnames(dat[,c(2, 12)])
valuechoices <- "DEATH_RATE"
pairsnames <- colnames(dat[-c(1)])
pairschoices <- colnames(dat[,c(12, 14)])
morecolours <- colnames(dat[,c(2, 12)])

pMiss <- function(x){ sum(is.na(x))/length(x)*100 }





