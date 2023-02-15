cat("Clear and load!\n")

setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")

#rm(list = ls())

library(tidyverse)
library(EBImage)
library(grid)
library(glue)
library(gridExtra)
library(e1071)
library(RImageJROI)
library(raster)
library(keras)
options(digits=20)  # this allows R to use decimals up to 20 places
options(repr.plot.width = 15, repr.plot.height = 20)

c(c(xtrain, ytrain), c(xtest, ytest)) %<-% dataset_mnist()
xtrain = xtrain/255
xtest = xtest/255