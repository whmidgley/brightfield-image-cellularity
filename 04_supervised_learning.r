cat("Clear and load!\n")

setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")

#rm(list = ls())

library(tidyverse)
library(EBImage)
#library(tiff)
library(png)
library(grid)
library(gridExtra)
library(e1071)
library(RImageJROI)
#library(caTools)
#library(class)
options(digits=20)  # this allows R to use decimals up to 20 places
options(repr.plot.width = 15, repr.plot.height = 20)

# ==========================================================================
# Read images
# ==========================================================================

load("r-objects/m_bf1_normal.RData")

m_bf <- readImage("brightfield-images/bf5.tif")
#m_bf <- array(m_bf, dim = dim(m_bf))
#m_bf_gs <- m_bf[,,1]
m_bf_gs_normal <- m_bf_normal[,,1]

roi <- read.ijroi("brightfield-images/roi6.ROI")
