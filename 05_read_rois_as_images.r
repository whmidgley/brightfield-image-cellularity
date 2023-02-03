cat("Clear and load!\n")

setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")

#rm(list = ls())

library(tidyverse)
library(EBImage)
#library(tiff)
library(png)
library(grid)
library(glue)
library(gridExtra)
library(e1071)
library(RImageJROI)
library(raster)
#library(caTools)
#library(class)
options(digits=20)  # this allows R to use decimals up to 20 places
options(repr.plot.width = 15, repr.plot.height = 20)

# ==========================================================================
# Read images
# ==========================================================================

load("r-objects/m_bf1_normal.RData")

m_bf <- readImage("brightfield-images/bf5.tif")

m_roi <- readImage("roi_plot.tiff")

m_roi <- round(m_roi[,,1])

m_roi <- abs(m_roi-1)

plot(m_roi)

(sum(m_roi)*100)/(nrow(m_roi)*ncol(m_roi))