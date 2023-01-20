cat("Clear and load!\n")

setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")

#rm(list = ls())

library(tidyverse)
library(tiff)
library(grid)
library(gridExtra)
library(e1071)
library(EBImage)
library(image.CannyEdges)
#library(denoiseR)
#library(SpatialPack)
library(patchwork)
options(digits=20)  # this allows R to use decimals up to 20 places
options(repr.plot.width = 15, repr.plot.height = 20)


# ==========================================================================
# Load image
# ==========================================================================

load("r-objects/m_bf1_normal.RData")
#m_bf1 <- readImage("brightfield-images/bf1.tif")

m_bf1_normal[,,1] <- t(m_bf1_normal[,,1])
m_bf1_normal[,,2] <- t(m_bf1_normal[,,2])
m_bf1_normal[,,3] <- t(m_bf1_normal[,,3])

img <- m_bf1_normal

# ==========================================================================
# Give it a go
# ==========================================================================
canny_edges

# 1. define filter for edge detection
hfilt <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 5)

vfilt <- t(hfilt)

# get horizontal and vertical edges
imgH <- filter2(img[,,1], hfilt, boundary="replicate")
imgV <- filter2(img[,,1], vfilt, boundary="replicate")

# combine edge pixel data to get overall edge data
hdata <- imageData(imgH)
vdata <- imageData(imgV)
edata <- sqrt((hdata)^2 + (vdata)^2)

# transform edge data to image
imgE <- Image(edata)
#print(display(combine(img, imgH, imgV, imgE), method = "raster", all = T))

display(imgH/10, method = "raster", all = T)


grid.raster(imgH)
grid.raster(imgV)
grid.raster(imgE)

canny <- image_canny_edge_detector((imgH/10)*255)

plot(canny)

canny_edges <- matrix(canny$edges, ncol = ncol(canny$edges))

canny_edges <- canny_edges/255

grid.raster(canny_edges)


xb <- gblur(canny_edges, 2)
  xt <- thresh(xb, offset = 0.0001)
  grid.raster(xt) # thresh.jpg



 xm <- bwlabel(xt)
 FS <- computeFeatures.shape(xm)
 sel <- which(FS[,"s.area"] > 10)
 xe1 <- rmObjects(xm, sel)
 xe1 <- thresh(xe1)

grid.raster(xe1)

 xm <- bwlabel(abs(xe1-1))
 FS <- computeFeatures.shape(xm)
 sel <- which(FS[,"s.area"] < 10)
 xe <- rmObjects(xm, sel)

#xe <- fillHull(xe)
#grid.raster(xe1)

xe[xe>1] <- 1

cell_area <- xe

grid.raster(cell_area)

d_cell_area <- data.frame(matrix(cell_area, ncol = 1))

cellularity <- sum(d_cell_area)*100/nrow(d_cell_area)

cat("cellularity is", round(cellularity),"%\n")

