# ==========================================================================
# Clear and load
# ==========================================================================
cat("Clear and load!\n")

setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")

#rm(list = ls())

library(tidyverse)
library(tiff)
library(grid)
library(gridExtra)
library(e1071)
library(EBImage)
#library(denoiseR)
#library(SpatialPack)
library(patchwork)
options(digits=20)  # this allows R to use decimals up to 20 places
options(repr.plot.width = 15, repr.plot.height = 20)


# ==========================================================================
# Load image
# ==========================================================================


m_gfp <- readImage("gfp-images/gfp1.tif")

img <- m_gfp

# ==========================================================================
# Edge detection
# ==========================================================================

#data(texmos2)
#
#grid.raster(texmos2)
#
#x <- imnoise(matrix(img), type = "saltnpepper", epsilon = 0.10)
#x <- matrix(x, ncol = 3)[,1] %>% matrix(ncol = 699)
#grid.raster()
#
#
#
#
#shrink_main <- adashrink(img, method = c("GSURE","SURE"),
#gamma.seq = seq(1, 5, by = 0.1), nbsim = 500, method.optim = "BFGS",
#center = "TRUE")
#
#shrink1 <- shrink_main$low.rank$u
#
##shrink1 <- matrix(shrink, ncol = 699) + 0.5
##shrink1 <- m_gfp_normal[,,1] + matrix(shrink, ncol = 699)
#
#
#shrink3 <- data.frame(matrix(shrink1, ncol=3))
#
#colnames(shrink3) <- c("red", "green", "blue")
#
#shrink3$red <- shrink3$red + 0.5 #matrix(m_gfp_normal[,,1], ncol=1)
#shrink3$green <- shrink3$green + 0.5 #matrix(m_gfp_normal[,,1], ncol=1)
#shrink3$blue <- shrink3$blue + 0.5 #matrix(m_gfp_normal[,,1], ncol=1)
#
#
#shrink4 <- array(
#  shrink3 %>%
#    select(
#          red,
#          green,
#          blue
#        ) %>%
#        unlist() %>%
#        unname(),
#  dim = dim(m_gfp)
#)
#
#
#grid.raster(shrink4)
#
#grid.raster(m_gfp_normal)



# 1. define filter for edge detection
hfilt <- matrix(c(1, 2, 3, 2, 1, 0, 0, 0, 0, 0, -1, -2, -3, -2, -1), nrow = 5) # sobel

# rotate horizontal filter to obtain vertical filter
vfilt <- t(hfilt)

# get horizontal and vertical edges
imgH <- filter2(img[,,2], hfilt, boundary="replicate")
imgV <- filter2(img[,,2], vfilt, boundary="replicate")

# combine edge pixel data to get overall edge data
hdata <- imageData(imgH)
vdata <- imageData(imgV)
edata <- sqrt(hdata^2 + vdata^2)

# transform edge data to image
imgE <- Image(edata)
#print(display(combine(img, imgH, imgV, imgE), method = "raster", all = T))

display(imgE, method = "raster", all = T)


imgE_black <- imgE

imgE_black <- round(imgE_black)

plot(imgE_black)
# 2. Enhance edges with low pass filter

hfilt <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1), nrow = 3) # low pass

# rotate horizontal filter to obtain vertical filter
vfilt <- t(hfilt)

# get horizontal and vertical edges
imgH <- filter2(imgE, hfilt, boundary="replicate")
imgV <- filter2(imgE, vfilt, boundary="replicate")

# combine edge pixel data to get overall edge data
hdata <- imageData(imgH)
vdata <- imageData(imgV)
edata <- sqrt(hdata^2 + vdata^2)


# transform edge data to image
imgE <- Image(edata)
plot(imgE)

imgE_white <- round(imgE)
#imgE_white <- abs(imgE_white-1)

plot(imgE_white)

# Low pass filter with gblur and make binary
  xb <- gblur(img[,,2], 1)
  xt <- thresh(xb, offset = 0.0001)
  grid.raster(xt) # thresh.jpg


 xm <- bwlabel(imgE)
 FS <- computeFeatures.shape(xm)
 sel <- which(FS[,"s.area"] > 1)
 xe1 <- rmObjects(xm, sel)
 xe1 <- thresh(xe1)

grid.raster(xe1)

 xm <- bwlabel(xe1)
 FS <- computeFeatures.shape(xm)
 sel <- which(FS[,"s.area"] < 1000)
 xe <- rmObjects(xm, sel)

#xe <- fillHull(xe)
#grid.raster(xe1)

xe[xe>1] <- 1

cell_area <- xe

grid.raster(cell_area)

d_cell_area <- data.frame(matrix(cell_area, ncol = 1))

cellularity <- sum(d_cell_area)*100/nrow(d_cell_area)

cat("cellularity is", round(cellularity),"%\n")


# ==========================================================================
# Calculate error
# ==========================================================================



# 1. define filter for edge detection
hfilt <- matrix(c(1, 2, 1, 0, 0, 0, -1, -2, -1), nrow = 3) # sobel

# rotate horizontal filter to obtain vertical filter
vfilt <- t(hfilt)

# get horizontal and vertical edges
imgH <- filter2(cell_area, hfilt, boundary="replicate")
imgV <- filter2(cell_area, vfilt, boundary="replicate")

# combine edge pixel data to get overall edge data
hdata <- imageData(imgH)
vdata <- imageData(imgV)
edata <- sqrt((hdata/2)^2 + (vdata*2)^2)

# transform edge data to image
imgE <- Image(edata)
#print(display(combine(img, imgH, imgV, imgE), method = "raster", all = T))

display(round(imgE), method = "raster", all = T)

d_error <- data.frame(matrix(round(imgE), ncol=1))
d_error[d_error>1] <- 1

p_error <- sum(d_error)*100/nrow(d_error)


# ==========================================================================
# Take account for error
# ==========================================================================

#bf5
31.8655/(cellularity*p_error)
cellularity*p_error*0.1075

#bf6
48.6454/(cellularity*p_error)
cellularity*p_error*0.1224

#bf7
14.64481/(cellularity*p_error)
cellularity*p_error*0.1567


cellularity*p_error*0.13