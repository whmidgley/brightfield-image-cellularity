# ==========================================================================
# Clear and load
# ==========================================================================
cat("Clear and load!\n")

setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")

rm(list = ls())

library(tidyverse)
library(tiff)
library(grid)
library(gridExtra)
library(e1071)
library(EBImage)
library(denoiseR)
library(SpatialPack)
options(digits=20)  # this allows R to use decimals up to 20 places
options(repr.plot.width = 15, repr.plot.height = 20)


# ==========================================================================
# Load image
# ==========================================================================

load("qsave/m_bf1_normal.RData")
m_bf1 <- readImage("brightfield-images/bf1.tif")

img <- m_bf1_normal
grid.raster(img)
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
##shrink1 <- m_bf1_normal[,,1] + matrix(shrink, ncol = 699)
#
#
#shrink3 <- data.frame(matrix(shrink1, ncol=3))
#
#colnames(shrink3) <- c("red", "green", "blue")
#
#shrink3$red <- shrink3$red + 0.5 #matrix(m_bf1_normal[,,1], ncol=1)
#shrink3$green <- shrink3$green + 0.5 #matrix(m_bf1_normal[,,1], ncol=1)
#shrink3$blue <- shrink3$blue + 0.5 #matrix(m_bf1_normal[,,1], ncol=1)
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
#  dim = dim(m_bf1)
#)
#
#
#grid.raster(shrink4)
#
#grid.raster(m_bf1_normal)



# 1. define filter for edge detection
hfilt <- matrix(c(1, 2, 1, 0, 0, 0, -1, -2, -1), nrow = 3) # sobel

# rotate horizontal filter to obtain vertical filter
vfilt <- t(hfilt)

# get horizontal and vertical edges
imgH <- filter2(t(img[,,1]), hfilt, boundary="replicate")
imgV <- filter2(t(img[,,1]), vfilt, boundary="replicate")

# combine edge pixel data to get overall edge data
hdata <- imageData(imgH)
vdata <- imageData(imgV)
edata <- sqrt(hdata^2 + vdata^2)

# transform edge data to image
imgE <- Image(edata)
print(display(combine(img, imgH, imgV, imgE), method = "raster", all = T))

display(imgV, method = "raster", all = T)

display(imgE, method = "raster", all = T)


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


# Low pass filter with gblur and make binary
  xb <- gblur(img, 3)
  xt <- thresh(xb, offset = 0.0001)
  grid.raster(xt) # thresh.jpg


  xm <- bwlabel(xt[,,1])
  FS <- computeFeatures.shape(xm)
  sel <- which(FS[,"s.area"] > 20000)
  xe <- rmObjects(xm, sel)

# Make binary again and plot
  xe <- thresh(xe)
  grid.raster(xe) # trimmed.jpg