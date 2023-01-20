# ==========================================================================
# Clear and load
# ==========================================================================
cat("Clear and load!\n")

setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")

rm(list = ls())

library(tidyverse)
library(EBImage)
library(grid)
library(gridExtra)
library(plot3D)
options(digits=20)  # this allows R to use decimals up to 20 places
options(repr.plot.width = 15, repr.plot.height = 20)


# ==========================================================================
# Read images
# ==========================================================================

m_bf1 <- readImage("brightfield-images/bf1.tif")


# ==========================================================================
# Let's have a look
# ==========================================================================

#dim(m_bf1)
#
#grid.raster(m_bf1)
#
#d_bf1 <- data.frame(
#  red   = matrix(m_bf1[,,1], ncol=1),
#  green = matrix(m_bf1[,,2], ncol=1),
#  blue  = matrix(m_bf1[,,3], ncol=1)
#  )

#nrow(unique(d_bf1))


#plot(d_bf1$red, d_bf1$green, pch = 16, col=rgb(d_bf1))

# ==========================================================================
# Check greyscale
# ==========================================================================

#for (i in 1:nrow(d_bf1)) {
#	if (d_bf1[i,1] != d_bf1[i,2] | d_bf1[i,1] != d_bf1[i,3] | d_bf1[i,2] != d_bf1[i,3]) {
#		cat("colour!\n")
#	}
#}
# no response so we can just use 

# ==========================================================================
# Plot 3D graph
# ==========================================================================

m_bf1_gs <- m_bf1[,,1]
#
persp3D(z = m_bf1_gs, theta = 120)


d_bf1_gs <- data.frame(matrix(m_bf1_gs, ncol=1))

d_bf1_gs <-
expand.grid(1:nrow(m_bf1_gs), 1:ncol(m_bf1_gs)) %>%
	data.frame() %>%
	cbind(d_bf1_gs)

colnames(d_bf1_gs) <- c("x", "y", "z")

fit_bf1_gs <- lm(z ~ x + y, data = d_bf1_gs)


fun <- function(i,j) {
	fit_bf1_gs$coefficients[1] + fit_bf1_gs$coefficients[2]*i + fit_bf1_gs$coefficients[3]*j
}

rows <- 1:699
cols <- 1:699

m_plane <- outer(rows,cols,FUN=fun)

# ==========================================================================
# Let's have a look at that plane
# ==========================================================================

#persp3D(z = m_plane, theta = 120)
#
#d_plane <- data.frame(matrix(m_plane, ncol=1))
#
#d_plane_clrd <- cbind(d_plane, d_plane, d_plane)
#colnames(d_plane_clrd) <- c("red", "green", "blue")
# 
#m_plane_clrd <- array(
#  d_plane_clrd %>%
#    select(
#      red,
#      green,
#      blue
#    ) %>%
#    unlist() %>%
#    unname(),
#  dim = dim(m_bf1)
#)
#
#grid.raster(m_plane_clrd)

# ==========================================================================
# Save plane
# ==========================================================================

save(m_plane, file ="qsave/m_plane.RData")