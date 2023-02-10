# ==========================================================================
# Clear and load
# ==========================================================================
cat("Clear and load!\n")

if (Sys.info()["user"] == "william.midgley") {
  setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")
} else {
  cat("Please add wd\n") 
}

#rm(list = ls())

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

cat("Don't source this, the readImage() hasn't been commented out")
m_bf1 <- readImage("brightfield-images/bf16.tif")
#m_bf1_gs <- m_bf1[,,1]

# ==========================================================================
# Let's have a look
# ==========================================================================

#dim(m_bf1)
#
grid.raster(m_bf1)
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

#m_bf1_gs <- m_bf1[,,1]
##
##persp3D(z = m_bf1_gs, theta = 120)
#
#
#d_bf1_gs <- data.frame(matrix(m_bf1_gs, ncol=1))
#
#d_bf1_gs <-
#expand.grid(1:nrow(m_bf1_gs), 1:ncol(m_bf1_gs)) %>%
#	data.frame() %>%
#	cbind(d_bf1_gs)
#
#colnames(d_bf1_gs) <- c("x", "y", "z")
#
#fit_bf1_gs <- lm(z ~ x + y, data = d_bf1_gs)
#
#
#fun <- function(i,j) {
#	fit_bf1_gs$coefficients[1] + fit_bf1_gs$coefficients[2]*i + fit_bf1_gs$coefficients[3]*j
#}
#
#rows <- 1:699
#cols <- 1:699
#
#m_plane <- outer(rows,cols,FUN=fun)

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
# Normalise the image using the plane
# ==========================================================================

normal <- m_bf1_gs

reps <- c(1:5)
rows <- 1:nrow(m_bf1)
cols <- 1:ncol(m_bf1)
fun <- function(i,j,fit) {
	fit$coefficients[1] + fit$coefficients[2]*i + fit$coefficients[3]*j
}
for (i in reps) {
	d_normal <- data.frame(matrix(normal, ncol=1))

	d_normal <-
	expand.grid(1:nrow(normal), 1:ncol(normal)) %>%
	data.frame() %>%
	cbind(d_normal)

	colnames(d_normal) <- c("x", "y", "z")

	fit_normal <- lm(z ~ x + y, data = d_normal)

	if (is.na(fit_normal$coefficients[1])) {
		fit_normal$coefficients[1] <- 0
	}

	if (is.na(fit_normal$coefficients[2])) {
		fit_normal$coefficients[2] <- 0
	}

	if (is.na(fit_normal$coefficients[3])) {
		fit_normal$coefficients[3] <- 0
	}

	plane_normal <- outer(rows,cols,fit_normal,FUN=fun)

	normal <- matrix(normal, ncol = 1) - matrix(plane_normal, ncol = 1) + 0.5

	if (i == length(reps)) {
		m_bf1_gs_normal <- normal
	}
}


d_bf1_gs_normal <- data.frame(matrix(m_bf1_gs_normal, ncol=1))

d_bf1_normal <- cbind(d_bf1_gs_normal, d_bf1_gs_normal, d_bf1_gs_normal)
colnames(d_bf1_normal) <- c("red", "green", "blue")
 
m_bf1_normal <- array(
  d_bf1_normal %>%
    dplyr::select(
      red,
      green,
      blue
    ) %>%
    unlist() %>%
    unname(),
  dim = dim(m_bf1)
)

grid.raster(m_bf1_normal)


# ==========================================================================
# Save normalised image
# ==========================================================================

save(m_bf1_normal, file ="r-objects/m_bf1_normal.RData")
