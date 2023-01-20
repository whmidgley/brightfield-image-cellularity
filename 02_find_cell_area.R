# ==========================================================================
# Clear and load
# ==========================================================================
cat("Clear and load!\n")

setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")

rm(list = ls())

library(tidyverse)
library(EBImage)
#library(tiff)
library(grid)
library(gridExtra)
library(e1071)
#library(caTools)
#library(class)
options(digits=20)  # this allows R to use decimals up to 20 places
options(repr.plot.width = 15, repr.plot.height = 20)

# ==========================================================================
# Read images
# ==========================================================================

m_bf1 <- readImage("brightfield-images/bf1.tif")
m_bf1 <- array(m_bf1, dim = dim(m_bf1))
m_bf1_gs <- m_bf1[,,1]

# ==========================================================================
# Let's have a look
# ==========================================================================

dim(m_bf1)

grid.raster(m_bf1)

d_bf1 <- data.frame(
  red   = matrix(m_bf1[,,1], ncol=1),
  green = matrix(m_bf1[,,2], ncol=1),
  blue  = matrix(m_bf1[,,3], ncol=1)
  )

#nrow(unique(d_bf1))


#plot(d_bf1$red, d_bf1$green, pch = 16, col=rgb(d_bf1))


# ==========================================================================
# Normalise the image using the plane
# ==========================================================================

m_bf1_gs <- t(m_bf1[,,1])

m_bf1_gs_normal <- matrix(m_bf1_gs, ncol=1) - matrix(m_plane, ncol=1) + 0.5


# ==========================================================================
# Have a look
# ==========================================================================

#persp3D(z = m_bf1_gs_normal, theta = 120)


d_bf1_gs_normal <- data.frame(matrix(m_bf1_gs_normal, ncol=1))

d_bf1_normal <- cbind(d_bf1_gs_normal, d_bf1_gs_normal, d_bf1_gs_normal)
colnames(d_bf1_normal) <- c("red", "green", "blue")
 
m_bf1_normal <- array(
  d_bf1_normal %>%
    select(
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
# k-means clustering
# ==========================================================================

k_bf1_normal <- kmeans(d_bf1_normal, 5) # separate into 3 colours


#error <- vector()   # define an empty object to store the error for each cluster
#for (i in 1:15){
#  error[i]<-kmeans(d_bf1_normal,i)$tot.withinss 
#}

#plot(error)


#head(k_bf1_normal)
d_bf1_normal_k <- d_bf1_normal
# Attach cluster labels to our dataframe
d_bf1_normal_k$label <- k_bf1_normal$cluster

# Let's also assign the cluster labels to the cluster colours
colours1 <- data.frame(k_bf1_normal$centers, c(1:5))
colnames(colours1) <- c("red.c","green.c","blue.c","label")
colours1


d_bf1_normal_k <- inner_join(d_bf1_normal_k, colours1 ,by="label")

#head(d_bf1_normal)


d_bf1_normal_k %>%
select(red.c,green.c,blue.c) %>%
unlist() %>%
unname()



m_bf1_normal_segmented <- array(
  d_bf1_normal_k %>%
    select(
      red.c,
      green.c,
      blue.c
    ) %>%
    unlist() %>%
    unname(),
  dim = dim(m_bf1)
)

grid.raster(m_bf1_normal_segmented)


