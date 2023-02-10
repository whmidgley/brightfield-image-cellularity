# ==========================================================================
# Clear and load
# ==========================================================================
cat("Clear and load!\n")

setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")

#rm(list = ls())

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

load("r-objects/m_bf1_normal.RData")
load("r-objects/human_cellularity.RData")

#m_bf1 <- readImage("brightfield-images/bf1.tif")
#m_bf1 <- array(m_bf1, dim = dim(m_bf1))
#m_bf1_gs <- m_bf1[,,1]


load("r-objects/m_bf1_blur.RData")

m_bf1 <- m_bf1_blur
m_bf1_normal <- m_bf1_blur

m_bf1_gs_normal <- m_bf1_normal


# ==========================================================================
# Let's have a look
# ==========================================================================
#
#dim(m_bf1)
#
#grid.raster(m_bf1)
#
#d_bf1 <- data.frame(
#  red   = matrix(m_bf1[,,1], ncol=1),
#  green = matrix(m_bf1[,,2], ncol=1),
#  blue  = matrix(m_bf1[,,3], ncol=1)
#  )
#
##nrow(unique(d_bf1))


#plot(d_bf1$red, d_bf1$green, pch = 16, col=rgb(d_bf1))

# ==========================================================================
# Have a look
# ==========================================================================

#persp3D(z = m_bf1_gs_normal, theta = 120)


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

m_bf1_normal <- m_bf1_normal/max(m_bf1_normal)


grid.raster(m_bf1_normal)

# ==========================================================================
# k-means clustering
# ==========================================================================

#k_bf1_normal <- kmeans(d_bf1_normal, 5) # separate into 3 colours


#error <- vector()   # define an empty object to store the error for each cluster
#for (i in 1:15){
#  error[i]<-kmeans(d_bf1_normal,i)$tot.withinss 
#}

#plot(error)

#
##head(k_bf1_normal)
#d_bf1_normal_k <- d_bf1_normal
## Attach cluster labels to our dataframe
#d_bf1_normal_k$label <- k_bf1_normal$cluster
#
## Let's also assign the cluster labels to the cluster colours
#colours1 <- data.frame(k_bf1_normal$centers, c(1:5))
#colnames(colours1) <- c("red.c","green.c","blue.c","label")
#colours1
#
#
#d_bf1_normal_k <- inner_join(d_bf1_normal_k, colours1 ,by="label")
#
##head(d_bf1_normal)
#
#
#d_bf1_normal_k %>%
#dplyr::select(red.c,green.c,blue.c) %>%
#unlist() %>%
#unname()
#
#
#
#m_bf1_normal_segmented <- array(
#  d_bf1_normal_k %>%
#    dplyr::select(
#      red.c,
#      green.c,
#      blue.c
#    ) %>%
#    unlist() %>%
#    unname(),
#  dim = dim(m_bf1)
#)
#
#grid.raster(m_bf1_normal_segmented)
#

# Find darkest colour



# I appear to be transforming the image somewhere (I think it might be in normalisation)
# I'm not going to worry about that too much for the time being



# Take the k-means from a good example and use these clusters:

segment <- function(pixel) {
  cluster1 <- abs(pixel-centres[1])
  cluster2 <- abs(pixel-centres[2])
  cluster3 <- abs(pixel-centres[3])
  cluster4 <- abs(pixel-centres[4])
  cluster5 <- abs(pixel-centres[5])

  minimum <- min(c(cluster1, cluster2, cluster3, cluster4, cluster5))

  pixel <- case_when(
    minimum == cluster1 ~ centres[1],
    minimum == cluster2 ~ centres[2],
    minimum == cluster3 ~ centres[3],
    minimum == cluster4 ~ centres[4],
    minimum == cluster5 ~ centres[5]
  )

  return(pixel)
}

d_bf1_normal_segmented_gs <- sapply(d_bf1_normal[,1], FUN = segment) %>% as.data.frame()
d_bf1_normal_segmented <- cbind(d_bf1_normal_segmented_gs, d_bf1_normal_segmented_gs, d_bf1_normal_segmented_gs)
colnames(d_bf1_normal_segmented) <- c("red", "green", "blue")


prop_background <- d_bf1_normal_segmented %>% dplyr::filter(
  red == min(centres[1])
  ) %>%
nrow()/nrow(d_bf1_normal_segmented_gs) # nolint

computer_cellularity <- (1-prop_background)*100

cat("cellularity is", round(computer_cellularity),"% as defined by the computer\n")
cat("cellularity is", round(human_cellularity),"% as defined by Amy\n")

m_bf1_normal_segmented <- d_bf1_normal_segmented %>% unlist() %>% array(dim = dim(m_bf1))

grid.raster(m_bf1_normal_segmented)