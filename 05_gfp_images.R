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

#load("r-objects/m_gfp1_normal.RData")

m_gfp1 <- readImage("gfp-images/gfp1.tif")
m_gfp1 <- array(m_gfp1, dim = dim(m_gfp1))
#m_gfp1_gs <- m_gfp1[,,1]
#m_gfp1_gs_normal <- m_gfp1_normal[,,1]

# ==========================================================================
# Let's have a look
# ==========================================================================

dim(m_gfp1)

grid.raster(m_gfp1)

d_gfp1 <- data.frame(
  red   = matrix(m_gfp1[,,1], ncol=1),
  green = matrix(m_gfp1[,,2], ncol=1),
  blue  = matrix(m_gfp1[,,3], ncol=1)
  )

#nrow(unique(d_gfp1))


#plot(d_gfp1$red, d_gfp1$green, pch = 16, col=rgb(d_gfp1))

# ==========================================================================
# k-means clustering
# ==========================================================================

k_gfp1 <- kmeans(d_gfp1, 2) # separate into 3 colours


#error <- vector()   # define an empty object to store the error for each cluster
#for (i in 1:15){
#  error[i]<-kmeans(d_gfp1,i)$tot.withinss 
#}

#plot(error)


#head(k_gfp1)
d_gfp1_k <- d_gfp1
# Attach cluster labels to our dataframe
d_gfp1_k$label <- k_gfp1$cluster

# Let's also assign the cluster labels to the cluster colours
colours1 <- data.frame(k_gfp1$centers, c(1:2))
colnames(colours1) <- c("red.c","green.c","blue.c","label")
colours1


d_gfp1_k <- inner_join(d_gfp1_k, colours1 ,by="label")

#head(d_gfp1)


d_gfp1_k %>%
select(red.c,green.c,blue.c) %>%
unlist() %>%
unname()



m_gfp1_segmented <- array(
  d_gfp1_k %>%
    select(
      red.c,
      green.c,
      blue.c
    ) %>%
    unlist() %>%
    unname(),
  dim = dim(m_gfp1)
)

grid.raster(m_gfp1_segmented)

# I appear to be transforming the image somewhere (I think it might be in normalisation)
# I'm not going to worry about that too much for the time being

