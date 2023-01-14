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
options(digits=20)  # this allows R to use decimals up to 20 places
options(repr.plot.width = 15, repr.plot.height = 20)

# ==========================================================================
# Read images
# ==========================================================================

m_bf1 <- readTIFF("brightfield-images/bf1.tif")


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
# k-means clustering
# ==========================================================================


k_bf1 <- kmeans(d_bf1, 4) # separate into 3 colours


#error <- vector()   # define an empty object to store the error for each cluster
#for (i in 1:15){
#  error[i]<-kmeans(d_bf1,i)$tot.withinss 
#}

#plot(error)


#head(k_bf1)

# Attach cluster labels to our dataframe
d_bf1$label <- k_bf1$cluster

# Let's also assign the cluster labels to the cluster colours
colours1 <- data.frame(k_bf1$centers, c(1:4))
colnames(colours1) <- c("red.c","green.c","blue.c","label")
colours1


d_bf1 <- inner_join(d_bf1, colours1 ,by="label")

#head(d_bf1)


d_bf1 %>%
select(red.c,green.c,blue.c) %>%
unlist() %>%
unname()



a_bf1_segmented <- array(
  d_bf1 %>%
    select(
      red.c,
      green.c,
      blue.c
    ) %>%
    unlist() %>%
    unname(),
  dim = dim(m_bf1)
)

grid.raster(a_bf1_segmented)