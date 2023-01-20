
setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")

library(stringr)
library(beepr)
library(EBImage)
# ==========================================================================
# Load in image
# ==========================================================================

images <- list.files(path = "brightfield-images", pattern = "tif", recursive = FALSE, full.names = TRUE) 

cellularities <- data.frame(matrix(nrow=length(images), ncol=2))
colnames(cellularities) <- c("image_name", "cellularity")

for (i in 1:length(images)) {
	cat("Image",i,"\n")
	m_bf1 <- readImage(paste0(images[i]))

m_bf1 <- array(m_bf1, dim = dim(m_bf1))
m_bf1_gs <- m_bf1[,,1]

source("01_find_gradiant.r")
source("03_edge_detection.r")

cellularities[i,] <- c(sub('.+/(.+)', '\\1', images[i]), cellularity)
}

write.csv(cellularities, "cellularities.csv", row.names = FALSE)
beep()