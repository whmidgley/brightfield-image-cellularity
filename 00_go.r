
if (Sys.info()["user"] == "william.midgley") {
  setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")
} else {
  cat("Please add wd\n") 
}
library(stringr)
library(beepr)
library(EBImage)
# ==========================================================================
# Load in image
# ==========================================================================

images <- list.files(path = "brightfield-images", pattern = "tif", recursive = FALSE, full.names = TRUE) 

cellularities <- data.frame(matrix(nrow=length(images), ncol=2))
colnames(cellularities) <- c("image_name", "cellularity")

for (j in 1:length(images)) {
	cat("Image",j,"=================\n")
	m_bf1 <- readImage(paste0(images[j]))

m_bf1 <- array(m_bf1, dim = dim(m_bf1))
m_bf1_gs <- m_bf1[,,1]

source("01_find_gradiant.r")
source("03_edge_detection.r")

cellularities[j,] <- c(sub('.+/(.+)', '\\1', images[j]), print(cellularity))
}

write.csv(cellularities, "cellularities.csv", row.names = FALSE)
beep()