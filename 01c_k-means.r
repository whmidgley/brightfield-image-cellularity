cat("Segmenting and calculating cellularity...\n")

# ==========================================================================
# Read images
# ==========================================================================

load("r-objects/m_bf_blur.RData")

m_bf_blur <- m_bf_blur/max(m_bf_blur)
d_bf_blur <- data.frame(matrix(m_bf_blur, ncol=1))

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

d_bf_blur_segmented_gs <- sapply(d_bf_blur[,1], FUN = segment) %>% as.data.frame()

prop_background <-
  d_bf_blur_segmented_gs[d_bf_blur_segmented_gs == background] %>%
    length() / nrow(d_bf_blur_segmented_gs)

computer_cellularity <- (1-prop_background)*100

cat("cellularity is", round(computer_cellularity),"% as defined by the computer\n")

m_bf_blur_segmented <- cbind(d_bf_blur_segmented_gs, d_bf_blur_segmented_gs, d_bf_blur_segmented_gs) %>% unlist() %>% array(dim = dim(m_bf))

grid.raster(m_bf_blur_segmented)