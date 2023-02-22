cat("Segmenting and calculating cellularity...\n")

# ==========================================================================
# Read images
# ==========================================================================

load("r-objects/m_bf_blur.RData")

m_bf_blur <- m_bf_blur/max(m_bf_blur)
d_bf_blur <- data.frame(matrix(m_bf_blur, ncol=1))

# Take the k-means from a good example and use these clusters:

cluster1 <- abs(d_bf_blur-centres[1])
cluster2 <- abs(d_bf_blur-centres[2])
cluster3 <- abs(d_bf_blur-centres[3])
cluster4 <- abs(d_bf_blur-centres[4])

cluster_errors <- data.frame(
  cluster1 = cluster1,
  cluster2 = cluster2,
  cluster3 = cluster3,
  cluster4 = cluster4
  )

min_error <- do.call(pmin, cluster_errors)

d_bf_blur_segmented_gs <- case_when(
  min_error == cluster1 ~ centres[1],
  min_error == cluster2 ~ centres[2],
  min_error == cluster3 ~ centres[3],
  min_error == cluster4 ~ centres[4]
)


prop_background <-
  d_bf_blur_segmented_gs[d_bf_blur_segmented_gs == background] %>%
    length() / length(d_bf_blur_segmented_gs)

computer_cellularity <- (1-prop_background)*100

cat("cellularity is", round(computer_cellularity),"% as defined by the computer\n")

m_bf_blur_segmented <- cbind(d_bf_blur_segmented_gs, d_bf_blur_segmented_gs, d_bf_blur_segmented_gs) %>% unlist() %>% array(dim = dim(m_bf))

m_bf_blur_segmented[,,1] <- t(m_bf_blur_segmented[,,1])
m_bf_blur_segmented[,,2] <- t(m_bf_blur_segmented[,,2])
m_bf_blur_segmented[,,3] <- t(m_bf_blur_segmented[,,3])


grid.raster(m_bf_blur_segmented)

writeImage(m_bf_blur_segmented, paste0("segmented/", sub('.+/(.+)', '\\1', images[j]) %>% str_replace_all(".tif", "_segmented.tif")))