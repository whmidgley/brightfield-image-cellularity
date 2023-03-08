cat("Calculating cellularity...\n")

# ==========================================================================
# Read images
# ==========================================================================

load("r-objects/m_bf_blur.RData")

m_bf_blur <- m_bf_blur/max(m_bf_blur)
d_bf_blur <- data.frame(matrix(m_bf_blur, ncol=1))

d_bf_cut_off <- case_when(
	d_bf_blur < 0.113 ~ 0,
	TRUE ~ 1)

prop_background <-
  d_bf_cut_off[d_bf_cut_off == 0] %>%
    length() / length(d_bf_cut_off)

computer_cellularity <- (1-prop_background)*100

cat("cellularity is", round(computer_cellularity),"% as defined by the computer\n")


m_bf_cut_off <- cbind(d_bf_cut_off, d_bf_cut_off, d_bf_cut_off) %>% unlist() %>% array(dim = dim(m_bf_blur))

writeImage(m_bf_cut_off, paste0("segmented/", sub('.+/(.+)', '\\1', images[j]) %>% str_replace_all(".tif", "_segmented.tif")))

grid.raster(m_bf_cut_off)