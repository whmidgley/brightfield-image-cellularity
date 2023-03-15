cat("Calculating cellularity...\n")

# ==========================================================================
# Read images
# ==========================================================================

load("r-objects/m_bf_blur.RData")

m_bf_blur <- m_bf_blur/max(m_bf_blur)
d_bf_blur <- data.frame(matrix(m_bf_blur, ncol=1))

d_bf_cut_off <- case_when(
	d_bf_blur < 0.08 ~ 0,
	TRUE ~ 1)

m_bf_cut_off <- Image(matrix(d_bf_cut_off, ncol = ncol(m_bf_blur)))

plot(m_bf_cut_off)

xb <- bwlabel(m_bf_cut_off)
FS <- computeFeatures.shape(xb)
sel <- which(FS[,"s.area"] < 500)
xe <- rmObjects(xb, sel)
plot(xe)

prop_background <-
  xe[xe == 0] %>%
    length() / length(d_bf_cut_off)

computer_cellularity <- (1-prop_background)*100

cat("cellularity is", round(computer_cellularity),"% as defined by the computer\n")


writeImage(xe, paste0("segmented/", sub('.+/(.+)', '\\1', images[j]) %>% str_replace_all(".tif", "_segmented.tif")))
