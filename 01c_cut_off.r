cat("Calculating cellularity...\n")

# ==========================================================================
# Read images
# ==========================================================================

load("r-objects/m_bf_blur.RData")

m_bf_blur <- m_bf_blur/max(m_bf_blur)
d_bf_blur <- data.frame(matrix(m_bf_blur, ncol=1))

d_bf_cut_off <- case_when(
	d_bf_blur < 0.07 ~ 0,
	TRUE ~ 1)

m_bf_cut_off <- Image(matrix(d_bf_cut_off, ncol = ncol(m_bf_blur)))

plot(m_bf_cut_off)
 
xb <- bwlabel(m_bf_cut_off)
FS <- computeFeatures.shape(xb)
sel <- which(FS[,"s.area"] < 500)
xe <- rmObjects(xb, sel)
plot(xe)
 
m_bf_segmented <- xe

hfilt <- matrix(c(1, 2, 1, 0, 0, 0, -1, -2, -1), nrow = 3) # sobel

# rotate horizontal filter to obtain vertical filter
vfilt <- t(hfilt)

# get horizontal and vertical edges
imgH <- filter2(xe, hfilt, boundary="replicate")
imgV <- filter2(xe, vfilt, boundary="replicate")

# combine edge pixel data to get overall edge data
hdata <- imageData(imgH)
vdata <- imageData(imgV)
edata <- sqrt((hdata/2)^2 + (vdata*2)^2)

# transform edge data to image
imgE <- Image(edata)
imgE <- matrix(case_when(imgE < 0.5 ~ 0, imgE >= 0.5 ~ 1), ncol = ncol(imgE)) %>% Image()

m_bf_edges <- imgE

plot(imgE)

prop_background_edge <-
  imgE[imgE == 0] %>%
    length() / length(d_bf_cut_off)


prop_background <-
  xe[xe == 0] %>%
    length() / length(d_bf_cut_off)

 
computer_cellularity <- ((1-prop_background)-(1-prop_background_edge)*error_factor)*100

cat("cellularity is", round(computer_cellularity),"% as defined by the computer\n")

m_bf_overlay <- array(dim = dim(m_bf))
m_bf_overlay[,,2:3] <- m_bf[,,2:3]
m_bf_overlay[,,1] <- m_bf_edges

m_bf_overlay <- Image(m_bf_overlay, colormode = "Color")

grid.raster(m_bf_overlay)

writeImage(xe, paste0("segmented/", sub('.+/(.+)', '\\1', images[j]) %>% str_replace_all(".tif", "_segmented.tif")))

writeImage(xe, paste0("overlay/", sub('.+/(.+)', '\\1', images[j]) %>% str_replace_all(".tif", "_overlay.tif")))
