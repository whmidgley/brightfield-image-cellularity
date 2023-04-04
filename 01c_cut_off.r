cat("Calculating cellularity...\n")

# ==========================================================================
# Read images
# ==========================================================================

m_bf_blur <- m_bf_blur/max(m_bf_blur)
d_bf_blur <- data.frame(matrix(m_bf_blur, ncol=1))

d_bf_cut_off <- case_when(
	d_bf_blur < cut_off ~ 0,
	TRUE ~ 1)

m_bf_cut_off <- Image(matrix(d_bf_cut_off, ncol = ncol(m_bf_blur)))

plot(m_bf_cut_off)

xb <- bwlabel(m_bf_cut_off)
FS <- computeFeatures.shape(xb)
sel <- which(FS[,"s.area"] < nrow(m_bf_cut_off)^2*0.0006)
xe <- rmObjects(xb, sel)
plot(xe)


xe <- case_when(
  matrix(xe) >= 0.5 ~ 1,
  TRUE ~ 0)
xe <- matrix(xe, ncol = ncol(m_bf)) %>% Image()

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
edata <- sqrt((hdata)^2 + (vdata)^2)

# transform edge data to image
imgE <- Image(edata)
imgE <- matrix(case_when(matrix(imgE) < 0.5 ~ 0, matrix(imgE) >= 0.5 ~ 1), ncol = ncol(imgE)) %>% Image()

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
m_bf_overlay <- m_bf

m_bf_overlay[,,1][m_bf_edges == 1] <- 1
m_bf_overlay[,,2][m_bf_edges == 1] <- 0
m_bf_overlay[,,3][m_bf_edges == 1] <- 0

plot(m_bf_overlay)

if(exists("image_names")) {
writeImage(xe, paste0("segmented-images/", image_names[j], " segmented.", desired_output_format))

writeImage(m_bf_overlay, paste0("overlay-images/", image_names[j], " overlay.", desired_output_format))
}