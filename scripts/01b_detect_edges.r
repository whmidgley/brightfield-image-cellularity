cat("Detecting edges and blurring...\n")

# ==========================================================================
# Load image
# ==========================================================================
if(file.exists("m_bf.rdata")) load("m_bf.rdata")
if(file.exists("j.rdata")) load("j.rdata")
if(file.exists("image_names.rdata")) load("image_names.rdata")
if(file.exists("blur.rdata")) load("blur.rdata")
if(file.exists("brightness_mean.rdata")) load("brightness_mean.rdata")
if(file.exists("cut_off.rdata")) load("cut_off.rdata")
if(file.exists("shrink_cutoff.rdata")) load("shrink_cutoff.rdata")
if(file.exists("grid_output.rdata")) load("grid_output.rdata")
if(file.exists("grid_no.rdata")) load("grid_no.rdata")
if(file.exists("change_grid_no.rdata")) load("change_grid_no.rdata")
if(file.exists("flag_thresh.rdata")) load("flag_thresh.rdata")
if(file.exists("desired_output_format.rdata")) load("desired_output_format.rdata")

img <- readImage(paste0("output/bf-analysis/bf-normalised/", image_names[j], " normalised.", desired_output_format))

# ==========================================================================
# Edge detection
# ==========================================================================

# 1. define filter for edge detection
hfilt <- matrix(c(1, 2, 1, 0, 0, 0, -1, -2, -1), nrow = 3) # sobel

# rotate horizontal filter to obtain vertical filter
vfilt <- t(hfilt)

# get horizontal and vertical edges
imgH <- filter2(img[,,1], hfilt, boundary="replicate")
imgV <- filter2(img[,,1], vfilt, boundary="replicate")

# combine edge pixel data to get overall edge data
hdata <- imageData(imgH)
vdata <- imageData(imgV)
edata <- sqrt((hdata/2)^2 + (vdata*2)^2)

# transform edge data to image
imgE <- Image(edata)

plot(imgE)
# Low pass filter with gblur
xb <- gblur(imgE, blur*nrow(imgE))

xb <- (xb - min(xb)) / (max(xb) - min(xb))

plot(xb)

m_bf_blur <- xb

while (mean(m_bf_blur[xb>cut_off]) < brightness_mean) {
  cat("Smooth image, cutting top end...\n")
  m_bf_blur <- case_when(matrix(m_bf_blur) > 0.95 ~ 0.95, TRUE ~ matrix(m_bf_blur, ncol= 1))
  m_bf_blur <- (m_bf_blur - min(m_bf_blur)) / (max(m_bf_blur) - min(m_bf_blur))
  m_bf_blur <- Image(matrix(m_bf_blur, ncol = ncol(img)))
}

plot(m_bf_blur)