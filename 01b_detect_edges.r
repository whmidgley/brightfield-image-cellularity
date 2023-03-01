cat("Detecting edges and blurring...\n")

# ==========================================================================
# Load image
# ==========================================================================

load("r-objects/m_bf_normal.RData")

img <- m_bf_normal

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

# Low pass filter with gblur
  xb <- gblur(imgE, blur)
#  plot(xb)

m_bf_blur <- xb
save(m_bf_blur, file ="r-objects/m_bf_blur.RData")
