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

plot(imgE)
# Low pass filter with gblur
xb <- gblur(imgE, blur)

xb <- (xb - min(xb)) / (max(xb) - min(xb))

plot(xb)

m_bf_blur <- xb

while (mean(m_bf_blur[xb>cut_off]) < 0.3) {
  cat("Smooth image, cutting top end...\n")
  m_bf_blur <- case_when(matrix(m_bf_blur) > 0.95 ~ 0.95, TRUE ~ matrix(m_bf_blur, ncol= 1))
  m_bf_blur <- (m_bf_blur - min(m_bf_blur)) / (max(m_bf_blur) - min(m_bf_blur))
  m_bf_blur <- Image(matrix(m_bf_blur, ncol = ncol(img)))
}

plot(m_bf_blur)

save(m_bf_blur, file ="r-objects/m_bf_blur.RData")
