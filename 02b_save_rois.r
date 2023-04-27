
library(spatstat)

gfp_rms[gfp_rms > 1] <- 1

gfp_overlay <- gfp_cut_top
gfp_overlay[,,1] <- 0
gfp_overlay[,,3] <- bf[,,3]
centroids <- data.frame(centroids)
for (i in 1:nrow(centroids)) {


pixel <- gfp_rms[centroids[i,2], centroids[i,1]]

if(pixel == 1) {
	# FIND TOP
	centroid_top <- centroids[i,1]
	pixel_top <- pixel
	while(pixel_top == 1) {
		# move up until black
		centroid_top[1] <- centroid_top - 1
		pixel_top <- gfp_rms[centroids[i,2], centroid_top]
	}
	# FIND BOTTOM
	centroid_bottom <- centroids[i,1]
	pixel_bottom <- pixel
	while(pixel_bottom == 1) {
		# move up until black
		centroid_bottom <- centroid_bottom + 1
		pixel_bottom <- gfp_rms[centroids[i,2], centroid_bottom]
	}
	# FIND LEFT
	centroid_left <- centroids[i,2]
	pixel_left <- pixel
	while(pixel_left == 1) {
		# move up until black
		centroid_left <- centroid_left - 1
		pixel_left <- gfp_rms[centroid_left, centroids[i,1]]
	}
	# FIND RIGHT
	centroid_right <- centroids[i,2]
	pixel_right <- pixel
	while(pixel_right == 1) {
		# move up until black
		centroid_right <- centroid_right + 1
		pixel_right <- gfp_rms[centroid_right, centroids[i,1]]
	}
}

centroids$centroid_top[i] <- centroid_top
centroids$centroid_bottom[i] <- centroid_bottom
centroids$centroid_left[i] <- centroid_left
centroids$centroid_right[i] <- centroid_right

xs <- seq(from = centroids$centroid_top[i], to = centroids$centroid_bottom[i], by = 1)
ys <- seq(from = centroids$centroid_left[i], to = centroids$centroid_right[i], by = 1)
xs <- xs[xs > 0 & xs <= nrow(gfp)]
ys <- ys[ys > 0 & ys <= nrow(gfp)]
gfp_overlay[ys, xs, 1] <- 1
}


display(gfp_overlay)