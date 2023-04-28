
library(spatstat)

gfp_rms[gfp_rms > 1] <- 1

gfp_overlay <- gfp_cut_top
gfp_overlay[,,1] <- 0
gfp_overlay[,,3] <- bf[,,3]
centroids <- data.frame(centroids)
for (i in 1:nrow(centroids)) {


gfp_box <- rmObjects(gfp_rms, index(gfp_rois[-i,]))

pixel <- gfp_rms[centroids[i,2], centroids[i,1]]

if(pixel == 1) {
	# FIND TOP
	centroid_top <- centroids[i,1]
	pixel_top <- pixel
	stop_top <- FALSE
	while(pixel_top == 1 & !stop_top) {
		# move up until black
		if(centroid_top - 1 %in% 1:nrow(gfp_box)) {
			centroid_top[1] <- centroid_top - 1
			pixel_top <- gfp_box[centroids[i,2], centroid_top]
		} else {stop_top <- TRUE}
		
	}
	# FIND BOTTOM
	centroid_bottom <- centroids[i,1]
	pixel_bottom <- pixel
	stop_bottom <- FALSE
	while(pixel_bottom == 1 & !stop_bottom) {
		# move up until black
		if(centroid_bottom + 1 %in% 1:nrow(gfp_box)) {
		centroid_bottom <- centroid_bottom + 1
			pixel_bottom <- gfp_box[centroids[i,2], centroid_bottom]
		} else {stop_bottom <- TRUE}
	}
	# FIND LEFT
	centroid_left <- centroids[i,2]
	pixel_left <- pixel
	stop_left <- FALSE
	while(pixel_left == 1 & !stop_left) {
		# move up until black
		if(centroid_left - 1 %in% 1:nrow(gfp_box)) {
		centroid_left <- centroid_left - 1
			pixel_left <- gfp_box[centroid_left, centroids[i,1]]
		} else {stop_left <- TRUE}
	}
	# FIND RIGHT
	centroid_right <- centroids[i,2]
	pixel_right <- pixel
	stop_right <- FALSE
	while(pixel_right == 1 & !stop_right) {
		# move up until black
		if(centroid_right + 1 %in% 1:nrow(gfp_box)) {
		centroid_right <- centroid_right + 1
			pixel_right <- gfp_box[centroid_right, centroids[i,1]]
		} else {stop_right <- TRUE}
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

display(gfp_rms)
display(gfp_overlay)