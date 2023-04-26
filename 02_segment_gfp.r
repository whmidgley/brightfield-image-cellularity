
source("r_clear_and_load.r")

# ==========================================================================
# Variables
# ==========================================================================


use_segmented_bf <- TRUE
input_image_type <- "gfp"


# ==========================================================================
# Check folders exist
# ==========================================================================

if(!dir.exists("input-images")) dir.create("input-images")
if(!dir.exists("normalised-images")) dir.create("normalised-images")
if(!dir.exists("overlay-images")) dir.create("overlay-images")
if(!dir.exists("segmented-images")) dir.create("segmented-images")

# ==========================================================================
# File formats
# ==========================================================================

# enter your desired output image format. Supported formats are tif, tiff, png, jpg and jpeg
desired_output_format <- "tif"
if(!desired_output_format %in% c("tif", "tiff", "png", "jpeg", "jpg")) stop("Output format not supported.\nSupported formats are tif, tiff, png, jpg and jpeg")

# Change to TRUE if you want to manually chage the input format
auto_lif_detect <- FALSE



# this is only relevant if you're dealing with images and not .lif files
# put TRUE if you're dealing with snapshots from leica
# put FALSE if you're dealing with image formats that aren't taken from leica image snapshots
leica_snapshot_flg <- TRUE


if(auto_lif_detect) {
	# input format can be lif or tif
	input_format <- "lif"
} else {
	image_file_contents <- list.files(path = "input-images", recursive = TRUE, full.names = TRUE)

	compatible_types <- c("lif", "tif", "tiff", "png", "jpeg", "jpg")

	if(length(image_file_contents) == 0) {
		stop("File input-images is empty. Please add images or lifs")
	}
	if(!sapply(X = compatible_types, FUN = filetype.test, file = image_file_contents) %>% vector.XOR()) {
		stop("Multiple file formats detected. Please remove unwanted file format")
	}
	if(filetype.test(image_file_contents, "tif") && !str_detect(image_file_contents, ".(tiff)|(jpeg)|(jpg)|(png)|(lif)$") %>% vector.AND()) {
		cat("Tifs detected. Switching to tif mode\n")
		input_format <- "tif"
	}
	if(filetype.test(image_file_contents, "tiff") && !str_detect(image_file_contents, ".(tif)|(jpeg)|(jpg)|(png)|(lif)$") %>% vector.AND()) {
		cat("Tiffs detected. Switching to tiff mode\n")
		input_format <- "tiff"
	}
	if(filetype.test(image_file_contents, "png") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(jpg)|(lif)$") %>% vector.AND()) {
		cat("Pngs detected. Switching to png mode\n")
		input_format <- "png"
	}
	if(filetype.test(image_file_contents, "jpeg") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpg)|(png)|(lif)$") %>% vector.AND()) {
		cat("Jpegs detected. Switching to jpeg mode\n")
		input_format <- "jpeg"
	}
	if(filetype.test(image_file_contents, "jpg") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(png)|(lif)$") %>% vector.AND()) {
		cat("Jpgs detected. Switching to jpg mode\n")
		input_format <- "jpg"
	}
	if(filetype.test(image_file_contents, "lif") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(jpg)|(png)$") %>% vector.AND()) {
		cat("Lifs detected. Switching to lif mode\n")
		input_format <- "lif"
	}
	if(!filetype.test(image_file_contents, "(tif)|(tiff)|(jpeg)|(jpg)|(png)|(lif)")) {
		stop("Files detected are neither lifs nor images. Please add images or lifs\n
			Supported file formats are .lif, .tif, .tiff, .png, jpg and .jpeg")
	} else if((!filetype.test(image_file_contents, "(tif)|(tiff)|(jpeg)|(jpg)|(png)|(lif)"))) {
		warning("Other files detected which are neither lifs nor images")
	}
}

# ==========================================================================
# Load images
# ==========================================================================

if(input_format == "lif") {

lif_dirs <- list.files(path = "input-images", pattern = "lif$", recursive = TRUE, full.names = TRUE) 


gfp_names <- c()
bf_names <- c()
lif_lengths <- c()
for(i in c(1:length(lif_dirs))) {
	lif <- extract.bf(lif_dirs[i])
	if(is.null(dim(lif))) {
		lif_length <- length(lif)
	} else {
		lif_length <- 1
	}
	lif_lengths[i] <- lif_length
	for(j in c(1:lif_length)) {
		if(lif_length == 1) {
			image_frame <- lif
		} else {
			image_frame <- lif[[j]]
		}
		if(i == 1) {
			row_num <- j
		} else {
			row_num <- sum(lif_lengths[1:(i-1)]) + j
		}
		bf  <- array(dim = c(dim(image_frame)[c(1:2)], 3))
		gfp <- array(dim = c(dim(image_frame)[c(1:2)], 3))
		bf[,,1] <- image_frame[,,2]
		bf[,,2] <- image_frame[,,2]
		bf[,,3] <- image_frame[,,2]
		gfp[,,1] <- image_frame[,,1]
		gfp[,,2] <- image_frame[,,1]
		gfp[,,3] <- image_frame[,,1]
		bf  <- Image(bf,  colormode = "Color")
		gfp <- Image(gfp, colormode = "Color")
		bf_names[row_num]  <- c(paste0(sub('.+/(.+)', '\\1', lif_dirs[i] %>% str_replace(".lif", "")), " BF ",  j))
		gfp_names[row_num] <- c(paste0(sub('.+/(.+)', '\\1', lif_dirs[i] %>% str_replace(".lif", "")), " GFP ", j))
		assign(paste0("image_", bf_names[row_num]  %>% str_replace_all(" ", "_")), bf)
		assign(paste0("image_", gfp_names[row_num] %>% str_replace_all(" ", "_")), gfp)
	}
}
} else {

images <- list.files(path = "input-images", pattern = input_format, recursive = TRUE, full.names = TRUE) 

cellularities <- data.frame(matrix(nrow=length(images), ncol=2))
colnames(cellularities) <- c("image_name", "cellularity")

if(leica_snapshot_flg) {
	image_names <- sub('.+/(.+)', '\\1', images) %>% str_replace("Effectene.lif_", "") %>% str_replace(paste0("Snapshot1.", input_format), "") %>% str_replace(".lif_", " ")
} else {
	image_names <- sub('.+/(.+)', '\\1', images) %>% str_replace("Effectene.lif_", "")
}

assign(paste0(input_image_type, "s"), images)
assign(paste0(input_image_type, "_names"), image_names)
}

for(row_num in 1:length(gfp_names)) {

#################    This will eventually be changed
bf_copy <- bf_names
bf_names <- bf_names %>% str_replace_all("BF", "Image")
#################

for(i in bf_names)
if(paste0(i, " segmented.", desired_output_format) %in% list.files("segmented-images")) {
	assign(paste0("image_", i %>% str_replace_all(" ", "_"), "_segmented"),
	readImage(paste0("segmented-images/", i, " segmented.", desired_output_format))
	)
}

segmented <- get(paste0("image_", bf_names[row_num] %>% str_replace_all(" ", "_"), "_segmented"))

#################
bf_names <- bf_copy
#################


gfp <- get(paste0("image_", gfp_names[row_num] %>% str_replace_all(" ", "_")))


bf <- get(paste0("image_", bf_names[row_num] %>% str_replace_all(" ", "_")))

#plot(gfp)

# Try cutting off top end like how amy does =====================================
cat("Try cutting off top end like how amy does\n")

gfp_cut_top <- gfp

cut_off <- 0.05

gfp_cut_top[gfp_cut_top > cut_off] <- cut_off

gfp_cut_top <- gfp_cut_top*(1/cut_off)

plot(gfp_cut_top)

gfp_b <- gblur(gfp_cut_top, 2)
plot(gfp_b)

hfilt <- matrix(c(1, 2, 1, 0, 0, 0, -1, -2, -1), nrow = 3) # sobel
vfilt <- t(hfilt)

gfpH <- filter2(gfp_b[,,1], hfilt, boundary="replicate")
gfpV <- filter2(gfp_b[,,1], vfilt, boundary="replicate")

hdata <- imageData(gfpH)
vdata <- imageData(gfpV)
edata <- sqrt((hdata)^2 + (vdata)^2)

# transform edge data to image
gfpE <- Image(edata)
plot(gfpE)

gfp_t <- thresh(gfp_b)
gfp_t[segmented == 0] <- 0
plot(gfp_t)
gfp_bw <- bwlabel(gfp_t)[,,1]
gfp_cfs <- computeFeatures.shape(gfp_bw)
sels <- which(gfp_cfs[,"s.area"] < nrow(gfp)^2*0.00015)
gfp_rms <- rmObjects(gfp_bw, sels)
plot(gfp_rms)

gfp_cfm <- computeFeatures.moment(gfp_rms)
selm <- which(gfp_cfm[,"m.eccentricity"] > 0.9)
gfp_rmm <- rmObjects(gfp_rms, selm)
plot(gfp_rmm)
gfp_rois <- computeFeatures.moment(gfp_rmm)
centroids <- round(gfp_rois[,c("m.cy","m.cx")])


gfp_overlay <- gfp_cut_top
gfp_overlay[,,3] <- bf[,,3]
for(i in 1:nrow(centroids)){
	xs <- seq(from = centroids[i,1] - 10, to = centroids[i,1] + 10, by = 1)
	ys <- seq(from = centroids[i,2] - 10, to = centroids[i,2] + 10, by = 1)
	xs <- xs[xs > 0 & xs <= nrow(gfp)]
	ys <- ys[ys > 0 & ys <= nrow(gfp)]
	gfp_overlay[ys, xs, 1] <- 1
}
gfp_overlay[,,1] <- gfp_rmm
#gfp_overlay[centroids[1,1], centroids[1,2], 2] <- 0
plot(gfp_overlay)

}
