
source("scripts/r_clear_and_load.r")


# Variables =====================================================================

use_segmented_bf <- TRUE
input_image_type <- "gfp"

brighten <- 0.04
blur <- 1
cut_off <- 0.45
min_cell_size <- 0.00013
cell_output <- TRUE
grid_output <- TRUE
change_grid_no <- FALSE
grid_no <- 4


if(change_grid_no) {
	cell_output <- FALSE
	grid_output <- TRUE
}

# ==========================================================================
# Check folders exist
# ==========================================================================

if(!dir.exists("input")) dir.create("input")
if(!dir.exists("output")) dir.create("output")
if(!dir.exists("output/gfp-analysis")) dir.create("output/gfp-analysis")
if(!dir.exists("output/gfp-analysis/gfp-segmented")) dir.create("output/gfp-analysis/gfp-segmented")
if(!dir.exists("output/gfp-analysis/gfp-overlay")) dir.create("output/gfp-analysis/gfp-overlay")	
if(!dir.exists("output/gfp-analysis/cell-stats") & cell_output) dir.create("output/gfp-analysis/cell-stats")
if(!dir.exists("output/gfp-analysis/grid-stats") & grid_output) dir.create("output/gfp-analysis/grid-stats")

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
	image_file_contents <- list.files(path = "input", recursive = TRUE, full.names = TRUE)

	compatible_types <- c("lif", "tif", "tiff", "png", "jpeg", "jpg")

	if(length(image_file_contents) == 0) {
		stop("File input is empty. Please add images or lifs")
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

lif_dirs <- list.files(path = "input", pattern = "lif$", recursive = TRUE, full.names = TRUE) 


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

images <- list.files(path = "input", pattern = input_format, recursive = TRUE, full.names = TRUE) 

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

gfp_overall_stats <- matrix(ncol = 4, nrow = length(gfp_names))
colnames(gfp_overall_stats) <- c("Name", "Percentage cellular area fluorescing", "Mean brightness of fluorescing cells", "SD brightness of fluorescing cells")


# ==========================================================================
# Check output file isn't open
# ==========================================================================


check.writeable("output/gfp-analysis/overall_stats.csv")

if(cell_output) invisible(sapply(paste0("output/gfp-analysis/cell-stats/", gfp_names, " stats.csv"), FUN = check.writeable.grid))
if(grid_output) invisible(sapply(paste0("output/gfp-analysis/grid-stats/", gfp_names[row_num], " ", grid_no, "x", grid_no, " grid stats.csv"), FUN = check.writeable.grid))


# ==========================================================================
# Start
# ==========================================================================

for(row_num in 1:length(gfp_names)) {

if(!change_grid_no) {


for(i in bf_names) {
if(paste0(i, " segmented.", desired_output_format) %in% list.files("output/bf-analysis/bf-segmented")) {
	assign(paste0("image_", i %>% str_replace_all(" ", "_"), "_segmented"),
	readImage(paste0("output/bf-analysis/bf-segmented/", i, " segmented.", desired_output_format))
	)
} else {
	bf_copy <- i
	i <- i %>% str_replace_all("BF", "Image")
	if(paste0(i, " segmented.", desired_output_format) %in% list.files("output/bf-analysis/bf-segmented")) {
		assign(paste0("image_", i %>% str_replace_all("Image", "BF") %>% str_replace_all(" ", "_"), "_segmented"),
		readImage(paste0("output/bf-analysis/bf-segmented/", i, " segmented.", desired_output_format))
		)
	} else {
		stop("Cellular area has not been determined. Please run cellularity script on these images")
	}
	i <- bf_copy
}
}
gfp <- get(paste0("image_", gfp_names[row_num] %>% str_replace_all(" ", "_")))
bf <- get(paste0("image_", bf_names[row_num] %>% str_replace_all(" ", "_")))
bf_segmented <- get(paste0("image_", bf_names[row_num] %>% str_replace_all(" ", "_"), "_segmented"))


gfp_cut_top <- gfp

gfp_cut_top[gfp_cut_top > brighten] <- brighten

gfp_cut_top <- gfp_cut_top*(1/brighten)

plot(gfp_cut_top)


gfp_b <- gblur(gfp_cut_top, blur)
plot(gfp_b)


gfp_t <- gfp_b

gfp_t[gfp_t > cut_off] <- 1
gfp_t[gfp_t <= cut_off] <- 0

gfp_t[bf_segmented == 0] <- 0

plot(gfp_t)



gfp_bw <- bwlabel(gfp_t[,,1])
gfp_cfs <- computeFeatures.shape(gfp_bw)
sels <- which(gfp_cfs[,"s.area"] < nrow(gfp)^2*min_cell_size)
gfp_rms <- rmObjects(gfp_bw, sels) 
gfp_rms <- fillHull(gfp_rms)
plot(gfp_rms)

gfp_rois <- computeFeatures.moment(gfp_rms)
if(!is.null(gfp_rois)) {
	centroids <- round(gfp_rois[, c("m.cy","m.cx")])
}

gfp_overlay <- gfp_cut_top
gfp_overlay[,,1] <- 0
gfp_overlay[,,3] <- bf[,,3]
gfp_overlay[,,1] <- gfp_rms

plot(gfp_overlay)


# % cellular area fluorescing ===========================================================
cat("% cellular area fluorescing\n")

gfp_rms[gfp_rms > 1] <- 1
gfp_rms[gfp_rms < 1] <- 0

perc_fluorescing <- sum(gfp_rms)*100/sum(bf_segmented)

cat("percentage of cellular area fluorescing is ", round(perc_fluorescing, 2), "%\n")

# Get general stats =============================================================
cat("Get general stats\n")

if(!is.null(gfp_rois)) {
	hist_data_cell <- data.frame(fluo = gfp[gfp_rms == 1], type = "cell")
	hist_data_all <- data.frame(fluo = matrix(gfp), type = "all")
	
	hist_data <- rbind(hist_data_cell, hist_data_all)
	
	hist_data %>%
	ggplot(aes(x = fluo)) + 
	    geom_density(alpha = 0.3) +
	    xlab("Brightness") +
	    facet_grid(vars(type), scales = "free_y")

gfp_overall_stats[row_num,] <- c(gfp_names[row_num], perc_fluorescing, mean(gfp[gfp_rms == 1]), sd(gfp[gfp_rms == 1]))
} else {
	gfp_overall_stats[row_num,] <- c(gfp_names[row_num], perc_fluorescing, mean(gfp[gfp_rms == 1]), sd(gfp[gfp_rms == 1]))
}


# Get cellular stats ============================================================
if(cell_output) {
cat("Get cellular stats\n")
if(!is.null(gfp_rois)) {
if(is.null(nrow(centroids))) {
	cell_stats <- matrix(ncol = 5, nrow = 1)
	cell_stats[,] <- c(1, mean(gfp[gfp_rms == 1]), sd(gfp[gfp_rms == 1]), gfp_rois[,"m.cx"], gfp_rois[,"m.cy"])
} else {
	cell_stats <- matrix(ncol = 5, nrow = nrow(centroids))
	for (i in 1:nrow(centroids)) {
		gfp_bw <- bwlabel(gfp_rms)
		sel_cell <- which(as.numeric(rownames(gfp_rois)) != i)
		gfp_cell <- rmObjects(gfp_bw, sel_cell)
		
		cell_stats[i,] <- c(i, mean(gfp[gfp_cell == 1]), sd(gfp[gfp_cell == 1]), gfp_rois[i,"m.cx"], gfp_rois[i,"m.cy"])
	}
}
colnames(cell_stats) <- c("cell_no", "mean_brightness", "sd_brightness", "x", "y (top to bottom)")


print(cell_stats)
write.csv(cell_stats, file = paste0("output/gfp-analysis/cell-stats/", gfp_names[row_num], " stats.csv"), row.names = FALSE)
}
}
writeImage(gfp_overlay, paste0("output/gfp-analysis/gfp-overlay/", gfp_names[row_num], " overlay.", desired_output_format))
writeImage(gfp_rms, paste0("output/gfp-analysis/gfp-segmented/", gfp_names[row_num], " segmented.", desired_output_format))

} # change_grid_no close

if(change_grid_no) gfp_rois <- "placeholder"

# Grid output ===================================================================
if(!is.null(gfp_rois)) {
if(grid_output) {
cat("Grid output\n")

if(change_grid_no) {
	gfp <- get(paste0("image_", gfp_names[row_num] %>% str_replace_all(" ", "_")))

	if(file.exists(paste0("output/gfp-analysis/gfp-segmented/", gfp_names[row_num], " segmented.", desired_output_format))) {
    	gfp_segmented <- readImage(paste0("output/gfp-analysis/gfp-segmented/", gfp_names[row_num], " segmented.", desired_output_format))
    } else stop("No gfp segmented image found. Please run full script")
    
    if(file.exists(paste0("output/bf-analysis/bf-segmented/", bf_names[row_num], " segmented.", desired_output_format))) {
    	bf_segmented <- readImage(paste0("output/bf-analysis/bf-segmented/", bf_names[row_num], " segmented.", desired_output_format))
	} else stop("No bf segmented image found. Please run cellularity script")
} else gfp_segmented <- gfp_rms

if (nrow(gfp_segmented) %% grid_no != 0) {
	warning("Number of edge segments is not a factor of the edge length of image.\nResizing image accordingly...\n")
	new_size <- nrow(gfp_segmented) - (nrow(gfp_segmented) %% grid_no)
	gfp_segmented <- resize(gfp_segmented, w = new_size, h = new_size)
}

gfp <- gfp[,,1]

grid_size <- nrow(gfp_segmented) / grid_no

gfp_split <- matsplitter(gfp, grid_size, grid_size)
gfp_seg_split <- matsplitter(gfp_segmented, grid_size, grid_size)
bf_seg_split <- matsplitter(bf_segmented, grid_size, grid_size)

d_stats_grid <- matrix(ncol = 5, nrow = grid_no^2)
colnames(d_stats_grid) <- c("Grid x (L>R)", "Grid y (U>D)", "Percentage cells fluorescing", "Mean brightness", "SD in brightness")

for(roi in 1:grid_no^2) {
	d_stats_grid[roi,1] <- ceiling(roi/grid_no)
	if(roi %% grid_no == 0) {
		d_stats_grid[roi,2] <- 4
	} else {
		d_stats_grid[roi,2] <- roi %% grid_no
	}
	d_stats_grid[roi,3:5] <- calc.fluro.stats(gfp_split[,,roi], gfp_seg_split[,,roi], bf_seg_split[,,roi])
}
write.csv(d_stats_grid, file = paste0("output/gfp-analysis/grid-stats/", gfp_names[row_num], " ", grid_no, "x", grid_no, " grid stats.csv"), row.names = FALSE)

}
}
}
if(!change_grid_no){
	write.csv(gfp_overall_stats, file = "output/gfp-analysis/overall_stats.csv", row.names = FALSE)
}