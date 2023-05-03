
source("r_clear_and_load.r")

# ==========================================================================
# Variables
# ==========================================================================

# I recomend between 0.0015 and 0.0035
blur <- 0.003

# I recomend between 0.1 and 0.5
brightness_mean <- 0.3

# I recomend between 0.05 and 0.1
cut_off <- 0.08

# 0.95 looks good
shrink_cutoff <- 0.95

# Do you want the cellularity to be outputted as a grid?
grid_output <- FALSE

# whatever you want (provided it's below ~600)
grid_no <- 4

# If you just want to change grid_no on the same variables as a previous run change this to TRUE
# N.B. it will still do a full run if you haven't run it before
change_grid_no <- FALSE

# I recomend between 10 and 20
flag_thresh <- 15

# This is if you have a file called human_cellularities.csv which contains human calculated cellularities for comparison
testing <- FALSE

# set defaults if variables are undefined
if(!exists("blur")) blur <- 0.003
if(!exists("brightness_mean")) brightness_mean <- 0.3
if(!exists("cut_off")) cut_off <- 0.08
if(!exists("shrink_cutoff")) shrink_cutoff <- 0.95
if(!exists("grid_no")) grid_no <- 4
if(!exists("flag_thresh")) flag_thresh <- 15
if(!exists("testing")) testing <- FALSE
if(!exists("grid_output")) grid_output <- TRUE
if(!grid_output & change_grid_no) warning("grid_output is FALSE but change_grid_no is TRUE.\nIgnoring change_grid_no and doing a full run")

# ==========================================================================
# Check folders exist
# ==========================================================================

if(!dir.exists("input")) dir.create("input")
if(!dir.exists("output/bf-analysis/grid-cellularities") & grid_output) dir.create("output/bf-analysis/grid-cellularities")
if(!dir.exists("output/bf-analysis/bf-normalised")) dir.create("output/bf-analysis/bf-normalised")
if(!dir.exists("output/bf-analysis/bf-overlay")) dir.create("output/bf-analysis/bf-overlay")
if(!dir.exists("output/bf-analysis/bf-segmented")) dir.create("output/bf-analysis/bf-segmented")

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

check.dim <- function(lif){
	twodim <- if(dim(lif)[3] == 2) TRUE else FALSE

	return(twodim)
}

extract.bf <- function(lif_name) {
	lif <- read.image(lif_name)
	if(is.null(dim(lif))) {
		twodims <- sapply(lif, FUN = check.dim)
		twodim_images <- lif[twodims]
	} else {
		twodim_images <- lif
	}
	return(twodim_images)
}

image_names <- c()
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
		image <- array(dim = c(dim(image_frame)[c(1:2)], 3))
		image[,,1] <- image_frame[,,2]
		image[,,2] <- image_frame[,,2]
		image[,,3] <- image_frame[,,2]
		image <- Image(image, colormode = "Color")
		if(i == 1) {
			row_num <- j
		} else {
			row_num <- sum(lif_lengths[1:(i-1)]) + j
		}
		image_names[row_num] <- c(paste0(sub('.+/(.+)', '\\1', lif_dirs[i] %>% str_replace(".lif", "")), " BF ", j))
		assign(paste0("image_", image_names[row_num] %>% str_replace_all(" ", "_")), image)
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
}
 
if(testing){
	# only test images I have the human counterpart for
	human_cellularities <- read.csv("human_cellularities_fixed.csv")[,c(1:2)]
	images <- images[image_names %in% human_cellularities[,1]]
	image_names <- image_names[image_names %in% human_cellularities[,1]]
}

# ==========================================================================
# Check output file isn't open
# ==========================================================================


check.writeable("output/bf-analysis/cellularities.csv")

if(testing) check.writeable("cellularities_test.csv")
if(grid_output) invisible(sapply(paste0("output/bf-analysis/grid-cellularities/", image_names, " ", grid_no, "x", grid_no, " grid.csv"), FUN = check.writeable.grid))

# ==========================================================================
# Calculate cellularities
# ==========================================================================

auto_cellularities <- data.frame(matrix(ncol = 3, nrow = length(image_names)))
colnames(auto_cellularities) <- c("name", "cellularity", "high_compensation_flag")


for (j in 1:length(image_names)) {
	cat("Image",j,"=================\n")
	if(input_format == "lif") {
		m_bf <- suppressWarnings(get(paste0("image_", image_names[j] %>% str_replace_all(" ", "_"))))
		} else {
		m_bf <- suppressWarnings(readImage(paste0(images[j])))
	}

	if(length(dim(m_bf)) == 2) {
		make_colour <- array(dim = c(dim(m_bf), "3"))
		make_colour[,,1] <- m_bf
		make_colour[,,2] <- m_bf
		make_colour[,,3] <- m_bf
		m_bf <- make_colour
	}

if(!(change_grid_no & grid_output)) {
if(file.exists(paste0("output/bf-analysis/bf-normalised/", image_names[j], " normalised.", desired_output_format))) {
	pre_normalised <- readImage(paste0("output/bf-analysis/bf-normalised/", image_names[j], " normalised.", desired_output_format))
	if(!((dim(pre_normalised) == dim(m_bf)) %>% vector.AND())) source("scripts/01a_remove_gradient.r")
	rm("pre_normalised")
} else {source("scripts/01a_remove_gradient.r")}

source("scripts/01b_detect_edges.r")
source("scripts/01c_cut_off.r")
}
if(grid_output) {
source("scripts/01d_by_grid.r")
}
if(!(change_grid_no & grid_output)) {
auto_cellularities[j,] <- c(image_names[j], print(computer_cellularity), case_when((1-prop_background_edge)*100 > flag_thresh ~ "CHECK OUTLINE",
																									TRUE ~ "image normal"))
}
}

write.csv(auto_cellularities, "output/bf-analysis/cellularities.csv", row.names = FALSE)


cellularities <- read.csv("output/bf-analysis/cellularities.csv")

if(testing) {
cellularities_test <- inner_join(human_cellularities, cellularities, by = c(human_name = "name"))

cellularities_test$error <- cellularities_test$cellularity - cellularities_test$human_cellularity

colnames(cellularities_test) <- c("name", "human_cellularity", "automated_cellularity", "high_compensation_flag", "error")

write.csv(cellularities_test, "cellularities_test.csv", row.names = FALSE)

mean_sqerror <- mean(cellularities_test$error^2, na.rm = TRUE)
mean_abs_error <- mean(abs(cellularities_test$error), na.rm = TRUE)
mean_error <- mean(cellularities_test$error, na.rm = TRUE)

cat("Mean square error is", mean_sqerror, "\n")
cat("Mean absolute error is", mean_abs_error, "\n")
cat("Mean error is", mean_error, "\n")
}

beep()
