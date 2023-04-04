
# ==========================================================================
# Setwd
# ==========================================================================

if (Sys.info()["user"] == "william.midgley") {
  setwd("C:/Users/william.midgley/OneDrive - Swansea University/Documents/projects/Amy's PhD/cell-analysis")
} else {
  stop("Please add wd\n")
}

# ==========================================================================
# Clear environment
# ==========================================================================

rm(list = ls())

if(!is.null(names(sessionInfo()$otherPkgs))) {
	suppressWarnings(
		invisible(
			lapply(
				paste0("package:",
					names(sessionInfo()$otherPkgs)),
					detach,
					character.only = TRUE,
					unload = TRUE
					)
			)
	)
}
# ==========================================================================
# Load
# ==========================================================================

pkgs <- c(
	"tidyverse",
	"beepr",
	"EBImage",
	"RBioFormats",
	"readr",
	"stringr",
	"grid",
	"gridExtra"
	)

for (pkg in pkgs) {
	suppressWarnings(
		suppressPackageStartupMessages(
			library(pkg, character.only = TRUE)
			)
		)
}

options(repr.plot.width = 15, repr.plot.height = 20)




# ==========================================================================
# Variables
# ==========================================================================

# I recomend between 0.0015 and 0.0025
blur <- 0.003

# I recomend between 0.1 and 0.5
brightness_mean <- 0.3

# I recomend between 0.05 and 0.1
cut_off <- 0.08

# I recomend between 0.75 and 1.75
error_factor <- 1.65

# Do you want the cellularity to be outputted as a grid?
grid_output <- TRUE

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
if(!exists("error_factor")) error_factor <- 1.65
if(!exists("grid_no")) grid_no <- 4
if(!exists("flag_thresh")) flag_thresh <- 15
if(!exists("testing")) testing <- FALSE
if(!exists("grid_output")) grid_output <- TRUE
if(!grid_output & change_grid_no) warning("grid_output is FALSE but change_grid_no is TRUE.\nIgnoring change_grid_no and doing a full run")

# ==========================================================================
# Check folders exist
# ==========================================================================

if(!dir.exists("input-images")) dir.create("input-images")
if(!dir.exists("grid-cellularities") & grid_output) dir.create("grid-cellularities")
if(!dir.exists("normalised-images")) dir.create("normalised-images")
if(!dir.exists("overlay-images")) dir.create("overlay-images")
if(!dir.exists("segmented-images")) dir.create("segmented-images")

# ==========================================================================
# File formats
# ==========================================================================

# enter your desired output image format. Supported formats are tif, tiff, png and jpeg
desired_output_format <- "tif"
if(!desired_output_format %in% c("tif", "tiff", "png", "jpeg")) stop("Output format not supported.\nSupported formats are tif, tiff, png and jpeg")

# Change to TRUE if you want to manually chage the input format
auto_lif_detect <- FALSE



# this is only relevant if you're dealing with images and not .lif files
# put TRUE if you're dealing with snapshots from leica
# put FALSE if you're dealing with image formats that aren't taken from leica image snapshots
leica_snapshot_flg <- TRUE

vector.OR <- function(vector) {
	sum(vector) > 0 & length(vector) != 0
}
vector.AND <- function(vector) {
	sum(vector) == length(vector)
}

if(auto_lif_detect) {
	# input format can be lif or tif
	input_format <- "lif"
} else {
	image_file_contents <- list.files(path = "input-images", recursive = FALSE, full.names = TRUE)

	if(length(image_file_contents) == 0) {
		stop("File input-images is empty. Please add images or lifs")
	}
	if(str_detect(image_file_contents, ".tif$") %>% vector.OR() && !str_detect(image_file_contents, ".(tiff)|(jpeg)|(png)|(lif)$") %>% vector.AND()) {
		cat("Tifs detected. Switching to tif mode\n")
		input_format <- "tif"
	}
	if(str_detect(image_file_contents, ".tiff$") %>% vector.OR() && !str_detect(image_file_contents, ".(tif)|(jpeg)|(png)|(lif)$") %>% vector.AND()) {
		cat("Tiffs detected. Switching to tiff mode\n")
		input_format <- "tiff"
	}
	if(str_detect(image_file_contents, ".png$") %>% vector.OR() && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(lif)$") %>% vector.AND()) {
		cat("Pngs detected. Switching to png mode\n")
		input_format <- "png"
	}
	if(str_detect(image_file_contents, ".jpeg$") %>% vector.OR() && !str_detect(image_file_contents, ".(tif)|(tiff)|(png)|(lif)$") %>% vector.AND()) {
		cat("Jpegs detected. Switching to jpeg mode\n")
		input_format <- "jpeg"
	}
	if(str_detect(image_file_contents, ".lif$") %>% vector.OR() && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(png)$") %>% vector.AND()) {
		cat("Lifs detected. Switching to lif mode\n")
		input_format <- "lif"
	}
	if(str_detect(image_file_contents, ".lif$") %>% vector.OR() && str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(png)$") %>% vector.OR()) {
		stop("Multiple file formats detected. Please remove unwanted file format")
	}
	if(!str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(png)|(lif)$") %>% vector.OR()) {
		stop("Files detected are neither lifs nor images. Please add images or lifs\n
			Supported file formats are .lif, .tif, .tiff, .png, and .jpeg")
	} else if((!str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(png)|(lif)$")) %>% vector.OR()) {
		warning("Other files detected which are neither lifs nor images")
	}
}

# ==========================================================================
# Load images
# ==========================================================================

if(input_format == "lif") {

lif_dirs <- list.files(path = "input-images", pattern = "lif$", recursive = FALSE, full.names = TRUE) 

extract.image <- function(lif_name) {
	lif <- read.image(lif_name)
	no_images <- length(lif)/2
	lif_seq <- c(1:no_images)
	
	return(lif[lif_seq])
}

image_names <- c()
for(i in c(1:length(lif_dirs))) {
	lif <- extract.image(lif_dirs[i])
	for(j in c(1:length(lif))) {
		image_frame <- lif[[j]]
		image <- array(dim = c(dim(image_frame)[c(1:2)], 3))
		image[,,1] <- image_frame[,,2]
		image[,,2] <- image_frame[,,2]
		image[,,3] <- image_frame[,,2]
		image <- Image(image, colormode = "Color")
		row_num <- length(lif)*(i-1) + j 
		image_names[row_num] <- c(paste0(sub('.+/(.+)', '\\1', lif_dirs[i] %>% str_replace(".lif", "")), " Image ", j))
		assign(paste0("image_", image_names[row_num] %>% str_replace_all(" ", "_")), image)
	}
}
} else {

images <- list.files(path = "input-images", pattern = input_format, recursive = FALSE, full.names = TRUE) 

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
check.writeable <- function(input_file) {
	if(file.exists(input_file)){
		try_cellularities <- read.csv(input_file)
			try_cellularities <- suppressWarnings(try(write.csv(try_cellularities, input_file), silent = TRUE))
			if(!is.null(try_cellularities)) {
				stop(paste0(input_file, " is open, please close in order to write over it.\n
					If you want to save the last run, please make another copy by another name"))
			}
		}
}

check.writeable("cellularities.csv")
if(testing) check.writeable("cellularities_test.csv")
if(grid_output) invisible(sapply(paste0("grid-cellularities/", image_names, " ", grid_no, "x", grid_no, " grid.csv"), FUN = check.writeable))

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

if(!(change_grid_no & grid_output)) {
if(file.exists(paste0("normalised-images/", image_names[j], " normalised.", desired_output_format))) {
	pre_normalised <- readImage(paste0("normalised-images/", image_names[j], " normalised.", desired_output_format))
	if(!((dim(pre_normalised) == dim(m_bf)) %>% vector.AND())) source("01a_remove_gradient.r")
	rm("pre_normalised")
} else {source("01a_remove_gradient.r")}

source("01b_detect_edges.r")
source("01c_cut_off.r")
}
if(grid_output) {
source("01d_by_grid.r")
}
if(!(change_grid_no & grid_output)) {
auto_cellularities[j,] <- c(image_names[j], print(computer_cellularity), case_when((1-prop_background_edge)*error_factor*100 > flag_thresh ~ "CHECK OUTLINE",
																									TRUE ~ "image normal"))
}
}

write.csv(auto_cellularities, "cellularities.csv", row.names = FALSE)


cellularities <- read.csv("cellularities.csv")

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