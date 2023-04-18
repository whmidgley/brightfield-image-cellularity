
# ==========================================================================
# Setwd
# ==========================================================================

if (Sys.info()["user"] == "william.midgley") {
  setwd("~/projects/brightfield-image-cellularity")
} else {
  stop("Please add wd\n")
}

suppressWarnings({
file.remove("m_bf.rdata")
file.remove("j.rdata")
file.remove("image_names.rdata")
file.remove("blur.rdata")
file.remove("brightness_mean.rdata")
file.remove("cut_off.rdata")
file.remove("error_factor.rdata")
file.remove("grid_output.rdata")
file.remove("grid_no.rdata")
file.remove("change_grid_no.rdata")
file.remove("flag_thresh.rdata")
file.remove("desired_output_format.rdata")
})

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
	"stringr"
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

# I recomend between 0.0015 and 0.0035
blur <- 0.003

# I recomend between 0.1 and 0.5
brightness_mean <- 0.3

# I recomend between 0.05 and 0.1
cut_off <- 0.08

# I recomend between 0.75 and 1.75
error_factor <- 1.65

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

# enter your desired output image format. Supported formats are tif, tiff, png, jpg and jpeg
desired_output_format <- "tif"
if(!desired_output_format %in% c("tif", "tiff", "png", "jpeg", "jpg")) stop("Output format not supported.\nSupported formats are tif, tiff, png, jpg and jpeg")

# Change to TRUE if you want to manually chage the input format
auto_lif_detect <- FALSE



# this is only relevant if you're dealing with images and not .lif files
# put TRUE if you're dealing with snapshots from leica
# put FALSE if you're dealing with image formats that aren't taken from leica image snapshots
leica_snapshot_flg <- TRUE

vector.OR <- function(vector) {
	sum(vector) > 0 & length(vector) != 0
}
vector.XOR <- function(vector) {
	sum(vector) == 1 & length(vector) != 0
}
vector.AND <- function(vector) {
	sum(vector) == length(vector)
}

filetype.test <- function(file, extension) {
	str_detect(file, paste0(".", extension, "$")) %>% vector.OR()
}

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

check.dim <- function(lif){
	twodim <- if(dim(lif)[3] == 2) TRUE else FALSE

	return(twodim)
}

extract.image <- function(lif_name) {
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
	lif <- extract.image(lif_dirs[i])
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
		image_names[row_num] <- c(paste0(sub('.+/(.+)', '\\1', lif_dirs[i] %>% str_replace(".lif", "")), " Image ", j))
		assign(paste0("image_", image_names[row_num] %>% str_replace_all(" ", "_")), image)
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
			try_cellularities <- suppressWarnings(try(write.csv(try_cellularities, input_file, row.names = FALSE), silent = TRUE))
			if(!is.null(try_cellularities)) {
				stop(paste0(input_file, " is open, please close in order to write over it.\n
					If you want to save the last run, please make another copy by another name"))
			}
		}
}

check.writeable.grid <- function(input_file) {
	if(file.exists(input_file)){
		try_cellularities <- read.csv(input_file)
			try_cellularities <- suppressWarnings(try(write.table(try_cellularities, input_file, row.names = FALSE, col.names = FALSE, sep = ","), silent = TRUE))
			if(!is.null(try_cellularities)) {
				shinyalert(paste0(input_file, " is open"), "please close in order to write over it.\n
					If you want to save the last run, please make a copy by another name", type = "error")
				return(TRUE)
			} else {return(FALSE)}
		} else {return(FALSE)}
}

check.writeable("cellularities.csv")

if(testing) check.writeable("cellularities_test.csv")
if(grid_output) invisible(sapply(paste0("grid-cellularities/", image_names, " ", grid_no, "x", grid_no, " grid.csv"), FUN = check.writeable.grid))

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
