
# ==========================================================================
# Setwd
# ==========================================================================

if (Sys.info()["user"] == "william.midgley") {
  setwd("~/projects/brightfield-image-cellularity")
} else if(Sys.info()["user"] == "molca") {
  setwd("C:/Users/molca/OneDrive - Swansea University/ALR_PhD/Image Analysis Colaboration/brightfield-image-cellularity")
}

suppressWarnings({
file.remove("m_bf.rdata")
file.remove("j.rdata")
file.remove("image_names.rdata")
file.remove("blur.rdata")
file.remove("brightness_mean.rdata")
file.remove("cut_off.rdata")
file.remove("shrink_cutoff.rdata")
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
# Clear loaded packages
# ==========================================================================
if(!is.null(sessionInfo()$otherPkgs)){
	suppressWarnings(
		invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
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
# Functions
# ==========================================================================

# Applies the logic to all bools in a vector (not sure why it's not already a function...)
vector.OR <- function(vector) {
	sum(vector) > 0 & length(vector) != 0
}
vector.XOR <- function(vector) {
	sum(vector) == 1 & length(vector) != 0
}
vector.AND <- function(vector) {
	sum(vector) == length(vector)
}

# Does the file contain a file with this extension?
filetype.test <- function(file, extension) {
	str_detect(file, paste0(".", extension, "$")) %>% vector.OR()
}

# Extract images from lifs
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

# Checks the lif images only have 2 frames: bf and gfp
check.dim <- function(lif){
	twodim <- if(dim(lif)[3] == 2) TRUE else FALSE
	return(twodim)
}

# Checks the csv files aren't open
check.writeable <- function(input_file) {
	if(file.exists(input_file)){
		try_empty <- suppressWarnings(try(read.csv(input_file)))
		if(!(summary(try_empty)[2] == "try-error")) {
			try_cellularities <- read.csv(input_file)
			try_cellularities <- suppressWarnings(try(write.csv(try_cellularities, input_file, row.names = FALSE), silent = TRUE))
			if(!is.null(try_cellularities)) {
				if( "shiny" %in% names(sessionInfo()$otherPkgs)) {
				shinyalert(paste0(input_file, " is open"), "please close in order to write over it.\n
					If you want to save the last run, please make a copy by another name", type = "error")
				} else {
					stop(paste0(input_file, " is open. Please close in order to write over it. If you want to save the last run, please make a copy by another name"))
				}
				return(TRUE)
			} else {return(FALSE)}
		} else {return(FALSE)}
	} else {return(FALSE)}
}

# does the same but removes column names
check.writeable.grid <- function(input_file) {
	if(file.exists(input_file)){
		try_empty <- suppressWarnings(try(read.csv(input_file, header = FALSE)))
		if(!(summary(try_empty)[2] == "try-error")) {
			try_cellularities <- read.csv(input_file, header = FALSE)
			try_cellularities <- suppressWarnings(try(write.table(try_cellularities, input_file, row.names = FALSE, col.names = FALSE, sep = ","), silent = TRUE))
			if(!is.null(try_cellularities)) {
				if( "shiny" %in% names(sessionInfo()$otherPkgs)) {
				shinyalert(paste0(input_file, " is open"), "please close in order to write over it.\n
					If you want to save the last run, please make a copy by another name", type = "error")
				} else {
					stop(paste0(input_file, " is open. Please close in order to write over it. If you want to save the last run, please make a copy by another name"))
				}
				return(TRUE)
			} else {return(FALSE)}
		} else {return(FALSE)}
	} else {return(FALSE)}
}


matsplitter <- function(m, r, c) {
    rg <- (row(m)-1)%/%r+1
    cg <- (col(m)-1)%/%c+1
    cri <- (rg-1)*max(cg) + cg
    n <- prod(dim(m))/c/r
    cv <- unlist(lapply(1:n, function(x) m[cri==x]))
    dim(cv)<-c(r,c,n)
    cv
}

calc.cellularity <- function(mat_seg) {

    prop_background_seg <-
  		mat_seg[mat_seg == 0] %>%
    	length() / nrow(mat_seg)^2

    comp_cell <-
    	(1-prop_background_seg)*100
    return(comp_cell)
}

calc.fluro.stats <- function(gfp_mat_seg, gfp_mat_seg_segd, bf_mat_seg_segd) {
	fluro_prop <- sum(gfp_mat_seg_segd)*100/sum(bf_mat_seg_segd)
	fluro_mean <- mean(gfp_mat_seg[gfp_mat_seg_segd == 1], na.rm = TRUE)
	fluro_sd <- sd(gfp_mat_seg[gfp_mat_seg_segd == 1], na.rm = TRUE)
	fluro_stats <- c(
		fluro_prop,
		fluro_mean,
		fluro_sd
		)
	return(fluro_stats)
}