
# ==========================================================================
# Setwd
# ==========================================================================

if (Sys.info()["user"] == "william.midgley") {
  setwd("C:/Users/william.midgley/OneDrive - Swansea University/Documents/projects/Amy's PhD/cell-analysis")
} else {
  cat("Please add wd\n")
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
	"e1071",
	"EBImage",
	"patchwork",
	"plot3D",
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

blur <- 1.5

brightness_mean <- 0.35

cut_off <- 0.08

error_factor <- 1.5

grid_no <- 4

flag_thresh <- 15

# ==========================================================================
# Load images
# ==========================================================================

images <- list.files(path = "brightfield-images", pattern = "tif", recursive = FALSE, full.names = TRUE) 

cellularities <- data.frame(matrix(nrow=length(images), ncol=2))
colnames(cellularities) <- c("image_name", "cellularity")

image_names <- sub('.+/(.+)', '\\1', images) %>% str_replace("Effectene.lif_", "") %>% str_replace("Snapshot1.tif", "") %>% str_replace(".lif_", " ")

# ==========================================================================
# Check I have the same number of reports as images
# ==========================================================================

reports <- list.files("Chart_0.csv", path = "brightfield-images/reports", recursive = TRUE, full.names = TRUE) 

if (length(images) != length(reports)) warning("No. images does not equal no. reports")

# ==========================================================================
# Calculate cellularities
# ==========================================================================

auto_cellularities <- data.frame(matrix(ncol = 3, nrow = length(image_names)))
colnames(auto_cellularities) <- c("name", "cellularity", "high_compensation_flag")


for (j in 1:length(images)) {
	cat("Image",j,"=================\n")
	m_bf <- suppressWarnings(readImage(paste0(images[j])))

if(!file.exists(paste0("normalised-images/", image_names[j], " normalised.tif"))) {
	source("01a_remove_gradient.r")
}
source("01b_detect_edges.r")
source("01c_cut_off.r")
source("01d_by_grid.r")

auto_cellularities[j,] <- c(image_names[j], print(computer_cellularity), case_when((1-prop_background_edge)*error_factor*100 > flag_thresh ~ "CHECK OUTLINE",
																									TRUE ~ "image normal"))
}

write.csv(auto_cellularities, "automated_cellularities.csv", row.names = FALSE)

#source("02_get_human_cellularity.r")

auto_cellularities <- read.csv("automated_cellularities.csv")
human_cellularities <- read.csv("human_cellularities_fixed.csv")[,c(1:2)]
cellularities <- inner_join(human_cellularities, auto_cellularities, by = c(human_name = "name"))

cellularities$error <- cellularities$cellularity - cellularities$human_cellularity

colnames(cellularities) <- c("name", "human_cellularity", "automated_cellularity", "high_compensation_flag", "error")

write.csv(cellularities, "cellularities.csv", row.names = FALSE)

mean_sqerror <- mean(cellularities$error^2, na.rm = TRUE)

cat("Mean square error is", mean_sqerror, "\n")

beep() 