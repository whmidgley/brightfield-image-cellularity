
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
	"RBioFormats",
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

blur <- 1.464

brightness_mean <- 0.3

cut_off <- 0.0723

error_factor <- 1.5

grid_no <- 4

flag_thresh <- 10

# ==========================================================================
# Load images
# ==========================================================================

lif_dirs <- list.files(path = "brightfield-images", pattern = "lif$", recursive = FALSE, full.names = TRUE) 


extract.image <- function(lif_name) {
	lif <- read.image(lif_name)
	no_images <- length(lif)/2
	lif_seq <- no_images + c(1:no_images)
	
	return(lif[lif_seq])
}

image_names <- c()
for(i in c(1:length(lif_dirs))) {
	lif <- extract.image(lif_dirs[i])
	for(j in c(1:length(lif))) {
		image <- lif[[j]]
		row_num <- length(lif)*(i-1) + j 
		image_names[row_num] <- c(paste0(sub('.+/(.+)', '\\1', lif_dirs[i] %>% str_replace(".lif", "")), " Image ", j))
		assign(paste0("image_", image_names[row_num] %>% str_replace_all(" ", "_")), image)
	}
}


cellularities <- data.frame(matrix(nrow=length(image_names), ncol=2))
colnames(cellularities) <- c("image_name", "cellularity")

# ==========================================================================
# Check I have the same number of reports as images
# ==========================================================================

reports <- list.files("Chart_0.csv", path = "brightfield-images/reports", recursive = TRUE, full.names = TRUE) 

if (length(image_names) != length(reports)) warning("No. images does not equal no. reports")

# ==========================================================================
# Calculate cellularities
# ==========================================================================

auto_cellularities <- data.frame(matrix(ncol = 3, nrow = length(image_names)))
colnames(auto_cellularities) <- c("name", "cellularity", "high_compensation_flag")


for (j in 1:length(image_names)) {
	cat("Image",j,"=================\n")
	m_bf <- suppressWarnings(get(paste0("image_", image_names[j] %>% str_replace_all(" ", "_"))))

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