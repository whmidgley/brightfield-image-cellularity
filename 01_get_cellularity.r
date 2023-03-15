
# ==========================================================================
# Setwd
# ==========================================================================

if (Sys.info()["user"] == "william.midgley") {
  setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")
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
# Load images
# ==========================================================================

images <- list.files(path = "brightfield-images", pattern = "tif", recursive = FALSE, full.names = TRUE) 

cellularities <- data.frame(matrix(nrow=length(images), ncol=2))
colnames(cellularities) <- c("image_name", "cellularity")

image_names <- sub('.+/(.+)', '\\1', images)

# ==========================================================================
# Calculate cellularities
# ==========================================================================

auto_cellularities <- data.frame(matrix(ncol = 2, nrow = length(image_names)))
colnames(auto_cellularities) <- c("name", "cellularity")

for (j in 1:length(images)) {
	cat("Image",j,"=================\n")
	m_bf <- suppressWarnings(readImage(paste0(images[j])))

source("01a_remove_gradient.r")
source("01b_detect_edges.r")
source("01c_cut_off.r")

auto_cellularities[j,] <- c(image_names[j], print(computer_cellularity))
}

write.csv(auto_cellularities, "automated_cellularities.csv", row.names = FALSE)

source("02_get_human_cellularity.r")

mean_error <- mean(cellularities$error, na.rm = TRUE)

cat("Mean error is", mean_error, "\n")

beep()