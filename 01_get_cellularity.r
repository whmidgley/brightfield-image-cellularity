
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

options(digits=20)  # this allows R to use decimals up to 20 places
options(repr.plot.width = 15, repr.plot.height = 20)

# ==========================================================================
# Perform an example k-means
# ==========================================================================

m_bf <- suppressWarnings(readImage("bf_example.tif"))

source("01a_remove_gradient.r")
source("01b_detect_edges.r")

m_bf_blur <- m_bf_blur/max(m_bf_blur)
d_bf_blur <- data.frame(matrix(m_bf_blur, ncol=1))

k_bf_blur <- kmeans(d_bf_blur, 5)

d_bf_blur_k <- d_bf_blur

d_bf_blur_k$label <- k_bf_blur$cluster

colours1 <- data.frame(k_bf_blur$centers, c(1:5))
colnames(colours1) <- c("centres","label")

centres 		<- colours1$centres
background 	<- min(colours1$centres)

# ==========================================================================
# Calculate cellularities
# ==========================================================================

images <- list.files(path = "brightfield-images", pattern = "tif", recursive = FALSE, full.names = TRUE) 

cellularities <- data.frame(matrix(nrow=length(images), ncol=2))
colnames(cellularities) <- c("image_name", "cellularity")

for (j in 1:length(images)) {
	cat("Image",j,"=================\n")
	m_bf <- readImage(paste0(images[j]))

source("01a_remove_gradient.r")
source("01b_detect_edges.r")
source("01c_k-means.r")

cellularities[j,] <- c(sub('.+/(.+)', '\\1', images[j]), print(computer_cellularity))
}

write.csv(cellularities, "automated_cellularities.csv", row.names = FALSE)
beep()