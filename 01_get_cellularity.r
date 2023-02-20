
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
# Perform example k-means
# ==========================================================================

images <- list.files(path = "brightfield-images", pattern = "tif", recursive = FALSE, full.names = TRUE) 

# sample 30%
sample_size <- round(length(images)*0.3)
samples <- sample(images, size = sample_size)

image_sizes <- c()
for (j in 1:sample_size) {
	assign(paste0("m_bf"),
		suppressWarnings(readImage(paste0(samples[j])))
		)

	source("01a_remove_gradient.r")
	source("01b_detect_edges.r")
	
	image_sizes[j] <- ncol(m_bf_blur)

	assign(paste0("m_sample_blur", j),
		m_bf_blur
		)
}

min_size <- min(image_sizes)

d_sample_blur <- c()
for (j in 1:sample_size) {
	assign(paste0("m_sample_blur", j),
		get(paste0("m_sample_blur", j))[1:min_size, 1:min_size]
		)
	d_sample_blur[j] <- get(paste0("m_sample_blur", j)) %>% matrix(ncol = 1) %>% as.data.frame()
}

d_sample_blur_long <- d_sample_blur[1]
	for (j in 2:sample_size) {
		assign("d_sample_blur_long",
			rbind(d_sample_blur_long, d_sample_blur[j]))
	}

d_sample_blur_long <- cbind(d_sample_blur_long, d_sample_blur_long, d_sample_blur_long)

m_sample_blur <- unlist(d_sample_blur_long) %>% array(dim = c(min_size, sample_size*min_size))


m_sample_blur <- m_sample_blur/max(m_sample_blur)
d_sample_blur <- data.frame(matrix(m_sample_blur, ncol=1))

k_sample_blur <- kmeans(d_sample_blur, 5)

d_sample_blur_k <- d_sample_blur

d_sample_blur_k$label <- k_sample_blur$cluster

colours1 <- data.frame(k_sample_blur$centers, c(1:5))
colnames(colours1) <- c("centres","label")

centres 		<- colours1$centres
background 	<- min(colours1$centres)

# ==========================================================================
# Calculate cellularities
# ==========================================================================

cellularities <- data.frame(matrix(nrow=length(images), ncol=2))
colnames(cellularities) <- c("image_name", "cellularity")

for (j in 1:length(images)) {
	cat("Image",j,"=================\n")
	m_bf <- suppressWarnings(readImage(paste0(images[j])))

source("01a_remove_gradient.r")
source("01b_detect_edges.r")
source("01c_k-means.r")

cellularities[j,] <- c(sub('.+/(.+)', '\\1', images[j]), print(computer_cellularity))
}

write.csv(cellularities, "automated_cellularities.csv", row.names = FALSE)
beep()
