
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

automated_cellularities <- read.csv("automated_cellularities.csv")

# ==========================================================================
# Calculate cellularities
# ==========================================================================

reports <- list.files("Chart_0.csv", path = "brightfield-images/reports", recursive = TRUE, full.names = TRUE) 

human_cellularities <- data.frame(matrix(nrow=length(reports), ncol=2))
colnames(human_cellularities) <- c("image_name", "human_cellularity")
image_names <- strsplit(reports, split = "/")
image_names <- as.data.frame(image_names)
image_names <- paste(image_names[4,], image_names[5,])

auto_names <- strsplit(automated_cellularities[,1], split = " ") %>% as.data.frame()
auto_names <- paste(auto_names[1,], auto_names[2,], auto_names[3,], auto_names[4,] %>% str_extract("Image..."))

human_names <- strsplit(image_names, split = " ") %>% as.data.frame()
human_names <- paste(human_names[1,], human_names[2,], human_names[3,], human_names[5,] %>% str_extract("Image..."))


for (j in 1:length(reports)) {
	cat("Image",j,"=================\n")
	path <- paste0(reports[j])
	source("02a_calc_cellularity.r")

	human_cellularities[j,] <- c(image_names[j], print(human_cellularity))
}

human_cellularities <- cbind(human_cellularities, human_names)
automated_cellularities <- cbind(automated_cellularities, auto_names)

cellularities <-
full_join(automated_cellularities, human_cellularities, by = c("auto_names" = "human_names")) %>% mutate(
	name = auto_names,
	auto_cellularity = mean_cellularities,
	error = as.numeric(mean_cellularities) - as.numeric(human_cellularity),
	over_10perc_off_flg = error > 10
	) %>% select(
	name,
	auto_cellularity,
	human_cellularity,
	error,
	over_10perc_off_flg
	)

write.csv(human_cellularities, "human_cellularities.csv", row.names = FALSE)
write.csv(cellularities, "cellularities.csv", row.names = FALSE)
beep()