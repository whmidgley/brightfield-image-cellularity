
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

reports <- list.files(path = "brightfield-images/reports", recursive = FALSE, full.names = TRUE) 

human_cellularities <- data.frame(matrix(nrow=length(reports), ncol=2))
colnames(human_cellularities) <- c("image_name", "human_cellularity")

for (j in 1:length(reports)) {
	cat("Image",j,"=================\n")
	path <- paste0(reports[j])
	source("02a_calc_cellularity.r")


	human_cellularities[j,] <- c(sub('.+/(.+)', '\\1', reports[j]), print(human_cellularity))
}

cellularities <- cbind(automated_cellularities, human_cellularities[,2])

cellularities %>% mutate(
	automated_cellularity = cellularity,
	error = automated_cellularity - human_cellularity
	) %>%
	select(
		image_name,
		automated_cellularity,
		human_cellularity,
		error
		)

write.csv(human_cellularities, "human_cellularities.csv", row.names = FALSE)
write.csv(cellularities, "cellularities.csv", row.names = FALSE)
beep()