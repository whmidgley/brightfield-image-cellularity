cat("Clear and load!\n")

setwd("C:/Users/william.midgley/Documents/Personal development/Amy's PhD/cell-analysis")

#rm(list = ls())

library(tidyverse)
library(EBImage)
#library(tiff)
library(png)
library(grid)
library(glue)
library(gridExtra)
library(e1071)
library(RImageJROI)
#library(caTools)
#library(class)
options(digits=20)  # this allows R to use decimals up to 20 places
options(repr.plot.width = 15, repr.plot.height = 20)

# ==========================================================================
# Read images
# ==========================================================================

load("r-objects/m_bf1_normal.RData")

m_bf <- readImage("brightfield-images/bf17.tif")
#m_bf <- array(m_bf, dim = dim(m_bf))
#m_bf_gs <- m_bf[,,1]
#m_bf_gs_normal <- m_bf_normal[,,1]


roi <- paste(readLines("brightfield-images/rois/roi17.roi"), collapse="\n")

plot(str_extract_all(roi,"(?<=X=\").+(?=\" Y=)") %>% unlist() %>% as.numeric(), str_extract_all(roi,"(?<=Y=\").+(?=\")") %>% unlist() %>% as.numeric())

roi_attachment <- str_extract_all(roi, "(?s)<Attachment\\s*(.*?)\\s*</Attachment>") %>% unlist()

remove.negative <- function(roi) {
	roi[roi<0] <- 0
	roi[roi>nrow(m_bf)] <- nrow(m_bf)
	return(roi)
}


for (j in 1:length(roi_attachment)) {
	assign(paste0("roi_attachment_",j,"_x"),
	str_extract_all(roi_attachment[j],"(?<=X=\").+(?=\" Y=)") %>% unlist() %>% as.numeric() %>% remove.negative())
}

for (j in 1:length(roi_attachment)) {
	assign(paste0("roi_attachment_",j,"_y"),
	str_extract_all(roi_attachment[j],"(?<=Y=\").+(?=\")") %>% unlist() %>% as.numeric() %>% remove.negative())
}

complete.roi <- function(roi) {
	append(roi, roi[1])
}

for (j in 1:length(roi_attachment)) {
	assign(paste0("roi_attachment_",j,"_x"),
	complete.roi(get(paste0("roi_attachment_",j,"_x"))))
}

for (j in 1:length(roi_attachment)) {
	assign(paste0("roi_attachment_",j,"_y"),
	complete.roi(get(paste0("roi_attachment_",j,"_y"))))
}



plot <- c("ggplot() + ")

for (j in 1:(length(roi_attachment)-1)) {
	plot <- append(plot, paste0("geom_polygon(aes(x=roi_attachment_",j,"_x, y=roi_attachment_",j,"_y)) + "))
}
plot <- append(plot, paste0("geom_polygon(aes(x=roi_attachment_",length(roi_attachment),"_x, y=roi_attachment_",length(roi_attachment),"_y))"))

plot <- paste(unlist(plot), collapse="")

roi_plot <- eval(parse(text=plot)) +
		scale_x_continuous(limits = c(0,nrow(m_bf)), expand = c(0, 0)) +
  		scale_y_continuous(limits = c(0,ncol(m_bf)), expand = c(0, 0)) +
		theme_void()


ggsave(plot=roi_plot, "roi_plot.tiff", device = "tiff", dpi = 1, width = ncol(m_bf), height = nrow(m_bf), limitsize = FALSE)





