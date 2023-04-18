if(file.exists("m_bf.rdata")) load("m_bf.rdata")
if(file.exists("j.rdata")) load("j.rdata")
if(file.exists("image_names.rdata")) load("image_names.rdata")
if(file.exists("blur.rdata")) load("blur.rdata")
if(file.exists("brightness_mean.rdata")) load("brightness_mean.rdata")
if(file.exists("cut_off.rdata")) load("cut_off.rdata")
if(file.exists("error_factor.rdata")) load("error_factor.rdata")
if(file.exists("grid_output.rdata")) load("grid_output.rdata")
if(file.exists("grid_no.rdata")) load("grid_no.rdata")
if(file.exists("change_grid_no.rdata")) load("change_grid_no.rdata")
if(file.exists("flag_thresh.rdata")) load("flag_thresh.rdata")
if(file.exists("desired_output_format.rdata")) load("desired_output_format.rdata")


if(change_grid_no) {
    m_bf_segmented <- readImage(paste0("segmented-images/", image_names[j], " segmented.", desired_output_format))
    m_bf_overlay <- readImage(paste0("overlay-images/", image_names[j], " overlay.", desired_output_format))
    m_bf_edges <- array(as.numeric(m_bf_overlay[,,2] != m_bf_overlay[,,1]), dim = dim(m_bf_overlay[,,1])) %>% Image()
}

if (nrow(m_bf_segmented) %% grid_no != 0) {
	warning("Number of edge segments is not a factor of the edge length of image.\nResizing image accordingly...\n")
	new_size <- nrow(m_bf_segmented) - (nrow(m_bf_segmented) %% grid_no)
	m_bf_segmented <- resize(m_bf_segmented, w = new_size, h = new_size)
	m_bf_edges <- resize(m_bf_edges, w = new_size, h = new_size)
}

grid_size <- nrow(m_bf_segmented) / grid_no

matsplitter <- function(m, r, c) {
    rg <- (row(m)-1)%/%r+1
    cg <- (col(m)-1)%/%c+1
    cri <- (rg-1)*max(cg) + cg
    n <- prod(dim(m))/c/r
    cv <- unlist(lapply(1:n, function(x) m[cri==x]))
    dim(cv)<-c(r,c,n)
    cv
}

calc.cellularity <- function(mat_seg, mat_edge) {
	prop_background_edge <-
  		mat_edge[mat_edge == 0] %>%
    	length() / nrow(mat_edge)^2

    prop_background_seg <-
  		mat_seg[mat_seg == 0] %>%
    	length() / nrow(mat_seg)^2

    comp_cell <-
    	((1-prop_background_seg)-(1-prop_background_edge)*error_factor)*100
    return(comp_cell)
}


m_bf_seg_split <- matsplitter(m_bf_segmented, grid_size, grid_size)

m_bf_edge_split <- matsplitter(m_bf_edges, grid_size, grid_size)

d_cellularity_grid <- matrix(ncol = 1, nrow = grid_no^2)

for(roi in 1:grid_no^2) {
	d_cellularity_grid[roi] <- calc.cellularity(m_bf_seg_split[,,roi], m_bf_edge_split[,,roi])
}

m_bf_overlay_grid <- m_bf_overlay

for(grid_no_seq in 1:(grid_no-1)) {
    m_bf_overlay_grid[, c(floor(grid_no_seq*grid_size), ceiling(grid_no_seq*grid_size)), 3] <- 1
    m_bf_overlay_grid[c(floor(grid_no_seq*grid_size), ceiling(grid_no_seq*grid_size)), , 3] <- 1

    m_bf_overlay_grid[, c(floor(grid_no_seq*grid_size), ceiling(grid_no_seq*grid_size)), 2] <- 0
    m_bf_overlay_grid[c(floor(grid_no_seq*grid_size), ceiling(grid_no_seq*grid_size)), , 2] <- 0
    m_bf_overlay_grid[, c(floor(grid_no_seq*grid_size), ceiling(grid_no_seq*grid_size)), 1] <- 0
    m_bf_overlay_grid[c(floor(grid_no_seq*grid_size), ceiling(grid_no_seq*grid_size)), , 1] <- 0
}
plot(m_bf_overlay_grid)

m_cellularity_grid <- matrix(d_cellularity_grid, ncol = grid_no)

m_cellularity_grid[m_cellularity_grid < 0] <- 0

if(exists("image_names")) {
write.table(m_cellularity_grid, paste0("grid-cellularities/", image_names[j], " ", grid_no, "x", grid_no, " grid.csv"), row.names = FALSE, col.names = FALSE, sep = ",")
}