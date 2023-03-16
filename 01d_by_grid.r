
grid_no <- 4

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
    rci <- (cg-1)*max(rg) + rg
    n <- prod(dim(m))/r/c
    cv <- unlist(lapply(1:n, function(x) m[rci==x]))
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
mb


m_bf_seg_split <- matsplitter(m_bf_segmented, grid_size, grid_size)

m_bf_edge_split <- matsplitter(m_bf_edges, grid_size, grid_size)

d_cellularity_grid <- matrix(ncol = 1, nrow = grid_no^2)

for(roi in 1:grid_no^2) {
	d_cellularity_grid[roi] <- calc.cellularity(m_bf_seg_split[,,roi], m_bf_edge_split[,,roi])
}

m_cellularity_grid <- matrix(d_cellularity_grid, ncol = grid_no)


write.table(m_cellularity_grid, paste0("grid-cellularities/", image_grid_names[j]), row.names = FALSE, col.names = FALSE, sep = ",")