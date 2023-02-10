cat("Removing background gradients...\n")

# ==========================================================================
# Normalise the image using the plane
# ==========================================================================

m_bf_gs <- m_bf[,,1]
normal <- m_bf_gs
reps <- c(1:5)
rows <- 1:nrow(m_bf)
cols <- 1:ncol(m_bf)
fun <- function(x,y,fit) {
	fit$coefficients[1] + fit$coefficients[2]*x + fit$coefficients[3]*y
}
for (rep in reps) {
	d_normal <- data.frame(matrix(normal, ncol=1))

	d_normal <-
	expand.grid(1:nrow(normal), 1:ncol(normal)) %>%
	data.frame() %>%
	cbind(d_normal)

	colnames(d_normal) <- c("x", "y", "z")

	fit_normal <- lm(z ~ x + y, data = d_normal)

	if (is.na(fit_normal$coefficients[1])) {
		fit_normal$coefficients[1] <- 0
	}

	if (is.na(fit_normal$coefficients[2])) {
		fit_normal$coefficients[2] <- 0
	}

	if (is.na(fit_normal$coefficients[3])) {
		fit_normal$coefficients[3] <- 0
	}

	plane_normal <- outer(rows,cols,fit_normal,FUN=fun)

	normal <- matrix(normal, ncol = 1) - matrix(plane_normal, ncol = 1) + 0.5

	if (rep == length(reps)) {
		m_bf_gs_normal <- normal
	}
}


d_bf_gs_normal <- data.frame(matrix(m_bf_gs_normal, ncol=1))

d_bf_normal <- cbind(d_bf_gs_normal, d_bf_gs_normal, d_bf_gs_normal)
colnames(d_bf_normal) <- c("red", "green", "blue")

m_bf_normal <- array(
  d_bf_normal %>%
    unlist() %>%
    unname(),
  dim = dim(m_bf)
)

# ==========================================================================
# Save normalised image
# ==========================================================================

save(m_bf_normal, file = "r-objects/m_bf_normal.RData")
