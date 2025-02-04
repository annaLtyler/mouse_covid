# This function does a decomposition on a matrix with NAs
#It clusters the covariance matrix instead.

decomp_with_nas <- function(mat, pc = 2, cols = NULL){  
  cov.mat <- cov(t(mat), use = "pairwise.complete.obs")
  no.na.mat <- cov.mat
  no.na.decomp <- plot.decomp(no.na.mat, cols = cols, pc = pc)
  invisible(no.na.decomp)
}