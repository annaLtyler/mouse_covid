#clusters matrices using pam or hclust and cutree.
#handles NAs either by replacing them with 0s (use.cov.mat = FALSE)
#or by using the covariance matrix (default; use.cov.mat = TRUE)
#It returns a list of clusters

cluster_mat <- function(mat, k = 4, cluster.by = c("pam", "cutree"),
  use.cov.mat = TRUE, plot.results = TRUE){
  
  cluster.by <- cluster.by[1]

  na.idx <- which(is.na(mat))
  if(length(na.idx) > 0){
    use.cov.mat <- TRUE #use covariance matrix automatically if there are any NAs
  }

  if(use.cov.mat){
    #use pairwise complete covariance
    cov.mat <- cov(t(mat), use = "pairwise.complete.obs")
    no.na.mat <- cov.mat

  }else{
    no.na.mat <- mat
  }

  if(cluster.by == "pam"){
    mat.groups <- pam(no.na.mat, k = k)$clustering
  }
  if(cluster.by == "cutree"){
    mat.groups <- cutree(hclust(dist(no.na.mat)), k = k)
  }

  group.df <- data.frame("group" = as.factor(mat.groups))
  group.order <- order(mat.groups)

  #pheatmap(mat[group.order,], cluster_cols = FALSE, cluster_rows = FALSE, annotation_row = group.df)

  if(plot.results){
    layout.mat <- get.layout.mat(k)
    layout(layout.mat)
    for(cl in 1:k){
      plot.new()
      plot.window(xlim = c(0, ncol(mat)), 
        ylim = c(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE)))
      cl.idx <- which(mat.groups == cl)
      for(i in 1:length(cl.idx)){
        points(mat[cl.idx[i],], type = "b")
      }
      abline(h = 0)
      mtext(paste("Cluster", cl), side = 3, line = 1.5)
      axis(1); axis(2)
    }
  }
  result <- list("Clusters" = mat.groups)
  return(result)
}