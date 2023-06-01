df <- read.csv('final_ver_df.csv')

# user defined matrix(distance_fn)
custom_distance <- function(x, y) {
  # user defined cost matrix
  # x와 y: data points
  # return calculated results
}

# k-means algorithm handmade
kmeans_custom_distance <- function(data, centers, distance_fn, max_iter = 100) {
  # reset
  cluster_assignments <- rep(0, nrow(data)) # allocate every data point to cluster 0(default)
  num_centers <- nrow(centers) # number of centers
  num_points <- nrow(data) # number pf data points
  converged <- FALSE # converged?
  iter <- 0 # number of iterations
  
  while (!converged && iter < max_iter) {
    # assign cluster 
    old_cluster_assignments <- cluster_assignments
    
    # relocate the data points to nearest centroid
    for (i in 1:num_points) {
      distances <- rep(0, num_centers)# vector that stores distance to every centroid
      for (j in 1:num_centers) {
        distances[j] <- distance_fn(data[i,], centers[j,])# calculate the user defined distance
      }
      cluster_assignments[i] <- which.min(distances)
    }
    
    # renew the centroid
    for (j in 1:num_centers) {
      cluster_points <- data[cluster_assignments == j, ]# select data points in selected center
      centers[j,] <- apply(cluster_points, 2, mean) # renew the centroid
    }
    
    
    # check converged
    if (all(old_cluster_assignments == cluster_assignments)) {
      converged <- TRUE
    }
    
    iter <- iter + 1
  }
  
  # return cluster results
  return(cluster_assignments)
}

# example
