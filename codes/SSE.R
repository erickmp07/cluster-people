#function to compute total sum of squares error

SSE = function(people_df, clustering) {
  #city_df is a dataframe with columns:
  #person name, longitude, latitude, distance
  #clustering is vector of cluster assignment of people

  #prepare argument
  people_longlat_mat <- as.matrix.data.frame(people_df[, 2:3])
  colnames(people_longlat_mat) <- NULL

  #compute centroid
  centroid_long <- vector()
  centroid_lat <- vector()
  for (k in c(1:max(clustering))) {
    cluster_k <- which(clustering == k) #people index of cluster k

    centroid_long[k] <- weighted.mean(
      people_df$longitude[cluster_k], people_df$distance[cluster_k]
    )

    centroid_lat[k] <- weighted.mean(
      people_df$latitude[cluster_k], people_df$distance[cluster_k]
    )
  }
  mat_centroid <- cbind(centroid_long, centroid_lat)
  colnames(mat_centroid) <- NULL

  #compute SSE
  sum_squares_err <- 0
  for (k in c(1:max(clustering))) {
    cluster_k <- which(clustering == k) #people index of cluster k
    for (i in c(1:length(cluster_k))) {
      for (j in c(1:2)) { #long, lat
        square_err <- (people_longlat_mat[cluster_k[i], j] -
        mat_centroid[k, j])^2
        sum_squares_err <- sum_squares_err + square_err
      }
    }
  }
  return(sum_squares_err)
}
