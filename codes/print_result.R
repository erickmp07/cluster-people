#print clustering result

K <- 14 #number of clusters
df_people <- read.csv("../data/people_data.csv")

print(commandArgs())

cluster_assignment <- weighted_kmeans(df_people, K)
df_people_cluster <- as.data.frame(
  cbind(
    name = df_people$name,
    longitude = df_people$longitude,
    latitude = df_people$latitude,
    cluster = cluster_assignment
  )
)

#calculate centroids (weighted average)
centroid_long <- vector()
centroid_lat <- vector()
names <- vector()

for (k in c(1:K)) {
  cluster_k <- which(cluster_assignment == k) #people index of cluster k
  names[k] <- df_people$name[cluster_k]

  centroid_long[k] <- weighted.mean(
    df_people$longitude[cluster_k],
    df_people$distance[cluster_k]
  )

  centroid_lat[k] <- weighted.mean(
    df_people$latitude[cluster_k],
    df_people$distance[cluster_k]
  )
}

#create data frame for centroid with dummy cluster number
df_centroid <- as.data.frame(
  cbind(
    name = names,
    longitude = centroid_long,
    latitude = centroid_lat,
    cluster = rep(K + 1, length(centroid_lat))
  )
)

#append df_city_cluster and df_centroid for ggplot
df_kmeans_result <- rbind.data.frame(df_people_cluster, df_centroid)

print(df_kmeans_result)