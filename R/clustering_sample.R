#' Perform clustering algorithm on a data sample
#'
#' Function to perform clara clustering algorithm in a fixed or fuzzy way.
#' The function can either be performed using a common dissimilarity metric or
#' a self-defined distance function.
#' @param data data.frame to be clustered
#' @param sample_ids ids for the sample's observations
#' @param clusters number of clusters
#' @param metric predefined dissimilarity metric (euclidean, manhattan) or
#' self-defined dissimilarity function
#' @param sample_size number of observations belonging to a sample
#' @param type fixed or fuzzy clustering
#' @param m fuzziness exponent (only for type = fuzzy)
#' @param verbose Can be set to integers between 0 and 2 to control the level of
#' detail of the printed diagnostic messages. Higher numbers lead to more detailed
#' messages. Defaults to 1.
#' @param verbose_toLogFile If TRUE, the diagnostic messages are printed to
#' a log file \code{clustering_progress.log}. Defaults to FALSE.
#' @param ... Additional arguments passed to the main clustering algorithm
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#' @return clustering solution for data sample
#' @import dplyr cluster
clustering_sample <- function(data, sample_ids, clusters = 5,
                              metric = "euclidean", sample_size = NULL,
                              type = "fixed", m = 2, verbose = 1,
                              verbose_toLogFile = FALSE, ...) {

  # Reduction of data to sample observations:
  data_sample <- data %>% dplyr::slice(sample_ids)

  # Extract name of metric:
  if (class(metric) == "function") {
    name_metric <- deparse(substitute(metric))
  }
  else {
    name_metric <- metric
  }

  # Computation of distance matrix:
  if (verbose >= 2) {
    print_logMessage("Calculating the distance matrix...",
                     verbose_toLogFile = verbose_toLogFile)
  }
  dist_matrix <- compute_distance_matrix(data = data_sample, metric = metric)

  # Clustering of data sample:
  if (verbose >= 2) {
    print_logMessage("Perform the main clustering step...",
                     verbose_toLogFile = verbose_toLogFile)
  }
  clustering_results_sample <- perform_sample_clustering(dist = dist_matrix,
                                                         clusters = clusters,
                                                         type = type,
                                                         names = data_sample$Name,
                                                         m = m,
                                                         ...)

  # Assignment of each observations of the entire dataset to closest medoid
  # and additional information on clustering:
  if (verbose >= 2) {
    print_logMessage("Assigning each observation to a cluster...",
                     verbose_toLogFile = verbose_toLogFile)
  }
  clustering_results <- assign_cluster(data = data,
                                       medoids = clustering_results_sample$medoid,
                                       metric = metric, type = type, m = m)

  # add information about subsample, distance matrix, clustering of subsample for silhouette plot
  clustering_results[["subsample_ids"]] <- sample_ids
  clustering_results[["dist_matrix"]] <- dist_matrix
  clustering_results[["subsample_clustering"]] <- clustering_results[["clustering"]][sample_ids]


  # Return of clustering results:
  return(clustering_results)
}


################################################################################

#' Compute the dissimilarity matrix for a data sample
#'
#' Function to compute the dissimilarity matrix based on a specified metric
#' @param data sample of data.frame to be clustered
#' @param metric predefined dissimilarity metric (euclidean, manhattan) or
#' self-defined dissimilarity function
#' @return dissimilarity matrix for data sample
#' @import proxy
compute_distance_matrix <- function(data, metric) {

  # Deletion of column "name":
  data <- data %>% dplyr::select(-Name)

  # Calculation of dissimilarity matrix:
  distance <- proxy::dist(data, method = metric)

  if(!(sum(is.na(distance)) + sum(is.infinite(distance)) == 0)){
    stop("The distance matrix contains NA or infinite values. Please specify a suitable distance metric.")
  }
  return(distance)
}

################################################################################

#' Perform pam or vegclust clustering on a data sample
#'
#' Function to perform pam in a fixed or fuzzy way on a data sample
#' @param dist dissimilarity matrix
#' @param clusters number of clusters
#' @param type fixed or fuzzy clustering
#' @param names vector of names for observations
#' @param m fuzziness exponent (only for type = fuzzy)
#' @param ... Additional arguments passed to the main clustering algorithm
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#' @return list with information on cluster results (medoid and cluster
#' assignment)
#' @import vegclust cluster
perform_sample_clustering <- function(dist, clusters, type, names, m, ...) {

  # Fixed pam clustering:
  if (type == "fixed") {
    pam_sample <- pam(x = dist, k = clusters, diss = TRUE, ...)
    medoids <- names[as.numeric(pam_sample$medoids)]
    clustering <- pam_sample$clustering
  }

  # Fuzzy pam clustering:
  if (type == "fuzzy") {
    fuzzy_sample <- my_vegclustdist(x = dist, mobileMemb = clusters,
                                           method = "FCMdd", m = m, ...)
    medoids <- names[as.numeric(fuzzy_sample$mobileCenters)]
    dist_to_clusters <- fuzzy_sample$dist2clusters
    clustering_df <- apply(X = dist_to_clusters, MARGIN = 1, FUN = which.min)
    clustering <- as.numeric(clustering_df)
  }

  # Return of clustering information:
  clustering_result <- list("medoids" = medoids, "clustering" = clustering)
  return(clustering_result)
}

################################################################################

#' Assign a cluster to each observation of the entire dataset
#'
#' Function to assign a cluster to each observation of the entire dataset
#' by selecting the closest medoid
#' @param data entire data.frame
#' @param metric predefined dissimilarity metric (euclidean, manhattan) or
#' self-defined dissimilarity function
#' @param medoids medoids of the obtained clustering solution for the data
#' sample
#' @param type fixed or fuzzy clustering
#' @param m fuzziness exponent (only for type = fuzzy)
#' @param return_distMatrix Should the distances to the cluster medoids be
#' returned?
#' @return list with information on cluster results (medoid, cluster
#' assignment, average distance to the closest medoid (weighted
#' average distance to the closest medoid in case of fuzzy clustering))
#' @import proxy
assign_cluster <- function(data, metric, medoids, type = "fixed",
                           m = 2, return_distMatrix = FALSE) {

  # Extraction of obtained medoids of the data:
  data_medoids <- data %>% filter(Name %in% medoids)

  # Calculate the distances to the cluster medoids:
  dist_dat <- proxy::dist(x = data[, -1], y = data_medoids[, -1],
                          method = metric)

  # Assignment to the medoid with minimum distance:
  cluster_assignments <- apply(dist_dat, 1, which.min)

  # Computation of membership scores in case of fuzzy clustering:
  if (type == "fuzzy") {
    memb_scores_list <- apply(dist_dat, 1, function(x) {
      data.frame(t(as.numeric(calculate_memb_score(dist_med = x, m = m))))
    })
    memb_scores <- dplyr::bind_rows(memb_scores_list)
    colnames(memb_scores) <- paste0("Cluster", 1:ncol(memb_scores))
  }

  # Computation of distance for fixed and fuzzy clustering:
  if (type == "fixed") {
    # Minimum distance:
    distances <- apply(dist_dat, 1, min)
  } else { # type = "fuzzy"
    # Weighted distance (membership scores as weights):
    distances <- rowSums(dist_dat * memb_scores)
  }

  # Return of clustering information:
  dist_dat <- as.data.frame(dist_dat[1:nrow(dist_dat),]) # conversion from 'crossdist' to 'matrix'
  colnames(dist_dat) <- paste0("Distance_to_Cluster", 1:ncol(dist_dat))
  assignment_dat <- data.frame("assignment" = cluster_assignments,
                               "distance"   = distances)
  assignment_dat$Distance_to_Clusters <- dist_dat
  if (type == "fuzzy") {
    assignment_dat$membership <- memb_scores
  }

  # Return of information about cluster assignments:
  assignment   <- assignment_dat$assignment
  distance     <- assignment_dat$distance
  average_dist <- mean(distance)
  clustering_result <- list("medoids" = medoids, "clustering" = assignment)

  # Computation of average distance for fixed clustering:
  if (type == "fixed") {
    clustering_result[["avg_min_dist"]] <- average_dist
  }

  # Computation of weighted average distance for fuzzy clustering:
  if (type == "fuzzy") {
    # Computation of membership scores:
    clustering_result[["avg_weighted_dist"]] <- average_dist
    membership <- as.data.frame(assignment_dat$membership)
    row.names(membership) <- data$Name
    clustering_result[["membership_scores"]] <- membership
  }

  if (return_distMatrix == TRUE) {
    distances_to_medoids <- round(as.data.frame(assignment_dat$Distance_to_Clusters), 2)
    row.names(distances_to_medoids) <- data$Name
    clustering_result[["distance_to_medoids"]] <- distances_to_medoids
  }

  # Return of clustering results:
  return(clustering_result)
}


################################################################################


#' Calculate membership score of one observation for each medoid
#'
#' Function to calculate a membership score for one observation
#' for each medoid based on the distance of this observation to all medoids
#' @param dist_med vector of distances to medoids
#' @param m fuzziness exponent (only for type = fuzzy)
#' @return list with membership scores for one observation
calculate_memb_score <- function(dist_med, m) {

  perfect_match <- match(x = 0, table = dist_med)
  list_memb <- as.list(rep(x = 0, times = length(dist_med)))
  names(list_memb) <- paste0("Cluster_", 1:length(dist_med))

  if (!is.na(perfect_match)) {
    list_memb[[paste0("Cluster_", perfect_match)]] <- 1

  } else {
    for (i in 1:length(dist_med)) {
      dist_proportion <- dist_med[i] / dist_med
      dist_proportion_exp <- dist_proportion ^ (1 / (m - 1))
      dist_proportion_exp_inv <- 1 / sum(dist_proportion_exp)
      list_memb[[i]] <- dist_proportion_exp_inv
    }

  }
  return(list_memb)
}


