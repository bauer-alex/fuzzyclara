#' Perform clustering algorithm on a data sample
#'
#' Function to perform clara clustering algorithm in a hard or fuzzy way.
#' The function can either be performed using a common dissimilarity metric or
#' a self-defined distance function.
#'
#' @inheritParams fuzzyclara
#' @param data data.frame to be clustered
#' @param sample_ids ids for the sample's observations
#' @param dist Dissimilarity matrix for subsample
#' @param sample_size Number of observations belonging to a sample
#' @param verbose_toLogFile If TRUE, the diagnostic messages are printed to
#' a log file \code{clustering_progress.log}. Defaults to FALSE.
#' @param build Additional build algorithm to choose initial medoids (only
#' relevant for type = "fuzzy". Default FALSE.)
#' @param ... Additional arguments passed to the main clustering algorithm
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#'
#' @return Clustering solution for data sample
#'
#' @import checkmate cluster dplyr
#'
clustering_sample <- function(data, sample_ids, dist, clusters = 5,
                              metric = "euclidean", sample_size = NULL,
                              type = "hard", m = 2, verbose = 1,
                              verbose_toLogFile = FALSE, build = FALSE, ...) {

  checkmate::assert_data_frame(data)
  # TODO how to check 'sample_ids'?
  checkmate::assert_vector(clusters)
  checkmate::assert_number(sample_size, null.ok = TRUE)
  if (!is.null(sample_size)) {
    checkmate::assert_true(sample_size <= nrow(data))
  }
  checkmate::assert_choice(type, choices = c("hard","fuzzy"))
  checkmate::assert_number(m, lower = 1)
  checkmate::assert_choice(verbose, choices = 0:2)
  checkmate::assert_logical(verbose_toLogFile, len = 1)
  checkmate::assert_logical(build, len = 1)

  # Reduction of data to sample observations:
  data_sample <- data %>% dplyr::slice(sample_ids)

  # Clustering of data sample:
  if (verbose >= 2) {
    print_logMessage("Perform the main clustering step...",
                     verbose_toLogFile = verbose_toLogFile)
  }
  clustering_results_sample <- perform_sample_clustering(dist = dist,
                                                         data = data_sample,
                                                         clusters = clusters,
                                                         type = type,
                                                         metric = metric,
                                                         names = data_sample$Name,
                                                         m = m,
                                                         build = build,
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

  # Add information about subsample, distance matrix, clustering of subsample
  # for silhouette plot:
  clustering_results[["subsample_ids"]] <- sample_ids
  clustering_results[["dist_matrix"]] <- dist
  clustering_results[["subsample_clustering"]] <- clustering_results[["clustering"]][sample_ids]


  # Return of clustering results:
  return(clustering_results)
}


#' Compute the dissimilarity matrix for a data sample
#'
#' Function to compute the dissimilarity matrix based on a specified metric
#'
#' @inheritParams fuzzyclara
#' @param data Sample of data.frame to be clustered
#' @param sample_ids ids for the sample's observations
#'
#' @return Dissimilarity matrix for data sample
#'
#' @import checkmate proxy
#'
compute_distance_matrix <- function(data, sample_ids, metric = "euclidean") {

  checkmate::assert_data_frame(data)

  
  # Some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2:
  Name <- NULL
  
  
  # Reduction of data to sample observations:
  data <- data %>% dplyr::slice(sample_ids)

  # Deletion of column "Name":
  data <- data %>% dplyr::select(-Name)

  # Calculation of dissimilarity matrix:
  distance <- proxy::dist(data, method = metric)

  if(!(sum(is.na(distance)) + sum(is.infinite(distance)) == 0)){
    stop("The distance matrix contains NA or infinite values. Please specify a suitable distance metric.")
  }

  return(distance)
}


#' Perform pam or vegclust clustering on a data sample
#'
#' Function to perform pam in a hard or fuzzy way on a data sample

#' @inheritParams fuzzyclara
#' @param dist Dissimilarity matrix
#' @param data Data sample
#' @param clusters Number of clusters
#' @param type Hard or fuzzy clustering
#' @param names Vector of names for observations
#' @param m Fuzziness exponent (only for type = fuzzy)
#' @param build Additional build algorithm to choose initial medoids (only
#' relevant for type = "fuzzy". Default FALSE.)
#' @param verbose_toLogFile If TRUE, the diagnostic messages are printed to
#' a log file \code{clustering_progress.log}. Defaults to FALSE.
#' @param ... Additional arguments passed to the main clustering algorithm
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#'
#' @return List with information on cluster results (medoid and cluster
#' assignment)
#'
#' @import checkmate cluster vegclust
#'
perform_sample_clustering <- function(dist, data, clusters, type, metric,
                                      names, m = 2, build = FALSE,
                                      verbose = 1, verbose_toLogFile = FALSE,
                                      ...) {

  checkmate::assert_class(dist, classes = "dist")
  checkmate::assert_vector(clusters)
  checkmate::assert_choice(type, choices = c("hard","fuzzy"))
  # TODO how to check 'names'?
  checkmate::assert_number(m, lower = 1)
  checkmate::assert_logical(build, len = 1)
  checkmate::assert_choice(verbose, choices = 0:2)
  checkmate::assert_logical(verbose_toLogFile, len = 1)


  # Hard pam clustering:
  if (type == "hard") {
    pam_sample <- pam(x = dist, k = clusters, diss = TRUE, ...)
    medoids    <- pam_sample$medoids
    clustering <- pam_sample$clustering
  }

  # Fuzzy pam clustering:
  if (type == "fuzzy") {

    # Additional build algorithm if specified:
    if (build == TRUE) {

      dist <- as.matrix(dist)

      if (verbose >= 2) {
        print_logMessage("Select starting medoids...",
                         verbose_toLogFile = verbose_toLogFile)
      }

      # Select first medoid as the one which has the smallest cost
      starting_medoids    <- c()
      starting_medoids[1] <- names[which.min(rowSums(dist))]

      # Select other medoids according to minimal costs:
      for (i in 2:clusters) {
        # Search for cost-minimizing next medoid:
        non_medoids <- names[!names %in% starting_medoids]
        costs <- lapply(X = non_medoids, FUN = function(non_medoid) {
          medoids <- c(starting_medoids, non_medoid)
          cost <- assign_cluster(data = data, medoids = medoids,
                                 metric = metric, dist_matrix = dist,
                                 type = "fuzzy", m = m)$avg_weighted_dist
          return(cost)
        })
        starting_medoids <- c(starting_medoids, non_medoids[which.min(costs)])
      }

      starting_medoids <- which(names %in% starting_medoids)
      dist <- proxy::as.dist(dist)
      attributes(dist)[["Labels"]] <- as.character(1:nrow(data))
      fuzzy_sample <- vegclustdist(x = dist, mobileMemb = starting_medoids,
                                      method = "FCMdd", m = m, ...)
    }

    if (build == FALSE) {
      attributes(dist)[["Labels"]] <- as.character(1:nrow(data))
      fuzzy_sample <- vegclustdist(x = dist, mobileMemb = clusters,
                                      method = "FCMdd", m = m, ...)
    }

    medoids          <- names[as.numeric(fuzzy_sample$mobileCenters)]
    dist_to_clusters <- fuzzy_sample$dist2clusters
    clustering_df    <- apply(X = dist_to_clusters, MARGIN = 1, FUN = which.min)
    clustering       <- as.numeric(clustering_df)
  }

  # Return of clustering information:
  clustering_result <- list("medoids" = medoids, "clustering" = clustering)
  return(clustering_result)
}






