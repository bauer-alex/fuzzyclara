#' Perform clustering algorithm on a data sample
#'
#' Function to perform clara clustering algorithm in a fixed or fuzzy way.
#' The function can either be performed using a common dissimilarity metric or
#' a self-defined distance function.
#'
#' @inheritParams fuzzyclara
#' @param data data.frame to be clustered
#' @param sample_ids ids for the sample's observations
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
clustering_sample <- function(data, sample_ids, clusters = 5,
                              metric = "euclidean", sample_size = NULL,
                              type = "fixed", m = 2, verbose = 1,
                              verbose_toLogFile = FALSE, build = FALSE, ...) {

  checkmate::assert_data_frame(data)
  # TODO how to check 'sample_ids'?
  checkmate::assert_number(clusters, lower = 2)
  checkmate::assert_number(sample_size, null.ok = TRUE)
  checkmate::assert_choice(type, choices = c("fixed","fuzzy"))
  checkmate::assert_number(m, lower = 1)
  checkmate::assert_choice(verbose, choices = 0:2)
  checkmate::assert_logical(verbose_toLogFile, len = 1)
  checkmate::assert_logical(build, len = 1)


  # Reduction of data to sample observations:
  data_sample <- data %>% dplyr::slice(sample_ids)

  # Extract name of metric:
  if (class(metric) == "function") {
    name_metric <- deparse(substitute(metric))

  } else { # 'metric' is no function, but a character name
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

  # add information about subsample, distance matrix, clustering of subsample for silhouette plot
  clustering_results[["subsample_ids"]] <- sample_ids
  clustering_results[["dist_matrix"]] <- dist_matrix
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
#'
#' @return Dissimilarity matrix for data sample
#'
#' @import checkmate proxy
#'
compute_distance_matrix <- function(data, metric = "euclidean") {

  checkmate::assert_data_frame(data)


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
#' Function to perform pam in a fixed or fuzzy way on a data sample
#'
#' @param dist Dissimilarity matrix
#' @param clusters Number of clusters
#' @param type Fixed or fuzzy clustering
#' @param names Vector of names for observations
#' @param m Fuzziness exponent (only for type = fuzzy)
#' @param build Additional build algorithm to choose initial medoids (only
#' relevant for type = "fuzzy". Default FALSE.)
#' @param ... Additional arguments passed to the main clustering algorithm
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#'
#' @return List with information on cluster results (medoid and cluster
#' assignment)
#'
#' @import checkmate cluster vegclust
#'
perform_sample_clustering <- function(dist, clusters, type, names, m,
                                      build = FALSE, ...) {

  # TODO how to check 'dist'?
  checkmate::assert_number(clusters, len = 1)
  checkmate::assert_choice(type, choices = c("fixed","fuzzy"))
  # TODO how to check 'names'?
  checkmate::assert_number(m, lower = 1)
  checkmate::assert_logical(build, len = 1)


  # Fixed pam clustering:
  if (type == "fixed") {
    pam_sample <- pam(x = dist, k = clusters, diss = TRUE, ...)
    medoids    <- names[as.numeric(pam_sample$medoids)]
    clustering <- pam_sample$clustering
  }

  # Fuzzy pam clustering:
  if (type == "fuzzy") {

    # Additional build algorithm if specified:
    if (build == TRUE) {

      # Select first medoid as the one which has the smallest cost
      starting_medoids    <- c()
      starting_medoids[1] <- which.min(rowSums(dist))

      # Select other medoids according to minimizing costs:
      # TODO
    }


    if (build == FALSE) {
      fuzzy_sample <- my_vegclustdist(x = dist, mobileMemb = clusters,
                                      method = "FCMdd", m = m, ...)
    }
    else {
      # TODO
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
