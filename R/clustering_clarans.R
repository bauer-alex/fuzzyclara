#' Perform clarans clustering algorithm
#'
#' Function to perform clara clustering algorithm in a fixed or fuzzy way.
#' The function can either be performed using a common dissimilarity metric or
#' a self-defined distance function.
#'
#' If the clustering is run on mulitple cores, the verbose messages are printed
#' in a file \code{clustering_progress.log} (if \code{verbose > 0}).
#'
#' @param data data.frame to be clustered
#' @param clusters number of clusters
#' @param metric predefined dissimilarity metric (euclidean, manhattan) or
#' self-defined dissimilarity function
#' @param max_neighbors maximum number of randomized medoid searches with each
#' cluster
#' @param num_local number of clustering iterations
#' @param type fixed or fuzzy clustering
#' @param cores numbers of cores for computation (cores > 1 implies
#' multithreading)
#' @param seed random number seed
#' @param m fuzziness exponent (only for type = "fuzzy")
#' @param verbose Can be set to integers between 0 and 2 to control the level of
#' detail of the printed diagnostic messages. Higher numbers lead to more detailed
#' messages. Defaults to 1.
#' @param ... Additional arguments passed to the main clustering algorithm and
#' to proxy::dist for the calculation of the distance matrix
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#'
#' @return object of class fuzzyclara
#' @import cluster parallel checkmate tibble dplyr tidyselect
clustering_clarans <- function(data, clusters = 5, metric = "euclidean",
                             max, type = "fixed", max_neighbors = 100,
                             num_local = num_local, cores = 1, seed = 1234,
                             m = 2, verbose = 1, ...) {

  # Setting a seed for random processes:
  set.seed(seed)

  # Randomly select starting medoids:
  medoids <- sample(x = 1:nrow(data), size = clusters, replace = FALSE)

  # Assign all observations to closest medoid:

  # for each cluster:
  num_neighbors <- 1
  # while loop for tested medoids by cluster:
  while (num_neighbors < max_neighbors) {

    # Compute sum of distances with assigned medoid:

    # Compute sum of distances with new medoid:

    # Raise num_neighbors if old medoid has lower distance:
    if (1 + 1) {
      num_neighbors <- num_neighbors + 1
    }
  }

  # Assign each observation to the closest cluster medoid:
  # (maybe use assign_cluster function of clara computations)

  # Return of clustering solution based on the best sample:
  class(best_solution) <- c("fuzzyclara", class(best_solution))
  return(best_solution)
}
