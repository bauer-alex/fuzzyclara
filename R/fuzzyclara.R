#' Perform clustering
#'
#' Function to perform a cluster analysis in a fixed or fuzzy way. The
#' function can either be performed using a common dissimilarity metric or
#' a self-defined distance function.
#'
#' If the clustering is run on mulitple cores, the verbose messages are printed
#' in a file \code{clustering_progress.log} (if \code{verbose > 0}).
#'
#' @param data data.frame to be clustered
#' @param clusters Number of clusters
#' @param metric Predefined dissimilarity metric (euclidean, manhattan) or
#' self-defined dissimilarity function
#' @param algorithm One of \code{c("clara","clarans")}
#' @param samples Number of subsamples (only for clara clustering)
#' @param sample_size Number of observations belonging to a sample. If NULL
#' (default), the minimum of \code{nrow(data)} and \code{40 + clusters * 2} is
#' used as sample size. (only for clara clustering)
#' @param max_neighbors Maximum number of randomized medoid searches with each
#' cluster (only for clarans clustering)
#' @param num_local Number of clustering iterations (only for clarans
#' clustering)
#' @param type One of \code{c("fixed","fuzzy")}, specifying the type of
#' clustering to be performed.
#' @param m Fuzziness exponent (only for \code{type = "fuzzy"})
#' @param cores Numbers of cores for computation (cores > 1 implies
#' multithreading)
#' @param seed Random number seed
#' @param verbose Can be set to integers between 0 and 2 to control the level of
#' detail of the printed diagnostic messages. Higher numbers lead to more detailed
#' messages. Defaults to 1.
#' @param scale Scale numeric variables before distance matrix calculation?
#' Default TRUE
#' @param build Additional build algorithm to choose initial medoids (only
#' relevant for type = "fuzzy". Default FALSE.)
#' @param ... Additional arguments passed to the main clustering algorithm and
#' to proxy::dist for the calculation of the distance matrix
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#'
#' @return Object of class claraclust
#'
#' @import cluster parallel checkmate tibble dplyr tidyselect
#' @export
#'
fuzzyclara <- function(data, clusters = 5, metric = "euclidean",
                       algorithm = "clara", samples = 10, sample_size = NULL,
                       max_neighbors = 100, num_local = 10, type = "fixed",
                       cores = 1, seed = 1234, m = 2, verbose = 1,
                       scale = TRUE, build = FALSE, ...) {

  # Input checking:
  checkmate::assert(checkmate::check_data_frame(data),
                    checkmate::check_matrix(data), combine = "or")
  checkmate::assert_numeric(x = clusters, lower = 1, upper = nrow(data))
  checkmate::assert_numeric(x = samples, lower = 1)
  checkmate::assert_numeric(x = sample_size, lower = clusters, null.ok = TRUE)
  checkmate::assert_numeric(x = max_neighbors, lower = 1)
  checkmate::assert_numeric(x = num_local, lower = 1)
  checkmate::assert_choice(x = algorithm, choices = c("clara", "clarans"))
  checkmate::assert_choice(x = type, choices = c("fixed", "fuzzy"))
  checkmate::assert_numeric(x = cores, lower = 1)

  # pam requires the number of clusters to be smaller than the number of
  # observations -> another check of sample_size
  if(type == "fixed" | m == 1 | clusters == 1){
    checkmate::assert_numeric(x = sample_size, lower = clusters + 1,
                              null.ok = TRUE)
  }

  # Convert data into data.frame if necessary:
  if (!("data.frame" %in% class(data))) {
    data <- as.data.frame(data)
  }

  # Extract name of metric:
  if (class(metric) == "function") {
    name_metric <- deparse(substitute(metric))
  }
  else {
    name_metric <- metric
  }

  if(scale == TRUE){
    # optional: Scaling of numerical (and ordinal) variables:
    ind <- unlist(lapply(data, is.numeric), use.names = TRUE)
    for (i in ind) {
      data[, ind] <- scale(data[, ind])
    }
  }

  # Choice of clustering algorithm:
  # clara algorithm:
  if (algorithm == "clara") {
    result <- clustering_clara(data, clusters = clusters, metric = metric,
                               samples = samples, sample_size = sample_size,
                               type = type, cores = cores, seed = seed, m = m,
                               verbose = verbose, build = build, ...)
  }
  if (algorithm == "clarans") {
    result <- clustering_clarans(data, clusters = clusters, metric = metric,
                                 max_neighbors = max_neighbors,
                                 num_local = num_local, type = type,
                                 cores = cores, seed = seed, m = m,
                                 verbose = verbose, ...)
  }

  # Return of clustering solution:
  return(result)
}
