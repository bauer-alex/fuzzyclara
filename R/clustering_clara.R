#' Perform clara clustering algorithm
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
#' @param samples number of subsamples
#' @param sample_size number of observations belonging to a sample. If NULL
#' (default), the minimum of \code{nrow(data)} and \code{40 + clusters * 2} is
#' used as sample size.
#' @param type fixed or fuzzy clustering
#' @param cores numbers of cores for computation (cores > 1 implies
#' multithreading)
#' @param seed random number seed
#' @param m fuzziness exponent (only for type = "fuzzy")
#' @param verbose Can be set to integers between 0 and 2 to control the level of
#' detail of the printed diagnostic messages. Higher numbers lead to more detailed
#' messages. Defaults to 1.
#' @param build additional build algorithm to choose initial medoids (only
#' relevant for type = "fuzzy". Default FALSE.)
#' @param ... Additional arguments passed to the main clustering algorithm and
#' to proxy::dist for the calculation of the distance matrix
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#'
#' @return object of class fuzzyclara
#' @import cluster parallel checkmate tibble dplyr tidyselect
clustering_clara <- function(data, clusters = 5, metric = "euclidean",
                             samples = 10, sample_size = NULL, type = "fixed",
                             cores = 1, seed = 1234, m = 2, verbose = 1,
                             build = FALSE, ...) {

  # Setting a seed for random processes:
  set.seed(seed)

  # Define default sample size if no sample size is specified:
  if (is.null(sample_size)) {
    sample_size <- min(nrow(data), (40 + clusters * 2))
  }

  # Warning if sample size is larger than number of observations:
  if (sample_size >= nrow(data)) {
    sample_size <- nrow(data)
    samples <- 1
    sample_ids <- list(1:nrow(data))
    warning("The specified sample size is larger than the number of
    observations. PAM clustering is performed on the entire dataset.")
  }
  else {
    # Random generation of subsamples of size sample_size:
    sample_ids <- lapply(X = 1:samples, FUN = function(i) {
      sample <- sample(x = 1:nrow(data), size = sample_size, replace = FALSE)
      return(sample)
    })
  }
  # Sample definition is performed before parallelized computations in order
  # to get reproducible results.

  # Use pam clustering if m = 1 or only a single cluster is used:
  change_output_style <- FALSE
  if ((type == "fuzzy" & m == 1) | (type == "fuzzy" & clusters == 1)) {
    type <- "fixed"
    change_output_style <- TRUE
  }
  # The resulting output, however, should look the usual fuzzy output.

  # Adding row.names to column:
  data <- data %>% tibble::rownames_to_column(var = "Name")

  # Calculation of clustering results for each sample:
  if (cores == 1) { # single core
    clustering_results_list <- lapply(X = 1:samples, FUN = function(i) {
      if (verbose >= 1) { message("--- Performing calculations for subsample ",i) }
      clustering <- clustering_sample(data = data, sample_ids = sample_ids[[i]],
                                      clusters = clusters, metric = metric,
                                      m = m, sample_size = sample_size,
                                      type = type, verbose = verbose,
                                      build = build, ...)
    })
  }
  else {
    # multi-core
    print_logMessage(paste0("Run clustering on ", cores, " cores."),
                     verbose_toLogFile = TRUE, reset_logFile = TRUE)
    # Windows:
    if (Sys.info()['sysname'] == "Windows") {
      local_cluster <- makePSOCKcluster(rep("localhost", cores))
      clusterExport(cl = local_cluster,
                    varlist = c("clustering_sample", "compute_distance_matrix",
                                "perform_sample_clustering",
                                "assign_cluster", "calculate_memb_score"),
                    envir = environment(claraclust))
      clustering_results_list <- parLapply(cl = local_cluster, X = 1:samples,
                                           fun = function(i) {
        if (verbose >= 1) {
          print_logMessage(paste0("--- Performing calculations for subsample ",i),
                           verbose_toLogFile = TRUE)
        }
        clustering <- clustering_sample(data = data, sample_ids = sample_ids[[i]],
                                        clusters = clusters, metric = metric,
                                        m = m, sample_size = sample_size,
                                        type = type, verbose = verbose,
                                        verbose_toLogFile = TRUE,
                                        build = build, ...)
        return(clustering)
      })
      stopCluster(local_cluster)
    }

    # Other OS:
    else {
      clustering_results_list <- mclapply(X = 1:samples, FUN = function(i) {
        if (verbose >= 1) {
          print_logMessage(paste0("--- Performing calculations for subsample ",i),
                           verbose_toLogFile = TRUE)
        }
        clustering <- clustering_sample(data = data, sample_ids = sample_ids[[i]],
                                        clusters = clusters, metric = metric,
                                        m = m, sample_size = sample_size,
                                        type = type, verbose = verbose,
                                        verbose_toLogFile = TRUE,
                                        build = build, ...)
        return(clustering)
      }, mc.cores = cores, mc.set.seed = seed)
    }
  }

  if (verbose >= 1) {
    print_logMessage("--- Selecting the best clustering solution...",
                     verbose_toLogFile = (cores > 1))
  }

  # Selection of best clustering solution (according to smallest average
  # distance to closest cluster medoid):
  min_distance_list <- lapply(X = 1:samples, FUN = function(i) {
    if (type == "fixed") {
      dist <- clustering_results_list[[i]]$avg_min_dist
    } else { # type = "fuzzy"
      dist <- clustering_results_list[[i]]$avg_weighted_dist
    }
    return(dist)
  })
  min_distance <- which.min(min_distance_list)
  best_solution <- clustering_results_list[[min_distance]]
  best_solution[["type"]] <- type
  if (type == "fixed") {
    m <- 1
  }
  best_solution[["fuzzyness"]] <- m
  best_solution[["algorithm"]] <- "clara"
  best_solution[["metric"]] <- metric

  # Attach the distance-to-medoids matrix of the full dataset to the result:
  clustering_results <- assign_cluster(data = data,
                                       medoids = best_solution$medoids,
                                       metric = metric, type = type, m = m,
                                       return_distMatrix = TRUE,
                                       return_data_medoids = TRUE)
  best_solution$distance_to_medoids <- clustering_results$distance_to_medoids
  best_solution$data_medoids <- clustering_results$data_medoids

  # Change output style if pam was used for type "fuzzy":
  if (change_output_style == TRUE) {
    # Type:
    best_solution$type <- "fuzzy"
    # Weighted distance:
    names(best_solution)[[3]] <- "avg_weighted_dist"
    # Membership scores:
    membership <- Matrix::sparseMatrix(i = 1:length(best_solution$clustering),
                                       j = best_solution$clustering, x = 1)
    membership <- as.data.frame(as.matrix(membership))
    colnames(membership) <- paste0("Cluster", 1:ncol(membership))
    row.names(membership) <- data$Name
    element_names <- names(best_solution)
    best_solution$membership_scores <- membership
    best_solution <- best_solution[c(element_names[1:3], "membership_scores",
                                     element_names[4:length(element_names)])]
  }

  # Return of clustering solution based on the best sample:
  class(best_solution) <- c("fuzzyclara", class(best_solution))
  return(best_solution)
}





