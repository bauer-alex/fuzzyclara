#' Perform CLARANS clustering
#'
#' Function to perform a CLARANS clustering in a fixed or fuzzy way.
#' The function can either be called using a common dissimilarity metric or
#' a self-defined distance function.
#'
#' If the clustering is run on mulitple cores, the verbose messages are printed
#' in a file \code{clustering_progress.log} (if \code{verbose > 0}).
#'
#' @inheritParams fuzzyclara
#' @param data data.frame to be clustered
#' @param max_neighbors Maximum number of randomized medoid searches with each
#' cluster
#' @param num_local Number of clustering iterations
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#'
#' @return Object of class fuzzyclara
#'
#' @import checkmate cluster dplyr parallel tibble tidyselect
#'
#' @references TODO add CLARANS paper
#'
clustering_clarans <- function(data, clusters = 5, metric = "euclidean",
                               type = "fixed", num_local = 5,
                               max_neighbors = 100, cores = 1, seed = 1234,
                               m = 2, verbose = 1, ...) {

  checkmate::assert_data_frame(data)
  checkmate::assert_number(clusters, lower = 1)
  checkmate::assert_choice(type, choices = c("fixed","fuzzy"))
  checkmate::assert_number(max_neighbors, lower = 1)
  checkmate::assert_number(num_local, lower = 1)
  checkmate::assert_number(cores, lower = 1)
  checkmate::assert_number(seed)
  checkmate::assert_number(m, lower = 1)
  checkmate::assert_choice(verbose, choices = 0:2)

  # Setting a seed for random processes:
  set.seed(seed)

  # Adding row.names to column:
  data <- data %>% tibble::rownames_to_column(var = "Name")

  # Randomly draw pairs of medoids and non-medoids as well as starting medoids
  # for each local iteration:
  samples_local <- lapply(X = 1:num_local, FUN = function(i) {
    sample_med <- sample(x = 1:clusters, size = max_neighbors, replace = TRUE)
    sample_non_med <- sample(x = 1:nrow(data), size = max_neighbors,
                             replace = TRUE)
    starting_medoids <- sample(x = data$Name[1:nrow(data)], size = clusters,
                               replace = FALSE)
    sample <- list("medoids" = sample_med, "non_medoids" = sample_non_med,
                   "start" = starting_medoids)
    return(sample)
  })
  # Sample definition is performed before parallelized computations in order
  # to get reproducible results.


  # Calculation of clustering results for each iteration (local minimum):
  if (cores == 1) { # single core
    clustering_results_list <- lapply(X = 1:num_local, FUN = function(i) {
      if (verbose >= 1) { message("--- Performing calculations for local iteration ", i) }
      clustering <- clustering_local(data = data,
                                     sample_local = samples_local[[i]],
                                     clusters = clusters, metric = metric,
                                     m = m, max_neighbors = max_neighbors,
                                     type = type, verbose = verbose)
      return(clustering)
    })

  } else { # cores > 1, i.e. multi-core computation

    print_logMessage(paste0("Run clustering on ", cores, " cores."),
                     verbose_toLogFile = TRUE, reset_logFile = TRUE)

    # Windows:
    if (Sys.info()['sysname'] == "Windows") {

      local_cluster <- makePSOCKcluster(rep("localhost", cores))
      clusterExport(cl = local_cluster,
                    varlist = c("clustering_local",
                                "assign_cluster", "calculate_memb_score"),
                    envir = environment(fuzzyclara))
      clustering_results_list <- parLapply(cl = local_cluster, X = 1:num_local,
                                           fun = function(i) {
                                             if (verbose >= 1) {
                                               print_logMessage(paste0("--- Performing calculations for subsample ",i),
                                                                verbose_toLogFile = TRUE)
                                             }
                                             clustering <- clustering_local(data = data,
                                                                            sample_local = samples_local[[i]],
                                                                            clusters = clusters, metric = metric,
                                                                            m = m, max_neighbors = max_neighbors,
                                                                            type = type, verbose = verbose,
                                                                            verbose_toLogFile = TRUE)
                                             return(clustering)
                                           })
      stopCluster(local_cluster)

    } else { # other OS than Windows

      clustering_results_list <- mclapply(X = 1:num_local, FUN = function(i) {
        if (verbose >= 1) {
          print_logMessage(paste0("--- Performing calculations for subsample ",i),
                           verbose_toLogFile = TRUE)
        }
        clustering <- clustering_sample(data = data, sample_ids = sample_ids[[i]],
                                        clusters = clusters, metric = metric,
                                        m = m, max_neighbors = max_neighbors,
                                        type = type, verbose = verbose,
                                        verbose_toLogFile = TRUE)
        return(clustering)
      }, mc.cores = cores, mc.set.seed = seed)
    }
  }

  # Selection of best clustering solution (according to smallest average
  # distance to closest cluster medoid):
  min_distance_list <- lapply(X = 1:num_local, FUN = function(i) {
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
  best_solution[["algorithm"]] <- "clarans"
  best_solution[["metric"]] <- metric

  # Return of clustering solution based on the best local iteration:
  class(best_solution) <- c("fuzzyclara", class(best_solution))
  return(best_solution)
}




#' Perform a local iteration of CLARANS clustering
#'
#' Function to perform a local iteration of the CLARANS clustering algorithm in
#' a fixed or fuzzy way. The function can either be called using a common
#' dissimilarity metric or a self-defined distance function.
#'
#' @inheritParams fuzzyclara
#' @param data data.frame to be clustered
#' @param sample_local list containing information on pairs of medoids and
#' non-medoids tested for swapping as well as starting medoids for the
#' algorithm
#' @param verbose_toLogFile If TRUE, the diagnostic messages are printed to
#' a log file \code{clustering_progress.log}. Defaults to FALSE.
#' @param ... Additional arguments passed to the main clustering algorithm
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#'
#' @return Clustering solution for data sample
#'
#' @import checkmate cluster dplyr
#'
#' @references TODO add CLARANS paper
#'
clustering_local <- function(data, sample_local, clusters = 5,
                             metric = "euclidean", max_neighbors = 100,
                             type = "fixed", m = 2, verbose = 1,
                             verbose_toLogFile = FALSE, ...) {

  checkmate::assert_data_frame(data)
  # TODO how to check 'samples_local'?
  checkmate::assert_number(clusters, lower = 1)
  checkmate::assert_choice(type, choices = c("fixed","fuzzy"))
  checkmate::assert_number(m, lower = 1)
  checkmate::assert_choice(verbose, choices = 0:2)
  checkmate::assert_logical(verbose_toLogFile, len = 1)

  # Extract name of metric:
  if (class(metric) == "function") {
    name_metric <- deparse(substitute(metric))

  } else { # 'metric' is no function, but a character name
    name_metric <- metric
  }

  # Compute average distance for clustering based on starting medoids:
  medoids_current <- sample_local$start
  if (type == "fixed") {
    cost_current <- assign_cluster(data = data, medoids = medoids_current,
                                   metric = metric, type = type,
                                   m = m)$avg_min_dist
  }
  else {
    cost_current <- assign_cluster(data = data, medoids = medoids_current,
                                   metric = metric, type = type,
                                   m = m)$avg_weighted_dist
  }

  # Iterative swap of medoids and non-medoids:
  neighbor <- 1
  while (neighbor <= max_neighbors) {

    # Change medoid and non-medoid:
    medoids <- medoids_current
    med <- sample_local$medoids[neighbor]
    non_med <- data$Name[sample_local$non_medoids[neighbor]]

    # Stop if the selected non-medoid is actually a medoid:
    if (non_med %in% medoids_current) {
      neighbor <- neighbor + 1
    }

    else {
      # Compute average distance based on new medoids:
      medoids[med] <- non_med

      # Change current local clustering solution if costs are lower:
      if (type == "fixed") {
        cost <- assign_cluster(data = data, medoids = medoids,
                               metric = metric, type = type,
                               m = m)$avg_min_dist
      }
      else {
        cost <- assign_cluster(data = data, medoids = medoids,
                               metric = metric, type = type,
                               m = m)$avg_weighted_dist
      }
      if (cost < cost_current) {
        cost_current <- cost
        medoids_current <- medoids
      }
      neighbor <- neighbor + 1
    }
  }

  # Create clustering output based on the best medoids:
  clustering_results <- assign_cluster(data = data, medoids = medoids_current,
                                       metric = metric, type = type,
                                       m = m, return_data_medoids = TRUE,
                                       return_distMatrix = TRUE)

  # Return of clustering results:
  return(clustering_results)
}
