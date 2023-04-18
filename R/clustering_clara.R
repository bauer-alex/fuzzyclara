#' Perform CLARA clustering algorithm
#'
#' Function to perform a CLARA clustering in a hard or fuzzy way.
#' The function can either be called using a common dissimilarity metric or
#' a self-defined distance function.
#'
#' If the clustering is run on mulitple cores, the verbose messages are printed
#' in a file \code{clustering_progress.log} (if \code{verbose > 0}).
#'
#' @inheritParams fuzzyclara
#' @param data data.frame to be clustered
#' @param samples Number of subsamples
#' @param sample_size Number of observations belonging to a sample. If NULL
#' (default), the minimum of \code{nrow(data)} and \code{40 + clusters * 2} is
#' used as sample size.
#' @param build Additional build algorithm to choose initial medoids (only
#' relevant for type = "fuzzy". Default FALSE.)
#' @param ... Additional arguments passed to the main clustering algorithm and
#' to proxy::dist for the calculation of the distance matrix
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#'
#' @return Object of class fuzzyclara
#'
#' @import cluster parallel checkmate tibble dplyr tidyselect
#'
#' @references TODO add CLARA paper
#'
clustering_clara <- function(data, clusters = 5, metric = "euclidean",
                             samples = 10, sample_size = NULL, type = "hard",
                             cores = 1, seed = 1234, m = 1.5, verbose = 1,
                             build = FALSE, ...) {

  checkmate::assert_data_frame(data)
  checkmate::assert_vector(clusters)
  checkmate::assert_number(samples, lower = 1)
  checkmate::assert_number(sample_size, lower = 1, null.ok = TRUE)
  checkmate::assert_number(sample_size, null.ok = TRUE)
  if (!is.null(sample_size)) {
    checkmate::assert_true(sample_size <= nrow(data))
  }
  checkmate::assert_choice(type, choices = c("hard","fuzzy"))
  checkmate::assert_number(cores, lower = 1)
  checkmate::assert_number(seed)
  checkmate::assert_number(m, lower = 1)
  checkmate::assert_choice(verbose, choices = 0:2)
  checkmate::assert_logical(build, len = 1)


  # Setting a seed for random processes:
  set.seed(seed)

  # Define default sample size if no sample size is specified:
  if (is.null(sample_size)) {
    sample_size <- min(nrow(data), (40 + clusters * 2))
  }

  # Warning if sample size is larger than number of observations:
  if (sample_size == nrow(data)) {
    sample_size <- nrow(data)
    samples     <- 1
    sample_ids  <- list(1:nrow(data))
    warning("The specified sample size is equal to the number of
    observations in the data. PAM clustering is performed on the entire data.")

  } else {
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
  if ((type == "fuzzy" & m == 1) | (type == "fuzzy" & length(clusters) == 1 &
      clusters[1] == 1)) {
    type <- "hard"
    change_output_style <- TRUE
  }
  type <- rep(type, length.out = length(clusters))
  if (clusters[1] == 1) {
    type[1] <- "hard"
    change_output_style <- TRUE
  }
  # The resulting output, however, should look the usual fuzzy output.

  # Adding row.names to column:
  data <- data %>% tibble::rownames_to_column(var = "Name")
  row.names(data) <- data$Name

  # Calculation of clustering results for each sample:
  if (cores == 1) { # single core
    clustering_results_list <- lapply(X = 1:samples, FUN = function(i) {

      # Create distance matrix for clustering sample:
      if (verbose >= 1) { message("--- Calculating the distance matrix for subsample ", i)}
      dist_matrix <- compute_distance_matrix(data = data,
                                             sample_ids = sample_ids[[i]],
                                             metric = metric)

      # Perform actual clustering:
      if (verbose >= 1) { message("--- Performing calculations for subsample ", i) }
      # Perform clustering with different cluster numbers:
      clustering_numbers_list <- lapply(X = seq_along(clusters), FUN = function(j) {
        clustering <- clustering_sample(data = data,
                                        sample_ids = sample_ids[[i]],
                                        dist = dist_matrix,
                                        clusters = clusters[j],
                                        metric = metric, m = m, seed = seed,
                                        sample_size = sample_size,
                                        type = type[j], verbose = verbose,
                                        build = build, ...)
        return(clustering)
      })
      return(clustering_numbers_list)
    })

  } else { # cores > 1, i.e. multi-core computation

    print_logMessage(paste0("Run clustering on ", cores, " cores."),
                     verbose_toLogFile = TRUE, reset_logFile = TRUE)

    # Windows:
    if (Sys.info()['sysname'] == "Windows") {

      local_cluster <- makePSOCKcluster(rep("localhost", cores))
      clusterExport(cl = local_cluster,
                    varlist = c("clustering_sample", "compute_distance_matrix",
                                "perform_sample_clustering",
                                "assign_cluster", "calculate_memb_score"),
                    envir = environment(fuzzyclara))
      clustering_results_list <- parLapply(cl = local_cluster, X = 1:samples,
                                           fun = function(i) {

                                             # Create distance matrix for clustering sample:
                                             if (verbose >= 1) { print_logMessage(paste0("--- Calculating the distance matrix for subsample ",i),
                                                                                  verbose_toLogFile = TRUE)}
                                             dist_matrix <- compute_distance_matrix(data = data,
                                                                                    sample_ids = sample_ids[[i]],
                                                                                    metric = metric)

                                             # Perform actual clustering:
                                             if (verbose >= 1) { print_logMessage(paste0("--- Performing calculations for subsample ",i),
                                                                                  verbose_toLogFile = TRUE) }
                                             # Perform clustering with different cluster numbers:
                                             clustering_numbers_list <- lapply(X = seq_along(clusters), FUN = function(j) {
                                               clustering <- clustering_sample(data = data,
                                                                               sample_ids = sample_ids[[i]],
                                                                               dist = dist_matrix, clusters = clusters[j],
                                                                               metric = metric, m = m, seed = seed,
                                                                               sample_size = sample_size,
                                                                               type = type[j], verbose = verbose,
                                                                               build = build, ...)
                                               return(clustering)
                                             })
                                             return(clustering_numbers_list)
                                           })
      stopCluster(local_cluster)

    } else { # other OS than Windows

      clustering_results_list <- mclapply(X = 1:samples, FUN = function(i) {
        # Create distance matrix for clustering sample:
        if (verbose >= 1) { print_logMessage(paste0("--- Calculating the distance matrix for subsample ",i),
                                             verbose_toLogFile = TRUE)}
        dist_matrix <- compute_distance_matrix(data = data,
                                               sample_ids = sample_ids[[i]],
                                               metric = metric)

        # Perform actual clustering:
        if (verbose >= 1) { print_logMessage(paste0("--- Performing calculations for subsample ",i),
                                             verbose_toLogFile = TRUE) }
        # Perform clustering with different cluster numbers:
        clustering_numbers_list <- lapply(X = seq_along(clusters), FUN = function(j) {
          clustering <- clustering_sample(data = data,
                                          sample_ids = sample_ids[[i]],
                                          dist = dist_matrix,
                                          clusters = clusters[j],
                                          metric = metric, m = m, seed = seed,
                                          sample_size = sample_size,
                                          type = type[j], verbose = verbose,
                                          build = build, ...)
          return(clustering)
        })
        return(clustering_numbers_list)
      }, mc.cores = cores, mc.set.seed = seed)
    }
  }

  if (verbose >= 1) {
    print_logMessage("--- Selecting the best clustering solution...",
                     verbose_toLogFile = (cores > 1))
  }

  # Return of results list for all numbers of clusters:
  results_list <- lapply(X = seq_along(clusters), FUN = function(j) {

    # No changed output style for more than one cluster:
    if (j > 1 & type[2] == "fuzzy" & m != 1) {
      change_output_style <- FALSE
    }
    
    # Selection of best clustering solution (according to smallest average
    # distance to closest cluster medoid):
    min_distance_list <- lapply(X = 1:samples, FUN = function(i) {
      if (type[[j]] == "hard") {
        dist <- clustering_results_list[[i]][[j]]$avg_min_dist
      } else { # type = "fuzzy"
        dist <- clustering_results_list[[i]][[j]]$avg_weighted_dist
      }
      return(dist)
    })
    min_distance <- which.min(min_distance_list)
    best_solution <- clustering_results_list[[min_distance]][[j]]
    best_solution[["type"]] <- type[j]
    if (type[[j]] == "hard") {
      m <- 1
    }
    best_solution[["fuzzyness_exponent"]] <- m
    best_solution[["algorithm"]] <- "clara"
    best_solution[["metric"]] <- metric

    # Attach the distance-to-medoids matrix of the full dataset to the result:
    clustering_results <- assign_cluster(data = data,
                                         medoids = best_solution$medoids,
                                         metric = metric, type = type[j], m = m,
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
  })
  
  if (verbose >= 1) {
    print_logMessage("--- Clustering process completed!",
                     verbose_toLogFile = (cores > 1))
  }

  # Change output format if only a single cluster is evaluated:
  if (length(clusters) == 1) {
    results_list <- results_list[[1]]
  }
  return(results_list)
}
