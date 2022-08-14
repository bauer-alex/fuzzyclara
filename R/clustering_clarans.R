#' Perform clarans clustering algorithm
#'
#' Function to perform clara clustering algorithm in a fixed or fuzzy way.
#' The function can either be performed using a common dissimilarity metric or
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
#' @param ... Additional arguments passed to the main clustering algorithm and
#' to proxy::dist for the calculation of the distance matrix
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#'
#' @return Object of class fuzzyclara
#'
#' @import checkmate cluster dplyr parallel tibble tidyselect
#'
clustering_clarans <- function(data, clusters = 5, metric = "euclidean",
                               type = "fixed", max_neighbors = 100,
                               num_local = num_local, cores = 1, seed = 1234, # TODO 'num_local = num_local'?!
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

  # For each clustering iteration:
  iteration <- 1
  while (iteration <= num_local) {

    # Randomly select starting medoids:
    medoids_best_it <- sample(x = data$Name[1:nrow(data)], size = clusters,
                              replace = FALSE)

    # Extraction of obtained medoids of the data:
    data_medoids <- data %>% filter(Name %in% medoids_best_it)

    # Calculate the distances to the cluster medoids:
    dist_dat <- proxy::dist(x = data[, -1], y = data_medoids[, -1],
                            method = metric)

    # Assignment to the medoid with minimum distance:
    # data$Clustering <- apply(dist_dat, 1, which.min)

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
    distance_best_it <- sum(distances)

    if (iteration == 1) {
      medoids_best <- medoids_best_it
      distance_best <- distance_best_it
    }

    # Test of other medoids:
    neighbor <- 0
    while (neighbor <= max_neighbors) {

      # Randomly draw a pair of medoid and non-medoids:
      sample_med <- sample(x = data_medoids$Name[1:nrow(data_medoids)],
                           size = 1)
      sample_non_med <- sample(x = data$Name[1:nrow(data)], size = 1)

      # Exchange drawn medoid and non-medoid:
      medoids <- medoids_best
      medoids[medoids == sample_med] <- data$Name[data$Name == sample_non_med]

      # Extraction of obtained medoids of the data:
      data_medoids <- data %>% filter(Name %in% medoids)

      # Calculate the distances to the cluster medoids:
      dist_dat <- proxy::dist(x = data[, -1], y = data_medoids[, -1],
                              method = metric)

      # Assignment to the medoid with minimum distance:
      # data$Clustering <- apply(dist_dat, 1, which.min)

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
      distance <- sum(distances)

      # Change best medoids if distance criteria is minimal:
      if (distance < distance_best_it) {
        medoids_best_it <- medoids
        distance_best_it <- distance
      }
      # Raise neighbor if old medoid has lower distance:
      else {
        neighbor <- neighbor + 1
      }
    }

    # Change current best cluster solution if minimum distance is reached:
    if (distance_best_it < distance_best) {
      medoids_best <- medoids_best_it
      distance_best <- distance_best_it
    }
    iteration <- iteration + 1
  }

  # Computation of clustering with best medoid set:

  # Subsampling of data:
  data_medoids <- data %>% filter(Name %in% medoids_best)

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
  clustering_result[["type"]] <- type
  clustering_results[["metric"]] <- metric

  # TODO Kann das weg?
  #if (return_distMatrix) {
  #  distances_to_medoids <- round(as.data.frame(assignment_dat$Distance_to_Clusters), 2)
  #  row.names(distances_to_medoids) <- data$Name
  #  clustering_result[["distance_to_medoids"]] <- distances_to_medoids
  #}

  # Return of clustering solution based on the best sample:
  class(clustering_result) <- c("fuzzyclara", class(clustering_result))
  return(clustering_result)
}
