#' Assign a cluster to each observation of the entire dataset
#'
#' Function to assign a cluster to each observation of the entire dataset
#' by selecting the closest medoid
#'
#' @inheritParams fuzzyclara
#' @param data Entire data.frame
#' @param medoids Medoids of the obtained clustering solution for the data
#' sample
#* @param dist_matrix Optional dissimilarity matrix (defaults to NULL)
#' @param return_distMatrix Indicator if the distances to the cluster medoids
#' should be returned. Defaults to FALSE.
#' @param return_data_medoids Indicator if the medoid data should be returned.
#' Defaults to FALSE.
#'
#' @return List with information on cluster results (medoid, cluster
#' assignment, average distance to the closest medoid (weighted
#' average distance to the closest medoid in case of fuzzy clustering))
#'
#' @import proxy
#'
assign_cluster <- function(data, metric, medoids, dist_matrix = NULL,
                           type = "fixed", m = 2, return_distMatrix = FALSE,
                           return_data_medoids = FALSE) {

  checkmate::assert_data_frame(data)
  # TODO what check to run on 'medoids'? The argument specification in the documentation above should also be more specific
  checkmate::assert_choice(type, choices = c("fixed", "fuzzy"))
  checkmate::assert_matrix(dist_matrix, null.ok = TRUE)
  checkmate::assert_number(m, lower = 1)
  checkmate::assert_logical(return_distMatrix, len = 1)
  checkmate::assert_logical(return_data_medoids, len = 1)

  # Calculate distance matrix between all observation and the medoids
  # (if not already given to the function):
  if (is.null(dist_matrix)) {
    # Extraction of obtained medoids of the data:
    data_medoids <- data %>% filter(Name %in% medoids)

    # Calculate the distances to the cluster medoids:
    dist <- proxy::dist(x = data[, -1], y = data_medoids[, -1],
                        method = metric)
  }
  else {
    dist <- dist_matrix[, medoids]
  }

  # Assignment to the medoid with minimum distance:
  cluster_assignments <- apply(dist, 1, which.min)

  # Computation of membership scores in case of fuzzy clustering:
  if (type == "fuzzy") {
    memb_scores_list <- apply(dist, 1, function(x) {
      data.frame(t(as.numeric(calculate_memb_score(dist_med = x, m = m))))
    })
    memb_scores <- dplyr::bind_rows(memb_scores_list)
    colnames(memb_scores) <- paste0("Cluster", 1:ncol(memb_scores))
  }

  # Computation of distance for fixed and fuzzy clustering:
  if (type == "fixed") {
    # Minimum distance:
    distances <- apply(dist, 1, min)
  } else { # type = "fuzzy"
    # Weighted distance (membership scores as weights):
    distances <- rowSums(dist * memb_scores)
  }

  # Return of clustering information:
  dist_dat <- as.data.frame(dist[1:nrow(dist),])
  colnames(dist_dat) <- paste0("Distance_to_Cluster", 1:ncol(dist_dat))
  assignment_dat     <- data.frame("assignment" = cluster_assignments,
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

  # Compute distance:
  if (type == "fixed") { # average distance for fixed clustering
    clustering_result[["avg_min_dist"]] <- average_dist

  } else { # type = "fuzzy": weighted average distance for fuzzy clustering

    # Computation of membership scores:
    clustering_result[["avg_weighted_dist"]] <- average_dist
    membership            <- as.data.frame(assignment_dat$membership)
    row.names(membership) <- data$Name
    clustering_result[["membership_scores"]] <- membership
  }

  if (return_distMatrix) {
    distances_to_medoids <- round(as.data.frame(assignment_dat$Distance_to_Clusters), 2)
    row.names(distances_to_medoids) <- data$Name
    clustering_result[["distance_to_medoids"]] <- distances_to_medoids
  }

  if (return_data_medoids) {
    row.names(data_medoids)             <- data_medoids[, 1]
    clustering_result[["data_medoids"]] <- data_medoids[, -1]
  }

  # Return of clustering results:
  return(clustering_result)
}


#' Calculate membership score of one observation for each medoid
#'
#' Function to calculate a membership score for one observation
#' for each medoid based on the distance of this observation to all medoids
#'
#' @param dist_med Vector of distances to medoids
#' @param m Fuzziness exponent, which has to be a numeric of minimum 1. Defaults
#' to 2.
#'
#' @return List with membership scores for one observation
#'
#' @import checkmate
#'
calculate_memb_score <- function(dist_med, m = 2) {

  # TODO what checkmate checks to run on 'dist_med'? The documentation above should also be more specific.
  checkmate::assert_number(m, lower = 1)


  perfect_match    <- match(x = 0, table = dist_med)
  list_memb        <- as.list(rep(x = 0, times = length(dist_med)))
  names(list_memb) <- paste0("Cluster_", 1:length(dist_med))

  if (!is.na(perfect_match)) {
    list_memb[[paste0("Cluster_", perfect_match)]] <- 1

  } else {
    for (i in 1:length(dist_med)) {
      dist_proportion         <- dist_med[i] / dist_med
      dist_proportion_exp     <- dist_proportion ^ (1 / (m - 1))
      dist_proportion_exp_inv <- 1 / sum(dist_proportion_exp)
      list_memb[[i]]          <- dist_proportion_exp_inv
    }

  }
  return(list_memb)
}
