#' Prediction of cluster assignments
#'
#' Function to predict cluster assignments
#' @param object an object of class claraclust
#' @param newdata data.frame to look variables with which to predict
#' @param return_distMatrix Should the distanceds to the cluster medoids be
#' returned?
#' @param ... further arguments for predict functions
#' @return clustering plot
#' @export
predict.fuzzyclara <- function(object, newdata, return_distMatrix...){

  # Input checking:
  checkmate::assert_class(x = object, class = "fuzzyclara")
  checkmate::assert(checkmate::check_data_frame(newdata),
                    checkmate::check_matrix(newdata),
                    checkmate::check_null(newdata), combine = "or")

  ## TODO: We need a data.frame with clustering information of the medians
  ##       in the clustering output as well as the underlying metric.
  ##       Otherwise, a cluster prediction won't work.

  # Check if newdata contains all columns the clustering is based on:
  # TO DO

  # Convertion of matrix to data.frame:
  if (!(any(class(newdata) == "data.frame"))) {
    data <- as.data.frame(data)
  }

  # Compute distance to medoids:
  # Calculate the distances to the cluster medoids:
  dist_dat <- proxy::dist(x = newdata, y = object$data_medoids,
                          method = object$metric)

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

  # Preparation of the output object:
  prediction <- list()
  prediction$prediction <- cluster_assignments

  if (return_distMatrix == TRUE) {
    distances_to_medoids <- round(as.data.frame(assignment_dat$Distance_to_Clusters), 2)
    row.names(distances_to_medoids) <- data$Name
    clustering_result[["distance_to_medoids"]] <- distances_to_medoids
  }

  if (type == "fuzzy") {
    # Computation of membership scores:
    prediction$membership_scores <- memb_scores
  }

  return(prediction)
}






