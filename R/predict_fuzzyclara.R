#' Prediction of cluster assignments
#'
#' Function to predict cluster assignments
#'
#' @param object an object of class "fuzzyclara"
#' @param newdata data.frame to look variables with which to predict
#' @param ... further arguments for predict functions
#'
#' @return clustering plot tibble
#'
#' @import checkmate
#' @export
#'
predict.fuzzyclara <- function(object, newdata, ...){

  checkmate::assert_class(object, class = "fuzzyclara")
  checkmate::assert(checkmate::check_data_frame(newdata),
                    checkmate::check_matrix(newdata), # TODO this check doesn't match the argument description above, where 'newdata' is only described as data.frame
                    checkmate::check_null(newdata), combine = "or")


  # Check if newdata contains all columns the clustering is based on:
  if (!any(colnames(object$data_medoids) %in% colnames(newdata))) {
    stop("Newdata does not contain all columns of the clustering.")
  }

  # Convertion of matrix to data.frame:
  if (!(any(class(newdata) == "data.frame"))) {
    newdata <- as.data.frame(newdata)
  }
  
  # Scaling of numerical variables:
  if(is.list(object$scaling)){
    ind <- unlist(lapply(newdata, is.numeric), use.names = TRUE)
    newdata[, ind] <- scale(x = newdata[, ind], center = object$scaling$mean,
                            scale = object$scaling$sd)
  }

  # Adding row.names to column:
  newdata <- newdata %>% tibble::rownames_to_column(var = "Name")
  object$data_medoids <- object$data_medoids %>%
    tibble::rownames_to_column(var = "Name")

  # Assign clusters to new observations:
  assignments <- assign_cluster(data = newdata,
                                medoids = object$medoids,
                                metric = object$metric,
                                type = object$type,
                                m = object$fuzzyness,
                                data_medoids = object$data_medoids,
                                return_distMatrix = TRUE)

  # Preparation of output object:
  if (object$type == "hard") {
    assignments <- list(assignments$clustering, assignments$distance_to_medoids)
    names(assignments) <- c("assignment", "distance_to_medoids")
  }
  else {
    assignments <- list(assignments$clustering, assignments$membership_scores,
                        assignments$distance_to_medoids)
    names(assignments) <- c("assignment", "membership_scores",
                            "distance_to_medoids")
  }

  return(assignments)
}
