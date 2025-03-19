#' Prediction of cluster assignments
#'
#' Function to predict cluster assignments
#'
#' @param object an object of class "fuzzyclara"
#' @param newdata data.frame containing the variables based on which the
#' predictions should be calculated
#' @param ... further arguments for predict functions
#'
#' @return clustering plot tibble
#'
#' @import checkmate
#' @export
#' @examples 
#'
#' # Split data intp test and training data
#' train_indices <- sample(x = nrow(USArrests), size = 0.7*nrow(USArrests))
#' USArrests_train <- USArrests[train_indices, ]
#' USArrests_test <- USArrests[-train_indices, ]
#' 
#' # Determine fuzzy clusters on training data
#' 
#' cc_fuzzy <- fuzzyclara(data        = USArrests_train,
#'                        clusters    = 3,
#'                        metric      = "euclidean",
#'                        samples     = 1,
#'                        sample_size = NULL,
#'                        type        = "fuzzy",
#'                        m           = 2,
#'                        seed        = 3526,
#'                        verbose     = 0)
#' cc_fuzzy
#'
#' # Determine distance matrix of the observations and cluster medoids
#' USArrests_medoids <- USArrests[rownames(USArrests) %in% cc_fuzzy$medoids,]
#' dist <- proxy::dist(x = USArrests_test[, -1], y = USArrests_medoids[, -1],
#'                     method = "euclidean") #use same metric as above
#'
#' # Make cluster prediction for test data
#'
#' USArrests_clusters_predicted <- predict(object = cc_fuzzy,
#'                                         newdata = USArrests_test,
#'                                         dist_matrix = dist)
#' USArrests_clusters_predicted$membership_scores
#'
predict.fuzzyclara <- function(object, newdata, ...){

  checkmate::assert_class(object, class = "fuzzyclara")
  checkmate::assert(checkmate::check_data_frame(newdata),
                    checkmate::check_matrix(newdata),
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
                                m = object$fuzzyness_exponent,
                                data_medoids = object$data_medoids,
                                return_distMatrix = TRUE)

  # Preparation of output object:
  if (object$type == "hard") {
    assignments <- list(assignments$clustering, assignments$distance_to_medoids)
    names(assignments) <- c("clustering", "distance_to_medoids")
  }
  else {
    assignments <- list(assignments$clustering, assignments$membership_scores,
                        assignments$distance_to_medoids)
    names(assignments) <- c("clustering", "membership_scores",
                            "distance_to_medoids")
  }

  return(assignments)
}
