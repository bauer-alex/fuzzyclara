#' Print output of "fuzzyclara" object
#'
#' Function to provide graphical visualization of distribution
#'
#' @param x an object of class "fuzzyclara"
#' @param ... additional arguments
#'
#' @return printed output
#'
#' @import checkmate
#' @export
print.fuzzyclara <- function(x, ...) {

  checkmate::assert_class(x, classes = "fuzzyclara")


  cat("Clustering results\n\n")
  cat("Medoids\n")
  print(x$medoids)

  cat("\n")
  cat("Clustering\n")
  print(x$clustering)

  cat("\n")
  if (x$type == "hard") {
    cat("Minimum average distance\n")
    print(x$avg_min_dist)

  } else if (x$type == "fuzzy") {
    cat("Minimum average weighted distance\n")
    print(x$avg_weighted_dist)
    cat("\n")
    cat("Membership scores\n")
    print(x$membership_scores)
  }
}




