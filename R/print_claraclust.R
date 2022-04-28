#' Print output of claraclust object
#'
#' Function to provide graphical visualization of distribution
#' @param x an object of class claraclust
#' @param ... additional arguments
#' @return printed output
#' @export

print.claraclust <- function(x, ...) {
  cat("Clustering results\n\n")
  cat("Medoids\n")
  print(x$medoids)
  cat("\n")
  cat("Clustering\n")
  print(x$clustering)
  cat("\n")
  if (x$type == "fixed") {
    cat("Minimum average distance\n")
    print(x$avg_min_dist)
  }
  if (x$type == "fuzzy") {
    cat("Minimum average weighted distance\n")
    print(x$avg_weighted_dist)
  }
}
