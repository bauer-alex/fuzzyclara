#' Visualization of the selection criterion for different cluster numbers
#'
#' Function to provide graphical visualization for selecting the optimal number of clusters
#' @param data data.frame to be clustered
#' @param clusters_range range for numbers of clusters
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
#' @param return_results return clustering results (claraclust objects) as a list
#' @param ... Additional arguments passed to the main clustering algorithm
#' (\code{\link{pam}} or \code{\link[vegclust]{vegclust}})
#'
#' @return object of class claraclust
#' @import cluster checkmate tibble dplyr tidyselect scales
#' @importFrom stats as.formula prcomp
#' @export

plot_cluster_numbers <- function(data, clusters_range = 2:5, metric = "euclidean", samples = 10,
                                 sample_size = NULL, type = "fixed", cores = 1,
                                 seed = 1234, m = 2, verbose = 1, return_results = FALSE, ...) {

  # Input checking:
  checkmate::assert_numeric(x = clusters_range, lower = 1, upper = nrow(data))

  # Create data.frame with criterion results:

  criterion_df <- data.frame(cluster_number = clusters_range,
                      criterion = rep(0, length(clusters_range)))


  # create list for results

  if(return_results == TRUE){
    results <- list()
  }

  # Extract and append criterion value for each cluster number:

  for(i in clusters_range){
    y <- fuzzyclara(data, clusters = i, metric = metric, sample_size = sample_size, samples = samples,
                    type = type, seed = seed, m = m, verbose = verbose, cores = cores)

    if(return_results == TRUE){
      results[[length(results) + 1]] <- y
    }

    if (type == "fixed") {
      criterion_df[criterion_df$cluster_number == i,]["criterion"] <- y$avg_min_dist
    } else {
      criterion_df[criterion_df$cluster_number == i,]["criterion"] <- y$avg_weighted_dist
    }
  }


  if (type == "fuzzy") {
    ylab_text <- "Minimal Weighted \nAverage Distance"
  } else {
    ylab_text <- "Minimal Average Distance"
  }

  plot <- ggplot(criterion_df) +
    geom_line(aes(x = cluster_number, y = criterion), size = 1, linetype = "dotted", col = "darkslategrey") +
    geom_point(aes(x = cluster_number, y = criterion), size = 5, shape = 19, col = "darkslategrey") +
    ylab(ylab_text) + xlab("Cluster number") +
    theme_minimal() +
    theme(text = element_text(size = 17), axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 17),
          plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
          strip.text.y = element_text(size = 17), legend.text.align = 0,
          strip.placement = "outside", strip.background = element_blank(),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          axis.title.x = element_text(margin = margin(10, 0, 0, 0)))+
    scale_x_continuous(breaks = breaks_width(1))

  # Return the plot (and the cluster results):
  if(return_results == FALSE){
    return(plot)
  } else{
    res <- list(plot, results)
    names(res) <- c("plot", "cluster_results")
    return(res)
  }

}



