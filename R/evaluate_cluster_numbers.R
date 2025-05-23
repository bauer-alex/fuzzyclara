#' Visualization of the selection criterion for different cluster numbers
#'
#' Function to provide graphical visualization for selecting the optimal number
#' of clusters.
#' The function performs clustering for a range of potential numbers of
#' clusters. The optional graphical visualization shows the minimal (weighted)
#' average distance for every cluster numbers. Detailed fuzzyclara clustering
#' results can be returned with return_results = TRUE. For clara clustering,
#' the same samples are used for all numbers of clusters.
#'
#' @param data data.frame to be clustered
#' @param clusters_range Evaluated range for the number of clusters. Defaults to
#' \code{2:5}.
#' @param metric A character specifying a predefined dissimilarity metric (like
#' \code{"euclidean"} or \code{"manhattan"}) or a self-defined dissimilarity
#' function. Defaults to \code{"euclidean"}. Will be passed as argument
#' \code{method} to \code{\link[proxy]{dist}}, so check \code{?proxy::dist} for
#' full details.
#' @param algorithm One of \code{c("clara","clarans")}
#' @param samples Number of subsamples
#' @param sample_size Number of observations belonging to a sample. If NULL
#' (default), the minimum of \code{nrow(data)} and \code{40 + clusters * 2} is
#' used as sample size.
#' @param build Additional build algorithm to choose initial medoids (only
#' relevant for type = "fuzzy". Default FALSE.)
#' @param max_neighbors Maximum number of randomized medoid searches with each
#' cluster (only if \code{algorithm = "clarans"})
#' @param num_local Number of clustering iterations (only if
#' \code{algorithm = "clarans"}).
#' @param type One of \code{c("hard","fuzzy")}, specifying the type of
#' clustering to be performed.
#' @param m Fuzziness exponent (only for \code{type = "fuzzy"}), which has to be
#' a numeric of minimum 1. Defaults to 2.
#' @param cores Numbers of cores for computation. \code{cores > 1} implies
#' a parallel call. Defaults to 1.
#' @param seed Random number seed. Defaults to 1234.
#' @param scale Scale numeric variables before distance matrix calculation?
#' Default TRUE
#' @param verbose Can be set to integers between 0 and 2 to control the level of
#' detail of the printed diagnostic messages. Higher numbers lead to more detailed
#' messages. Defaults to 1.
#' @param plot Should a plot with minimum distances be returned? Defaults to
#' TRUE.
#' @param return_results Indicator if clustering results ("fuzzyclara" objects)
#' should be returned as a list. Defaults to FALSE.
#' @param ... Additional arguments passed to the main clustering algorithm call
#' with \code{\link{fuzzyclara}}.
#'
#' @return Object of class "fuzzyclara"
#'
#' @import checkmate cluster dplyr tibble tidyselect scales
#' @importFrom stats as.formula prcomp
#' @export
#' @examples 
#' # Select optimal number of clusters
#' cc_number <- evaluate_cluster_numbers(data            = USArrests,
#'                                       clusters_range  = 2:6,
#'                                       metric          = "euclidean",
#'                                       samples         = 1,
#'                                       sample_size     = NULL,
#'                                       type            = "hard",
#'                                       seed            = 3526,
#'                                       verbose         = 0)
#' cc_number
#' 
evaluate_cluster_numbers <- function(data,
                                     clusters_range = 2:5,
                                     metric         = "euclidean",
                                     algorithm      = "clara",
                                     samples        = 10,
                                     sample_size    = NULL, 
                                     num_local      = 5,
                                     max_neighbors  = 100,
                                     type           = "hard",
                                     cores          = 1,
                                     seed           = 1234,
                                     m              = 1.5,
                                     scale          = TRUE,
                                     build          = FALSE,
                                     verbose        = 1,
                                     plot           = TRUE,
                                     return_results = FALSE,
                                     ...) {

  checkmate::assert_data_frame(data)
  checkmate::assert_integer(clusters_range, lower = 1, upper = nrow(data))
  checkmate::assert_number(samples, lower = 1)
  checkmate::assert_number(sample_size, lower = 1, null.ok = TRUE)
  checkmate::assert_choice(type, choices = c("hard","fuzzy"))
  checkmate::assert_number(cores, lower = 1)
  checkmate::assert_number(seed)
  checkmate::assert_number(m, lower = 1)
  checkmate::assert_choice(verbose, choices = 0:2)
  checkmate::assert_logical(return_results, len = 1)


  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  sd <- NULL

  # scaling of numerical variables
  if(scale == TRUE){
    ind <- unlist(lapply(data, is.numeric), use.names = TRUE)
    # store scaling parameters in a list
    scaling <- list()
    scaling$mean <- colMeans(data[, ind])
    scaling$sd <- apply(X = data, MARGIN = 2, FUN = sd)
    # scaling
    data[, ind] <- scale(data[, ind])
  }

  # compute clustering for different cluster numbers
  if (algorithm == "clara") {
    y <- clustering_clara(data,
                          clusters    = clusters_range,
                          metric      = metric,
                          sample_size = sample_size,
                          samples     = samples,
                          build       = build,
                          type        = type,
                          seed        = seed,
                          m           = m,
                          verbose     = verbose,
                          cores       = cores)
    # change criteria for fuzzy clustering with a single cluster
    if (type == "fuzzy") {
      names(y[[1]])[3] <- "avg_weighted_dist"
    }
  }
  if (algorithm == "clarans") {
    y <- clustering_clarans(data,
                            clusters      = clusters_range,
                            metric        = metric,
                            max_neighbors = max_neighbors,
                            num_local     = num_local,
                            algorithm     = "clarans",
                            type          = type,
                            seed          = seed,
                            m             = m,
                            verbose       = verbose,
                            cores         = cores,
                            ...)
  }

  if (plot == TRUE) {
    plot_cluster <- plot_cluster_numbers(y)

    # return the plot (and the cluster results)
      if(return_results == FALSE){
        return(plot_cluster)
      } else{
        res <- list(plot_cluster, y)
        names(res) <- c("plot", "cluster_results")
        
        # add scaling parameters to output information
        if (scale == TRUE) {
          res$cluster_results <- lapply(X   = seq_along(res$cluster_results),
                                        FUN = function(i) {
            res$cluster_results[[i]]$scaling <- scaling
            class(res$cluster_results[[i]])  <- c("fuzzyclara", class(res))
            return(res$cluster_results[[i]])
          })
        }
        if (scale == FALSE) {
          res$cluster_results <- lapply(X   = seq_along(res$cluster_results),
                                        FUN = function(i) {
            res$cluster_results[[i]]$scaling <- FALSE
            class(res$cluster_results[[i]])  <- c("fuzzyclara", class(res))
            return(res$cluster_results[[i]])
          })
        }
        
        return(res)
      }
    }

  if (plot == FALSE) {
    if (return_results == TRUE) {
      res <- y
      return(res)
    }
  }
}



#' Function to provide graphical visualization for selecting the optimal number
#' of clusters.
#' The function provides graphical visualization showing the minimal (weighted)
#' average distance for every cluster number.
#' @param cluster_results List of fuzzyclara clustering results with different
#' numbers of clusters.
#' @param clusters_range Optional range for the number of clusters. Defaults to
#' \code{NULL}.
#' @export
plot_cluster_numbers <- function(cluster_results,
                                 clusters_range = NULL) {
  
  checkmate::assert_list(cluster_results)
  checkmate::assert_integer(clusters_range, null.ok = TRUE)
  for (i in seq_along(cluster_results)) {
    checkmate::assert_class(cluster_results[[i]], "fuzzyclara")
  }
  
  # extract cluster range from list of clustering results if not provided
  if (is.null(clusters_range)) {
    clusters_range <- sapply(X = seq_along(cluster_results),
                             FUN = function(i) {length(cluster_results[[i]]$medoids)})
  }
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  cluster_number <- NULL
  
  # create data.frame with criterion results
  criterion_df <- data.frame(cluster_number = clusters_range,
                             criterion = rep(0, length(clusters_range)))
  criterion <- ifelse(cluster_results[[1]]$type == "fuzzy",
                      yes = "avg_weighted_dist", no = "avg_min_dist")
  # select criterion for choice of cluster number
  criterion_df$criterion <- as.numeric(sapply(X   = seq_along(clusters_range),
                                              FUN = function(i) {cluster_results[[i]][criterion]}))
  # sort data.frame according to cluster number in ascending order
  criterion_df <- criterion_df %>% arrange(cluster_number, descending = FALSE)
  
  # visualization
  ylab_text <- ifelse(cluster_results[[1]]$type == "hard", "Minimal Average Distance",
                      "Minimal Weighted \nAverage Distance") # for type = "fuzzy"
  
  plot_cluster <- ggplot(criterion_df) +
    geom_line(aes(x = cluster_number, y = criterion), linewidth = 1,
              linetype = "dotted", col = "darkslategrey") +
    geom_point(aes(x = cluster_number, y = criterion), size = 5, shape = 19,
               col = "darkslategrey") +
    ylab(ylab_text) + xlab("Cluster number") +
    theme_minimal() +
    theme(plot.title        = element_text(hjust = 0.5),
          strip.placement   = "outside",
          strip.background  = element_blank(),
          axis.title.y      = element_text(margin = margin(0, 10, 0, 0)),
          axis.title.x      = element_text(margin = margin(10, 0, 0, 0)))+
    scale_x_continuous(breaks = breaks_width(1))
  return(plot_cluster)
}
