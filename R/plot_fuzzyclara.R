#' Visualization of clustering solution by variables
#'
#' Function to provide graphical visualization of distribution
#'
#' @param x An object of class "fuzzyclara"
#' @param data data.frame or matrix used for clustering
#' @param type,variable Type of plot. One of \code{c("barplot","boxplot","wordclouds",
#' "silhouette","pca","scatterplot")}. Defaults to NULL, which either plots
#' a barplot or a boxplot, depending on the class of \code{variable}.
#' @param na.omit Should missing values be excluded for plotting? Defaults to
#' FALSE.
#' @param confidence_threshold Threshold for fuzzy clustering observations to
#' be plotted. Must be a number between 0 and 1. Defaults to 0.
#' @param ... Further arguments for internal plot functions.
#'
#' @return Clustering plot
#'
#' @import checkmate cluster dplyr factoextra ggplot2 ggpubr
#' @importFrom stats as.formula prcomp
#' @export
#'
plot.fuzzyclara <- function(x, data, type = NULL, variable = NULL,
                            na.omit = FALSE, confidence_threshold = 0, ...){

  checkmate::assert_class(x, class = "fuzzyclara")
  checkmate::assert(checkmate::check_data_frame(data),
                    checkmate::check_matrix(data), combine = "or")
  checkmate::assert_choice(type,
                           choices = c("barplot","boxplot","wordclouds",
                                       "silhouette","pca", "scatterplot"),
                           null.ok = TRUE)
  checkmate::assert_character(variable, null.ok = TRUE)
  checkmate::assert_logical(na.omit, len = 1)


  # Convertion of matrix to data.frame:
  if (!(any(class(data) == "data.frame"))) {
    data <- as.data.frame(data)
  }

  # Data preparation:
  data <- data %>%
    mutate(cluster = as.factor(x$clustering))

  int_vars <- unlist(lapply(data, is.integer))
  data[, int_vars] <- lapply(data[, int_vars], as.numeric)

  # if PCA, scale the data
  if (!is.null(type) && type == "pca"){
    ind <- unlist(lapply(data, is.numeric), use.names = TRUE)
    for (i in ind) {
      data[, ind] <- scale(data[, ind])
    }
  }
 
  # Handle 'type = NULL':
  if (is.null(type)) {
    # Check if 'variable' argument was specified
    if (is.null(variable)) {
      stop("Please specify the 'type' or the variable' argument.")
    }
    
    type <- ifelse(class(data[[variable]]) != "numeric", "barplot", "boxplot")
  }
  

  # Creation of plot object:
  if (type == "barplot") {
    plot <- clara_barplot(x = x, data = data, variable = variable,
                          na.omit = na.omit,
                          confidence_threshold = confidence_threshold, ...)

  } else if (type == "boxplot") {
    plot <- clara_boxplot(x = x, data = data, variable = variable,
                          na.omit = na.omit,
                          confidence_threshold = confidence_threshold, ...)

  } else if (type == "wordclouds") {
    plot <- clara_wordcloud(x = x, data = data, variable = variable,
                            na.omit = na.omit, ...)

  } else if (type == "silhouette") {
    plot <- clara_silhouette(x = x, data = data,
                             confidence_threshold = confidence_threshold, ...)

  } else if (type == "pca") {
    plot <- clara_pca(x = x, data = data,
                      confidence_threshold = confidence_threshold, ...)

  } else if (type == "scatterplot") {
    plot <- clara_scatterplot(x = x, data = data, na.omit = na.omit,
                              confidence_threshold = confidence_threshold, ...)
  }


  # Return plot:
  return(plot)
}


#' Plot function barplot
#'
#' Function to plot a barplot
#' @param x An object of class "fuzzyclara"
#' @param data Prepared data.frame (contains cluster variable, observations are
#' already filtered by threshold (fuzzy))
#' @param variable Name of variable to plot
#' @param group_by Optional grouping variable
#' @param na.omit Should missing values be excluded for plotting? Defaults to
#' FALSE.
#' @param confidence_threshold Threshold for fuzzy clustering observations to
#' be plotted. Must be a number between 0 and 1. Defaults to 0.
#'
#' @return barplot
#'
#' @import checkmate cluster dplyr factoextra ggplot2 ggpubr
#' @export
#'
clara_barplot <- function(x, data, variable, group_by = NULL,
                          na.omit = FALSE, confidence_threshold = 0) {

  checkmate::assert_class(x, classes = "fuzzyclara")
  checkmate::assert_data_frame(data)
  checkmate::assert_choice(variable, choices = names(data))
  checkmate::assert_character(group_by, null.ok = TRUE)
  checkmate::assert_logical(na.omit, len = 1)
  
  # Some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2:
  cluster <- max_memb_score <- NULL
  
  # Select observations based on confidence_threshold:
  if (x$type == "fuzzy") {
    # Filter relevant observation based on the membership score threshold:
    relevant_obs <- x$membership_scores %>%
      mutate(max_memb_score = do.call(pmax, c(x$membership_scores, na.rm = TRUE))) %>%
      filter(max_memb_score >= confidence_threshold)
    data <- data %>% dplyr::filter(row.names(data) %in%  rownames(relevant_obs))
  }
  
  # Remove missing values if specified:
  if (na.omit == TRUE) {
    data <- data %>% filter(!is.na(!!sym(variable)))
  }

  if (is.numeric(as.data.frame(data)[, variable])){
    stop("The specified 'variable' has to be non-numeric.")
  }

  plot <- ggplot2::ggplot(data = data,
                          mapping = aes(x = cluster, fill = !!ensym(variable))) +
    geom_bar(position = "fill") + theme_minimal() +
    scale_fill_brewer(palette = "Accent")

  if(!is.null(group_by)){
    if (!(group_by %in% names(data))) {
      stop("Dataset does not contain the given grouping variable.")
    }

    plot <- plot + facet_wrap(as.formula(paste("~", group_by)))
  }

  return(plot)
}



#' Plot function boxplot
#'
#' Function to plot a boxplot
#'
#' @inheritParams clara_barplot
#'
#' @return boxplot
#'
#' @import checkmate cluster dplyr factoextra ggplot2 ggpubr
#' @export
#'
clara_boxplot <- function(x, data, variable, group_by = NULL,
                          na.omit = FALSE, confidence_threshold = 0) {

  checkmate::assert_class(x, classes = "fuzzyclara")
  checkmate::assert_data_frame(data)
  checkmate::assert_choice(variable, choices = names(data))
  checkmate::assert_character(group_by, null.ok = TRUE)
  checkmate::assert_logical(na.omit, len = 1)


  # Some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2:
  cluster <- max_memb_score <- NULL
  
  # Select observations based on confidence_threshold:
  if (x$type == "fuzzy") {
    # Filter relevant observation based on the membership score threshold:
    relevant_obs <- x$membership_scores %>%
      mutate(max_memb_score = do.call(pmax, c(x$membership_scores, na.rm = TRUE))) %>%
      filter(max_memb_score >= confidence_threshold)
    data <- data %>% dplyr::filter(row.names(data) %in%  rownames(relevant_obs))
  }
  
  # Remove missing values if specified:
  if (na.omit == TRUE) {
    data <- data %>% filter(!is.na(!!sym(variable)))
  }

  if (!is.numeric(as.data.frame(data)[, variable])){
    stop("The specified 'variable' has to be numeric.")
  }

  plot <- ggplot2::ggplot(data = data,
                          mapping = aes(x = cluster, y = !!ensym(variable),
                                        fill = cluster)) +
    geom_boxplot() + theme_minimal()

  if(!is.null(group_by)){
    if (!(group_by %in% names(data))) {
      stop("Dataset does not contain the given grouping variable.")
    }

    plot <- plot + facet_wrap(as.formula(paste("~", group_by)))
  }

  return(plot)
}


#' Plot function wordcloud
#'
#' Function to plot a wordcloud
#'
#' @param x An object of class "fuzzyclara"
#' @param data Prepared data.frame (contains cluster variable, observations are already filtered by threshold (fuzzy))
#' @param variable Name of variable to plot
#' @param na.omit Should missing values be excluded for plotting? Defaults to
#' FALSE.
#' @param seed Random number seed. Defaults to 42.
#'
#' @return wordcloud plot
#'
#' @import checkmate cluster dplyr factoextra ggplot2 ggpubr ggwordcloud
#' @export
#'
clara_wordcloud <- function(x, data, variable, na.omit = na.omit, seed = 42){

  checkmate::assert_class(x, classes = "fuzzyclara")
  checkmate::assert_data_frame(data)
  checkmate::assert_choice(variable, choices = names(data))
  checkmate::assert_number(seed)
  checkmate::assert_logical(na.omit, len = 1)
  
  # Some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2:
  cluster <- var <- angle <- NULL
  
  # Select observations based on confidence_threshold:
  #if (x$type == "fuzzy") {
  #  # Filter relevant observation based on the membership score threshold:
  #  relevant_obs <- x$membership_scores %>%
  #    mutate(max_memb_score = do.call(pmax, c(x$membership_scores, na.rm = TRUE))) %>%
  #    filter(max_memb_score >= confidence_threshold)
  #  data <- data %>% dplyr::filter(!(row.names(data) %in% rownames(relevant_obs)))
  #}
  
  # Remove missing values if specified:
  if (na.omit == TRUE) {
    data <- data %>% filter(!is.na(!!sym(variable)))
  }

  data$var <- data[, variable] # dplyr::count() doesn't work with !!ensym(variable)

  set.seed(seed)
  plot <- data %>%
    dplyr::group_by(cluster) %>%
    dplyr::count(var) %>%
    mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))) %>%
    ggplot(
      aes(
        label = var,
        color = cluster,
        angle = angle
      )
    ) +
    ggwordcloud::geom_text_wordcloud_area(area_corr_power = 1) +
    ggplot2::scale_size_area(max_size = 4) +
    theme_minimal() +
    facet_wrap(~ cluster)

  return(plot)


}


#' Plot function PCA
#'
#' Function to plot PCA results
#'
#' @inheritParams plot.fuzzyclara
#' @param group_by Optional grouping variable
#' @param plot_all_fuzzy For fuzzy clustering and threshold: should observations
#' below threshold be plotted transparent? Defaults to TRUE.
#' @param confidence_threshold Threshold for fuzzy clustering observations to
#' be plotted. Must be a number between 0 and 1. Defaults to 0.
#' @param alpha_fuzzy Alpha value for observations below threshold, only
#' relevant for \code{plot_all_fuzzy = TRUE}. Defaults to 0.4.
#' @param focus For fuzzy clustering, focus on clusters given by variable
#' \code{focus_clusters} and plot observations based on probability of belonging
#' to the respective cluster. Defaults to FALSE.
#' @param focus_clusters Optional vector of integers to focus on specific
#' clusters.
#'
#' @return PCA plot
#'
#' @import checkmate cluster dplyr factoextra ggplot2 ggpubr
#' @importFrom stats as.formula prcomp
#' @export
#'
clara_pca <- function(x, data, group_by = NULL, plot_all_fuzzy = TRUE,
                      confidence_threshold = 0, alpha_fuzzy = 0.4,
                      focus = FALSE, focus_clusters = NULL) {

  checkmate::assert_class(x, classes = "fuzzyclara")
  checkmate::assert_data_frame(data)
  checkmate::assert_character(group_by, null.ok = TRUE)
  checkmate::assert_logical(plot_all_fuzzy, len = 1)
  checkmate::assert_number(confidence_threshold, lower = 0, upper = 1)
  checkmate::assert_number(alpha_fuzzy, lower = 0, upper = 1)
  checkmate::assert_logical(focus, len = 1)
  checkmate::assert_vector(focus_clusters, null.ok = TRUE)

  
  # Some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2:
  max_memb_score <- cluster <- Dim.1 <- Dim.2 <- NULL
  
  
  if (x$type == "fuzzy") {
    # Filter relevant observation based on the membership score threshold:
    relevant_obs <- x$membership_scores %>%
      mutate(max_memb_score = do.call(pmax, c(x$membership_scores, na.rm = TRUE))) %>%
      filter(max_memb_score >= confidence_threshold)
    rel_obs <- rownames(relevant_obs)
    
    # Transparent observations for scatterplots:
    transparent_obs <- data %>% dplyr::filter(!(row.names(data) %in%  rownames(relevant_obs)))
    
  } else {
    transparent_obs <- NULL
  }
  
  
  if(x$type == "fuzzy" && focus == TRUE){ # for focus = TRUE, perform PCA on whole dataset
      data$cluster <- NULL
  }

  num_vars <- unlist(lapply(data, is.numeric))

  # Dimension reduction using PCA
  pca_result <- stats::prcomp(data[, num_vars], center = FALSE, scale = FALSE) # data are already scaled
  individuals_coord <- as.data.frame(get_pca_ind(pca_result)$coord)

  if (!is.null(group_by)) {
    individuals_coord[, group_by] <- data[, group_by]
  }

  
  # Compute the eigenvalues
  eigenvalue    <- round(get_eigenvalue(pca_result), 1)
  variance_perc <- eigenvalue$variance.percent

  # Plot with memberships of all clusters:
  if(x$type == "fuzzy" & focus == TRUE){
    # convert data into long format containing information on membership scores
    individuals_coord <- cbind(individuals_coord, x$membership_scores)
    data_long         <- individuals_coord %>%
      tidyr::gather("cluster", "prob", colnames(x$membership_scores))

    # select only clusters given by focus_clusters
    if(!is.null(focus_clusters)){
      clusters_select <- paste0("Cluster", focus_clusters)
      if(!all(clusters_select %in% data_long$cluster)){
        stop("clusters specified by focus_clusters aren't found in the data.")
      }

      data_long <- data_long %>%
        filter(cluster %in% clusters_select)
    }

    if (!is.null(group_by)) {
      plot <- ggscatter(
        data_long, x = "Dim.1", y = "Dim.2",
        color = "cluster", palette = "npg", # ellipse = TRUE, ellipse.type = "convex",
        alpha = "prob",
        shape = group_by, size = 1.5,  legend = "right", ggtheme = theme_bw(),
        xlab = paste0("Dim 1 (", variance_perc[1], "% )" ),
        ylab = paste0("Dim 2 (", variance_perc[2], "% )" )
      ) + theme_minimal() +
        facet_wrap(~cluster) +
        guides(color = "none", alpha = guide_legend(title = "membership \n probability"))

    } else { # group_by = NULL
      plot <- ggscatter(
        data_long, x = "Dim.1", y = "Dim.2",
        color = "cluster", palette = "npg", # ellipse = TRUE, ellipse.type = "convex",
        alpha = "prob",
        size = 1.5,  legend = "right", ggtheme = theme_bw(),
        xlab = paste0("Dim 1 (", variance_perc[1], "% )" ),
        ylab = paste0("Dim 2 (", variance_perc[2], "% )" )
      ) + theme_minimal() +
        facet_wrap(~cluster) +
        guides(color = "none", alpha = guide_legend(title = "membership \n probability"))
    }

  } else { # normal PCA plot

    # Add clusters
    individuals_coord$cluster <- data$cluster

    if (x$type == "fuzzy") {
      individuals_coord_fuzzy <- individuals_coord %>%
        filter(row.names(individuals_coord) %in% row.names(transparent_obs))
      individuals_coord <- individuals_coord %>%
        filter(!(row.names(individuals_coord) %in% row.names(transparent_obs)))
    }

    if (!is.null(group_by)) {
      plot <- ggscatter(
        individuals_coord, x = "Dim.1", y = "Dim.2",
        color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
        shape = group_by, size = 1.5,  legend = "right", ggtheme = theme_bw(),
        xlab = paste0("Dim 1 (", variance_perc[1], "% )" ),
        ylab = paste0("Dim 2 (", variance_perc[2], "% )" )
      ) + stat_mean(aes(color = cluster), size = 4) + theme_minimal()
    } else {
      plot <- ggscatter(
        individuals_coord, x = "Dim.1", y = "Dim.2",
        color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
        size = 1.5,  legend = "right", ggtheme = theme_bw(),
        xlab = paste0("Dim 1 (", variance_perc[1], "% )" ),
        ylab = paste0("Dim 2 (", variance_perc[2], "% )" )
      ) + stat_mean(aes(color = cluster), size = 4) + theme_minimal()
    }
    
    # Add transparent values for fuzzy clustering:
    if (x$type == "fuzzy" && plot_all_fuzzy == TRUE &&
        nrow(transparent_obs != 0)) {
      if (!is.null(group_by)) {
        plot <- plot + geom_point(
          data = individuals_coord_fuzzy,
          aes(x = Dim.1, y = Dim.2,
              color = cluster, shape = !!ensym(group_by),
              alpha = alpha_fuzzy),
          size = 1.5, show.legend = FALSE)
      } else {
        plot <- plot + geom_point(
          data = individuals_coord_fuzzy,
          aes(x = Dim.1, y = Dim.2, color = cluster,
              alpha = alpha_fuzzy),
          size = 1.5, show.legend = FALSE)
      }
    }
  }

  return(plot)
}


#' Plot function scatterplot
#'
#' Function to plot a scatterplot
#'
#' @inheritParams plot.fuzzyclara
#' @param x_var,y_var Names of x and y variable
#' @param plot_all_fuzzy For fuzzy clustering and threshold: should observations
#' below threshold be plotted transparent? Defaults to TRUE.
#' @param confidence_threshold Threshold for fuzzy clustering observations to
#' be plotted. Must be a number between 0 and 1. Defaults to 0.
#' @param alpha_fuzzy Alpha value for observations below threshold, only
#' relevant for fuzzy clustering and \code{focus = FALSE}. Defaults to 0.4.
#' @param focus For fuzzy clustering, focus on clusters given by variable
#' \code{focus_clusters} and plot observations based on probability of belonging
#' to the respective cluster. Defaults to FALSE.
#' @param focus_clusters Optional vector of integers to focus on specific
#' clusters
#' @param na.omit Should missing values be excluded for plotting? Defaults to
#' FALSE.
#'
#' @return scatterplot
#'
#' @import checkmate cluster dplyr factoextra ggplot2 ggpubr tidyr
#' @export
#'
clara_scatterplot <- function(x, data, x_var, y_var, plot_all_fuzzy = TRUE,
                              confidence_threshold = 0, alpha_fuzzy = 0.4,
                              focus = FALSE, focus_clusters = NULL,
                              na.omit = FALSE) {

  checkmate::assert_class(x, classes = "fuzzyclara")
  checkmate::assert_data_frame(data)
  checkmate::assert_character(x_var, len = 1)
  checkmate::assert_character(y_var, len = 1)
  checkmate::assert_logical(plot_all_fuzzy, len = 1)
  checkmate::assert_number(confidence_threshold, lower = 0, upper = 1)
  checkmate::assert_number(alpha_fuzzy, lower = 0, upper = 1)
  checkmate::assert_logical(focus, len = 1)
  checkmate::assert_vector(focus_clusters, null.ok = TRUE)
  checkmate::assert_logical(na.omit, len = 1)
  

  # Some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2:
  max_memb_score <- variable <- cluster <- prob <- NULL
  
  
  if (((!(!is.null(x_var) & !is.null(y_var)) ) | !(class(data[[x_var]]) == "numeric" & class(data[[y_var]]) == "numeric"))) {
    stop("Please specify the variables correctly. Both variable and group_by should contain the names of metric variables.")
  }
  
  # Remove missing values if specified:
  if (na.omit == TRUE) {
    data <- data %>% filter(!is.na(!!sym(variable)))
  }

  if (x$type == "fuzzy") {
    # Filter relevant observation based on the membership score threshold:
    relevant_obs <- x$membership_scores %>%
      mutate(max_memb_score = do.call(pmax, c(x$membership_scores, na.rm = TRUE))) %>%
      filter(max_memb_score >= confidence_threshold)
    rel_obs <- rownames(relevant_obs)
    
    # Transparent observations for scatterplots:
    transparent_obs <- data %>% dplyr::filter(!(row.names(data) %in%  rownames(relevant_obs)))
    
    data <- data %>% dplyr::filter(row.names(data) %in%  rownames(relevant_obs))
    
  } else {
    transparent_obs <- NULL
  }
  
  
  if (x$type == "fuzzy" & focus == TRUE) {
    data         <- rbind(data, transparent_obs)
    data$cluster <- NULL

    # convert data into long format containing information on membership scores
    data      <- cbind(data, x$membership_scores)
    data_long <- data %>%
      tidyr::gather("cluster", "prob", colnames(x$membership_scores))

    # select only clusters given by focus_clusters
    if (!is.null(focus_clusters)) {
      clusters_select <- paste0("Cluster", focus_clusters)
      if (!all(clusters_select %in% data_long$cluster)) {
        stop("clusters specified by focus_clusters aren't found in the data.")
      }

      data_long <- data_long %>%
        filter(cluster %in% clusters_select)
    }

    plot <- data_long %>%
      ggplot(aes(x = !!ensym(x_var), y = !!ensym(y_var), alpha = prob, color = cluster)) +
      geom_point() +
      theme_minimal()  +
      facet_wrap(~cluster) +
      guides(color = "none", alpha = guide_legend(title = "membership \n probability"))

  } else{ # normal scatterplot
    plot <- data %>%
      ggplot(aes(x = !!ensym(x_var), y = !!ensym(y_var), color = cluster) )+
      geom_point() +
      geom_smooth(method = "lm") +
      theme_minimal()

    if(x$type == "fuzzy" && plot_all_fuzzy == TRUE){
      plot <- plot +
        geom_point(data = transparent_obs, aes(x = !!ensym(x_var), y = !!ensym(y_var)), alpha = alpha_fuzzy)
    }
  }

  return(plot)
}


#' Plot function silhouette
#'
#' Function to plot a scatterplot
#'
#' @inheritParams plot.fuzzyclara
#' @param metric  A character specifying a predefined dissimilarity metric (like
#' \code{"euclidean"} or \code{"manhattan"}) or a self-defined dissimilarity
#' function. Defaults to \code{"euclidean"}. Irrelevant if \code{silhouette_subsample} is TRUE.
#' @param silhouette_subsample Use the subsample from 'x' for silhouette
#' plot instead of all samples? Defaults to FALSE.
#' @param scale_sil Scale numeric variables for silhouette plot? Defaults to
#' TRUE. Irrelevant if \code{silhouette_subsample} is TRUE.
#' @param confidence_threshold Threshold for fuzzy clustering observations to
#' be plotted. Must be a number between 0 and 1. Defaults to 0.
#'
#' @return silhouette plot
#'
#' @import checkmate cluster dplyr factoextra ggplot2 ggpubr
#' @export
#'
clara_silhouette <- function(x, data,
                             metric = "euclidean",
                             silhouette_subsample = FALSE,
                             scale_sil = TRUE,
                             confidence_threshold = 0){

  checkmate::assert_class(x, classes = "fuzzyclara")
  checkmate::assert_data_frame(data)
  # TODO how to check 'metric'? (Edit Jana: in fuzzycara we don't check it either) At least specify 'metric' a bit more in the above documentation -> DONE. Similar to the proxy::dist metric? (Jana: Yes)
  checkmate::assert_logical(silhouette_subsample, len = 1)
  checkmate::assert_logical(scale_sil, len = 1)
  checkmate::assert_number(confidence_threshold, lower = 0, upper = 1)


  # Some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2:
  max_memb_score <- cluster <- NULL
  
  
  if (x$type == "fuzzy") {
    # Filter relevant observation based on the membership score threshold:
    relevant_obs <- x$membership_scores %>%
      mutate(max_memb_score = do.call(pmax, c(x$membership_scores, na.rm = TRUE))) %>%
      filter(max_memb_score >= confidence_threshold)
    rel_obs <- rownames(relevant_obs)
    
    data <- data %>% dplyr::filter(row.names(data) %in%  rownames(relevant_obs))
    
  } else {
    rel_obs <- rownames(x$membership_scores)
  }
  
  
  if (scale_sil == TRUE) {
    ind <- unlist(lapply(data, is.numeric), use.names = TRUE)
    for (i in ind) {
      data[, ind] <- scale(data[, ind])
    }
  }

  if (silhouette_subsample == FALSE) {

    sil <- silhouette(as.numeric(data$cluster), dist(select(data, -cluster), method = metric))

  } else { # use only subsamples from 'x' in order to not calculate the distance matrix between all samples

    checkmate::assert_true(x$algorithm == "clara") # because for clarans, no distance matrix is calculated

    if(x$type == "hard"){ # hard clustering

      data_sub <- data[x$subsample_ids, ]
      sil <- silhouette(as.numeric(data_sub$cluster), x$dist_matrix)

    } else { # x$type = "fuzzy" -> data is already filtered by threshold. Distance matrix has to be filtered too

      # considered observations: part of subsample and rel_obs
      rel_obs_sil <- intersect(rel_obs, rownames(x$distance_to_medoids)[x$subsample_ids])

      data_sub <- data[rel_obs_sil,]

      # get corresponding distance matrix:
      dist_matrix           <- as.matrix(x$dist_matrix)
      rownames(dist_matrix) <- rownames(x$distance_to_medoids)[x$subsample_ids]
      colnames(dist_matrix) <- rownames(x$distance_to_medoids)[x$subsample_ids]

      dist <- dist_matrix[rel_obs_sil, rel_obs_sil]

      sil <- silhouette(as.numeric(data_sub$cluster), dist)

    }
  }

  plot <- fviz_silhouette(sil) + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(plot)
}
