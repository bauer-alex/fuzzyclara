#' Visualization of clustering solution by variables
#'
#' Function to provide graphical visualization of distribution
#' @param x an object of class claraclust
#' @param data data.frame used for clustering
#' @param type type of plot
#' @param confidence_threshold threshold for fuzzy clustering observations to
#' be plotted
#' @param ... further arguments for plot functions
#' @return clustering plot
#' @import ggplot2 dplyr cluster factoextra ggpubr ggsci ggwordcloud
#' @importFrom stats as.formula prcomp
#' @export
plot.claraclust <- function(x, data,
                            type = NULL,
                            confidence_threshold = 0,
                            ...) {

  # Input checking:
  checkmate::assert(checkmate::check_data_frame(data),
                    checkmate::check_matrix(data), combine = "or")
  checkmate::assert_list(x = x)
  checkmate::assert_numeric(x = confidence_threshold, lower = 0, upper = 1)
  checkmate::assert_choice(x = type,
                           choices = c("wordclouds", "silhouette",
                                       "pca", "scatterplot"), null.ok = TRUE)

  # Convertion of matrix to data.frame:
  if (!(any(class(data) == "data.frame"))) {
    data <- as.data.frame(data)
  }

  # Data preparation:
  data <- data %>%
    mutate(cluster = as.factor(x$clustering))

  int_vars <- unlist(lapply(data, is.integer))
  data[, int_vars] <- lapply(data[, int_vars], as.numeric)


  if (x$type == "fuzzy") {
    # Filter relevant observation based on the membership score threshold
    relevant_obs <- x$membership_scores %>%
      mutate(max_memb_score = do.call(pmax, c(., na.rm = TRUE))) %>%
      filter(max_memb_score >= confidence_threshold)
    rel_obs <- rownames(relevant_obs)

    # transparent observations for scatterplot and pca
    transparent_obs <- data %>% dplyr::filter(!(row.names(data) %in%  rownames(relevant_obs)))

    data <- data %>% dplyr::filter(row.names(data) %in%  rownames(relevant_obs))

  } else{
    transparent_obs <- NULL
  }



  # Creation of plot object:
  if (is.null(type)) {
    plot <- clara_bar_boxplot(x = x, data = data, ...)
  }
  else if (!is.null(type) && type == "wordclouds") {
    plot <- clara_wordcloud(x = x, data = data, ...)
  }
  else if (!is.null(type) && type == "silhouette") {
    plot <- clara_silhouette(x = x, data = data,
                             rel_obs = rel_obs, ...)
  }
  else if (!is.null(type) && type == "pca") {
    plot <- clara_pca(x = x, data = data, ...)
  }
  else if (!is.null(type) && type == "scatterplot") {
    plot <- clara_scatterplot(x = x, data = data,
                              transparent_obs = transparent_obs, ...)
  }


  # Return plot:
  return(plot)
}



#' Plot function barplot or boxplot
#'
#' Function to plot a barplot or a boxplot
#' @param x an object of class claraclust
#' @param data prepared data.frame (contains cluster variable, observations are already filtered by threshold (fuzzy))
#' @param variable name of variable to plot
#' @param group_by optional grouping variable
#' @return barplot or boxplot
#' @import ggplot2 dplyr cluster factoextra ggpubr ggsci ggwordcloud
#' @export
clara_bar_boxplot <- function(x, data, variable, group_by = NULL){

  checkmate::assert_character(x = variable)
  checkmate::assert_character(x = group_by, null.ok = TRUE)
  if (!(variable %in% names(data))) {
    stop("Dataset does not contain the given variable.")
  }


  if (class(data[, variable]) == "numeric"){ # boxplot
    plot <- ggplot2::ggplot(data = data,
                            mapping = aes(x = cluster, y = !!ensym(variable), fill = cluster)) +
      geom_boxplot() + theme_minimal() +
      scale_fill_npg()
  } else{ # barplot
    plot <- ggplot2::ggplot(data = data,
                          mapping = aes(x = cluster, fill = !!ensym(variable))) +
      geom_bar(position = "fill") + theme_minimal() +
      scale_fill_brewer(palette = "Accent")
  }

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
#' @param x an object of class claraclust
#' @param data prepared data.frame (contains cluster variable, observations are already filtered by threshold (fuzzy))
#' @param variable name of variable to plot
#' @param seed optional seed
#' @return wordcloud plot
#' @import ggplot2 dplyr cluster factoextra ggpubr ggsci ggwordcloud
#' @export
clara_wordcloud <- function(x, data, variable, seed = 42){

  checkmate::assert_character(x = variable)
  if (!(variable %in% names(data))) {
    stop("Dataset does not contain the given variable.")
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
    geom_text_wordcloud_area(area_corr_power = 1) +
    ggplot2::scale_size_area(max_size = 4) +
    theme_minimal() +
    facet_wrap(~ cluster) +
    scale_color_npg()

  return(plot)


}


#' Plot function PCA
#'
#' Function to plot PCA results
#' @param x an object of class claraclust
#' @param data prepared data.frame (contains cluster variable, observations are already filtered by threshold (fuzzy))
#' @param group_by optional grouping variable
#' @return PCA plot
#' @import ggplot2 dplyr cluster factoextra ggpubr ggsci ggwordcloud
#' @importFrom stats as.formula prcomp
#' @export
clara_pca <- function(x, data, group_by = NULL){

  checkmate::assert_character(x = group_by, null.ok = TRUE)

  num_vars <- unlist(lapply(data, is.numeric))

  # Dimension reduction using PCA
  pca_result <- stats::prcomp(data[, num_vars],  scale = TRUE)
  individuals_coord <- as.data.frame(get_pca_ind(pca_result)$coord)

  # Add clusters
  individuals_coord$cluster <- data$cluster

  if (!is.null(group_by)) {
    individuals_coord[, group_by] <- data[, group_by]
  }

  # Compute the eigenvalues
  eigenvalue <- round(get_eigenvalue(pca_result), 1)
  variance_perc <- eigenvalue$variance.percent


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

  return(plot)


}


#' Plot function scatterplot
#'
#' Function to plot a scatterplot
#' @param x an object of class claraclust
#' @param data prepared data.frame (contains cluster variable, observations are already filtered by threshold (fuzzy))
#' @param x_var name of x variable
#' @param y_var name of y variable
#' @return scatterplot
#' @import ggplot2 dplyr cluster factoextra ggpubr ggsci ggwordcloud
#' @export
clara_scatterplot <- function(x, data, x_var, y_var, transparent_obs = NULL, plot_all_fuzzy = FALSE){

  if (((!(!is.null(x_var) & !is.null(y_var)) ) | !(class(data[, x_var]) == "numeric" & class(data[, y_var]) == "numeric"))) {
    stop("Please specify the variables correctly. Both variable and group_by should contain the names of metric variables.")
  }

  plot <- data %>%
    ggplot(aes(x = !!ensym(x_var), y = !!ensym(y_var), color = cluster) )+
    geom_point() +
    geom_smooth(method = "lm") +
    theme_minimal()  +
    scale_color_npg()

  if(x$type == "fuzzy" && plot_all_fuzzy == TRUE){
    plot <- plot +
      geom_point(data = transparent_obs, aes(x = !!ensym(x_var), y = !!ensym(y_var)), alpha = 0.4)
  }

  return(plot)


}


#' Plot function silhouette
#'
#' Function to plot a scatterplot
#' @param x an object of class claraclust
#' @param data prepared data.frame (contains cluster variable, observations are already filtered by threshold (fuzzy))
#' @param metric distance metric for silhouette plot, default is euclidean. Irrelevant if silhouette_subsample is TRUE
#' @param silhouette_subsample use the subsample from claraclust for silhouette plot instead of all samples
#' @param scale_sil scale numeric variables for silhouette plot? Default TRUE, irrelevant if silhouette_subsample is TRUE
#' @param rel_obs names of observations > threshold
#' @return silhouette plot
#' @import ggplot2 dplyr cluster factoextra ggpubr ggsci ggwordcloud
#' @export
clara_silhouette <- function(x, data,
                             metric = "Euclidean",
                             silhouette_subsample = FALSE,
                             scale_sil = TRUE,
                             rel_obs = NULL){

  if(scale_sil == TRUE){
    ind <- unlist(lapply(data, is.numeric), use.names = TRUE)
    for (i in ind) {
      data[, ind] <- scale(data[, ind])
    }
  }

  if(silhouette_subsample == FALSE){

    sil <- silhouette(as.numeric(data$cluster), dist(select(data, -cluster), method = metric))

  } else { # use only subsamples from claraclust in order to not calculate the distance matrix between all samples

    if(x$type == "fixed"){ # fixed clustering

      data_sub <- data[x$subsample_ids, ]
      sil <- silhouette(as.numeric(data_sub$cluster), x$dist_matrix)

    } else{ # fuzzy clustering -> data is already filtered by threshold. Distance matrix has to be filtered too

      # considered observations: part of subsample and rel_obs
      rel_obs_sil <- intersect(rel_obs, rownames(x$distance_to_medoids)[x$subsample_ids])

      data_sub <- data[rel_obs_sil,]

      # get corresponding distance matrix:
      dist_matrix <- as.matrix(cc_fuzzy$dist_matrix)
      rownames(dist_matrix) <- rownames(x$distance_to_medoids)[x$subsample_ids]
      colnames(dist_matrix) <- rownames(x$distance_to_medoids)[x$subsample_ids]

      dist <- dist_matrix[rel_obs_sil, rel_obs_sil]

      sil <- silhouette(as.numeric(data_sub$cluster), dist)

    }


  }

  plot <- fviz_silhouette(sil) + theme_minimal() +
    scale_fill_npg() + scale_color_npg() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


  return(plot)


}


