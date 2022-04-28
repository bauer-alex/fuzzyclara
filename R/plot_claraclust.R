#' Visualization of clustering solution by variables
#'
#' Function to provide graphical visualization of distribution
#' @param x an object of class claraclust
#' @param data data.frame used for clustering
#' @param variable name of variable to plot
#' @param type type of plot
#' @param group_by grouping variable for scatterplots
#' @param confidence_threshold threshold for fuzzy clustering observations to
#' be plotted
#' @param metric distance metric for silhouette plot, default is euclidean. Irrelevant if silhouette_subsample is TRUE
#' @param silhouette_subsample use the subsample from claraclust for silhouette plot instead of all samples
#' @param scale_sil scale numeric variables for silhouette plot? Default TRUE, irrelevant if silhouette_subsample is TRUE
#' @param plot_all_fuzzy for scatterplot and fuzzy clustering: if confidence threshold > 0, should the observations below the threshold be plotted transparent? Default is FALSE
#' @param ... additional arguments e.g. for distance metric
#' @return clustering plot
#' @import ggplot2 dplyr cluster factoextra ggpubr ggsci ggwordcloud
#' @importFrom stats as.formula prcomp
#' @export

plot.claraclust <- function(x, data, variable = NULL, type = NULL,
                            group_by = NULL, confidence_threshold = 0, 
                            metric = "euclidean", silhouette_subsample = FALSE, scale_sil = TRUE,
                            plot_all_fuzzy = FALSE,
                            ...) {

  # Input checking:
  checkmate::assert(checkmate::check_data_frame(data),
                    checkmate::check_matrix(data), combine = "or")
  checkmate::assert_character(x = variable, null.ok = TRUE)
  checkmate::assert_list(x = x)
  checkmate::assert_numeric(x = confidence_threshold, lower = 0, upper = 1)
  checkmate::assert_choice(x = type,
                           choices = c("grouped", "wordclouds", "silhouette",
                                       "pca", "scatterplot"), null.ok = TRUE)

  # Convertion of matrix to data.frame:
  if (!(any(class(data) == "data.frame"))) {
    data <- as.data.frame(data)
  }

  if (!is.null(variable) && !(variable %in% names(data))) {
    stop("Dataset does not contain the given variable.")
  }

  # Data preparation:

  data <- data %>% # dplyr::select(!!variable) %>%
    mutate(cluster = as.factor(x$clustering))

  int_vars <- unlist(lapply(data, is.integer))
  data[, int_vars] <- lapply(data[, int_vars], as.numeric)


  if (x$type == "fuzzy") {
    # Filter relevant observation based on the membership score threshhold
    relevant_obs <- x$membership_scores %>%
      mutate(max_memb_score = do.call(pmax, c(., na.rm = TRUE))) %>%
      filter(max_memb_score >= confidence_threshold)
    
    
    if(!is.null(type) && type == "scatterplot"){
      transparent_obs <- data %>% dplyr::filter(!(row.names(data) %in%  rownames(relevant_obs)))
    }

    data <- data %>% dplyr::filter(row.names(data) %in%  rownames(relevant_obs))
  }

  # Creation of plot object:
  if (class(data[, variable]) == "numeric") {
    plot <- ggplot2::ggplot(data = data,
                   mapping = aes(x = cluster, y = !!ensym(variable), fill = cluster)) +
      geom_boxplot() + theme_minimal() +
      scale_fill_npg()
  }
  else if (class(data[, variable]) == "factor" & (is.null(type) | !is.null(group_by))) {
    plot <- ggplot2::ggplot(data = data,
                   mapping = aes(x = cluster, fill = !!ensym(variable))) +
      geom_bar(position = "fill") + theme_minimal() +
      scale_fill_brewer(palette = "Accent")
  }

  if (!is.null(type) && type == "grouped") {
    if (is.null(group_by))  stop("Please specify the grouping variable.")

    if (!(group_by %in% names(data))) {
      stop("Dataset does not contain the given grouping variable.")
    }

    plot <- plot + facet_wrap(as.formula(paste("~", group_by)))

  } else if (!is.null(type) && type == "wordclouds") {
    set.seed(42)
    plot <- data %>%
      dplyr::group_by(cluster) %>%
      dplyr::count(!!ensym(variable)) %>%
      mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))) %>%
      ggplot(
        aes(
          label = !!ensym(variable),
          color = cluster,
          angle = angle
        )
      ) +
      geom_text_wordcloud_area(area_corr_power = 1) +
      ggplot2::scale_size_area(max_size = 4) +
      theme_minimal() +
      facet_wrap(~ cluster) +
      scale_color_npg()

  } else if (!is.null(type) && type == "silhouette") {

    if(scale_sil == TRUE){
      ind <- unlist(lapply(data, is.numeric), use.names = TRUE)
      for (i in ind) {
        data[, ind] <- scale(data[, ind])
      }
    }
    
    
    if(silhouette_subsample == FALSE){
      sil <- silhouette(as.numeric(data$cluster), dist(select(data, -cluster), method = metric, ...))
    } else { # use only subsamples from claraclust in order to not calculate the distance matrix between all samples
      data_sub <- data[x$subsample_ids, ]
      sil <- silhouette(as.numeric(data_sub$cluster), x$dist_matrix)
    }

    plot <- fviz_silhouette(sil) + theme_minimal() +
      scale_fill_npg() + scale_color_npg() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  } else if (!is.null(type) && type == "pca") {

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

  } else if (!is.null(type) && type == "scatterplot") {

    if (((!(!is.null(variable) & !is.null(group_by)) ) | !(class(data[, variable]) == "numeric" & class(data[, group_by]) == "numeric"))) {
      stop("Please specify the variables correctly. Both variable and group_by should contain the names of metric variables.")
    }

    plot <- data %>%
      ggplot(aes(x = !!ensym(variable), y = !!ensym(group_by), color = cluster) )+
      geom_point() +
      geom_smooth(method = "lm") +
      theme_minimal()  +
      scale_color_npg()
    
    if(x$type == "fuzzy" && plot_all_fuzzy == TRUE){
      plot <- plot +
        geom_point(data = transparent_obs, aes(x = !!ensym(variable), y = !!ensym(group_by)), alpha = 0.4)
    }

  }
  # Return of the plot:
  return(plot)
}



