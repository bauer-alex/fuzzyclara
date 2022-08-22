test_that("plot_fuzzyclara_clara", { # plots

  data(USArrests)

  cc_fixed <- fuzzyclara(data        = USArrests,
                         clusters    = 3,
                         metric      = "euclidean",
                         samples     = 1,
                         sample_size = NULL,
                         type        = "fixed",
                         seed        = 3526,
                         verbose     = 0)

  USArrests_enriched <- USArrests %>%
    mutate(State = as.factor(rownames(USArrests)),
           Area  = as.factor(case_when(State %in% c("Washington", "Oregon",
                                                    "California", "Nevada", "Arizona", "Idaho", "Montana",
                                                    "Wyoming", "Colorado", "New Mexico", "Utah", "Hawaii",
                                                    "Alaska") ~ "West",
                                       State %in% c("Texas", "Oklahoma", "Arkansas", "Louisiana",
                                                    "Mississippi", "Alabama", "Tennessee", "Kentucky", "Georgia",
                                                    "Florida", "South Carolina", "North Carolina", "Virginia",
                                                    "West Virginia") ~ "South",
                                       State %in% c("Kansas", "Nebraska", "South Dakota",
                                                    "North Dakota", "Minnesota", "Missouri", "Iowa", "Illinois",
                                                    "Indiana", "Michigan", "Wisconsin", "Ohio") ~ "Midwest",
                                       State %in% c("Maine", "New Hampshire", "New York",
                                                    "Massachusetts", "Rhode Island", "Vermont", "Pennsylvania",
                                                    "New Jersey", "Connecticut", "Delaware", "Maryland") ~
                                         "Northeast")))

  ## error for wrong variable specification
  expect_error(plot(x = cc_fixed, data = USArrests_enriched, variable = "Assalt"))

  ## convert data if class is not data.frame
  p <- plot(x = cc_fixed, data = as.matrix(USArrests_enriched[, 1:2]), variable = "Assault")
  # check class of object
  expect_s3_class(p, "ggplot")


  ## Boxplot
  p <- plot(x = cc_fixed, data = USArrests_enriched, variable = "Assault")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBoxplot")

  # Grouped Boxplot
  p <- plot(x = cc_fixed, data = USArrests_enriched, variable = "Assault", group_by = "Area")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBoxplot")


  ## Barplot
  p <- plot(x = cc_fixed, data = USArrests_enriched, variable = "Area")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")

  ## Grouped Barplot
  p <- plot(x = cc_fixed, data = USArrests_enriched, variable = "Area", group_by = "State")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")


  ## Wordcloud
  p <- plot(x = cc_fixed, data = USArrests_enriched, variable = "State",
            type = "wordclouds")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomTextWordcloud")


  ## Scatterplot
  p <- plot(x = cc_fixed, data = USArrests_enriched, type = "scatterplot",
            x_var = "Murder", y_var = "Assault")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  expect_error(plot(x = cc_fixed, data = USArrests_enriched, type = "scatterplot",
                    x_var = "Murder", y_var = "Area"))

  ## PCA
  p <- plot(x = cc_fixed, data = USArrests_enriched, type = "pca")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  p <- plot(x = cc_fixed, data = USArrests_enriched, type = "pca", group_by = "Area")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  ## Silhouette
  invisible(capture.output(p <- plot(x = cc_fixed, data = USArrests, type = "silhouette",
                                     silhouette_subsample = TRUE)))
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")

  invisible(capture.output(p <- plot(x = cc_fixed, data = USArrests, type = "silhouette")))
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")

  # compare silhouette values to manually calculated values
  dat_sil <- p$data
  data <- scale(USArrests)
  dat_sil_manual <- silhouette(as.numeric(cc_fixed$clustering), dist(data))
  expect_identical(round(sort(dat_sil$sil_width), 2), round(sort(dat_sil_manual[, 3]), 2))


  # errors:
  expect_error(plot(x = cc_fixed, data = USArrests_enriched,
                    variable = "Assault", group_by = "area"))
  expect_error(plot(x = cc_fixed, data = USArrests_enriched,
                    variable = "Area", group_by = "area"))
  expect_error(plot(x = cc_fixed, data = USArrests_enriched, variable = "state",
                    type = "wordclouds"))


  ## confidence threshold for fuzzy clustering
  cc_fuzzy <- fuzzyclara(data        = USArrests,
                         clusters    = 3,
                         metric      = "euclidean",
                         samples     = 1,
                         sample_size = NULL,
                         type        = "fuzzy",
                         m           = 2,
                         seed        = 3526,
                         verbose     = 0)

  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "scatterplot",
            x_var = "Murder", y_var = "Assault", confidence_threshold = 0.5,
            plot_all_fuzzy = TRUE)
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  # check if number of observations for plot is right
  n_obs_conf <- sum(pmax(cc_fuzzy$membership_scores$Cluster1,
                         cc_fuzzy$membership_scores$Cluster2,
                         cc_fuzzy$membership_scores$Cluster3) >= 0.5)
  expect_identical(n_obs_conf, nrow(p$data))

  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "pca", confidence_threshold = 0.5,
            plot_all_fuzzy = TRUE)
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "pca", group_by = "Area",
            confidence_threshold = 0.5,
            plot_all_fuzzy = TRUE)
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")


  # silhouette, fuzzy, silhouette_subsample = TRUE
  invisible(capture.output(p <- plot(x = cc_fuzzy, data = USArrests, type = "silhouette",
                                     silhouette_subsample = TRUE)))
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")


  # focus scatterplot
  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "scatterplot",
            x_var = "Murder", y_var = "Assault", focus = TRUE, focus_clusters = c(1, 2))
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  expect_error(plot(x = cc_fuzzy, data = USArrests_enriched, type = "scatterplot",
                    x_var = "Murder", y_var = "Assault",
                    focus = TRUE, focus_clusters = c(0, 2)))

  # focus pca
  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "pca",
            focus = TRUE, focus_clusters = c(1, 2))
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "pca",
            focus = TRUE, focus_clusters = c(1, 2), group_by = "Area")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  expect_error(plot(x = cc_fuzzy, data = USArrests_enriched, type = "pca",
                    focus = TRUE, focus_clusters = c(0, 2)))


})

test_that("plot_fuzzyclara_clarans", { # plots

  data(USArrests)

  cc_fixed   <- fuzzyclara(data        = USArrests,
                           clusters    = 3,
                           metric      = "euclidean",
                           algorithm   = "clarans",
                           num_local   = 2,
                           max_neighbors = 20,
                           type        = "fixed",
                           seed        = 3526,
                           verbose     = 0)

  USArrests_enriched <- USArrests %>%
    mutate(State = as.factor(rownames(USArrests)),
           Area  = as.factor(case_when(State %in% c("Washington", "Oregon",
                                                    "California", "Nevada", "Arizona", "Idaho", "Montana",
                                                    "Wyoming", "Colorado", "New Mexico", "Utah", "Hawaii",
                                                    "Alaska") ~ "West",
                                       State %in% c("Texas", "Oklahoma", "Arkansas", "Louisiana",
                                                    "Mississippi", "Alabama", "Tennessee", "Kentucky", "Georgia",
                                                    "Florida", "South Carolina", "North Carolina", "Virginia",
                                                    "West Virginia") ~ "South",
                                       State %in% c("Kansas", "Nebraska", "South Dakota",
                                                    "North Dakota", "Minnesota", "Missouri", "Iowa", "Illinois",
                                                    "Indiana", "Michigan", "Wisconsin", "Ohio") ~ "Midwest",
                                       State %in% c("Maine", "New Hampshire", "New York",
                                                    "Massachusetts", "Rhode Island", "Vermont", "Pennsylvania",
                                                    "New Jersey", "Connecticut", "Delaware", "Maryland") ~
                                         "Northeast")))

  ## error for wrong variable specification
  expect_error(plot(x = cc_fixed, data = USArrests_enriched, variable = "Assalt"))

  ## convert data if class is not data.frame
  p <- plot(x = cc_fixed, data = as.matrix(USArrests_enriched[, 1:2]), variable = "Assault")
  # check class of object
  expect_s3_class(p, "ggplot")


  ## Boxplot
  p <- plot(x = cc_fixed, data = USArrests_enriched, variable = "Assault")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBoxplot")

  # Grouped Boxplot
  p <- plot(x = cc_fixed, data = USArrests_enriched, variable = "Assault", group_by = "Area")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBoxplot")


  ## Barplot
  p <- plot(x = cc_fixed, data = USArrests_enriched, variable = "Area")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")

  ## Grouped Barplot
  p <- plot(x = cc_fixed, data = USArrests_enriched, variable = "Area", group_by = "State")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")


  ## Wordcloud
  p <- plot(x = cc_fixed, data = USArrests_enriched, variable = "State",
            type = "wordclouds")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomTextWordcloud")


  ## Scatterplot
  p <- plot(x = cc_fixed, data = USArrests_enriched, type = "scatterplot",
            x_var = "Murder", y_var = "Assault")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  expect_error(plot(x = cc_fixed, data = USArrests_enriched, type = "scatterplot",
                    x_var = "Murder", y_var = "Area"))

  ## PCA
  p <- plot(x = cc_fixed, data = USArrests_enriched, type = "pca")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  p <- plot(x = cc_fixed, data = USArrests_enriched, type = "pca", group_by = "Area")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  ## Silhouette
  expect_error(plot(x = cc_fixed, data = USArrests, type = "silhouette",
                                     silhouette_subsample = TRUE))

  invisible(capture.output(p <- plot(x = cc_fixed, data = USArrests, type = "silhouette")))
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")

  # compare silhouette values to manually calculated values
  dat_sil <- p$data
  data <- scale(USArrests)
  dat_sil_manual <- silhouette(as.numeric(cc_fixed$clustering), dist(data))
  expect_identical(round(sort(dat_sil$sil_width), 2), round(sort(dat_sil_manual[, 3]), 2))


  # errors:
  expect_error(plot(x = cc_fixed, data = USArrests_enriched,
                    variable = "Assault", group_by = "area"))
  expect_error(plot(x = cc_fixed, data = USArrests_enriched,
                    variable = "Area", group_by = "area"))
  expect_error(plot(x = cc_fixed, data = USArrests_enriched, variable = "state",
                    type = "wordclouds"))


  ## confidence threshold for fuzzy clustering
  cc_fuzzy  <- fuzzyclara(data        = USArrests,
                          clusters    = 3,
                          metric      = "euclidean",
                          algorithm   = "clarans",
                          num_local   = 2,
                          max_neighbors = 20,
                          type        = "fuzzy",
                          m = 3,
                          seed        = 3526,
                          verbose     = 0)

  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "scatterplot",
            x_var = "Murder", y_var = "Assault", confidence_threshold = 0.5,
            plot_all_fuzzy = TRUE)
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  # check if number of observations for plot is right
  n_obs_conf <- sum(pmax(cc_fuzzy$membership_scores$Cluster1,
                         cc_fuzzy$membership_scores$Cluster2,
                         cc_fuzzy$membership_scores$Cluster3) >= 0.5)
  expect_identical(n_obs_conf, nrow(p$data))

  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "pca", confidence_threshold = 0.5,
            plot_all_fuzzy = TRUE)
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "pca", group_by = "Area",
            confidence_threshold = 0.5,
            plot_all_fuzzy = TRUE)
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")


  # silhouette, fuzzy
  expect_error(plot(x = cc_fuzzy, data = USArrests, type = "silhouette",
                                     silhouette_subsample = TRUE))

  invisible(capture.output(p <- plot(x = cc_fuzzy, data = USArrests, type = "silhouette")))
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")


  # focus scatterplot
  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "scatterplot",
            x_var = "Murder", y_var = "Assault", focus = TRUE, focus_clusters = c(1, 2))
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  expect_error(plot(x = cc_fuzzy, data = USArrests_enriched, type = "scatterplot",
                    x_var = "Murder", y_var = "Assault",
                    focus = TRUE, focus_clusters = c(0, 2)))

  # focus pca
  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "pca",
            focus = TRUE, focus_clusters = c(1, 2))
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  p <- plot(x = cc_fuzzy, data = USArrests_enriched, type = "pca",
            focus = TRUE, focus_clusters = c(1, 2), group_by = "Area")
  # check class of object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

  expect_error(plot(x = cc_fuzzy, data = USArrests_enriched, type = "pca",
                    focus = TRUE, focus_clusters = c(0, 2)))


})
