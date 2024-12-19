test_that("evaluate_cl_nums_clara", { # plot to select number of clusters

  data(USArrests)

  # hard clustering
  cc_number <- evaluate_cluster_numbers(
    data            = USArrests,
    clusters_range  = 2:6,
    metric          = "euclidean",
    samples         = 1,
    sample_size     = 10,
    type            = "hard",
    seed            = 3526,
    verbose         = 0)

  # check whole object
  expect_s3_class(cc_number, "ggplot")
  expect_s3_class(cc_number$layers[[1]]$geom, "GeomLine")

  expect_length(cc_number, 11)

  expect_identical(dim(cc_number$data),
                   as.integer(c(5, 2)))

  # compare result to manually calculated clustering result:
  cc_hard <- fuzzyclara(data        = USArrests,
                        clusters    = 2,
                        metric      = "euclidean",
                        samples     = 1,
                        sample_size = 10,
                        type        = "hard",
                        seed        = 3526,
                        verbose     = 0)
  expect_identical(round(cc_number$data[1, 2], 2), round(cc_hard$avg_min_dist, 2))

  # fuzzy clustering
  # return clustering results
  cc_number <- evaluate_cluster_numbers(
    data           = USArrests,
    clusters_range = 2:6,
    metric         = "euclidean",
    samples        = 1,
    sample_size    = 10,
    type           = "fuzzy",
    m              = 2,
    seed           = 3526,
    verbose        = 0,
    return_results = TRUE)

  # check whole object
  expect_class(cc_number, "list")
  expect_length(cc_number, 2)

  cluster_results <- cc_number$cluster_results
  expect_class(cluster_results, "list")
  expect_length(cluster_results, 5)

  expect_class(cluster_results[[1]], "fuzzyclara")
  cc_fuzzy <- fuzzyclara(data        = USArrests,
                         clusters    = 2,
                         metric      = "euclidean",
                         samples     = 1,
                         sample_size = 10,
                         type        = "fuzzy",
                         m           = 2,
                         seed        = 3526,
                         verbose     = 0)
  expect_identical(cluster_results[[1]], cc_fuzzy)
  
  # results without plot and without scaling:
  cc_results <- evaluate_cluster_numbers(
    data           = USArrests,
    clusters_range = 2:6,
    metric         = "euclidean",
    samples        = 1,
    sample_size    = 10,
    type           = "fuzzy",
    m              = 2,
    seed           = 3526,
    verbose        = 0,
    return_results = TRUE,
    plot           = FALSE,
    scale          = FALSE)
  
  expect_class(cc_results, "list")
  expect_class(cc_results[[1]], "fuzzyclara")
})



test_that("evaluate_cl_nums_clarans", { # plot to select number of clusters

  data(USArrests)

  # hard clustering
  cc_number <- evaluate_cluster_numbers(
    data           = USArrests,
    clusters_range = 2:6,
    metric         = "euclidean",
    num_local      = 2,
    max_neighbors  = 10,
    type           = "hard",
    algorithm      = "clarans",
    seed           = 3526,
    verbose        = 0)

  # check whole object
  expect_s3_class(cc_number, "ggplot")
  expect_s3_class(cc_number$layers[[1]]$geom, "GeomLine")

  expect_length(cc_number, 11)

  expect_identical(dim(cc_number$data),
                   as.integer(c(5, 2)))

  # fuzzy clustering
  # return clustering results
  cc_number <- evaluate_cluster_numbers(
    data           = USArrests,
    clusters_range = 2:6,
    metric         = "euclidean",
    num_local      = 2,
    max_neighbors  = 10,
    type           = "fuzzy",
    algorithm      = "clarans",
    m              = 2,
    seed           = 3526,
    verbose        = 0,
    return_results = TRUE)

  # check whole object
  expect_class(cc_number, "list")
  expect_length(cc_number, 2)

  cluster_results <- cc_number$cluster_results
  expect_class(cluster_results, "list")
  expect_length(cluster_results, 5)

  expect_class(cluster_results[[1]], "fuzzyclara")
})


test_that("plot_cl_nums", { # plot to select number of clusters
  
  data(USArrests)
  
  # hard clustering
  cc_number <- evaluate_cluster_numbers(
    data           = USArrests,
    clusters_range = 2:6,
    metric         = "euclidean",
    num_local      = 2,
    max_neighbors  = 10,
    type           = "hard",
    algorithm      = "clarans",
    seed           = 3526,
    verbose        = 0,
    return_results = TRUE,
    plot           = FALSE)
  
  # check without given cluster range
  cc_plot <- plot_cluster_numbers(cluster_results = cc_number)
  expect_s3_class(cc_plot, "ggplot")
  expect_s3_class(cc_plot$layers[[1]]$geom, "GeomLine")
  
  # check with given cluster range:
  cc_plot <- plot_cluster_numbers(cluster_results = cc_number,
                                  clusters_range = 2:6)
  expect_s3_class(cc_plot, "ggplot")
  expect_s3_class(cc_plot$layers[[1]]$geom, "GeomLine")
})
