test_that("plot_select_cl_nums", { # plot to select number of clusters
  
  data(USArrests)
  
  # fixed clustering
  cc_number <- plot_cluster_numbers(
    data            = USArrests,
    clusters_range  = 2:6,
    metric          = "euclidean",
    samples         = 1,
    sample_size     = 10,
    type            = "fixed",
    seed            = 3526,
    verbose         = 0)
  
  # check whole object
  expect_s3_class(cc_number, "ggplot")
  expect_s3_class(cc_number$layers[[1]]$geom, "GeomLine")
  
  expect_length(cc_number, 9)
  
  expect_identical(dim(cc_number$data),
                   as.integer(c(5, 2)))
  
  # compare result to manually calculated clustering result:
  cc_fixed <- claraclust(data        = USArrests,
                         clusters    = 2,
                         metric      = "euclidean",
                         samples     = 1,
                         sample_size = 10,
                         type        = "fixed",
                         seed        = 3526,
                         verbose     = 0)
  expect_identical(round(cc_number$data[1, 2], 2), round(cc_fixed$avg_min_dist, 2))
  
  # fuzzy clustering
  # return clustering results
  cc_number <- plot_cluster_numbers(
    data            = USArrests,
    clusters_range  = 2:6,
    metric          = "euclidean",
    samples         = 1,
    sample_size     = 10,
    type            = "fuzzy",
    m = 2,
    seed            = 3526,
    verbose         = 0,
    return_results = TRUE)
  
  # check whole object
  expect_class(cc_number, "list")
  expect_length(cc_number, 2)
  
  cluster_results <- cc_number$cluster_results
  expect_class(cluster_results, "list")
  expect_length(cluster_results, 5)
  
  expect_class(cluster_results[[1]], "claraclust")
  cc_fuzzy <- claraclust(data        = USArrests,
                         clusters    = 2,
                         metric      = "euclidean",
                         samples     = 1,
                         sample_size = 10,
                         type        = "fuzzy",
                         m = 2,
                         seed        = 3526,
                         verbose     = 0)
  expect_identical(cluster_results[[1]], cc_fuzzy)
  
  
  
})

