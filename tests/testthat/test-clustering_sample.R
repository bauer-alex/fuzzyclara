test_that("clustering_sample", { # clustering_sample function

  data(USArrests)
  data <- USArrests %>% tibble::rownames_to_column(var = "Name")
  row.names(data) <- data$Name


  # fixed clustering
  cc_fixed <- clustering_sample(data = data,
                                clusters = 3,
                                sample_ids = 1:10,
                                metric = "euclidean",
                                type = "fixed")

  # check whole object
  expect_class(cc_fixed, "list")
  expect_length(cc_fixed, 6)
  expect_identical(dim(cc_fixed$dist_matrix), as.integer(c(10, 10)))

  # fuzzy clustering
  cc_fuzzy <- clustering_sample(data = data,
                                clusters = 3,
                                sample_ids = 1:10,
                                metric = "euclidean",
                                type = "fuzzy")

  # fuzzy clustering with build algorithm
  cc_fuzzy_build <- clustering_sample(data = data,
                                      clusters = 3,
                                      sample_ids = 1:10,
                                      metric = "euclidean",
                                      type = "fuzzy",
                                      build = TRUE)

  # check whole object
  expect_class(cc_fuzzy, "list")
  expect_length(cc_fuzzy, 7)
  expect_identical(dim(cc_fuzzy$membership_scores), as.integer(c(nrow(data), 3)))



})


test_that("compute_dist_matrix", {

  data(USArrests)
  data <- USArrests %>% tibble::rownames_to_column(var = "Name")

  dist_mat <- compute_distance_matrix(data[1:10,], metric = "Euclidean")

  # check whole object
  expect_class(dist_mat, "dist")
  expect_identical(dim(dist_mat), as.integer(c(10, 10)))

  # self defined euclidean distance:
  dist_function <- function(x, y) {
    sqrt(sum((x - y)^2))
  }
  dist_mat_2 <- compute_distance_matrix(data[1:10,], metric = dist_function)

  # calculate manually:
  dist_mat_3 <- proxy::dist(USArrests[1:10,], method = "Euclidean")

  # all three matrices must be equal:
  expect_identical(round(matrix(dist_mat), 2), round(matrix(dist_mat_2), 2))
  expect_identical(round(matrix(dist_mat), 2), round(matrix(dist_mat_3), 2))


  # try Manhattan and Gower:
  dist_gow <- compute_distance_matrix(data[1:10,], metric = "Gower")
  dist_manh <- compute_distance_matrix(data[1:10,], metric = "Manhattan")

  expect_class(dist_gow, "dist")
  expect_class(dist_manh, "dist")

})


test_that("perform_sample_clustering", {

  data(USArrests)
  data <- USArrests %>% tibble::rownames_to_column(var = "Name")
  row.names(data) <- data$Name

  dist_mat <- compute_distance_matrix(data[1:10,], metric = "Euclidean")

  # fixed
  clust <- perform_sample_clustering(dist = dist_mat, data = data[1:10,],
                                     clusters = 3, type = "fixed",
                                     names = data[1:10,]$Name,
                                     metric = "Euclidean")

  # check whole object
  expect_class(clust, "list")
  expect_identical(names(clust), c("medoids", "clustering"))


  # fuzzy
  clust <- perform_sample_clustering(dist = dist_mat, data = data[1:10, ],
                                     clusters = 3, type = "fuzzy",
                                     names = data[1:10,]$Name, m = 3,
                                     metric = "Euclidean")

  # check whole object
  expect_class(clust, "list")
  expect_identical(names(clust), c("medoids", "clustering"))

})

