test_that("fuzzyclara_hard_clara", { # hard CLARA clustering

  data(USArrests)

  # hard clustering
  n_clusters <- 3
  cc_hard   <- fuzzyclara(data        = USArrests,
                           clusters    = n_clusters,
                           metric      = "euclidean",
                           samples     = 1,
                           sample_size = NULL,
                           type        = "hard",
                           seed        = 3526,
                           verbose     = 0)

  invisible(capture.output(print(cc_hard)))

  # check whole object
  expect_s3_class(cc_hard, "fuzzyclara")
  expect_s3_class(cc_hard, "list")

  expect_length(cc_hard, 13)


  # check final cluster distance matrix
  expect_s3_class(cc_hard$distance_to_medoids, "data.frame")
  expect_identical(dim(cc_hard$distance_to_medoids),
                   as.integer(c(nrow(USArrests), n_clusters)))


  # data as matrix
  n_clusters <- 3
  cc_hard   <- fuzzyclara(data        = as.matrix(USArrests),
                           clusters    = n_clusters,
                           metric      = "euclidean",
                           samples     = 1,
                           sample_size = NULL,
                           type        = "hard",
                           seed        = 3526,
                           verbose     = 0)

  # check whole object
  expect_s3_class(cc_hard, "fuzzyclara")
  expect_s3_class(cc_hard, "list")

  # warning if sample size > n_obs
  expect_warning(fuzzyclara(data        = as.matrix(USArrests),
                            clusters    = n_clusters,
                            metric      = "euclidean",
                            samples     = 1,
                            sample_size = nrow(USArrests),
                            type        = "hard",
                            seed        = 3526,
                            verbose     = 0))

})


test_that("fuzzyclara_hard_clarans", { # hard CLARANS clustering

  data(USArrests)

  # hard clustering
  n_clusters <- 3
  cc_hard   <- fuzzyclara(data        = USArrests,
                           clusters    = n_clusters,
                           metric      = "euclidean",
                           algorithm   = "clarans",
                           num_local   = 2,
                           max_neighbors = 20,
                           type        = "hard",
                           seed        = 3526,
                           verbose     = 0)

  invisible(capture.output(print(cc_hard)))

  # check whole object
  expect_s3_class(cc_hard, "fuzzyclara")
  expect_s3_class(cc_hard, "list")

  expect_length(cc_hard, 10)


  # check final cluster distance matrix
  expect_s3_class(cc_hard$distance_to_medoids, "data.frame")
  expect_identical(dim(cc_hard$distance_to_medoids),
                   as.integer(c(nrow(USArrests), n_clusters)))


  # data as matrix
  n_clusters <- 3
  cc_hard   <- fuzzyclara(data        = as.matrix(USArrests),
                           clusters    = n_clusters,
                           metric      = "euclidean",
                           algorithm   = "clarans",
                           num_local   = 2,
                           max_neighbors = 20,
                           type        = "hard",
                           seed        = 3526,
                           verbose     = 0)

  # check whole object
  expect_s3_class(cc_hard, "fuzzyclara")
  expect_s3_class(cc_hard, "list")

})


test_that("fuzzyclara_fuzzy_clara", { # fuzzy clustering

  data(USArrests)

  # fuzzy clustering
  n_clusters <- 3
  cc_fuzzy  <- fuzzyclara(data        = USArrests,
                          clusters    = n_clusters,
                          metric      = "euclidean",
                          samples     = 1,
                          sample_size = 20,
                          type        = "fuzzy",
                          m = 3,
                          seed        = 3526,
                          verbose     = 0)

  invisible(capture.output(print(cc_fuzzy)))

  # check whole object
  expect_s3_class(cc_fuzzy, "fuzzyclara")
  expect_s3_class(cc_fuzzy, "list")

  expect_length(cc_fuzzy, 14)


  # check membership scores
  expect_s3_class(cc_fuzzy$membership_scores, "data.frame")
  expect_identical(dim(cc_fuzzy$membership_scores),
                   as.integer(c(nrow(USArrests), n_clusters)))


})


test_that("fuzzyclara_fuzzy_clara_build", { # fuzzy clustering with build algorithm

  data(USArrests)

  # fuzzy clustering
  n_clusters <- 3
  cc_fuzzy  <- fuzzyclara(data        = USArrests,
                          clusters    = n_clusters,
                          metric      = "euclidean",
                          samples     = 1,
                          sample_size = NULL,
                          type        = "fuzzy",
                          m = 3,
                          build = TRUE,
                          seed        = 3526,
                          verbose     = 0)

  invisible(capture.output(print(cc_fuzzy)))

  # check whole object
  expect_s3_class(cc_fuzzy, "fuzzyclara")
  expect_s3_class(cc_fuzzy, "list")

  expect_length(cc_fuzzy, 14)


  # check membership scores
  expect_s3_class(cc_fuzzy$membership_scores, "data.frame")
  expect_identical(dim(cc_fuzzy$membership_scores),
                   as.integer(c(nrow(USArrests), n_clusters)))


})


test_that("fuzzyclara_fuzzy_clarans", { # fuzzy clustering

  data(USArrests)

  # fuzzy clustering
  n_clusters <- 3
  cc_fuzzy  <- fuzzyclara(data        = USArrests,
                          clusters    = n_clusters,
                          metric      = "euclidean",
                          algorithm   = "clarans",
                          num_local   = 2,
                          max_neighbors = 20,
                          type        = "fuzzy",
                          m = 3,
                          seed        = 3526,
                          verbose     = 0)

  invisible(capture.output(print(cc_fuzzy)))

  # check whole object
  expect_s3_class(cc_fuzzy, "fuzzyclara")
  expect_s3_class(cc_fuzzy, "list")

  expect_length(cc_fuzzy, 11)


  # check membership scores
  expect_s3_class(cc_fuzzy$membership_scores, "data.frame")
  expect_identical(dim(cc_fuzzy$membership_scores),
                   as.integer(c(nrow(USArrests), n_clusters)))


})



test_that("fuzzyclara_fuzzy_pam", { # use pam clustering if m = 1 or clusters = 1

  data(USArrests)

  # fuzzy clustering with m = 1
  n_clusters <- 3
  cc_fuzzy  <- fuzzyclara(data        = USArrests,
                          clusters    = n_clusters,
                          metric      = "euclidean",
                          samples     = 1,
                          sample_size = NULL,
                          type        = "fuzzy",
                          m = 1,
                          seed        = 3526,
                          verbose     = 0)

  cc_hard   <- fuzzyclara(data        = USArrests,
                           clusters    = n_clusters,
                           metric      = "euclidean",
                           samples     = 1,
                           sample_size = NULL,
                           type        = "hard",
                           seed        = 3526,
                           verbose     = 0)


  # check if same solution
  expect_identical(cc_hard$medoids, cc_fuzzy$medoids)

  # check if type is still fuzzy
  expect_true(cc_fuzzy$type == "fuzzy")

  # check if membership scores are all 0 or 1
  expect_true(all(as.matrix(cc_fuzzy$membership_scores) %in% c(0,1)))


  # fuzzy clustering with clusters = 1
  n_clusters <- 1
  cc_fuzzy  <- fuzzyclara(data        = USArrests,
                          clusters    = n_clusters,
                          metric      = "euclidean",
                          samples     = 1,
                          sample_size = NULL,
                          type        = "fuzzy",
                          m = 2,
                          seed        = 3526,
                          verbose     = 0)

  cc_hard   <- fuzzyclara(data        = USArrests,
                           clusters    = n_clusters,
                           metric      = "euclidean",
                           samples     = 1,
                           sample_size = NULL,
                           type        = "hard",
                           seed        = 3526,
                           verbose     = 0)

  # check if same solution
  expect_identical(cc_hard$medoids, cc_fuzzy$medoids)

  # check if type is still fuzzy
  expect_true(cc_fuzzy$type == "fuzzy")

  # check if all membership scores are 1
  expect_true(all(as.matrix(cc_fuzzy$membership_scores) == 1))


})


test_that("fuzzyclara_dist", { # check other distance functions

  data(USArrests)

  # clustering with self defined distance function
  n_clusters <- 3
  dist_function <- function(x, y) { # euclidean distance
    sqrt(sum((x - y)^2))
  }

  cc_dist <- fuzzyclara(data        = USArrests,
                        clusters    = 3,
                        metric      = dist_function,
                        samples     = 1,
                        sample_size = NULL,
                        type        = "hard",
                        m           = 2,
                        seed        = 3526,
                        verbose     = 0)

  # compare to euclidean -> result must be the same
  cc_hard   <- fuzzyclara(data        = USArrests,
                           clusters    = n_clusters,
                           metric      = "euclidean",
                           samples     = 1,
                           sample_size = NULL,
                           type        = "hard",
                           seed        = 3526,
                           verbose     = 0)


  # check if same solution
  expect_identical(cc_hard$medoids, cc_dist$medoids)
  expect_identical(cc_hard$distance_to_medoids, cc_dist$distance_to_medoids)


  # clustering with Manhattan and Minkowski
  cc_manh <- fuzzyclara(data        = USArrests,
                        clusters    = 3,
                        metric      = "manhattan",
                        samples     = 1,
                        sample_size = NULL,
                        type        = "fuzzy",
                        seed        = 3526,
                        verbose     = 0)

  dist_mink <- function(x, y) {
    proxy::dist(list(x, y), method = "minkowski", p = 1)
  }
  cc_mink <- fuzzyclara(data        = USArrests,
                        clusters    = 3,
                        metric      = dist_mink,
                        samples     = 1,
                        sample_size = NULL,
                        type        = "fuzzy",
                        seed        = 3526,
                        verbose     = 0)

  # check if same solution
  expect_identical(cc_manh$medoids, cc_mink$medoids)
  expect_identical(round(cc_manh$membership_scores, 2), round(cc_mink$membership_scores, 2))


})


test_that("fuzzyclara_errors", { # throw errors for wrong input

  data(USArrests)

  # claraclust throwing an error with wrong data
  expect_error(fuzzyclara(data = 1:10))

  # too many clusters
  expect_error(fuzzyclara(data = USArrests, clusters = 1000))

  # wrong type
  expect_error(fuzzyclara(data = USArrests, type = "fix"))

  # wrong sample size
  expect_error(fuzzyclara(data = USArrests, clusters = 5, sample_size = 5, type = "hard"))

  # unsuitable distance metric
  expect_error(fuzzyclara(data = USArrests, metric = "Phi", sample_size = 10))


})


test_that("fuzzyclara_scale", { # scaling of variables

  data(USArrests)

  # hard clustering
  n_clusters <- 1
  cc_hard   <- fuzzyclara(data        = USArrests,
                           clusters    = n_clusters,
                           metric      = "euclidean",
                           samples     = 1,
                           sample_size = 10,
                           type        = "hard",
                           seed        = 3526,
                           verbose     = 0,
                           scale = TRUE)

  # compute distance matrix manually
  data <- scale(USArrests)
  dist_matrix <- proxy::dist(data[cc_hard$subsample_ids,])

  expect_identical(round(matrix(dist_matrix), 2), round(matrix(cc_hard$dist_matrix), 2))

  # without scaling
  cc_hard   <- fuzzyclara(data        = USArrests,
                           clusters    = n_clusters,
                           metric      = "euclidean",
                           samples     = 1,
                           sample_size = 10,
                           type        = "hard",
                           seed        = 3526,
                           verbose     = 0,
                           scale = FALSE)

  # compute distance matrix manually
  data <- USArrests
  dist_matrix <- proxy::dist(data[cc_hard$subsample_ids,])

  expect_identical(round(matrix(dist_matrix), 2), round(matrix(cc_hard$dist_matrix), 2))

})
