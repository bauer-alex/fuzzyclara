test_that("clustering_sample", { # clustering_sample function
  
  data(USArrests)
  data <- USArrests %>% tibble::rownames_to_column(var = "Name")
  
  
  # fixed clustering
  cc_fixed <- clustering_sample(data = data, 
                                clusters = 3,
                                sample_ids = 1:10, 
                                metric = "euclidean", 
                                type = "fixed",
                                verbose = 2)
  
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
  
  # check whole object
  expect_class(cc_fuzzy, "list")
  expect_length(cc_fuzzy, 7)
  expect_identical(dim(cc_fuzzy$membership_scores), as.integer(c(nrow(data), 3)))
  
  
  
})


test_that("compute_dist_matrix", { 
  
  data(USArrests)
  data <- USArrests %>% tibble::rownames_to_column(var = "Name")
  
  dist_mat <- compute_distance_matrix(data[1:10,], metric = "Euclidean", dist_file = NULL)
  
  # check whole object
  expect_class(dist_mat, "dist")
  expect_identical(dim(dist_mat), as.integer(c(10, 10)))
  
  # self defined euclidean distance:
  dist_function <- function(x, y) { 
    sqrt(sum((x - y)^2))
  }
  dist_mat_2 <- compute_distance_matrix(data[1:10,], metric = dist_function, dist_file = NULL)
  
  # calculate manually:
  dist_mat_3 <- proxy::dist(USArrests[1:10,], method = "Euclidean")
  
  # all three matrices must be equal:
  expect_identical(round(matrix(dist_mat), 2), round(matrix(dist_mat_2), 2))
  expect_identical(round(matrix(dist_mat), 2), round(matrix(dist_mat_3), 2))
  
  
  # try Manhattan and Gower:
  dist_gow <- compute_distance_matrix(data[1:10,], metric = "Gower", dist_file = NULL)
  dist_manh <- compute_distance_matrix(data[1:10,], metric = "Manhattan", dist_file = NULL)
  
  expect_class(dist_gow, "dist")
  expect_class(dist_manh, "dist")
  
})


test_that("perform_sample_clustering", { 
  
  data(USArrests)
  data <- USArrests %>% tibble::rownames_to_column(var = "Name")
  
  dist_mat <- compute_distance_matrix(data[1:10,], metric = "Euclidean", dist_file = NULL)
  
  # fixed
  clust <- perform_sample_clustering(dist = dist_mat, clusters = 3, type = "fixed", names = data[1:10,]$Name)
  
  # check whole object
  expect_class(clust, "list")
  expect_identical(names(clust), c("medoids", "clustering"))
  
  
  # fuzzy
  clust <- perform_sample_clustering(dist = dist_mat, clusters = 3, type = "fuzzy", names = data[1:10,]$Name, m = 3)
  
  # check whole object
  expect_class(clust, "list")
  expect_identical(names(clust), c("medoids", "clustering"))
  
})


test_that("assign_cluster", { 
  
  data(USArrests)
  data <- USArrests %>% tibble::rownames_to_column(var = "Name")
  
  # fixed
  result <- assign_cluster(data = data,
                           medoids = c("Alabama", "Alaska", "Arizona"),
                           metric = "Euclidean", dist_file = NULL,
                           type = "fixed", return_distMatrix = TRUE)
  
  # check whole object
  expect_class(result, "list")
  expect_length(result, 4)
  expect_identical(dim(result$distance_to_medoids), as.integer(c(nrow(data), 3)))
  
  # fuzzy
  result <- assign_cluster(data = data,
                           medoids = c("Alabama", "Alaska", "Arizona"),
                           metric = "Euclidean", dist_file = NULL,
                           type = "fuzzy", m = 3, return_distMatrix = TRUE)
  
  # check whole object
  expect_class(result, "list")
  expect_length(result, 5)
  expect_identical(dim(result$membership_scores), as.integer(c(nrow(data), 3)))
})


test_that("calculate_memb_score", { 
  
  data(USArrests)
  data <- USArrests %>% tibble::rownames_to_column(var = "Name")
  medoids <- c("Alabama", "Alaska", "Arizona")
  data_medoids <- data %>% filter(Name %in% medoids)
  
  dist_dat <- proxy::dist(x         = data[, -1],
                          y         = data_medoids[, -1],
                          method    = "Euclidean")
  
  memb <- calculate_memb_score(dist_med = dist_dat[1,], m = 2) 
  
  # check whole object
  expect_class(memb, "list")
  expect_length(memb, 3)
  
  # check concrete membership scores
  memb2 <- calculate_memb_score(dist_med = c(0, 3, 3), m = 2) # zero distance to one medoid
  expect_identical(unname(unlist(memb2)), c(1, 0, 0))
  memb3 <- calculate_memb_score(dist_med = c(1, 1, 1), m = 2) # same distance to all medoids
  expect_identical(round(unname(unlist(memb3))), round(rep(1/3, 3)))
  

})
