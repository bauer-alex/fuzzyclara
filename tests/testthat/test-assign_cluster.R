test_that("assign_cluster", {

  data(USArrests)
  data <- USArrests %>% tibble::rownames_to_column(var = "Name")
  row.names(data) <- data$Name

  # hard
  result <- assign_cluster(data = data,
                           medoids = c("Alabama", "Alaska", "Arizona"),
                           metric = "Euclidean", type = "hard",
                           return_distMatrix = TRUE)

  # check whole object
  expect_class(result, "list")
  expect_length(result, 4)
  expect_identical(dim(result$distance_to_medoids), as.integer(c(nrow(data), 3)))

  # fuzzy
  result <- assign_cluster(data = data,
                           medoids = c("Alabama", "Alaska", "Arizona"),
                           metric = "Euclidean", type = "fuzzy", m = 3,
                           return_distMatrix = TRUE)

  # check whole object
  expect_class(result, "list")
  expect_length(result, 5)
  expect_identical(dim(result$membership_scores), as.integer(c(nrow(data), 3)))

  # with distance matrix
  dist <- as.matrix(proxy::dist(data[, -1]))
  result <- assign_cluster(data = data, dist_matrix = dist,
                           medoids = c("Alabama", "Alaska", "Arizona"),
                           metric = "Euclidean", type = "fuzzy", m = 3,
                           return_distMatrix = TRUE)

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
