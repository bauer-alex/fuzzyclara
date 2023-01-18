test_that("predict_fuzzyclara_hard_clara", { # hard CLARA clustering
  
  data(USArrests)
  
  # hard clustering
  n_clusters <- 3
  cc_hard   <- fuzzyclara(data        = USArrests[1:30, ],
                          clusters    = n_clusters,
                          metric      = "euclidean",
                          samples     = 1,
                          sample_size = 10,
                          type        = "hard",
                          seed        = 3526,
                          verbose     = 0)
  pred_cc_hard <- predict(object = cc_hard, newdata = USArrests[31:50, ])
  
  invisible(capture.output(print(pred_cc_hard)))
  
  # check whole object
  expect_list(pred_cc_hard)
  expect_length(pred_cc_hard, 2)
  
  # wrong input with missing columns
  expect_error(predict(object = cc_hard, newdata = USArrests[31:50, 1:2]))
})


test_that("predict_fuzzyclara_hard_clarans", { # hard CLARANS clustering
  
  data(USArrests)
  
  # hard clustering
  n_clusters <- 3
  cc_hard   <- fuzzyclara(data        = USArrests[1:30, ],
                          clusters    = n_clusters,
                          metric      = "euclidean",
                          algorithm   = "clarans",
                          samples     = 1,
                          sample_size = 10,
                          type        = "hard",
                          seed        = 3526,
                          verbose     = 0)
  pred_cc_hard <- predict(object = cc_hard, newdata = USArrests[31:50, ])
  
  invisible(capture.output(print(pred_cc_hard)))
  
  # check whole object
  expect_list(pred_cc_hard)
  expect_length(pred_cc_hard, 2)
})


test_that("predict_fuzzyclara_fuzzy_clara", { # hard CLARANS clustering
  
  data(USArrests)
  
  # hard clustering
  n_clusters <- 3
  cc_fuzzy   <- fuzzyclara(data        = USArrests[1:30, ],
                          clusters    = n_clusters,
                          metric      = "euclidean",
                          algorithm   = "clara",
                          type        = "fuzzy",
                          samples     = 1,
                          sample_size = 10,
                          seed        = 3526,
                          verbose     = 0)
  pred_cc_fuzzy <- predict(object = cc_fuzzy, newdata = USArrests[31:50, ])
  
  invisible(capture.output(print(pred_cc_fuzzy)))
  
  # check whole object
  expect_list(cc_fuzzy)
  expect_length(pred_cc_fuzzy, 3)
})

