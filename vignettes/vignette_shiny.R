## ----packages, message = FALSE------------------------------------------------
library(fuzzyclara)
library(dplyr)
library(purrr)
library(shiny)
library(rlang)

## ---- hard clustering example-------------------------------------------------
# TODO replace USArrests by a better dataset (maybe TourIST)
cc_fixed <- fuzzyclara(data        = USArrests,
                       clusters    = 3,
                       metric      = "euclidean",
                       samples     = 1,
                       sample_size = NULL,
                       type        = "fixed",
                       seed        = 3526,
                       verbose     = 0)
cc_fixed

## ---- fuzzy clustering example------------------------------------------------
cc_fuzzy <- fuzzyclara(data        = USArrests,
                       clusters    = 3,
                       metric      = "euclidean",
                       samples     = 1,
                       sample_size = NULL,
                       type        = "fuzzy",
                       m           = 2,
                       seed        = 3526,
                       verbose     = 0)
cc_fuzzy

## ---- self-defined distance function------------------------------------------
dist_function <- function(x, y) {
  sqrt(sum((x - y)^2))
}

cc_dist <- fuzzyclara(data        = USArrests,
                      clusters    = 3,
                      metric      = dist_function,
                      samples     = 1,
                      sample_size = NULL,
                      type        = "fuzzy",
                      m           = 2,
                      seed        = 3526,
                      verbose     = 0)
cc_dist

## ----manhattan, warning = FALSE-----------------------------------------------
cc_manh <- fuzzyclara(data        = USArrests,
                       clusters    = 3,
                       metric      = "manhattan",
                       samples     = 1,
                       sample_size = NULL,
                       type        = "fixed",
                       seed        = 3526,
                       verbose     = 0)
cc_manh

dist_mink <- function(x, y) {
  proxy::dist(list(x, y), method = "minkowski", p = 1)
}
cc_mink <- fuzzyclara(data        = USArrests,
                       clusters    = 3,
                       metric      = dist_mink,
                       samples     = 1,
                       sample_size = NULL,
                       type        = "fixed",
                       seed        = 3526,
                       verbose     = 0)
cc_mink

