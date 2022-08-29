library(fuzzyclara)
library(dplyr)



## Clustering

evaluate_cluster_numbers(data        = USArrests,
                        cluster_range    = 3:6,
                        metric      = "euclidean",
                        samples     = 10,
                        sample_size = 20,
                        type        = "fuzzy",
                        seed        = 3526,
                        verbose     = 0,
                        m = 2, plot = TRUE)

cc_fuzzy <- fuzzyclara(data        = USArrests,
                       clusters    = 3,
                       metric      = "euclidean",
                       samples     = 10,
                       sample_size = 20,
                       type        = "fuzzy",
                       seed        = 3526,
                       verbose     = 0,
                       m = 2)
plot(cc_fuzzy, data = USArrests, type = "pca")
plot(cc_fuzzy, data = USArrests, type = "pca", plot_all_fuzzy = TRUE,
     confidence_threshold = 0.4)
plot(cc_fuzzy, data = USArrests, type = "pca", confidence_threshold = 0.4)
USArrests$state <- row.names(USArrests)
plot(cc_fuzzy, variable = "state", data = USArrests, type = "wordclouds")
plot(cc_fuzzy, variable = "state", data = USArrests, type = "wordclouds",
     confidence_threshold = 0.5)
