
library(fuzzyclara)
library(dplyr)
library(tidyr)
library(ggplot2)

data(USArrests)



# data prep ---------------------------------------------------------------
# reformat to long format
dat_long <- USArrests %>% 
  mutate(state = row.names(.)) %>% 
  tidyr::pivot_longer(cols = 1:4, names_to = "variable")

# standardize the individual variables
dat_long <- dat_long %>% 
  group_by(variable) %>% 
  mutate(value = scale(value)) %>% 
  ungroup()



# plot unclustered data ---------------------------------------------------
ggplot(dat_long, aes(x = variable, y = value, group = state)) + 
  geom_line(alpha = 0.5) +
  ylab("standardized value") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank())
ggsave("USArrests_description.png", bg = "white", width = 5, height = 3)



# cluster data ------------------------------------------------------------
# select number of clusters
choice <- evaluate_cluster_numbers(data = USArrests, clusters_range = 2:12,
                                   metric = "euclidean", samples = 20,
                                   sample_size = 30,
                                   type        = "fuzzy",
                                   seed        = 3526,
                                   verbose     = 0,
                                   m = 1, plot = TRUE, return_results = TRUE)
choice$plot

dat_long <- dat_long %>% 
  mutate(cluster = paste("Cluster", rep(choice$cluster_results[[4]]$clustering, each = 4)),
         cluster = factor(cluster))



# plot clustered data -----------------------------------------------------
ggplot(dat_long, aes(x = variable, y = value, group = state, col = cluster)) +
  geom_line() +
  ylab("standardized value") +
  facet_wrap(~ cluster, nrow = 1) +
  theme_minimal(base_size = 12) +
  theme(axis.title.x    = element_blank(),
        legend.position = "none",
        axis.text.x     = element_text(angle = 45, hjust = 1))
ggsave("USArrests_clustered.png", bg = "white", width = 10, height = 3)
