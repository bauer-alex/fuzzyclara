
library(fuzzyclara)
library(dplyr)
library(tidyr)
library(ggplot2)

data(travel)



# data prep ---------------------------------------------------------------
# reformat to long format
dat_long <- travel %>% 
  mutate(traveler_id = 1:nrow(.)) %>% 
  tidyr::pivot_longer(cols = 2:5, names_to = "variable")

# standardize the individual variables
dat_long <- dat_long %>% 
  group_by(variable) %>% 
  mutate(value = scale(value)) %>% 
  ungroup()



# plot unclustered data ---------------------------------------------------
# only plot randomly chosen n travelers
n <- 500
random_ids <- sample(unique(dat_long$traveler_id), size = n)

dat_long %>% 
  filter(traveler_id %in% random_ids) %>% 
  ggplot(aes(x = variable, y = value, group = traveler_id)) + 
  geom_line(alpha = 0.05) +
  ylab("standardized value") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank())
ggsave("travel_description.png", bg = "white", width = 5, height = 4)



# cluster data ------------------------------------------------------------
# select number of clusters
choice <- evaluate_cluster_numbers(data           = travel,
                                   clusters_range = 1:10,
                                   metric         = "euclidean",
                                   samples        = 20,
                                   sample_size    = 1000,
                                   type           = "fuzzy",
                                   seed           = 2022,
                                   verbose        = 1,
                                   m              = 1.5,
                                   plot           = TRUE,
                                   return_results = TRUE)
choice$plot
ggsave("travel_elbow.png", bg = "white", width = 10, height = 4)


# choose the solution with 4 clusters
n_clusters <- 5
dat_long <- dat_long %>% 
  mutate(cluster    = paste("Cluster", rep(choice$cluster_results[[n_clusters]]$clustering, each = 4)),
         memb_score = rep(apply(choice$cluster_results[[n_clusters]]$membership_scores, 1, max), each = 4),
         cluster    = factor(cluster))
dat_long_medoids <- dat_long %>% 
  filter(traveler_id %in% choice$cluster_results[[n_clusters]]$medoids)



# plot clustered data -----------------------------------------------------
# only plot randomly chosen n travelers
n <- 500
set.seed(2022)
random_ids <- sample(unique(dat_long$traveler_id), size = n)

# with highlighted medoids per cluster
ggplot(mapping = aes(x = variable, y = value, group = traveler_id, col = cluster)) +
  geom_line(data = dat_long %>% filter(traveler_id %in% random_ids), aes(alpha = memb_score)) +
  geom_line(data = dat_long_medoids, size = 1.5, col = gray(0.2)) +
  ylab("standardized value") +
  ylim(c(-2,4)) +
  facet_wrap(~ cluster, nrow = 1) +
  theme_minimal(base_size = 12) +
  theme(axis.title.x    = element_blank(),
        legend.position = "none",
        axis.text.x     = element_text(angle = 45, hjust = 1))
ggsave("travel_clustered.png", bg = "white", width = 10, height = 4)



# boxplots of travel expenses

library(ggpubr)
plot1 <- plot(x = choice$cluster_results[[4]], data = travel, type = "boxplot",
     variable = "totalExpenses")# + ylim(0, 6000)
plot2 <- plot(x = choice$cluster_results[[4]], data = travel, type = "boxplot",
     variable = "totalExpenses", confidence_threshold = 0.4) #+ ylim(0, 6000)
ggarrange(plot1, plot2)



# PCA plot
plot(choice$cluster_results[[n_clusters]], data = travel[,2:5], type = "pca")



# share of core clusters --------------------------------------------------
threshold_coreCluster <- 0.6

n_cluster_memberships <- table(choice$cluster_results[[n_clusters]]$clustering)
memb_table                  <- choice$cluster_results[[n_clusters]]$membership_scores
rows_membTable_coreClusters <- which(apply(memb_table, 1, max) > threshold_coreCluster)
memb_table_coreClusters     <- memb_table[rows_membTable_coreClusters,]
coreCluster_vector          <- apply(memb_table_coreClusters, 1, which.max)
n_coreCluster_memberships   <- table(coreCluster_vector)

# core cluster share per cluster
n_coreCluster_memberships / n_cluster_memberships
