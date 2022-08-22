
# code to create the hex sticker for the 'fuzzyclara' package

library(hexSticker)
library(dplyr)
library(ggplot2)



# data with normal densities and some points ------------------------------
set.seed(6)
dat_norm1 <- data.frame(x = rnorm(n = 10000, mean = 0),   y = rnorm(n = 10000, mean = 0))
dat_norm2 <- data.frame(x = rnorm(n = 10000, mean = 3),   y = rnorm(n = 10000, mean = 0))
dat_norm3 <- data.frame(x = rnorm(n = 10000, mean = 1.5), y = rnorm(n = 10000, mean = -2))

dat_points <- dplyr::bind_rows(dat_norm1 %>% sample_n(5),
                               dat_norm2 %>% sample_n(5),
                               dat_norm3 %>% sample_n(5))



# main plot ---------------------------------------------------------------
gg <- ggplot() +
  geom_density_2d(data = dat_norm1, aes(x = x, y = y), col = "firebrick", h = c(1,1), bins = 4) +
  geom_density_2d(data = dat_norm2, aes(x = x, y = y), col = "dodgerblue3", h = c(1,1), bins = 4) +
  geom_density_2d(data = dat_norm3, aes(x = x, y = y), col = "orange", h = c(1,1), bins = 4) +
  geom_point(data = dat_points, aes(x = x, y = y), col = gray(.4)) +
  theme(axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        panel.background = element_blank(),
        legend.position  = "none",
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.grid       = element_blank())



# sticker -----------------------------------------------------------------
sticker(subplot  = gg,
        package  = "fuzzyclara",
        filename = "fuzzyclara.png",
        p_size   = 20,  # title size
        s_x      = 1,   # plot positioning
        s_y      = .75,
        s_width  = 1.5, # plot size
        s_height = 1.2,
        h_fill   = "lightgray", # hexagon color
        h_color  = "darkgreen")
