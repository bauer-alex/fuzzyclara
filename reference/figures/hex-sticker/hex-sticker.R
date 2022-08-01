
# code to create the hex sticker for the 'fuzzyclara' package

library(hexSticker)
library(ggplot2)



p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() + theme_transparent()

sticker(p, package = "fuzzyclara", filename="fuzzyclara.png",
        p_size = 20, s_x = 1, s_y = .75, s_width = 1.3, s_height = 1,
        h_fill = "lightgray", h_color = "limegreen")
