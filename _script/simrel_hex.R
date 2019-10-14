require(simrel)
require(hexSticker)
require(ggplot2)

hex_sobj <- simrel(n = 100, p = 10, m = 4, q = c(2, 3, 4), relpos = list(1, c(2, 4), c(3, 5, 7)), ypos = list(c(1, 4), 2, 3),
                   R2 = c(0.8, 0.8, 0.7), gamma = 0.7, eta = 0.2, type = "multivariate")
plt <- cov_plot(hex_sobj, type = "relpred", facetting = FALSE) +
  theme(legend.position = 'none',
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_blank())
hex_sticker <- hexSticker::sticker(plt,
                                   p_size = 7,
                                   p_y = 1.6,
                                   s_x = 1,
                                   s_y = 0.86,
                                   s_width = 1,
                                   s_height = 1,
                                   package = "simrel",
                                   h_fill = "whitesmoke",
                                   h_color = "#5c5c5c",
                                   p_color = "#377EB8") +
  hexSticker::geom_url(
    url = "http://simulatr.github.io/simrel",
    angle = 0,
    x = 1, y = 1.45,
    size = 1.1,
    family = "mono",
    hjust = 0.5
  )
hexSticker::save_sticker(filename = "_images/simrel-hex.svg", hex_sticker)
# knitr::include_graphics("_images/simrel-hex.png")