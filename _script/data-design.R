df <- data.frame(
  id = factor(1:10, levels = 1:10),
  x = c(
    paste0("Number of Predictors\n", "p:", "50 and 250"),
    paste0("Level of Multicollinearity\n", 
           "gamma: 0.2 and 0.9"),
    paste0("Level of Response Correlation\n",
           "eta: 0, 0.4, 0.8 and 1.2"),
    paste0("Position of relevant predictor components\n",
           "relpos: 1, 2, 3, 4 and 5, 6, 7, 8"),
    paste0("Methods\n PCR, PLS1, PLS2, Xenv and Senv"),
    paste0("Number of Components\n 0, 1, 2, ..., 10"),
    paste0("Min. Prediction Error (Min. Components)\n",
           "Corresponding to Response Y", 1:4)
  ),
  fct = c(rep("Factors", 6), paste0("Y", 1:4)),
  fill = c(rep("Factors", 6), rep("Y", 4))
)

data_design_tbl <- ggplot(df, aes(x, y = 0)) +
  geom_tile(aes(fill = fill), alpha = 0.3, show.legend = FALSE,
            color = "black") +
  geom_text(aes(label = x), angle = 90, hjust = 1, nudge_y = 0.45,
            family = "mono", size = rel(6), lineheight=0.9, parse = FALSE) +
  facet_grid(. ~ fct, drop = TRUE, scales = 'free_x',
             space = 'free_x') +
  theme_void(base_size = 16) +
  theme(
    strip.text = element_text(
      size = rel(1.5),
      margin = unit(c(2, 0, 3, 0), "mm"),
    ),
    strip.background = element_rect(
      fill = "lightgrey",
      color = "darkgrey",
      size = 1
    ),
    panel.spacing = unit(0, "mm"),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    plot.subtitle = element_text(margin = unit(c(2, 0, 5, 0), "mm")),
    plot.title = element_text(size = rel(1.8))
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Data for Further Analysis",
       subtitle = paste0(
         "Response can be Average Miniumum Error or\n",
         "Avg. Number of Components used to obtain Minimum Error"))


# ggsave(plot = plt, filename = "_images/data-design.svg", width = 7, height = 5.7, unit = "in")
