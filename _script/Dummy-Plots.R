library(simrel)
library(tidyverse)

source("_script/00-function.R")

dummy_cov_plot <- function(sobj, plot_type = "relpred") {
  plt <- simrel::plot_cov(sobj, type = plot_type, facetting = FALSE) + 
    theme(text        = element_text(size = rel(4)),
          legend.text = element_text(size = rel(4), family = "sans"),
          axis.text   = element_text(size = rel(1), family  = "sans"),
          axis.text.x = element_text(angle = 0, hjust = 0.5))
  return(plt)
}

dummy_obs_plot <- function(sobj, plot_type = "X") {
  fct_lbl <- `names<-`(paste0(plot_type, 1:sobj$p), 1:sobj$p)
  dta <- if (sobj$type == "multivariate") {
    if (plot_type == "X") sobj$X else sobj$Z
  } else {
    if (plot_type == "X") sobj$X else sobj$X %*% sobj$Rotation
  }
  dta <- reshape2::melt(dta) %>% 
    mutate_at("Var2", ~parse_number(as.character(Var2))) %>% 
    mutate(Var2 = as.factor(Var2))
  dta %>% 
    ggplot(aes(
      x = Var2, y = value, 
      color = if(plot_type == "X") Var2 %in% unlist(sobj$relpred)[1:sum(sobj$q)] else Var2 %in% unlist(sobj$relpos))) +
    geom_point(position = position_jitter(width = 0.25, height = 0.05),
               shape = 4) +
    scale_color_discrete(labels = rev(c("Relevant", "Irrelevant"))) +
    labs(color = NULL, y = NULL, x = NULL) +
    scale_x_discrete(labels = function(x) paste0(plot_type, x)) +
    theme(legend.position = c(1, 0),
          legend.box.margin = margin(3, 3, 3, 3),
          legend.justification = c(1, 0),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    facet_grid(. ~ Var2, scales = 'free', space = 'free',
               labeller = labeller(Var2 = fct_lbl))
}

## ---- Multivariate Simulation ----
sobj <- multisimrel(
  n      = 100,
  p      = 10,
  q      = 7,
  m      = 3,
  ypos   = list(1:3),
  relpos = list(1:5),
  gamma  = 0.8,
  eta    = 0.1,
  R2     = 0.8
)

col_vec <- rev(RColorBrewer::brewer.pal(4, "Set1"))
rel_col <- alpha(col_vec[2], 1)
irrel_col <- alpha(col_vec[1], 0.2)
plt <- plot_model(rel_col, irrel_col, 1, 0.2)
plt1 <- grid_by_name(plt,"rect_box|labels|xrel_space")

plt2 <- dummy_obs_plot(sobj, "Z") +
  theme(text = element_text(size = rel(5)),
        strip.text = element_text(size = rel(3)),
        legend.position = "none")
plt3 <- dummy_obs_plot(sobj, "X") +
  theme(text = element_text(size = rel(5)),
        strip.text = element_text(size = rel(3)),
        legend.text = element_text(size = rel(3)))

all_plt <- gridExtra::arrangeGrob(plt2, plt3, ncol = 2)

ggsave(plt2, filename = "~/Creative Cloud Files/Multivariate/observation-plot-z.svg",
       width = 3, height = 2, scale = 1.8)
ggsave(plt3, filename = "~/Creative Cloud Files/Multivariate/observation-plot-x.svg",
       width = 3, height = 2, scale = 1.8)

covs <- c("relpos", "rotation", "relpred") %>% name_it()
cov_plts <- map(covs, ~dummy_cov_plot(sobj, plot_type = ..1))
walk(covs, function(x) {
  ggsave(cov_plts[[x]], filename = paste0("~/Creative Cloud Files/Multivariate/", x, ".svg"),
         width = 3, height = 3, scale = 1.8)
})


## ---- Univariate Simulation ----

sobj <- simrel(
  n      = 100,
  p      = 10,
  q      = 8,
  relpos = 1:5,
  gamma  = 0.8,
  R2     = 0.8,
  type = "univariate"
)

col_vec <- rev(RColorBrewer::brewer.pal(4, "Set1"))
rel_col <- alpha(col_vec[2], 1)
irrel_col <- alpha(col_vec[1], 0.2)
plt <- plot_model(rel_col, irrel_col, 1, 0.2)
plt1 <- grid_by_name(plt,"rect_box|labels|xrel_space")

plt2 <- dummy_obs_plot(sobj, "Z") +
  theme(text = element_text(size = rel(5)),
        strip.text = element_text(size = rel(3)),
        legend.position = "none")
plt3 <- dummy_obs_plot(sobj, "X") +
  theme(text = element_text(size = rel(5)),
        strip.text = element_text(size = rel(3)),
        legend.text = element_text(size = rel(3)))

all_plt <- gridExtra::arrangeGrob(plt2, plt3, ncol = 2)

ggsave(plt3, filename = "~/Creative Cloud Files/Univariate/observation-plot.svg",
       width = 3, height = 2, scale = 1.8)

covs <- c("relpos", "rotation", "relpred") %>% name_it()
cov_plts <- map(covs, ~dummy_cov_plot(sobj, plot_type = ..1))
walk(covs, function(x) {
  ggsave(cov_plts[[x]], filename = paste0("~/Creative Cloud Files/Univariate/", x, ".svg"),
         width = 3, height = 3, scale = 1.8)
})

