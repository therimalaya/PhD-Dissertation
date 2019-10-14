library(simrel)
library(tidyverse)

## ---- Simrel Object ----
set.seed(777)
sobj <- simrel(n = 100, p = 10, m = 5, q = c(3, 4), relpos = list(1:2, 5:6),
               ypos = list(c(1, 3), c(2, 4, 5)), R2 = c(0.8, 0.8), gamma = 0.7, eta = 0,
               type = "multivariate")

## ---- Plot Function ----
df_plot <- function(sobj, before = TRUE, fltr = NULL, ordered = FALSE) {
  p <- sobj$p
  m <- sobj$m
  l <- length(sobj$R2)
  x_symb <- ifelse(before, "Z", "X")
  y_symb <- ifelse(before, "W", "Y")
  x_dta <- if (before) sobj$Z else sobj$X
  y_dta <- if (before) sobj$W else sobj$Y
  colnames(x_dta) <- paste0(x_symb, 1:ncol(x_dta))
  colnames(y_dta) <- paste0(y_symb, 1:ncol(y_dta))
  
  ## ---- Initial Values ----
  cols <- rev(RColorBrewer::brewer.pal(l + 1, "YlGnBu"))
  col_vec <- c(paste0(y_symb, 1:length(sobj$R2)), "None")
  
  y_idx <- 1:m
  y_lbl <- paste0(y_symb, y_idx)
  y_col_vec <- function(col_vec, lst = sobj$ypos, before = TRUE) {
    out <- unlist(lst)
    cvec <- unname(unlist(mapply(rep, col_vec[-length(col_vec)], 
                                 each = sapply(lst, length), SIMPLIFY = FALSE)))
    col_out <- cvec[out]
    if (before) col_out <- c(col_out[1:length(lst)], 
                             rep(col_vec[length(col_vec)], 
                                 length(col_out) - length(lst)))
    return(col_out)
  }
  y_fill <- y_col_vec(col_vec, sobj$ypos, before = before)
  
  x_idx <- 1:p
  x_lbl <- paste0(x_symb, x_idx)
  x_col_vec <- function(sobj, col_vec, lst = sboj$relpred) {
    rel_col <- unname(unlist(mapply(rep, col_vec[-length(col_vec)], Filter(function(x) x > 0, sapply(lst, length)))))
    rel_idx <- unname(unlist(lst))
    all_idx <- 1:sobj$p
    irrel_col <- rep(col_vec[length(col_vec)], length(all_idx) - length(rel_idx))
    out <- c(rel_col, irrel_col)
    out_idx <- order(c(rel_idx, setdiff(all_idx, rel_idx)))
    out[out_idx]
  }
  x_fill <- x_col_vec(sobj, col_vec, if (before) sobj$relpos else sobj$relpred)
  
  #ordered idx
  x_rel_idx <- if (before) unname(unlist(sobj$relpos)) else unname(unlist(sobj$relpred))
  x_irrel_idx <- setdiff(x_idx, x_rel_idx)
  y_rel_idx <- unname(unlist(sobj$ypos))
  y_irrel_idx <- setdiff(y_idx, y_rel_idx)
  ordered_idx <- c(paste0(y_symb, c(y_rel_idx, y_irrel_idx)),
                   paste0(x_symb, c(x_rel_idx, x_irrel_idx)))
  
  
  normalize <- function(x) (x - min(x)) / diff(range(x))
  ## ---- Dataset for plot ----
  dta_ <- list(y_dta, x_dta)
  names(dta_) <- c(y_symb, x_symb)
  dta <- map_df(dta_, ~as.data.frame(.x) %>% gather(), .id = "which_df") %>% 
    as_tibble()
  
  df_ <- list(
    tibble(idx = y_idx, key = y_lbl, fill = y_fill),
    tibble(idx = x_idx, key = x_lbl, fill = x_fill)
  )
  names(df_) <- c(y_symb, x_symb)
  df <- do.call(bind_rows, `$<-`(df_, .id, "which_df"))
  
  plt_dta <- dta %>% left_join(df, by = c('which_df', 'key')) %>% 
    mutate(
      label = factor(key, if (ordered) ordered_idx else c(y_lbl, x_lbl)),
      which_df = as_factor(which_df, labels = c(y_symb, x_symb)),
      fill = as_factor(fill, labels = unique(fill)),
      y = 1/100
    ) %>% 
    group_by(which_df, key, fill, label) %>% 
    mutate_at('value', normalize)
  
  ## ---- The Plot ----
  my_theme <- function(plt) {
    plt +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(color = "#2a2a2a"),
        panel.grid = element_blank(),
        panel.spacing.x = unit(0.05, "npc"),
        plot.margin = margin(t = 30, r = 20, b = 30, l = 20),
        legend.position = "bottom"
      )
  }
  if (!is.null(fltr)) {
    fltr_cond <- substitute(fltr)
    fltr_rows <- eval(fltr_cond, plt_dta)
    plt_dta <- plt_dta[fltr_rows, ]
  }
  assign("plt_dta", plt_dta, envir = .GlobalEnv)
  
  plt <- ggplot(plt_dta, aes(label, value, fill = fill)) +
    geom_bar(stat = "identity", aes(y = y), alpha = 0.4) +
    geom_violin(scale = 'width', color = NA) +
    geom_point(position = position_jitter(width = 0.1), shape = 21, alpha = 0.7, stroke = 0.3) +
    facet_grid(. ~ which_df, scales = 'free', space = 'free', switch = "x") +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = cols, labels = col_vec) +
    labs(x = NULL, y = NULL, fill = "Relevant For:") +
    ggtitle("Structure of simulated data",
            subtitle = "With Normalized values")
  
  plt %>% my_theme
}


## ---- The Plot ----
# ## Before ----
# plot(df_plot(sobj, TRUE))
# plot(cov_plot(sobj, type = "relpos", facetting = FALSE) +
#   scale_fill_brewer(palette = "YlGnBu", direction = -1))
# ## After ----
plot(df_plot(sobj, FALSE, ordered = TRUE))
# plot(cov_plot(sobj, type = "relpred", facetting = FALSE) +
#        scale_fill_brewer(palette = "YlGnBu", direction = -1))
