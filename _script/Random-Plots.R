## ---- Load Packages ----
library(tidyverse)
library(grid)
library(gridExtra)

## ---- Load Data and Functions ----
load("../../Papers/03-prediction-comparison/scripts/robj/design.rdata")
pred_error <- readRDS("../../Papers/03-prediction-comparison/scripts/robj/prediction-error.rds")
est_error <- readRDS("../../Papers/04-estimation-comparison/scripts/robj/estimation-error.rds")
source("../../Papers/03-prediction-comparison/scripts/00-function.r")

## ---- Design Plot ----
relpos_lbl <- function(x) {
  paste0("Relevant predictors at: ", x)
}
p_lbl <- function(x) paste0("n-pred: ", x)

design_plt <- design_chr %>%
  rownames_to_column("Design") %>% 
  ggplot(aes(eta, gamma)) +
  geom_point(shape = 4, size = 2, color = "grey") +
  geom_text(aes(label = Design), vjust = "inward", hjust = "inward",
            family = "mono") +
  facet_grid(p ~ relpos, labeller = labeller(relpos = relpos_lbl, p = p_lbl)) +
  theme_minimal(base_family = "mono", base_size = 12) +
  coord_fixed(ratio = 0.6) +
  scale_x_continuous(breaks = design_chr$eta %>% unique()) +
  scale_y_reverse(breaks = design_chr$gamma %>% unique()) +
  labs(y = expression(paste("gamma(", gamma, ")")),
       x = expression(paste("eta(", eta, ")")),
       caption = paste0("eta and gamma are the decay rate of eigenvalues\n",
                        "corresponding to response and predictors respectively")) +
  ggtitle("Experimental Design for Method Comparison") +
  theme(plot.caption = element_text(size = rel(0.8)),
        plot.caption.position = "panel")

ggsave(design_plt, filename = "./_images/design-plot.svg", 
       width = 8, height = 3, scale = 1.3)

design_plt_2 <- design_chr %>%
  rownames_to_column("Design") %>% 
  ggplot(aes(eta, gamma)) +
  geom_point(shape = 4, size = 2, color = "grey") +
  geom_text(aes(label = Design), vjust = "inward", hjust = "inward",
            family = "mono") +
  facet_grid(p + relpos ~ ., labeller = labeller(relpos = label_value, p = p_lbl)) +
  theme_minimal(base_family = "mono", base_size = 12) +
  coord_fixed(ratio = 0.8) +
  scale_x_continuous(breaks = design_chr$eta %>% unique()) +
  scale_y_reverse(breaks = design_chr$gamma %>% unique()) +
  labs(y = expression(paste("gamma(", gamma, ")")),
       x = expression(paste("eta(", eta, ")"))) +
  theme(plot.caption = element_text(size = rel(0.8)),
        plot.caption.position = "panel")
ggsave(design_plt_2, filename = "./_images/design-plot-2.svg", 
       width = 2, height = 7, scale = 1.5)

## ---- Gamma Plot ----
lmd <- function(x, l) exp(-l * (x - 1))

plt2 <- crossing(
  p = list(1:min(design$p)),
  gamma = unique(design$gamma)
) %>% 
  mutate(lambda = map2(p, gamma, lmd)) %>% 
  unnest(p, lambda) %>% 
  ggplot(aes(p, lambda, fill = factor(gamma), color = factor(gamma))) +
  theme_minimal(base_family = "mono", base_size = 16) +
  layer(stat = "function",
        position = "identity",
        geom = "line",
        mapping = aes(color = paste0("gamma: ", design$gamma[1])),
        params = list(fun = lmd, args = list(l = design$gamma[1]))) +
  layer(stat = "function",
        position = "identity",
        geom = "line",
        mapping = aes(color = paste0("gamma: ", design$gamma[2])),
        params = list(fun = lmd, args = list(l = design$gamma[2]))) +
  scale_color_manual(values = c("grey40", "firebrick")) +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  theme(legend.position = c(1, 1), legend.justification = c(1.5, 1.5),
        legend.background = element_rect(fill = "white", colour = NA)) +
  labs(x = "Number of Components", y = "Eigenvalues", color = "Decay Factor")

ggsave(plt2, filename = "_images/gamma.svg", width = 4, height = 5, scale = 1.2)

## ---- Prediction Model Manova ----
pred_dta <- design_chr %>%
  select_if(function(x) n_distinct(x) > 1) %>%
  mutate(Design = as.character(1:n())) %>%
  mutate_at(vars(p, gamma, relpos, eta), as.factor) %>%
  right_join(pred_error, by = "Design") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("p", as.factor) %>%
  mutate(Response = paste0("Y", Response))
pred_spread_df <- pred_dta %>%
  as.data.frame() %>%
  select(-Design, -q) %>%
  spread(Response, Pred_Error)
min_comp_stk <- pred_dta %>%
  group_by(p, relpos, eta, gamma, Method, Tuning_Param, Response) %>%
  summarize(Pred_Error = mean(Pred_Error)) %>%
  group_by(p, relpos, eta, gamma, Method, Response) %>%
  summarize(Tuning_Param = Tuning_Param[which.min(Pred_Error)])
pred_min <- pred_dta %>%
  select(-Design, -q) %>%
  semi_join(min_comp_stk, by = c(
    "p", "relpos", "eta", "gamma", "Method",
    "Tuning_Param", "Response"
  )) %>% select(-Tuning_Param) %>%
  spread(Response, Pred_Error)
comp_min <- pred_dta %>%
  group_by(p, relpos, eta, gamma, Method, Replication, Response) %>%
  summarize(Tuning_Param = Tuning_Param[which.min(Pred_Error)]) %>%
  spread(Response, Tuning_Param)

pred_mdl <- lm(
  formula = cbind(Y1, Y2, Y3, Y4) ~ (p + gamma + eta + relpos + Method) ^ 3,
  data = pred_min)
comp_mdl <- lm(
  formula = cbind(Y1, Y2, Y3, Y4) ~ (p + gamma + eta + relpos + Method) ^ 3,
  data = comp_min)
pred_aov <- anova(pred_mdl) %>%
  as_tibble(rownames = "Factors")
comp_aov <- anova(comp_mdl) %>%
  as_tibble(rownames = "Factors")
aov_df <- bind_rows(list(Pred = pred_aov, Comp = comp_aov), .id = "Type")

model_labels <- c(
  Comp = "Number of Components",
  Pred = "Prediction Error"
)
pred_manova_plot <- aov_df %>%
  filter(!(Factors %in% c('Residuals', '(Intercept)'))) %>%
  select(Model = Type, Factors, Pillai,
         Fvalue = `approx F`, Pvalue = `Pr(>F)`) %>%
  mutate(Model = factor(Model, levels = c("Pred", "Comp"))) %>%
  mutate(Pvalue = ifelse(Pvalue < 0.05, "<0.05", ">=0.05")) %>%
  ggplot(aes(reorder(Factors, Pillai), Pillai, fill = Pvalue)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Fvalue, 2)), family = 'mono',
            angle = 0, hjust = "inward", size = 3) +
  facet_grid(cols = vars(Model), scales = 'free_y',
             labeller = labeller(Model = model_labels)) +
  theme_grey(base_family = "mono") +
  theme(legend.position = c(0.85, 0.1),
        legend.direction = 'vertical',
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  labs(x = NULL, y = "Pillai Statistic", 
       title = "Multivariate Analysis of Variance",
       subtitle = "Model: Prediction Error") +
  coord_flip()

ggsave(pred_manova_plot, filename = "_images/pred-manova.svg",
       width = 6.5, height = 8, scale = 0.8)

## ---- Effect Plots ----
## Plot 1
thm <- theme(plot.title = element_blank(),
             plot.subtitle = element_blank(),
             legend.position = "top",
             axis.title = element_blank())
plt1 <- eff_df("eta:relpos:Method", pred_mdl) %>%
  eff_plot3(reorder = FALSE, labeller = label_both,
            scales = "free_y") +
  theme_grey(base_family = "mono") +
  scale_x_discrete(labels = function(x) str_replace(x, "PLS", "PLSR")) +
  thm
plt2 <- eff_df("relpos:gamma:Method", pred_mdl) %>%
  eff_plot3(reorder = FALSE, labeller = label_both,
            scales = "free_y") +
  theme_grey(base_family = "mono") +
  scale_x_discrete(labels = function(x) str_replace(x, "PLS", "PLSR")) +
  thm
plt <- gridExtra::arrangeGrob(
  plt1, plt2, ncol = 2,
  padding = unit(0.04, 'npc'),
  bottom = grid::textGrob("Methods", gp = gpar(fontfamily = "monospace")), 
  left = grid::textGrob("Fitted Prediction Error",
                        gp = gpar(fontfamily = "monospace"), rot = 90))

ggsave(plt, filename = "_images/pred-comp-eff-plot-1.svg", width = 9, height = 5, scale = 0.8)

## Plot 2
thm <- theme(plot.title = element_blank(),
             plot.subtitle = element_blank(),
             legend.position = "top",
             axis.title = element_blank())
plt1 <- eff_df("eta:relpos:Method", comp_mdl) %>%
  eff_plot3(reorder = FALSE, labeller = label_both,
            scales = "free_y") +
  theme_grey(base_family = "mono") +
  scale_x_discrete(labels = function(x) str_replace(x, "PLS", "PLSR")) +
  thm
plt2 <- eff_df("relpos:gamma:Method", comp_mdl) %>%
  eff_plot3(reorder = FALSE, labeller = label_both,
            scales = "free_y") +
  theme_grey(base_family = "mono") +
  scale_x_discrete(labels = function(x) str_replace(x, "PLS", "PLSR")) +
  thm
plt <- gridExtra::arrangeGrob(
  plt1, plt2, ncol = 2,
  padding = unit(0.04, 'npc'),
  bottom = grid::textGrob("Methods", gp = gpar(fontfamily = "monospace")), 
  left = grid::textGrob("Fitted Number of Components",
                        gp = gpar(fontfamily = "monospace"), rot = 90))

ggsave(plt, filename = "_images/pred-comp-eff-plot-2.svg", width = 9, height = 5, scale = 0.8)

## ---- Estimation Model Manova ----
est_dta <- design_chr %>%
  select_if(function(x) n_distinct(x) > 1) %>%
  mutate(Design = as.character(1:n())) %>%
  mutate_at(vars(p, gamma, relpos, eta), as.factor) %>%
  right_join(est_error, by = "Design") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("p", as.factor) %>%
  mutate(Response = paste0("Y", Response))
est_spread_df <- est_dta %>%
  as.data.frame() %>%
  select(-Design, -q) %>%
  spread(Response, Est_Error)
min_comp_stk <- est_dta %>%
  group_by(p, relpos, eta, gamma, Method, Tuning_Param, Response) %>%
  summarize(Est_Error = mean(Est_Error)) %>%
  group_by(p, relpos, eta, gamma, Method, Response) %>%
  summarize(Tuning_Param = Tuning_Param[which.min(Est_Error)])
est_min <- est_dta %>%
  select(-Design, -q) %>%
  semi_join(min_comp_stk, by = c(
    "p", "relpos", "eta", "gamma", "Method",
    "Tuning_Param", "Response"
  )) %>% select(-Tuning_Param) %>%
  spread(Response, Est_Error)
comp_min <- est_dta %>%
  group_by(p, relpos, eta, gamma, Method, Replication, Response) %>%
  summarize(Tuning_Param = Tuning_Param[which.min(Est_Error)]) %>%
  spread(Response, Tuning_Param)

est_mdl <- lm(
  formula = cbind(Y1, Y2, Y3, Y4) ~ (p + gamma + eta + relpos + Method) ^ 3,
  data = est_min)
comp_mdl <- lm(
  formula = cbind(Y1, Y2, Y3, Y4) ~ (p + gamma + eta + relpos + Method) ^ 3,
  data = comp_min)
est_aov <- anova(est_mdl) %>%
  as_tibble(rownames = "Factors")
comp_aov <- anova(comp_mdl) %>%
  as_tibble(rownames = "Factors")
aov_df <- bind_rows(list(est = est_aov, Comp = comp_aov), .id = "Type")

model_labels <- c(
  Comp = "Number of Components",
  est = "Estimation Error"
)
est_manova_plot <- aov_df %>%
  filter(!(Factors %in% c('Residuals', '(Intercept)'))) %>%
  select(Model = Type, Factors, Pillai,
         Fvalue = `approx F`, Pvalue = `Pr(>F)`) %>%
  mutate(Model = factor(Model, levels = c("est", "Comp"))) %>%
  mutate(Pvalue = ifelse(Pvalue < 0.05, "<0.05", ">=0.05")) %>%
  ggplot(aes(reorder(Factors, Pillai), Pillai, fill = Pvalue)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Fvalue, 2)), family = 'mono',
            angle = 0, hjust = "inward", size = 3) +
  facet_grid(cols = vars(Model), scales = 'free',
             labeller = labeller(Model = model_labels)) +
  theme_grey(base_family = "mono") +
  theme(legend.position = c(0.85, 0.1),
        legend.direction = 'vertical',
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  labs(x = NULL, y = "Pillai Statistic", 
       title = "Multivariate Analysis of Variance",
       subtitle = "Model: Estimation Error") +
  coord_flip()

ggsave(est_manova_plot, filename = "_images/est-manova.svg",
       width = 6.5, height = 8, scale = 0.8)


## ---- Effect Plots ----
## Plot 1
thm <- theme(plot.title = element_blank(),
             plot.subtitle = element_blank(),
             legend.position = "top",
             axis.title = element_blank())
plt1 <- eff_df("eta:relpos:Method", est_mdl) %>%
  eff_plot3(reorder = FALSE, labeller = label_both,
            scales = "free_y") +
  theme_grey(base_family = "mono") +
  scale_x_discrete(labels = function(x) str_replace(x, "PLS", "PLSR")) +
  thm
plt2 <- eff_df("relpos:gamma:Method", est_mdl) %>%
  eff_plot3(reorder = FALSE, labeller = label_both,
            scales = "free_y") +
  theme_grey(base_family = "mono") +
  scale_x_discrete(labels = function(x) str_replace(x, "PLS", "PLSR")) +
  thm
plt <- gridExtra::arrangeGrob(
  plt1, plt2, ncol = 2,
  padding = unit(0.04, 'npc'),
  bottom = grid::textGrob("Methods", gp = gpar(fontfamily = "monospace")), 
  left = grid::textGrob("Fitted Estimation Error",
                        gp = gpar(fontfamily = "monospace"), rot = 90))

ggsave(plt, filename = "_images/est-comp-eff-plot-1.svg", width = 9, height = 5, scale = 0.8)

## Plot 2
thm <- theme(plot.title = element_blank(),
             plot.subtitle = element_blank(),
             legend.position = "top",
             axis.title = element_blank())
plt1 <- eff_df("eta:relpos:Method", comp_mdl) %>%
  eff_plot3(reorder = FALSE, labeller = label_both,
            scales = "free_y") +
  theme_grey(base_family = "mono") +
  scale_x_discrete(labels = function(x) str_replace(x, "PLS", "PLSR")) +
  thm
plt2 <- eff_df("relpos:gamma:Method", comp_mdl) %>%
  eff_plot3(reorder = FALSE, labeller = label_both,
            scales = "free_y") +
  theme_grey(base_family = "mono") +
  scale_x_discrete(labels = function(x) str_replace(x, "PLS", "PLSR")) +
  thm
plt <- gridExtra::arrangeGrob(
  plt1, plt2, ncol = 2,
  padding = unit(0.04, 'npc'),
  bottom = grid::textGrob("Methods", gp = gpar(fontfamily = "monospace")), 
  left = grid::textGrob("Fitted Number of Components",
                        gp = gpar(fontfamily = "monospace"), rot = 90))

ggsave(plt, filename = "_images/est-comp-eff-plot-2.svg", width = 9, height = 5, scale = 0.8)

## ---- Print Prediction Error and Estimation Error ----
print_df <- function(df, head_foot_df) {
  df_ <- head_foot_df %>% as_tibble()
  head_foot <- capture.output(df_)
  cat(head_foot[1], sep = "\n")
  cat(capture.output(df)[-c(1, 14)], sep = "\n")
  cat(head_foot[length(head_foot)], sep = "\n")
}

pred_df_print <- pred_dta %>% 
  filter(Design %in% c(1, 32)) %>% 
  spread(Response, Pred_Error) %>% 
  select(-Replication, -Tuning_Param, -Design, -q) %>% 
  distinct_at(1:5, .keep_all = TRUE) %>% 
  mutate_at("Method", str_replace, "PLS", "PLSR")

est_df_print <- est_dta %>% 
  filter(Design %in% c(1, 32)) %>% 
  spread(Response, Est_Error) %>% 
  select(-Replication, -Tuning_Param, -Design, -q) %>% 
  distinct_at(1:5, .keep_all = TRUE) %>% 
  mutate_at("Method", str_replace, "PLS", "PLSR")

print_df(pred_df_print, pred_spread_df)
print_df(est_df_print, est_spread_df %>% as_tibble() %>% select(-Replication, -Tuning_Param))


## ---- PCR vs PLS ------------------------------------
library(simrel)
library(pls)
library(GGally)
seq_range <- function(x, n) seq(min(x), max(x), length.out = n)
pred2df <- function(mdl, newdata, ...) {
  mdl_name <- deparse(mdl$call[[1]])
  dta <- as.data.frame(drop(predict(mdl, newdata, ...)))
  names(dta) <- paste0(mdl_name, "_Comp", 1:ncol(dta))
  return(dta)
}

set.seed(007)
sobj <- simrel(
  n = 100,
  p = 2,
  q = 2, 
  relpos = 2,
  gamma = 0.9,
  R2 = 0.9,
  type = "univariate"
)

# 
df <- tibble(
  y = c(sobj$Y[, 1]),
  x1 = c(sobj$X[, 1]),
  x2 = c(sobj$X[, 2])
)
pcr_scores <- bind_cols(df, as_tibble(unclass(scores(pcr(y ~ x1 + x2, data = df)))))
pls_scores <- bind_cols(df, as_tibble(unclass(scores(plsr(y ~ x1 + x2, data = df)))))
scores_df <- bind_rows(pcr = pcr_scores, pls = pls_scores, .id = "Method")

scores_plot <- pcr_scores %>% 
  select(-x1, -x2) %>% 
  gather(Component, Scores, -y) %>% 
  ggplot(aes(Scores, y)) +
  geom_point() +
  theme_gray(base_family = "sans-serif", base_size = 16) +
  facet_grid(.~Component) +
  labs(y = "Response",
       title = "Principal Components vs Response")
ggsave(scores_plot, filename = "_images/scores-plot.svg", width = 9, height = 4)

plot_fit <- function(sobj, mthd, ncomp, grid_size = 25) {
  require(plot3D)
  x <- with(sobj, c(X[, 1]))
  y <- with(sobj, c(X[, 2]))
  z <- with(sobj, c(Y[, 1]))
  method <- get(mthd)(z ~ x + y)
  grid_size <- grid_size
  x_pred <- seq(min(x), max(x), length.out = grid_size)
  y_pred <- seq(min(y), max(y), length.out = grid_size)
  xy <- expand.grid(x = x_pred, y = y_pred)
  z_pred <- matrix(predict(method, newdata = xy, ncomp = ncomp), grid_size, grid_size)
  fitpoints <- predict(method, ncomp = ncomp)
  par(family = "mono")
  scatter3D(x, y, z, ticktype = "detailed", pch = 21, bty = "g", col = "grey50",
            cex = 1, theta = 55, phi = 0, colkey = FALSE, bg = viridis::viridis(length(z)),
            main = paste("Method:", toupper(mthd), "|", ncomp, "Components"),
            xlab = "Predictor 1", ylab = "Predictor 2", zlab = "Response",
            surf = list(x = x_pred, y = y_pred, z = z_pred,  shade = 0.1,
                        fit = fitpoints, facets = NA, col = viridis::viridis(length(z)),
                        alpha = 0.2, lcol = viridis::viridis(length(z))))
  scatter3D(x, y, fitpoints, add = TRUE, colkey = FALSE, pch = 16, 
            col = viridis::viridis(length(fitpoints)),
            cex = 0.7)
}
plotly_fit <- function(sobj, mthd, ncomp, grid_size = 25) {
  require(plotly)
  x <- with(sobj, c(X[, 1]))
  y <- with(sobj, c(X[, 2]))
  z <- with(sobj, c(Y[, 1]))
  method <- get(mthd)(z ~ x + y)
  grid_size <- grid_size
  x_pred <- seq(min(x), max(x), length.out = grid_size)
  y_pred <- seq(min(y), max(y), length.out = grid_size)
  xy <- expand.grid(x = x_pred, y = y_pred)
  z_pred <- matrix(predict(method, newdata = xy, ncomp = ncomp), grid_size, grid_size)
  fitpoints <- predict(method, ncomp = ncomp)
  
  # custom grid style
  axx <- list(
    gridcolor='rgb(255, 255, 255)',
    zerolinecolor='rgb(255, 255, 255)',
    showbackground=TRUE,
    backgroundcolor='rgb(230, 230,230)'
  )
  # The Plot
  plot_ly() %>% 
    add_markers(
      x = x, y = y, z = z,
      showlegend = FALSE,
      marker = list(
        color = "CadetBlue",
        size = 5,
        line = list(
          color = "black",
          width = 0.5
        )
      )) %>% 
    add_surface(
      x = ~x_pred,
      y = ~y_pred,
      z = ~z_pred,
      showscale = FALSE,
      inherit = FALSE,
      opacity = 0.7
    ) %>%
    layout(
      title = paste("Method:", toupper(mthd), "; ", ncomp, "Components"),
      scene = list(
        xaxis = append(axx, list(title = "Predictor 1", range = c(-2, 2))),
        yaxis = append(axx, list(title = "Predictor 2", range = c(-2, 2))),
        zaxis = append(axx, list(title = "Response", range = c(-4, 4))),
        camera=list(
          eye = list(x=-1.8, y=1.8, z=0.8)
        )
      )
    )
}


svglite::svglite(file = "_images/pcr.svg", width = 9, height = 5)
op <- par(mfrow = c(1, 2), mar = c(2, 0, 3, 0))
for (method in c("pcr")) {
  for (ncomp in 1:2) {
    plot_fit(sobj, method, ncomp)
    box("figure", col = "#ababab", lwd = 5)
  }
  box("outer", col = "#ababab", lwd = 10)
}
par(op)
dev.off()

svglite::svglite(file = "_images/pls.svg", width = 9, height = 5)
op <- par(mfrow = c(1, 2), mar = c(2, 0, 3, 0))
for (method in c("plsr")) {
  for (ncomp in 1:2) {
    plot_fit(sobj, method, ncomp)
    box("figure", col = "#ababab", lwd = 5)
  }
  box("outer", col = "#ababab", lwd = 10)
}
par(op)
dev.off()


## ---- Envelope Methods ------------------------------------

sobj2 <- simrel(
  n = 100,
  p = 6,
  m = 4,
  q = c(3, 2),
  relpos = list(1:2, 4:5),
  gamma = 0.8, 
  R2 = c(0.8, 0.8),
  ypos = list(c(1, 3), c(2, 4)),
  type = "multivariate"
)

x <- sobj2$X
y <- sobj2$Y

library(Renvlp)
mdl_env <- env(x, y, u = 4)
op <- par(mfrow = c(1, 3))
image(with(sobj2, minerror), main = "Model Error")
image(with(mdl_env, Gamma %*% Omega %*% t(Gamma)), main = "Material")
image(with(mdl_env, Gamma0 %*% Omega0 %*% t(Gamma0)), main = "Immaterial")
par(op)

op <- par(mfrow = c(2, 1))
mat1 <- matrix(c(1, 2, 3, 3, 
                 1, 2, 3, 3,
                 1, 0, 0, 0), ncol = 4, nrow = 3, byrow = TRUE)
mat2 <- matrix(c(4, 5, 6, 6, 
                 4, 5, 6, 6,
                 4, 0, 0, 0), ncol = 4, nrow = 3, byrow = TRUE)
layout(rbind(mat1, mat2))
with(mdl_env, image(t(Gamma), main = "Gamma"))
with(mdl_env, image(Omega, main = "Omega"))
with(mdl_env, image(Gamma, main = "Gamma^T"))
with(mdl_env, image(t(Gamma0), main = "Gamma0"))
with(mdl_env, image(Omega0, main = "Omega0"))
with(mdl_env, image(Gamma0, main = "Gamma0^T"))
par(op)

## ---- Relevant Components and Relevant Spaces ----
relspace_plot <- function(xlab = "Predictor (X)", ylab = "Response (Y)",
                          pred_comp = TRUE, resp_comp = TRUE,
                          pred_relspace = !pred_comp, resp_relspace = !resp_comp, 
                          arrow = TRUE,
                          rect_fill = c("SteelBlue", "SteelBlue"),
                          space_fill = c("SeaGreen", "SeaGreen"),
                          rect_alpha = c(0.5, 0.5), space_alpha = c(0.8, 0.8),
                          title = "Linear Regression Model",
                          subtitle = "Relevant and Irrelevant Space") {
  library(grid)
  grid.newpage()
  pushViewport(viewport(
    width = unit(0.98, "npc"),
    height = unit(0.98, "npc"),
    layout = grid.layout(
      nrow = 3, ncol = 5, 
      widths = c(1/12, 2/12, 2/12, 6/12, 1/12),
      heights = c(1.5/6, 3.5/6, 1/6))))
  grid.rect(gp = gpar(fill = "whitesmoke"))
  
  ## Response Label -----
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
  grid.text(label = ylab, y = unit(0, "npc"), vjust = -1, 
            gp = gpar(fontfamily = "mono", fontsize = 16))
  popViewport(1)
  ## Response Rectangle -----
  pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 2))
  grid.rect(gp = gpar(fill = rect_fill[1], alpha = rect_alpha[1]))
  if (resp_relspace) {
    df_y <- data.frame(
      x = c(0.05, 0.05, 0.05, 1),
      y = c(0.05, 0.05, 0.05, 1)
    )
    grid.xspline(
      x = rep(df_y$x, 2),
      y = rep(rev(df_y$y), 2),
      gp = gpar(fill = space_fill[1], lwd = 2, 
                alpha = space_alpha[1]),
      shape = 1, open = FALSE
    )
  }
  if (resp_comp) {
    grid.rect(x = 0.3, width = 0.5,
              gp = gpar(fill = space_fill[1], alpha = space_alpha[1]))
  }
  popViewport(1)
  ## Predictor Label -----
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 4))
  grid.text(label = xlab, y = unit(0, "npc"), vjust = -1, 
            gp = gpar(fontfamily = "mono", fontsize = 16))
  popViewport(1)
  ## Predictor Rectangle -----
  pushViewport(viewport(layout.pos.col = 4, layout.pos.row = 2))
  grid.rect(gp = gpar(fill = rect_fill[2], alpha = rect_alpha[2]))
  if (pred_comp) {
    grid.rect(x = unit(0.3, "npc"), width = 0.3,
              gp = gpar(fill = space_fill[2], alpha = space_alpha[2]))
  }
  if (pred_relspace) {
    df_x <- data.frame(
      x = c(0.05, 0.05, 0.05, 1),
      y = c(0.05, 0.05, 0.05, 1)
    )
    grid.xspline(
      x = rep(df_x$x, 2),
      y = rep(rev(df_x$y), 2),
      gp = gpar(fill = space_fill[2], lwd = 2,
                alpha = space_alpha[2]),
      shape = 1, open = FALSE
    )
  }
  popViewport(1)
  ## Arrow ------
  pushViewport(viewport(layout.pos.col = 3, layout.pos.row = 2))
  grid.segments(x0 = 0.92, x1 = 0.4, y0 = 0.5, y1 = 0.5,
               arrow = arrow(length = unit(0.38, "inches")),
               gp = gpar(lwd = 45, linejoin = "mitre",
                         lineend = "butt", col = "grey40"))
  grid.segments(x0 = 0.9, x1 = 0.4, y0 = 0.5, y1 = 0.5,
               arrow = arrow(length = unit(0.35, "inches")),
               gp = gpar(lwd = 40, linejoin = "mitre",
                         lineend = "butt", col = "grey60"))
  popViewport(1)
  if (all(resp_relspace, pred_relspace)) {
    ## Extra Labels 
    pushViewport(viewport(layout.pos.col = 3, layout.pos.row = 3))
    grid.text(label = "X and Y envelope/ Relevant Spaces", y = unit(0, "npc"),
              vjust = -4, gp = gpar(fontfamily = "mono", fontsize = 16))
    popViewport(1)
    ## Extra Arrows 
    pushViewport(viewport(layout.pos.col = 2:4, layout.pos.row = 2:3))
    grid.segments(x0 = 0.05, x1 = 0.6, y0 = 0.18, y1 = 0.18, gp = gpar(lwd = 2))
    grid.segments(x0 = 0.05, x1 = 0.05, y0 = 0.18, y1 = 0.35, gp = gpar(lwd = 2),
                  arrow = arrow(length = unit(0.2, "cm")))
    grid.segments(x0 = 0.6, x1 = 0.6, y0 = 0.18, y1 = 0.35, gp = gpar(lwd = 2),
                  arrow = arrow(length = unit(0.2, "cm")))
    popViewport(1)
  }
  ## Title and Subtitles -------
  pushViewport(viewport(layout.pos.col = 2:4, layout.pos.row = 1))
  grid.text(label = title, y = unit(0.75, "npc"), x = unit(0, "npc"), just = "left",
            gp = gpar(font=2, fontsize = 18, fontfamily = "mono"))
  grid.text(label = subtitle, y = unit(0.5, "npc"), x = unit(0, "npc"), just = "left",
            gp = gpar(fontfamily = "mono", fontsize = 16))
  popViewport(1)
}

## Save the plot -------
svglite::svglite(file = "_images/relspace-plot-1.svg", width = 8.5, height = 5, standalone = TRUE)
relspace_plot(resp_relspace = FALSE, pred_relspace = FALSE, subtitle = NULL)
dev.off()

svglite::svglite(file = "_images/relspace-plot-2.svg", width = 8.5, height = 5, standalone = TRUE)
relspace_plot(resp_relspace = FALSE, pred_relspace = TRUE, pred_comp = FALSE, resp_comp = FALSE,
              subtitle = "Relevant and Irrelevant Predictor Space")
dev.off()

svglite::svglite(file = "_images/relspace-plot-3.svg", width = 8.5, height = 5, standalone = TRUE)
relspace_plot(resp_relspace = FALSE, pred_comp = TRUE, resp_comp = FALSE, 
              xlab = "Predictor Components (Z)",
              subtitle = "Relevant and Irrelevant Predictor Components")
dev.off()

svglite::svglite(file = "_images/relspace-plot-4.svg", width = 8.5, height = 5, standalone = TRUE)
relspace_plot(pred_comp = FALSE, resp_comp = FALSE, 
              subtitle = "Relevant and Irrelevant Space")
dev.off()

svglite::svglite(file = "_images/relspace-plot-5.svg", width = 8.5, height = 5, standalone = TRUE)
relspace_plot(pred_comp = TRUE, resp_comp = TRUE, 
              xlab = "Predictor Components (Z)",
              ylab = "Resp. Comp. (W)",
              subtitle = "Relevant and Irrelevant Components")
dev.off()

