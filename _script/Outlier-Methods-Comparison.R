## ---- Outlier vs Methods ----
library(simrel)
library(pls)
library(tidyverse)
library(Renvlp)

set.seed(007)
sobj <- simrel(
  n = 50,
  p = 10,
  q = 10,
  relpos = 1:3,
  R2 = 0.8,
  gamma = 0.6,
  ntest = 100,
  type = "univariate"
)

add_outlier <- function(x, at = 1, by = 5) {
  x[at, ] <- x[at, ] + by
  return(x)
}
get_data <- function(sobj, outlier_in = c("none", "x", "y", "both")) {
  dta_mat <- cbind(sobj$Y, sobj$X)
  dta_mat <- switch (outlier_in,
    none = dta_mat,
    x = cbind(dta_mat[, 1, drop = FALSE], add_outlier(dta_mat[, -1])),
    y = cbind(add_outlier(dta_mat[, 1, drop = FALSE]), dta_mat[, -1]),
    both = cbind(add_outlier(dta_mat[, 1, drop = FALSE]), add_outlier(dta_mat[, -1]))
  )
  colnames(dta_mat) <- c("y", paste0("x", 1:ncol(sobj$X)))
  return(as_tibble(dta_mat))
}
fit_xenv <- function(x, y, ncomp = 1:10) {
  map(ncomp, ~xenv(x, y, ..1))
}

train <- tibble(
  None = list(get_data(sobj, "none")),
  X = list(get_data(sobj, "x")),
  Y = list(get_data(sobj, "y")),
  Both = list(get_data(sobj, "both"))
) %>% gather(Design, Train) %>% 
  mutate(Design = factor(Design, levels = c("None", "X", "Y", "Both")))
test <- cbind(y = sobj$TESTY, sobj$TESTX) %>% 
  `colnames<-`(c("y", paste0("x", 1:ncol(sobj$TESTX)))) %>% 
  as_tibble()

fit <- train %>% 
  group_by(Design) %>% 
  mutate(ols = map(Train, ~lm(y ~ ., data = ..1))) %>% 
  mutate(pcr = map(Train, ~pcr(y ~ ., data = ..1))) %>% 
  mutate(pls = map(Train, ~plsr(y ~ ., data = ..1))) %>%
  mutate(xenv = map(Train, ~fit_xenv(..1[, -1], ..1[, 1]))) %>% 
  gather(Method, Fit, -Design, -Train)

err <- fit %>% 
  group_by(Design, Method) %>% 
  transmute(PredError = map2(Method, Fit, function(mthd, obj) {
    if (mthd == "xenv") {
      coef <- do.call(cbind, map(obj, ~c(..1[["mu"]], ..1[["beta"]])))
      colnames(coef) <- paste(1:ncol(coef), "comps")
    } else {
      coef <- as.matrix(drop(coef(obj, intercept = TRUE, ncomp = 1:10))) 
    }
    pred <- cbind(1, as.matrix(test[, -1])) %*% coef
    msep <- apply(pred, 2, function(x) mean((x - test$y)^2))
    if (mthd == "ols") names(msep) <- "1 comps"
    tibble(Comp = names(msep), PredError = msep)
  })) %>% unnest(PredError)

plt_cap <- as.list(sobj$call)[-c(1, 8)]
plt_cap <- paste(names(plt_cap), plt_cap, sep = ":", collapse = ", ")

plt <- err %>% mutate(Comp = parse_number(Comp)) %>%
  ggplot(aes(Comp, PredError, color = Method)) +
  geom_line(aes(group = Method)) +
  geom_point(size = 1, shape = 21, fill = "whitesmoke") + 
  geom_hline(aes(yintercept = PredError, color = Method), 
             linetype = "dashed",
             data = err %>% filter(Method == "ols")) +
  facet_wrap(~Design, nrow = 1, labeller = label_both) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_brewer(palette = "Set1", labels = toupper) +
  labs(x = "Number of Components", 
       y = "Mean Square Error\nof Prediction",
       caption = paste("Params::", plt_cap)) +
  theme_grey(base_family = "mono", base_size = 16) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_line(size = 0.1)) +
  ggtitle("Effect of single outlier",
          "A comparison of different estimators")

ggsave(plt, filename = "_images/outlier-vs-methods.svg",
       width = 7, height = 3, scale = 1.4)
