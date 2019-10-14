pkgs <- c("tidyverse", "simrel", "pls", "Renvlp")
for (pkg in pkgs) suppressPackageStartupMessages(require(pkg, character.only = TRUE))

source("_script/00-function.R")

simulate <- function(gamma, relpos) {
  relpos <- list(eval(parse(text = relpos)))
  simrel(n = 100, p = 10, q = 10, m = 3,
         relpos = relpos, R2 = 0.9, gamma = gamma,
         eta = 0, ypos = list(1:3),
         type = "multivariate")
}
get_coef_err <- function(sobj_list, method, which = NULL) {
  ce <- map(sobj_list, coef_errors, method)
  if (is.null(which)) return(ce)
  switch(
    which,
    prediction = map_df(ce, "prediction_error", .id = "Replication"),
    estimation = map_df(ce, 'estimation_error', .id = "Replication"),
    coefficient = map_df(ce, 'coefficients', .id = "Replication")
  )
}

design <- crossing(
  gamma = seq(0, 1.2, length.out = 2),
  relpos = c('1:5', '4:7'),
  Method = c("PCR", "PLS1", "PLS2", 'Xenv')
) %>% mutate(
  Design = row_number(),
  Replication = list(1:15)
) %>% unnest()

sim_obj <- design %>% 
  group_by_all() %>% 
  mutate(sobj = map2(gamma, relpos, simulate))

coef_err <- sim_obj %>% 
  group_by(gamma, relpos, Method) %>% 
  summarize(coef_error = list(get_coef_err(sobj, unique(Method))))

prediction_error <- coef_err %>% 
  group_by(gamma, relpos, Method) %>% 
  transmute(pe = map(coef_error, ~map_df(.x, 'prediction_error', .id = "Replication"))) %>% 
  unnest() %>%
  mutate(Response = paste0("Y", Response)) %>% 
  rename(ncomp = Tuning_Param)

estimation_error <- coef_err %>% 
  group_by(gamma, relpos, Method) %>% 
  transmute(pe = map(coef_error, map_df, 'prediction_error', .id = "Replication")) %>% 
  unnest() %>%
  mutate(Response = paste0("Y", Response)) %>% 
  rename(ncomp = Tuning_Param)

prediction_error_plot <- coef_err %>% 
  group_by(gamma, relpos, Method) %>% 
  transmute(pred_plot = map(coef_error, err_plot, params = c('gamma', 'relpos'))) %>% 
  mutate(fname = paste0("pe-", gamma, '-', gsub(":", "-", relpos), "-", Method, ".svg"))

with(prediction_error_plot, {
  walk2(pred_plot, fname, function(plt, path) {
    plt <- plt +
      scale_y_continuous(limits = c(0, 1.3))
    ggsave(file.path("_images", "plots", path), plt,
           width = 7, height = 5)
  })
})

save(prediction_error, file = "_script/pred-error.rdata")

# pred_err_plot <- coef_err %>% 
#   group_by(gamma, relpos, Method) %>% 
#   mutate(pred_error_plot = map(pred_error, err_plot))
# 
# pred_error <- sim_obj %>% 
#   group_by(gamma, relpos, Method) %>% 
#   summarize(pred_error = list(extract_pe(sobj, unique(Method), 'prediction'))) %>% 
#   unnest() %>% 
#   rename(ncomp = Tuning_Param) %>% 
#   mutate(Response = paste0("Y", Response))
# 
# min_pred_error <- pred_error %>% 
#   group_by(gamma, relpos, Method, Replication, Response) %>% 
#   summarize(ncomp = ncomp[which.min(Pred_Error)],
#             pred_error = min(Pred_Error)) %>% 
#   ungroup() %>% 
#   mutate_if(is_character, as_factor)
# 
# mdl <- lm(cbind(Y1, Y2, Y3) ~ gamma * relpos * Method, 
#           data = min_pred_error %>% select(-pred_error) %>% spread(Response, ncomp))
