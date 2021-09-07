library(tidyverse)

# load the data (from package "psych")
data(bfi, package = "psych")

# make dataframe without item-only data
bfi_items <- bfi %>% select(-c(gender, education, age))

# custom function
sia_parallel <- function(Data, method = "quantile", p = .95, n_iter = 20, fm = "minres", use = "pairwise") {
  n_subj <- nrow(Data)
  n_vars <- ncol(Data)

  if (p < 0 || p > 1) stop("Probability must be between 0 and 1!", call. = FALSE)

  # use base R parallel computation, seems quite fast!
  sim_eigen_list <- parallel::mclapply(seq_len(n_iter), function(XX) {
    # simulate random dataset with dimensions same as original data
    sim_data <- matrix(rnorm(n_subj * n_vars), nrow = n_subj, ncol = n_vars)
    sim_corr <- cor(sim_data) # just simple Pearson

    # compute & output eigenvalues based on simulated sample
    psych::fa(sim_corr, fm = fm, rotate = "none", warnings = FALSE)$values
    # note: no rotation is used in fa.parallel
  })

  sim_eigen_df <- t(matrix(unlist(sim_eigen_list), ncol = n_iter)) # weird but fastest
  sim_eigen <- switch(match.arg(method, c("mean", "quantile")),
    mean = apply(sim_eigen_df, 2, mean), # psych-like, i.e. original Horn's method (see below)
    quantile = apply(sim_eigen_df, 2, function(x) {
      quantile(x, p)
    }) # edited, more stringent Horn's method, as in jamovi or JASP (recommended),
  )

  # see also https://doi.apa.org/doi/10.1037/met0000230

  data_eigen <- psych::fa(Data, fm = fm, warnings = FALSE, use = use)$values

  # into the dataframe
  df <- list()
  df[["n_fact"]] <- factor(c(1:length(data_eigen), 1:length(sim_eigen)))
  df[["type"]] <- factor(c(rep("data", length(data_eigen)), rep("sim", length(sim_eigen))))
  df[["eigen"]] <- c(data_eigen, sim_eigen)

  # weird but very fast, dpylr not neccessary!
  attr(df, "row.names") <- seq_len(length(df[[1]]))
  attr(df, "class") <- "data.frame"

  df
}

# results comparison
method_comparison <- map_dfr(
  1:50,
  ~ {
    # SIA
    sia_out <- bfi_items %>%
      sia_parallel() %>%
      add_column(source = "sia", run = .x, .before = 1)

    psych_out <- psych::fa.parallel(bfi_items, fa = "fa", sim = T, plot = F)

    psych_out <- tibble(
      n_fact = seq_len(25) %>% as.factor(),
      data_eigen = psych_out$fa.values,
      sim_eigen = psych_out$fa.sim # simulated vals
    ) %>%
      pivot_longer(-n_fact,
        names_to = "type", values_to = "eigen",
        names_ptypes = list("type" = factor())
      ) %>%
      add_column(source = "psych", run = .x, .before = 1)

    bind_rows(sia_out, psych_out)
  }
)

# differences in simulated eigenvals by n_fact
method_comparison %>%
  pivot_wider(names_from = c(source, type), values_from = eigenvalue) %>%
  ggplot(aes(n_fact, psych_sim_eigen - sia_sim_eigen)) +
  geom_point()

# corr of simulated eigenvals
method_comparison %>%
  pivot_wider(names_from = c(source, type), values_from = eigenvalue) %>%
  ggplot(aes(psych_sim_eigen, sia_sim_eigen)) +
  geom_point()


method_comparison %>%
  filter(type %in% c("sim_eigen", "sim")) %>%
  ggplot(aes(n_fact, eigen, col = source)) +
  stat_summary(fun.data = "mean_cl_normal", size = .2)

# DISCOVERY: methods differ systematicaly by naked eye


# THERE'S WHY:
# maybe psych returns means
psy_ret$values[, c(76:100)] %>%
  colMeans() %>%
  as.numeric()
psy_ret$fa.sim # hmmhmhhmmhm.... doesnt seem like .95 quantile!

# psych returns simulated eigenvalues in $sim.fa for all 20 iterations
# raw values are in $values, which is quite a mess..
# first 50 cols are PCA results, the rest is FA

# let's display the relationship between quant arg and eigenvals
psych_quants_mean <- map_dfr(seq(0, 1, .01), ~
tibble(eigen = psych::fa.parallel(bfi_items, fa = "fa", sim = T, plot = F, quant = .x)$fa.sim, quant = .x))

psych_quants_mean %>% ggplot(aes(x = quant, y = eigen)) +
  stat_summary(fun = mean)
# just random

# SIA version:
sia_quants_mean <- map_dfr(seq(0, 1, .01), ~
tibble(eigen = sia_parallel(bfi_items, q = .x)$eigen[26:50], quant = .x))

sia_quants_mean %>% ggplot(aes(x = quant, y = eigen)) +
  stat_summary(fun = mean)
# depends on quantile used as it should

# DISCOVERY: quant is ignored in psych and fa.sim are MEANs of eigenvals



# 100 runs speed comparison
library(microbenchmark)
bench_out <- microbenchmark(
  "sia" = {
    sia_parallel(bfi_items)
  },
  "psych" = {
    psych::fa.parallel(bfi_items, fa = "fa", sim = T, plot = F)
  }, times = 100
)

# psych is doing lots of stuff unrelated to FA, time is lost...

# expr       min        lq      mean    median        uq       max neval cld
# sia   292.4066  319.8437  341.9678  335.4093  352.2635  667.9526   100  a
# psych 995.6267 1043.8132 1094.5048 1072.7061 1110.2288 1444.9413   100   b

ggplot2::autoplot(bench_out)
