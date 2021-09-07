library(ShinyItemAnalysis)
library(lme4)


# prepare stan model beforehand
set.seed(1357)
stan_code <- brms::make_stancode(Score ~ 1 + (1 | ID), data = AIBS)
# compile the stan code
stan_models <- rstan::stan_model(model_code = stan_code, auto_write = TRUE)

# compiled model can be found in (see ?stan_model for more information)
# system2("open", tempdir())


ICCrestricted <- function(Data, case, var, rank = NULL,
                          method = "REML", stan_model = NULL,
                          dir = "top", sel = 1,
                          nsim = 1000, ci = .95, seed = 1357) {
  sel_max <- max(Data[[rank]], na.rm = TRUE)

  if (between(sel, 0, 1)) {
    sel <- round(sel * sel_max)
  }

  formula <- formula(paste0(var, " ~ 1 + (1 | ", case, ")"))

  dir <- match.arg(dir, c("top", "bottom"))

  Data <- switch(dir,
    top = Data[Data[[rank]] <= sel, ],
    bottom = Data[Data[[rank]] > sel_max - sel, ]
  )

  probs <- c(1 - ci, 1 + ci) / 2

  method <- match.arg(method, c("REML", "MCMC"))

  if (method == "REML") {
    fit <- lmer(formula = formula, data = Data)

    var_ratee <- as.numeric(VarCorr(fit)[case])
    var_resid <- sigma(fit)^2
    var_total <- var_ratee + var_resid

    ICC1 <- var_ratee / var_total
    ICC3 <- var_ratee / (var_ratee + var_resid / 3)

    bs <- bootMer(
      fit,
      function(mm) {
        c(as.numeric(VarCorr(mm)), sigma(mm)^2)
      }, nsim, seed
    )$t

    bICC1 <- bs[, 1] / rowSums(bs)
    ICC1_CI <- quantile(bICC1, probs, names = FALSE)

    bICC3 <- bs[, 1] / (bs[, 1] + bs[, 2] / 3)
    ICC3_CI <- quantile(bICC3, probs, names = FALSE)

    out <- data.frame(
      n_sel = sel,
      prop_sel = sel / sel_max,
      dir,
      var_ratee,
      var_resid,
      var_total,
      ICC1,
      ICC1_LCI = ICC1_CI[1],
      ICC1_UCI = ICC1_CI[2],
      ICC3,
      ICC3_LCI = ICC3_CI[1],
      ICC3_UCI = ICC3_CI[2]
    )
  } else { # if MCMC
    if (!is.null(stan_model) && is(stan_model, "stanmodel")) {
      fit <- rstan::sampling(stan_model,
        data = brms::make_standata(formula, data = Data),
        control = list(adapt_delta = .95), refresh = 0
      )
    } else {
      stop("Model is either not provided or it is not of class 'stanmodel'.",
        call. = FALSE
      )
    }

    samples <- rstan::extract(fit)
    tau <- as.vector(samples$sd_1)
    sigma <- samples$sigma

    prop_var_ci <- quantile(tau^2, probs, names = FALSE)
    res_var_ci <- quantile(sigma^2, probs, names = FALSE)

    IRR <- tau^2 / (tau^2 + sigma^2)
    IRR1_ci <- quantile(IRR, probs, names = FALSE)

    IRR3 <- tau^2 / (tau^2 + (sigma^2) / 3)
    IRR3_ci <- quantile(IRR3, probs, names = FALSE)

    out <- data.frame(
      n_sel = sel,
      prop_sel = sel / sel_max,
      dir,
      Proj = quantile(tau^2, 0.5, names = FALSE),
      ProjLCI = prop_var_ci[1],
      ProjUCI = prop_var_ci[2],
      Res = quantile(sigma^2, 0.5, names = FALSE),
      ResLCI = res_var_ci[1],
      ResUCI = res_var_ci[2],
      Total = quantile(tau^2, 0.5, names = FALSE) + quantile(sigma^2, 0.5, names = FALSE),
      ICC1 = quantile(IRR, 0.5, names = FALSE),
      ICC1_LCI = IRR1_ci[1],
      ICC1_UCI = IRR1_ci[2],
      ICC3 = quantile(IRR3, 0.5, names = FALSE),
      ICC3_LCI = IRR3_ci[1],
      ICC3_UCI = IRR3_ci[2]
    )
  }

  out
}


ICCrestricted(AIBS, "ID", "Score", "ScoreRankAdj", "REML", stan_models,
              sel = 1, dir = "b"
)




pb <- progress_estimated(n = length(seq_along(2:72)))
mc <- map_dfr(
  2:72,
  ~ {
    pb$tick()$print()
    ICCrestricted(AIBS, "ID", "Score", "ScoreRankAdj", "MCMC", stan_models,
      sel = .x
    )
  }
)

bind_rows(mc) %>%
  ggplot(aes(prop_sel, ICC1, ymin = ICC1_LCI, ymax = ICC1_UCI)) + # TODO general
  geom_pointrange() +
  scale_x_continuous(labels = scales::percent) +
  labs(
    x = paste0("Proportion of ", "top", " ratees"),
    y = "Reliability"
  ) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) + # TODO general
  theme_app()
