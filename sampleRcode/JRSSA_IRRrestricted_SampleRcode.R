library(ShinyItemAnalysis) # data, graphics, restricted IRR and interactive app
library(lme4)              # mixed effect models
library(brms)              # Bayesian mixed effect models
library(ggplot2)           # graphics
library(cowplot)           # graphics
library(purrr)             # iterations
library(dplyr)             # data wrangling & progress bars

options(digits = 4)

######################################################################
############# AIBS data

data(AIBS)
head(AIBS, n = 2)
#summary(AIBS)
summary(AIBS[,c("ID", "Score", "ScoreAvg", "ScoreRankAdj")])

# Histogram of Score
hist(AIBS$Score, main = "Histogram of Score", xlab = "Score")
g0a <- AIBS %>%
  ggplot(aes(Score)) + geom_histogram() +
  xlab("AIBS overall score") + ylab("Frequency") +
  theme_app()
g0a

# Caterpillar plot
g1a <-
    AIBS %>%
    ggplot(aes(x = ScoreRankAdj, y = Score, group = ID)) +
    geom_line(col = "gray") +
    geom_point(shape = 1, size = 1.5) +
    stat_summary(
      fun = mean, fun.args = list(na.rm = TRUE), geom = "point", col = "red",
      shape = 5, size = 1.5, stroke = .35
      ) +
    labs(x = "AIBS application rank", y = "Overall scientific merit") +
    coord_cartesian(ylim = c(1, 5)) +
    theme_app(base_size = 12)

######################################################################
############# NIH data

data(NIH)
#load(file = "data/NIH.rda")
head(NIH[,c("ID", "GroupID", "Score", "ScoreAvg",
                 "ScoreRankAdj")], n = 2)
summary(NIH[,c("ID", "GroupID", "Score", "ScoreAvg",
               "ScoreRankAdj")])
hist(NIH$Score)
g0b <- NIH %>%
  ggplot(aes(Score)) + geom_histogram() +
  xlab("NIH overall score") + ylab("Frequency") +
  coord_cartesian(xlim = c(1, 9)) + theme_app()
g0b
(g0 <- plot_grid(g0a, g0b))

# sample of black applicants present in our data
unique(NIH$ID[NIH$GroupID != "Random White"])

g1b <-
    NIH %>%
    ggplot(aes(x = ScoreRankAdj, y = Score, group = ID)) +
    geom_line(col = "gray") +
    geom_point(shape = 1, size = 1.5) +
    stat_summary(
      fun = mean, fun.args = list(na.rm = TRUE), geom = "point", col = "red",
      shape = 5, size = 1.5, stroke = .35
      ) +
    labs(x = "NIH application rank", y = "Overall scientific merit") +
    coord_cartesian(ylim = c(1, 9)) +
    theme_app(base_size = 12)


(g1 <- plot_grid(g1a, g1b))

#ggsave(g1, file = "sampleRcode/IRRfiles/Figure1.png", width = 8, height = 4, dpi = 300, device = "png")
#ggsave(g1, file = "sampleRcode/IRRfiles/Figure1.eps", width = 8, height = 4, dpi = 300, device = "eps")

ggsave(g1, file = "sampleRcode/IRRfiles/Figure1.png", width = 8, height = 4, dpi = 600, device = "png")
ggsave(g1, file = "sampleRcode/IRRfiles/Figure1.eps", width = 8, height = 4, dpi = 600, device = "eps")

# TODO:
# check figures are OK:
# https://authorservices.wiley.com/asset/photos/electronic_artwork_guidelines.pdf
# higher resolution was set, but not sure about width/length



# Define the main function governing all the fitting ----------------------

# - handles both range-restriction directions (top/bottom)
# - handles both MLE and Bayesian estimates
# - provides control of CI width
# - customisable seed, default to 1357
# - customisable number of bootstrap simulations
# - handles pre-compiled STAN model as an argument "stan_model" with check for proper class
# - easily iterable/mapable via for loop, lapply or preferably purrr:map_dfr (returns data.frame directly)

ICCrestricted <- function(Data, case, var, rank = NULL,
                          method = "REML", stan_model = NULL,
                          dir = "top", sel = 1,
                          nsim = 1000, ci = .95, seed = 1357) {
  sel_max <- max(Data[[rank]], na.rm = TRUE)

  if (sel <= 1) {
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

    VarID <- as.numeric(VarCorr(fit)[case])
    VarResid <- sigma(fit)^2
    VarTotal <- VarID + VarResid

    ICC1 <- VarID / VarTotal
    ICC3 <- VarID / (VarID + VarResid / 3)

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
      K = sel,
      Kperc = sel / sel_max,
      #dir,
      Proj = VarID,
      Res = VarResid,
      Total =VarTotal,
      IRR1 = ICC1,
      LCI = ICC1_CI[1],
      UCI = ICC1_CI[2],
      IRR3 = ICC3,
      LCI3 = ICC3_CI[1],
      UCI3 = ICC3_CI[2]
    )
  } else { # if MCMC
    if (!is.null(stan_model) && is(stan_model, "stanmodel")) {
      fit <- rstan::sampling(stan_model,
                             data = brms::make_standata(formula, data = Data),
                             control = list(adapt_delta = .95), refresh = 0, seed = seed
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
      K = sel,
      Kperc = sel / sel_max,
      #dir,
      Proj = quantile(tau^2, 0.5, names = FALSE),
      ProjLCI = prop_var_ci[1],
      ProjUCI = prop_var_ci[2],
      Res = quantile(sigma^2, 0.5, names = FALSE),
      ResLCI = res_var_ci[1],
      ResUCI = res_var_ci[2],
      Total = quantile(tau^2, 0.5, names = FALSE) + quantile(sigma^2, 0.5, names = FALSE),
      IRR1 = quantile(IRR, 0.5, names = FALSE),
      LCI = IRR1_ci[1],
      UCI = IRR1_ci[2],
      IRR3 = quantile(IRR3, 0.5, names = FALSE),
      LCI3 = IRR3_ci[1],
      UCI3 = IRR3_ci[2]
    )
  }

  out
}


######################################################################
### MLE estimate on full sample

ICCrestricted(Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj")

######################################################################
### MCMC on full sample

# prepare stan model beforehand
set.seed(1357)
stan_code_AIBS <- brms::make_stancode(Score ~ 1 + (1 | ID), data = AIBS)
# compile the stan code
stan_model_AIBS <- rstan::stan_model(model_code = stan_code_AIBS, auto_write = TRUE)

# compiled model can be found with:
# system2("open", tempdir())

# fit
ICCrestricted(
  Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj",
  method = "MCMC", stan_model = stan_model_AIBS
)

#####################################################################
#####################################################################

### REML on restricted samples TOP - may take over 10 minutes!

# Can the following code be shortened if no progress bar is needed?
tabIRRrestrTOP_REML_AIBS <- map_dfr(
  2:72,
  ~ {
    ICCrestricted(AIBS, "ID", "Score", "ScoreRankAdj",
                  sel = .x
    )
  }
)


# initialize progress bar
pb <- progress_estimated(n = length(seq_along(2:72)))

# estimate
tabIRRrestrTOP_REML_AIBS <- map_dfr(
  2:72,
  ~ {
    pb$tick()$print() # tick progress bar
    ICCrestricted(AIBS, "ID", "Score", "ScoreRankAdj",
                  sel = .x
    )
  }
)

# Save/read
write.csv(tabIRRrestrTOP_REML_AIBS, file="tabIRRrestrTOP_reml_seed1357_AIBS.csv", row.names = FALSE) # Save the results
#tabIRRrestrTOP_REML_AIBS <- read.csv("tabIRRrestrTOP_reml_seed1357_AIBS.csv")

### data IRR_MLE top AIBS table}
tabIRRrestrTOP_REML_AIBS

####################################
####################################

### REML on restricted samples bottom - may take over 10 minutes!

# initialize progress bar
pb <- progress_estimated(n = length(seq_along(2:72)))

# estimate
tabIRRrestrBOTTOM_REML_AIBS <- map_dfr(
  2:72,
  ~ {
    pb$tick()$print() # tick progress bar
    ICCrestricted(AIBS, "ID", "Score", "ScoreRankAdj", dir = "bottom",
                  sel = .x
    )
  }
)

### Save/read
write.csv(tabIRRrestrBOTTOM_REML_AIBS, file="tabIRRrestrBOTTOM_reml_seed1357_AIBS.csv", row.names = FALSE) # Save the results
#tabIRRrestrBOTTOM_REML_AIBS <- read.csv("tabIRRrestrBOTTOM_reml_seed1357_AIBS.csv")

### data IRR_MLE bottom AIBS table}
round(tabIRRrestrBOTTOM_REML_AIBS,3)[,]

#####################################################################
#####################################################################

### MCMC on restricted samples AIBS top - may take well over 5 minutes!

# initialize progress bar
pb <- progress_estimated(n = length(seq_along(2:72)))

# estimate, use STAN model compiled above
tabIRRrestrTOP_BAYES_AIBS <- map_dfr(
  2:72,
  ~ {
    pb$tick()$print() # tick progress bar
    ICCrestricted(AIBS, "ID", "Score", "ScoreRankAdj", "MCMC", stan_model_AIBS,
                  sel = .x
    )
  }
)

### Save/read
write.csv(tabIRRrestrTOP_BAYES_AIBS, file="tabIRRrestrTOP_Ht25_seed1357_AIBS.csv", row.names = FALSE)
# tabIRRrestrTOP_BAYES_AIBS <- read.csv("tabIRRrestrTOP_Ht25_seed1357_AIBS.csv")

### MCMC IRR restricted top AIBS read
tabIRRrestrTOP_BAYES_AIBS[,c("K", "Kperc", "Proj", "Res", "Total",
                         "IRR1", "LCI", "UCI", "IRR3", "LCI3", "UCI3")]

####################################
####################################

### Bottom-range samples
## MCMC on restricted samples bottom AIBS - may take well over 5 minutes!

# initialize progress bar
pb <- progress_estimated(n = length(seq_along(2:72)))

# estimate, use STAN model compiled above
tabIRRrestrBOTTOM_BAYES_AIBS <- map_dfr(
  2:72,
  ~ {
    pb$tick()$print() # tick progress bar
    ICCrestricted(AIBS, "ID", "Score", "ScoreRankAdj", "MCMC", stan_model_AIBS,
                  dir = "bottom",
                  sel = .x
    )
  }
)

### Save/read
write.csv(tabIRRrestrBOTTOM_BAYES_AIBS, file="tabIRRrestrBOTTOM_Ht25_seed1357_AIBS.csv", row.names = FALSE) # Save the results
#tabIRRrestrBOTTOM_BAYES_AIBS <- read.csv("tabIRRrestrBOTTOM_Ht25_seed135_AIBS.csv")

### MCMC IRR restricted read bottom AIBS
tabIRRrestrBOTTOM_BAYES_AIBS[,c("K", "Kperc", "Proj", "Res", "Total",
                                "IRR1", "LCI", "UCI", "IRR3", "LCI3", "UCI3")]

###############################################################################
###############################################################################
###############################################################################
##### NIH DATA

### REML on full data
ICCrestricted(NIH, "ID", "Score", "ScoreRankAdj")

# MCMC on full data
# prepare stan model beforehand
set.seed(1357)
stan_code_NIH <- brms::make_stancode(Score ~ 1 + (1 | ID), data = NIH)
# compile the stan code
stan_model_NIH <- rstan::stan_model(model_code = stan_code_NIH, auto_write = TRUE)

# compiled model can be found with:
# system2("open", tempdir())

# fit
# ICCrestricted(Data = NIH, case = "ID", var = "Score", rank = "ScoreRankAdj",
#               method = "MCMC", stan_model = stan_model_NIH)


### REML on restricted top samples of NIH - may take long!

percentage <- 1:100                     # vector of percentages considered
Ks <- ceiling(2076/100*percentage)      # ranks considered

# initialize progress bar to see whats happening and ETA
pb <- progress_estimated(n = length(seq_along(Ks)))

# estimate
tabIRRrestrTOP_REML_NIH <- map_dfr(
  Ks,
  ~ {
    pb$tick()$print() # tick progress bar
    ICCrestricted(NIH, "ID", "Score", "ScoreRankAdj",
                  sel = .x
    )
  }
)

write.csv(tabIRRrestrTOP_REML_NIH, file="tabIRRrestrTOP_reml_seed1357_NIH.csv", row.names = FALSE) # Save the results
# tabIRRrestrTOP_REML_NIH <- read.csv("tabIRRrestrTOP_reml_seed1357_NIH.csv")

round(tabIRRrestrTOP_REML_NIH, 3)

####################################
####################################

##################################
### Bottom-range samples
##
## REML on restricted bottom samples of NIH - takes about 50 min!
percentage <- 1:100                     # vector of percentages considered
Ks <- ceiling(2076/100*percentage)      # ranks considered

# initialize progress bar to see whats happening and ETA
pb <- progress_estimated(n = length(seq_along(Ks)))

# estimate
tabIRRrestrBOTTOM_REML_NIH <- map_dfr(
  Ks,
  ~ {
    pb$tick()$print() # tick progress bar
    ICCrestricted(NIH, "ID", "Score", "ScoreRankAdj",
                  dir = "bottom",
                  sel = .x
    )
  }
)

### Save/read
write.csv(tabIRRrestrBOTTOM_REML_NIH, file="tabIRRrestrBOTTOM_reml_seed1357_NIH.csv", row.names = FALSE) # Save the results
# tabIRRrestrBOTTOM_REML_NIH <- read.csv("tabIRRrestrBOTTOM_reml_seed1357_NIH.csv")

### data IRR_MLE bottom NIH table}
tabIRRrestrBOTTOM_REML_NIH


#####################################################################
#####################################################################

### Top-range samples MCMC
##
## MCMC on restricted samples NIH top - takes about 2 hours!

# initialize progress bar to see whats happening and ETA
pb <- progress_estimated(n = length(seq_along(Ks)))

# estimate, use STAN model compiled above
tabIRRrestrTOP_BAYES_NIH <- map_dfr(
  Ks,
  ~ {
    pb$tick()$print() # tick progress bar
    ICCrestricted(NIH, "ID", "Score", "ScoreRankAdj",
                  method = "MCMC", stan_model = stan_model_NIH,
                  sel = .x
    )
  }
)




### Save/read
write.csv(tabIRRrestrTOP_BAYES_NIH, file="tabIRRrestrTOP_Ht25_seed1357_NIH.csv", row.names = FALSE)
#tabIRRrestrTOP_BAYES_NIH <- read.csv("tabIRRrestrTOP_Ht25_seed1357_NIH.csv")

## MCMC IRR restricted top read NIH
# MCMC estimates
tabIRRrestrTOP_BAYES_NIH[,c("K", "Kperc", "Proj", "Res", "Total",
                         "IRR1", "LCI", "UCI", "IRR3", "LCI3", "UCI3")]

##################################
##################################
### Bottom-range samples MCMC

# initialize progress bar to see whats happening and ETA
pb <- progress_estimated(n = length(seq_along(Ks)))

# estimate, use STAN model compiled above
tabIRRrestrBOTTOM_BAYES_NIH <- map_dfr(
  Ks,
  ~ {
    pb$tick()$print() # tick progress bar
    ICCrestricted(NIH, "ID", "Score", "ScoreRankAdj",
                  method = "MCMC", stan_model = stan_model_NIH,
                  dir = "bottom",
                  sel = .x
    )
  }
)

## Save/read
write.csv(tabIRRrestrBOTTOM_BAYES_NIH, file = "tabIRRrestrBOTTOM_Ht25_seed1357_NIH.csv", row.names = FALSE) # Save the results
#tabIRRrestrBOTTOM_BAYES_NIH <- read.csv("tabIRRrestrBOTTOM_Ht25_seed1357_NIH.csv")

## MCMC IRR restricted bottom read NIH
# MCMC estimates
round(tabIRRrestrBOTTOM_BAYES_NIH[,c("K", "Kperc", "Proj", "Res", "Total",
                         "IRR1", "LCI", "UCI", "IRR3", "LCI3", "UCI3")], 3)


#################################################################
#################################################################
# Figure 2
tabIRRrestrTOP_REML_AIBS <- read.csv("tabIRRrestrTOP_reml_seed1357_AIBS.csv")
tabIRRrestrTOP_REML_NIH <- read.csv("tabIRRrestrTOP_reml_seed1357_NIH.csv")
tabIRRrestrTOP_BAYES_AIBS <- read.csv("tabIRRrestrTOP_Ht25_seed1357_AIBS.csv")
tabIRRrestrTOP_BAYES_NIH <- read.csv("tabIRRrestrTOP_Ht25_seed1357_NIH.csv")


# Figure IRR restricted top, warning=FALSE, message=FALSE, eval = FALSE
g2a <- ggplot() +
  geom_point(data = tabIRRrestrTOP_REML_AIBS, aes(x = K, y = IRR1, col = "REML")) +
  geom_errorbar(data = tabIRRrestrTOP_REML_AIBS, aes(x = K, y = IRR1,
                                                     ymax = UCI, ymin = LCI, col = "REML"), width = 0) +
  geom_point(data = tabIRRrestrTOP_BAYES_AIBS, aes(x = K + 0.3, y = IRR1, col = "MCMC")) +
  geom_errorbar(data = tabIRRrestrTOP_BAYES_AIBS, aes(x = K + 0.3, y = IRR1,
                                                      ymax = UCI, ymin = LCI, col = "MCMC"), width = 0) +
  scale_color_manual("", breaks = c("REML", "MCMC"),
                     labels = c("REML estimate", "MCMC estimate"), values = c("black", "blue")) +
  xlab("Number of top AIBS proposals") +
  ylab("IRR") +
  ylim(0, 1) +
  ggtitle("  ") +
  theme_app(base_size = 12) +
  theme(legend.position = c(0.97, 0.97), legend.justification = c(1, 1))


# Figure IRR restricted top 2
g2a2 <- ggplot() +
  geom_point(data = tabIRRrestrTOP_REML_AIBS, aes(x = 100*Kperc, y = IRR1, col = "REML")) +
  geom_errorbar(data = tabIRRrestrTOP_REML_AIBS, aes(x = 100*Kperc, y = IRR1,
                                                     ymax = UCI, ymin = LCI, col = "REML"), width = 0) +
  geom_point(data = tabIRRrestrTOP_BAYES_AIBS, aes(x = 100*Kperc + 0.5, y = IRR1, col = "MCMC")) +
  geom_errorbar(data = tabIRRrestrTOP_BAYES_AIBS, aes(x = 100*Kperc + 0.5, y = IRR1,
                                                      ymax = UCI, ymin = LCI, col = "MCMC"), width = 0) +
  scale_color_manual("", breaks = c("REML", "MCMC"),
                     labels = c("REML estimate", "MCMC estimate"), values = c("black", "blue")) +
  xlab("Percentage of top AIBS proposals") +
  ylab("IRR") +
  ylim(0, 1) +
  ggtitle("  ") +
  theme_app(base_size = 12) +
  theme(legend.position = c(0.97, 0.97), legend.justification = c(1, 1))


# Figure IRR restricted NIH top
g2b <- ggplot() +
  geom_point(data = tabIRRrestrTOP_REML_NIH[2*(1:50),], aes(x = Kperc, y = IRR1, col = "REML")) +
  geom_errorbar(data = tabIRRrestrTOP_REML_NIH[2*(1:50),], aes(x = Kperc, y = IRR1,
                                                               ymax = UCI, ymin = LCI, col = "REML"), width = 0) +
  geom_point(data = tabIRRrestrTOP_BAYES_NIH[2*(1:50),], aes(x = Kperc + 0.9,
                                                             y = IRR1, col = "MCMC")) +
  geom_errorbar(data = tabIRRrestrTOP_BAYES_NIH[2*(1:50),], aes(x = Kperc + 0.9, y = IRR1,
                                                                ymax = UCI, ymin = LCI, col = "MCMC"), width = 0) +
  scale_color_manual("", breaks = c("REML", "MCMC"),
                     labels = c("REML estimate", "MCMC estimate"),
                     values = c("black", "blue")) +
  xlab("Percentage of top NIH proposals") +
  ylab("IRR") +
  ylim(0, 1) +
  ggtitle("  ") +
  theme_app(base_size = 12) +
  theme(legend.position = c(0.97, 0.97), legend.justification = c(1, 1))
(g2 <- plot_grid(g2a2, g2b))

ggsave(g2, file = "sampleRcode/IRRfiles/Figure2.png", width = 8, height = 4, dpi = 600, device = "png")
ggsave(g2, file = "sampleRcode/IRRfiles/Figure2.eps", width = 8, height = 4, dpi = 600, device = "eps")

#################################################################
# Figure 3
#tabIRRrestrBOTTOM_REML_AIBS <- read.csv("sampleRcode/IRRfiles/tabIRRrestrBOTTOM_reml_seed1357_AIBS.csv")
#tabIRRrestrBOTTOM_REML_NIH <- read.csv("sampleRcode/IRRfiles/tabIRRrestrBOTTOM_reml_seed1357_NIH.csv")
#tabIRRrestrBOTTOM_BAYES_AIBS <- read.csv("sampleRcode/IRRfiles/tabIRRrestrBOTTOM_Ht25_seed1357_AIBS.csv")
#tabIRRrestrBOTTOM_BAYES_NIH <- read.csv("sampleRcode/IRRfiles/tabIRRrestrBOTTOM_Ht25_seed1357_NIH.csv")

# Figure IRR restricted bottom, warning=FALSE, message=FALSE, eval = FALSE
g3a <- ggplot() +
  geom_point(data = tabIRRrestrBOTTOM_REML_AIBS, aes(x = K, y = IRR1, col = "REML")) +
  geom_errorbar(data = tabIRRrestrBOTTOM_REML_AIBS, aes(x = K, y = IRR1,
                                                        ymax = UCI, ymin = LCI, col = "REML"), width = 0) +
  geom_point(data = tabIRRrestrBOTTOM_BAYES_AIBS, aes(x = K + 0.3, y = IRR1,
                                                      col = "MCMC")) +
  geom_errorbar(data = tabIRRrestrBOTTOM_BAYES_AIBS, aes(x = K + 0.3, y = IRR1,
                                                         ymax = UCI, ymin = LCI, col = "MCMC"), width = 0) +
  scale_color_manual("", breaks = c("REML", "MCMC"),
                     labels = c("REML estimate", "MCMC estimate"), values = c("black", "blue")) +
  xlab("Number of bottom AIBS proposals") +
  ylab("IRR") +
  ylim(0, 1) +
  ggtitle("  ") +
  theme_app(base_size = 12) +
  theme(legend.position = c(0.97, 0.97), legend.justification = c(1, 1))


# Figure IRR restricted 2 bottom
g3a2 <- ggplot() +
  geom_point(data = tabIRRrestrBOTTOM_REML_AIBS, aes(x = 100*Kperc, y = IRR1, col = "REML")) +
  geom_errorbar(data = tabIRRrestrBOTTOM_REML_AIBS, aes(x = 100*Kperc, y = IRR1,
                                                        ymax = UCI, ymin = LCI, col = "REML"), width = 0) +
  geom_point(data = tabIRRrestrBOTTOM_BAYES_AIBS, aes(x = 100*Kperc + 0.5, y = IRR1,
                                                      col = "MCMC")) +
  geom_errorbar(data = tabIRRrestrBOTTOM_BAYES_AIBS, aes(x = 100*Kperc + 0.5, y = IRR1,
                                                         ymax = UCI, ymin = LCI, col = "MCMC"), width = 0) +
  scale_color_manual("", breaks = c("REML", "MCMC"),
                     labels = c("REML estimate", "MCMC estimate"), values = c("black", "blue")) +
  xlab("Percentage of bottom AIBS proposals") +
  ylab("IRR") +
  ylim(0, 1) +
  ggtitle("  ") +
  theme_app(base_size = 12) +
  theme(legend.position = c(0.97, 0.97), legend.justification = c(1, 1))


# Figure IRR restricted NIH bottom
g3b <- ggplot() +
  geom_point(data = tabIRRrestrBOTTOM_REML_NIH[2*(1:50),], aes(x = Kperc, y = IRR1,
                                                               col = "REML")) +
  geom_errorbar(data = tabIRRrestrBOTTOM_REML_NIH[2*(1:50),], aes(x = Kperc, y = IRR1,
                                                                  ymax = UCI, ymin = LCI, col = "REML"), width = 0) +
  geom_point(data = tabIRRrestrBOTTOM_BAYES_NIH[2*(1:50),], aes(x = Kperc + 0.9,
                                                                y = IRR1, col = "MCMC")) +
  geom_errorbar(data = tabIRRrestrBOTTOM_BAYES_NIH[2*(1:50),], aes(x = Kperc + 0.9, y = IRR1,
                                                                   ymax = UCI, ymin = LCI, col = "MCMC"), width = 0) +
  scale_color_manual("", breaks = c("REML", "MCMC"),
                     labels = c("REML estimate", "MCMC estimate"),
                     values = c("black", "blue")) +
  xlab("Percentage of bottom NIH proposals") +
  ylab("IRR") +
  ylim(0, 1) +
  ggtitle("  ") +
  theme_app(base_size = 12) +
  theme(legend.position = c(0.97, 0.97), legend.justification = c(1, 1))

# IRR estimates combined AIBS NIH}
(g3 <- plot_grid(g3a2, g3b))

ggsave(g3, file = "sampleRcode/IRRfiles/Figure3.png", width = 8, height = 4, dpi = 600, device = "png")
ggsave(g3, file = "sampleRcode/IRRfiles/Figure3.eps", width = 8, height = 4, dpi = 600, device = "eps")


#################################################################
#################################################################
# Issues with bias in the case of a small number of ratings
# Figure 4

# Top 55 AIBS}
round(tabIRRrestrTOP_REML_AIBS,3)[55,2:9]    # REML estimates
tau2 = tabIRRrestrTOP_REML_AIBS[55,"Proj"]   # Estimated Proposal variance
sigma2 = tabIRRrestrTOP_REML_AIBS[55,"Res"]  # Estimated Residual variance

# SSb top 55}
n = 3  # Number of ratings per proposal
J = 55 # Number of proposals in restricted sample

# Sums of squares
dataTopJ = AIBS[AIBS$ScoreRankAdj <= J,]
mean.proposals = tapply(dataTopJ$Score, dataTopJ$ID, mean)
mean.proposals = mean.proposals[!is.na(mean.proposals)] # length = J
mean.overall = mean(dataTopJ$Score)
(SSw = sum((dataTopJ$Score - dataTopJ$ScoreAvg)^2)) # Observed SSw
(SSw = sigma2*(n-1)*J)           # Expected sum of squares within proposals

(SSb = 3*sum((mean.proposals - mean.overall)^2))     # Observed SSb
(SSb = (n*tau2 + sigma2)*(J-1))  # Expected sum of squares between proposals
(SSb1 = SSb - sqrt((J-1)*2*(n*tau2+sigma2)^2)) # SSb 1SD below its expected value
#MSb <- SSb/(J-1)
#(MSw <- SSw/(J*(3-1)))  # REML estimate of varResid, OK
#(MSb - MSw)/3           # REML estimates of varAppl, OK

# Figure Likelihood}
Lik <- function(tau2, sigma2, J, n, SSw, SSb){
  #    (2*pi)^(-J*n/2) * sigma2^(-J*(n-1)/2) *   # Very small constant, removed
  (n*tau2 + sigma2)^(-J/2) *
    exp(-1/2 * (SSw/sigma2 + SSb/(n*tau2 + sigma2)))
}

tauV = seq(0, 0.3, by=0.001)   # Vector of structural proposal variances (x-axis)
L = Lik(tau2 = tauV, sigma2=sigma2 , J=J, n=n, SSw=SSw, SSb=SSb)
L1 = Lik(tau2 = tauV, sigma2=sigma2 , J=J, n=3, SSw=SSw, SSb=SSb1)

# Data to be plotted out, L1 values scaled
df <- data.frame(tauV = tauV, L = L, L1 = L1)

(g4 <- ggplot(df, aes(x = tauV)) +
    geom_line(aes(y = L, linetype = "L")) +
    geom_line(aes(y = L1/250, linetype = "L1")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    ggtitle("") +
    xlab(bquote(tau^2)) +
    ylab("Likelihood") +
    theme_app(base_size = 12) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()))

# Figure Likelihood - save
ggsave(g4, file = "sampleRcode/IRRfiles/Figure4.png", width = 8, height = 8, dpi = 600, units = "cm", device = "png")
ggsave(g4, file = "sampleRcode/IRRfiles/Figure4.eps", width = 8, height = 8, dpi = 600, units = "cm", device = "eps")

#################################################################
#################################################################
# Interactive app
# To try examples interactively, run the ShinyItemAnalysis application,
#  set the AIBS toy dataset in the Data section by clicking on the menu in the upper left corner
#  and go to the Reliability/Restricted range section.
startShinyItemAnalysis()
