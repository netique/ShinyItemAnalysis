# R code to create figures in DIF/Fairness section in About DIF and DDF tab

# ----------------------
# Packages
# ----------------------

library(ggplot2)
library(difNLR)
library(svglite)
library(here)

# ----------------------
# Data
# ----------------------

data(GMAT, GMATtest, GMATkey)

# ----------------------
# DIF
# ----------------------

(DIF <- difNLR(GMAT[, 1:20], GMAT$group, focal.name = 1, model = "2PL"))

# uniform
plot(DIF, item = "Item1")[[1]] +
  ggtitle("") +
  xlab("Ability") +
  theme(text = element_text(size = 15))

ggsave(
  here("inst/shiny-examples/ShinyItemAnalysis/www/fig_DIF_uniform.svg"),
  height = 5.5, width = 7
)

# non-uniform
plot(DIF, item = "Item2")[[1]] +
  ggtitle("") +
  xlab("Ability") +
  theme(text = element_text(size = 15))

ggsave(
  here("inst/shiny-examples/ShinyItemAnalysis/www/fig_DIF_nonuniform.svg"),
  height = 5.5, width = 7
)

# ----------------------
# DDF
# ----------------------

Data <- GMATtest[, 1:20]
group <- GMATtest[, "group"]
key <- GMATkey

(DDF <- ddfMLR(Data, group, focal.name = 1, key))

# uniform
plot(DDF, item = "Item1")[[1]] +
  ggtitle("") +
  xlab("Ability") +
  theme(text = element_text(size = 15))

ggsave(
  here("inst/shiny-examples/ShinyItemAnalysis/www/fig_DDF_uniform.svg"),
  height = 5.5, width = 7
)

# non-uniform
plot(DDF, item = "Item2")[[1]] +
  ggtitle("") +
  xlab("Ability") +
  theme(text = element_text(size = 15))

ggsave(
  here("inst/shiny-examples/ShinyItemAnalysis/www/fig_DDF_nonuniform.svg"),
  height = 5.5, width = 7
)
