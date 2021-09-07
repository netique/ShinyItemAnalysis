# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * RASCH ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(mirt)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")

# fitting Rasch model
fit <- mirt(GMAT[, 1:20], model = 1, itemtype = "Rasch", SE = TRUE)

# item characteristic curves
plot(fit, type = "trace", facet_items = FALSE)
# test score curve
plot(fit)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE) # classical intercept-slope parametrization
coef(fit) # including confidence intervals
coef(fit, IRTpars = TRUE, simplify = TRUE) # IRT parametrization
coef(fit, IRTpars = TRUE) # including confidence intervals

# item fit statistics
itemfit(fit)

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
head(fs)
fs.se <- fscores(fit, full.scores.SE = TRUE) # with SE
head(fs.se)

sts <- as.vector(scale(rowSums(GMAT[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# Wright map
b <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items[, "b"]
ggWrightMap(fs, b)

# you can also use the ltm package
library(ltm)

# fitting Rasch model
fit <- rasch(GMAT[, 1:20], constraint = cbind(ncol(GMAT[, 1:20]) + 1, 1))

# item characteristic curves
plot(fit)
# item information curves
plot(fit, type = "IIC")
# test information curve
plot(fit, items = 0, type = "IIC")

# estimated parameters
coef(fit)

# factor scores vs standardized total scores
df1 <- ltm::factor.scores(fit, return.MIvalues = TRUE)$score.dat
FS <- as.vector(df1[, "z1"])
df2 <- df1
df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
STS <- as.vector(scale(rowSums(df2[, 1:20])))
df <- data.frame(FS, STS)
plot(FS ~ STS, data = df, xlab = "Standardized total score", ylab = "Factor score")
cor(FS, STS)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * 1PL IRT ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ltm)
library(mirt)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")

# fitting 1PL model
fit <- mirt(GMAT[, 1:20],
  model = 1, itemtype = "2PL",
  constrain = list((1:20) + seq(0, (20 - 1) * 3, 3)), SE = TRUE
)

# item characteristic curves
plot(fit, type = "trace", facet_items = FALSE)
# test score curve
plot(fit)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE) # classical intercept-slope parametrization
coef(fit) # including confidence intervals
coef(fit, IRTpars = TRUE, simplify = TRUE) # IRT parametrization
coef(fit, IRTpars = TRUE) # including confidence intervals

# item fit statistics
itemfit(fit)

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(GMAT[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# Wright map
b <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items[, "b"]
ggWrightMap(fs, b)

# you can also use the ltm package
library(ltm)

# fitting 1PL model
fit <- rasch(GMAT[, 1:20])

# item characteristic curves
plot(fit)
# item information curves
plot(fit, type = "IIC")
# test information curve
plot(fit, items = 0, type = "IIC")

# estimated parameters
coef(fit)

# factor scores vs standardized total scores
df1 <- ltm::factor.scores(fit, return.MIvalues = TRUE)$score.dat
FS <- as.vector(df1[, "z1"])
df2 <- df1
df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
STS <- as.vector(scale(rowSums(df2[, 1:20])))
df <- data.frame(FS, STS)
plot(FS ~ STS, data = df, xlab = "Standardized total score", ylab = "Factor score")
cor(FS, STS)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * 2PL IRT ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ltm)
library(mirt)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")

# fitting 2PL model
fit <- mirt(GMAT[, 1:20], model = 1, itemtype = "2PL", SE = TRUE)

# item characteristic curves
plot(fit, type = "trace", facet_items = FALSE)
# test score curve
plot(fit)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE) # classical intercept-slope parametrization
coef(fit) # including confidence intervals
coef(fit, IRTpars = TRUE, simplify = TRUE) # IRT parametrization
coef(fit, IRTpars = TRUE) # including confidence intervals

# item fit statistics
itemfit(fit)

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(GMAT[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# you can also use the ltm package
library(ltm)

# fitting 2PL model
fit <- ltm(GMAT[, 1:20] ~ z1, IRT.param = TRUE)

# item characteristic curves
plot(fit)
# item information curves
plot(fit, type = "IIC")
# test information curve
plot(fit, items = 0, type = "IIC")

# estimated parameters
coef(fit)

# factor scores vs standardized total scores
df1 <- ltm::factor.scores(fit, return.MIvalues = TRUE)$score.dat
FS <- as.vector(df1[, "z1"])
df2 <- df1
df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
STS <- as.vector(scale(rowSums(df2[, 1:20])))
df <- data.frame(FS, STS)
plot(FS ~ STS, data = df, xlab = "Standardized total score", ylab = "Factor score")
cor(FS, STS)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * 3PL IRT ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ltm)
library(mirt)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")

# fitting 3PL model
fit <- mirt(GMAT[, 1:20], model = 1, itemtype = "3PL", SE = TRUE)

# item characteristic curves
plot(fit, type = "trace", facet_items = FALSE)
# test score curve
plot(fit)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE) # classical intercept-slope parametrization
coef(fit) # including confidence intervals
coef(fit, IRTpars = TRUE, simplify = TRUE) # IRT parametrization
coef(fit, IRTpars = TRUE) # including confidence intervals

# item fit statistics
itemfit(fit)

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(GMAT[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# you can also use the ltm package
library(ltm)

# fitting 3PL model
fit <- tpm(GMAT[, 1:20], IRT.param = TRUE)

# item characteristic curves
plot(fit)
# item information curves
plot(fit, type = "IIC")
# test information curve
plot(fit, items = 0, type = "IIC")

# estimated parameters
coef(fit)

# factor scores vs standardized total scores
df1 <- ltm::factor.scores(fit, return.MIvalues = TRUE)$score.dat
FS <- as.vector(df1[, "z1"])
df2 <- df1
df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
STS <- as.vector(scale(rowSums(df2[, 1:20])))
df <- data.frame(FS, STS)
plot(FS ~ STS, data = df, xlab = "Standardized total score", ylab = "Factor score")
cor(FS, STS)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * 4PL IRT ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(mirt)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")

# fitting 4PL model
fit <- mirt(GMAT[, 1:20], model = 1, itemtype = "4PL", SE = TRUE)

# item characteristic curves
plot(fit, type = "trace", facet_items = FALSE)
# test score curve
plot(fit)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE) # classical intercept-slope parametrization
coef(fit) # including confidence intervals
coef(fit, IRTpars = TRUE, simplify = TRUE) # IRT parametrization
coef(fit, IRTpars = TRUE) # including confidence intervals

# item fit statistics
itemfit(fit)

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(GMAT[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT COMPARISON ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(mirt)

# loading data
data(GMAT, package = "difNLR")

# 1PL IRT model
fit1PL <- mirt(GMAT[, 1:20],
  model = 1,
  constrain = list((1:20) + seq(0, (20 - 1) * 3, 3)), itemtype = "2PL"
)
# 2PL IRT model
fit2PL <- mirt(GMAT[, 1:20], model = 1, itemtype = "2PL")
# 3PL IRT model
fit3PL <- mirt(GMAT[, 1:20], model = 1, itemtype = "3PL")
# 4PL IRT model
fit4PL <- mirt(GMAT[, 1:20], model = 1, itemtype = "4PL")

# comparison
anova(fit1PL, fit2PL)
anova(fit2PL, fit3PL)
anova(fit3PL, fit4PL)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * BOCKS NOMINAL MODEL ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(mirt)

# loading data
data(HCItest, HCI, package = "ShinyItemAnalysis")
HCInumeric <- HCItest[, 1:20]
HCInumeric[] <- sapply(HCInumeric, as.numeric)

# model
fit <- mirt(HCInumeric, model = 1, itemtype = "nominal")

# item response curves
plot(fit, type = "trace")
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE) # classical intercept-slope parametrization
coef(fit, IRTpars = TRUE, simplify = TRUE) # IRT parametrization

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(HCI[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TRAINING - DICHOTOMOUS MODELS ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)
library(data.table)

# parameters
a1 <- 1
b1 <- 0
c1 <- 0
d1 <- 1
a2 <- 2
b2 <- 0.5
c2 <- 0
d2 <- 1

# latent ability
theta <- seq(-4, 4, 0.01)
# latent ability level
theta0 <- 0

# function for IRT characteristic curve
icc_irt <- function(theta, a, b, c, d) {
  return(c + (d - c) / (1 + exp(-a * (theta - b))))
}

# calculation of characteristic curves
df <- data.frame(theta,
  "icc1" = icc_irt(theta, a1, b1, c1, d1),
  "icc2" = icc_irt(theta, a2, b2, c2, d2)
)
df <- melt(df, id.vars = "theta")

# plot for characteristic curves
ggplot(df, aes(x = theta, y = value, color = variable)) +
  geom_line() +
  geom_segment(aes(
    y = icc_irt(theta0, a = a1, b = b1, c = c1, d = d1),
    yend = icc_irt(theta0, a = a1, b = b1, c = c1, d = d1),
    x = -4, xend = theta0
  ),
  color = "gray", linetype = "dashed"
  ) +
  geom_segment(aes(
    y = icc_irt(theta0, a = a2, b = b2, c = c2, d = d2),
    yend = icc_irt(theta0, a = a2, b = b2, c = c2, d = d2),
    x = -4, xend = theta0
  ),
  color = "gray", linetype = "dashed"
  ) +
  geom_segment(aes(
    y = 0,
    yend = max(
      icc_irt(theta0, a = a1, b = b1, c = c1, d = d1),
      icc_irt(theta0, a = a2, b = b2, c = c2, d = d2)
    ),
    x = theta0, xend = theta0
  ),
  color = "gray", linetype = "dashed"
  ) +
  xlim(-4, 4) +
  xlab("Ability") +
  ylab("Probability of correct answer") +
  theme_bw() +
  ylim(0, 1) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Item characteristic curve")

# function for IRT information function
iic_irt <- function(theta, a, b, c, d) {
  pi <- c + (d - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  return(a^2 * (pi - c)^2 * (d - pi)^2 / (pi * (1 - pi) * (d - c)^2))
}

# calculation of information curves
df <- data.frame(theta,
  "iic1" = iic_irt(theta, a1, b1, c1, d1),
  "iic2" = iic_irt(theta, a2, b2, c2, d2)
)
df <- melt(df, id.vars = "theta")

# plot for information curves
ggplot(df, aes(x = theta, y = value, color = variable)) +
  geom_line() +
  xlim(-4, 4) +
  xlab("Ability") +
  ylab("Information") +
  theme_bw() +
  ylim(0, 4) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Item information curve")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TRAINING - POLYTOMOUS MODELS - GRM ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)
library(data.table)

# setting parameters
a <- 1
b <- c(-1.5, -1, -0.5, 0)
theta <- seq(-4, 4, 0.01)

# calculating cumulative probabilities
ccirt <- function(theta, a, b) {
  return(1 / (1 + exp(-a * (theta - b))))
}
df1 <- data.frame(sapply(1:length(b), function(i) ccirt(theta, a, b[i])), theta)
df1 <- melt(df1, id.vars = "theta")

# plotting cumulative probabilities
ggplot(data = df1, aes(x = theta, y = value, col = variable)) +
  geom_line() +
  xlab("Ability") +
  ylab("Cumulative probability") +
  xlim(-4, 4) +
  ylim(0, 1) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Cumulative probabilities") +
  scale_color_manual("",
    values = c("red", "yellow", "green", "blue"),
    labels = paste0("P(Y >= ", 1:4, ")")
  )

# calculating category probabilities
df2 <- data.frame(1, sapply(
  1:length(b),
  function(i) ccirt(theta, a, b[i])
))
df2 <- data.frame(sapply(
  1:length(b),
  function(i) df2[, i] - df2[, i + 1]
), df2[, ncol(df2)], theta)
df2 <- melt(df2, id.vars = "theta")

# plotting category probabilities
ggplot(data = df2, aes(x = theta, y = value, col = variable)) +
  geom_line() +
  xlab("Ability") +
  ylab("Category probability") +
  xlim(-4, 4) +
  ylim(0, 1) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Category probabilities") +
  scale_color_manual("",
    values = c("black", "red", "yellow", "green", "blue"),
    labels = paste0("P(Y >= ", 0:4, ")")
  )

# calculating expected item score
df3 <- data.frame(1, sapply(
  1:length(b),
  function(i) ccirt(theta, a, b[i])
))
df3 <- data.frame(sapply(
  1:length(b),
  function(i) df3[, i] - df3[, i + 1]
), df3[, ncol(df3)])
df3 <- data.frame(exp = as.matrix(df3) %*% 0:4, theta)

# plotting category probabilities
ggplot(data = df3, aes(x = theta, y = exp)) +
  geom_line() +
  xlab("Ability") +
  ylab("Expected item score") +
  xlim(-4, 4) +
  ylim(0, 4) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Expected item score")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TRAINING - POLYTOMOUS MODELS - GPCM ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)
library(data.table)

# setting parameters
a <- 1
d <- c(-1.5, -1, -0.5, 0)
theta <- seq(-4, 4, 0.01)

# calculating category probabilities
ccgpcm <- function(theta, a, d) {
  a * (theta - d)
}
df <- sapply(1:length(d), function(i) ccgpcm(theta, a, d[i]))
pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))
pk <- cbind(0, pk)
pk <- exp(pk)
denom <- apply(pk, 1, sum)
df <- apply(pk, 2, function(x) x / denom)
df1 <- melt(data.frame(df, theta), id.vars = "theta")

# plotting category probabilities
ggplot(data = df1, aes(x = theta, y = value, col = variable)) +
  geom_line() +
  xlab("Ability") +
  ylab("Category probability") +
  xlim(-4, 4) +
  ylim(0, 1) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Category probabilities") +
  scale_color_manual("",
    values = c("black", "red", "yellow", "green", "blue"),
    labels = paste0("P(Y = ", 0:4, ")")
  )

# calculating expected item score
df2 <- data.frame(exp = as.matrix(df) %*% 0:4, theta)
# plotting expected item score
ggplot(data = df2, aes(x = theta, y = exp)) +
  geom_line() +
  xlab("Ability") +
  ylab("Expected item score") +
  xlim(-4, 4) +
  ylim(0, 4) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Expected item score")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TRAINING - POLYTOMOUS MODELS - NRM ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)
library(data.table)

# setting parameters
a <- c(2.5, 2, 1, 1.5)
d <- c(-1.5, -1, -0.5, 0)
theta <- seq(-4, 4, 0.01)

# calculating category probabilities
ccnrm <- function(theta, a, d) {
  exp(d + a * theta)
}
df <- sapply(1:length(d), function(i) ccnrm(theta, a[i], d[i]))
df <- data.frame(1, df)
denom <- apply(df, 1, sum)
df <- apply(df, 2, function(x) x / denom)
df1 <- melt(data.frame(df, theta), id.vars = "theta")

# plotting category probabilities
ggplot(data = df1, aes(x = theta, y = value, col = variable)) +
  geom_line() +
  xlab("Ability") +
  ylab("Category probability") +
  xlim(-4, 4) +
  ylim(0, 1) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Category probabilities") +
  scale_color_manual("",
    values = c("black", "red", "yellow", "green", "blue"),
    labels = paste0("P(Y = ", 0:4, ")")
  )

# calculating expected item score
df2 <- data.frame(exp = as.matrix(df) %*% 0:4, theta)

# plotting expected item score
ggplot(data = df2, aes(x = theta, y = exp)) +
  geom_line() +
  xlab("Ability") +
  ylab("Expected item score") +
  xlim(-4, 4) +
  ylim(0, 4) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Expected item score")

