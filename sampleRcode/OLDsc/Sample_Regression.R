# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
score <- rowSums(data) # total score

# logistic model for item 1
fit <- glm(data[, 1] ~ score, family = binomial)

# coefficients
coef(fit) # estimates
sqrt(diag(vcov(fit))) # SE
summary(fit)$coefficients[, 1:2] # estimates and SE

# function for plot
fun <- function(x, b0, b1) {
  exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
}

# empirical probabilities calculation
df <- data.frame(
  x = sort(unique(score)),
  y = tapply(data[, 1], score, mean),
  size = as.numeric(table(score))
)

# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = size),
    color = "darkblue",
    fill = "darkblue",
    shape = 21, alpha = 0.5
  ) +
  stat_function(
    fun = fun, geom = "line",
    args = list(
      b0 = coef(fit)[1],
      b1 = coef(fit)[2]
    ),
    size = 1,
    color = "darkblue"
  ) +
  xlab("Total score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 1") +
  theme_app()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC Z ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
zscore <- scale(rowSums(data)) # standardized total score

# logistic model for item 1
fit <- glm(data[, 1] ~ zscore, family = binomial)

# coefficients
coef(fit) # estimates
sqrt(diag(vcov(fit))) # SE
summary(fit)$coefficients[, 1:2] # estimates and SE

# function for plot
fun <- function(x, b0, b1) {
  exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
}

# empirical probabilities calculation
df <- data.frame(
  x = sort(unique(zscore)),
  y = tapply(data[, 1], zscore, mean),
  size = as.numeric(table(zscore))
)

# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = size),
    color = "darkblue",
    fill = "darkblue",
    shape = 21, alpha = 0.5
  ) +
  stat_function(
    fun = fun, geom = "line",
    args = list(
      b0 = coef(fit)[1],
      b1 = coef(fit)[2]
    ),
    size = 1,
    color = "darkblue"
  ) +
  xlab("Standardized total score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 1") +
  theme_app()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC IRT Z ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)
library(msm)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
zscore <- scale(rowSums(data)) # standardized total score

# logistic model for item 1
fit <- glm(data[, 1] ~ zscore, family = binomial)

# coefficients
(coef <- c(a = coef(fit)[2], b = -coef(fit)[1] / coef(fit)[2])) # estimates
# SE using delta method
(se <- deltamethod(
  list(~x2, ~ -x1 / x2),
  mean = coef(fit),
  cov = vcov(fit),
  ses = TRUE
))
cbind(coef, se) # estimates and SE

# function for plot
fun <- function(x, a, b) {
  exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

# empirical probabilities calculation
df <- data.frame(
  x = sort(unique(zscore)),
  y = tapply(data[, 1], zscore, mean),
  size = as.numeric(table(zscore))
)

# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = size),
    color = "darkblue",
    fill = "darkblue",
    shape = 21, alpha = 0.5
  ) +
  stat_function(
    fun = fun, geom = "line",
    args = list(
      a = coef[1],
      b = coef[2]
    ),
    size = 1,
    color = "darkblue"
  ) +
  xlab("Standardized total score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 1") +
  theme_app()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NONLINEAR 3P IRT Z #####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difNLR)
library(ggplot2)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
zscore <- scale(rowSums(data)) # standardized total score

# NLR 3P model for item 1
fun <- function(x, a, b, c) {
  c + (1 - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

fit <- nls(data[, 1] ~ fun(zscore, a, b, c),
  algorithm = "port",
  start = startNLR(
    data, GMAT[, "group"],
    model = "3PLcg",
    parameterization = "classic"
  )[[1]][1:3],
  lower = c(-Inf, -Inf, 0),
  upper = c(Inf, Inf, 1)
)
# coefficients
coef(fit) # estimates
sqrt(diag(vcov(fit))) # SE
summary(fit)$coefficients[, 1:2] # estimates and SE

# empirical probabilities calculation
df <- data.frame(
  x = sort(unique(zscore)),
  y = tapply(data[, 1], zscore, mean),
  size = as.numeric(table(zscore))
)

# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = size),
    color = "darkblue",
    fill = "darkblue",
    shape = 21, alpha = 0.5
  ) +
  stat_function(
    fun = fun, geom = "line",
    args = list(
      a = coef(fit)[1],
      b = coef(fit)[2],
      c = coef(fit)[3]
    ),
    size = 1,
    color = "darkblue"
  ) +
  xlab("Standardized total score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 1") +
  theme_app()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NONLINEAR 4P IRT Z ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difNLR)
library(ggplot2)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
zscore <- scale(rowSums(data)) # standardized total score

# NLR 4P model for item 1
fun <- function(x, a, b, c, d) {
  c + (d - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

fit <- nls(data[, 1] ~ fun(zscore, a, b, c, d),
  algorithm = "port",
  start = startNLR(
    data, GMAT[, "group"],
    model = "4PLcgdg",
    parameterization = "classic"
  )[[1]][1:4],
  lower = c(-Inf, -Inf, 0, 0),
  upper = c(Inf, Inf, 1, 1)
)

# coefficients
coef(fit) # estimates
sqrt(diag(vcov(fit))) # SE
summary(fit)$coefficients[, 1:2] # estimates and SE

# empirical probabilities calculation
df <- data.frame(
  x = sort(unique(zscore)),
  y = tapply(data[, 1], zscore, mean),
  size = as.numeric(table(zscore))
)

# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = size),
    color = "darkblue",
    fill = "darkblue",
    shape = 21, alpha = 0.5
  ) +
  stat_function(
    fun = fun, geom = "line",
    args = list(
      a = coef(fit)[1],
      b = coef(fit)[2],
      c = coef(fit)[3],
      d = coef(fit)[4]
    ),
    size = 1,
    color = "darkblue"
  ) +
  xlab("Standardized total score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 1") +
  theme_app()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL COMPARISON ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difNLR)

# loading data
data(GMAT, package = "difNLR")
Data <- GMAT[, 1:20]
zscore <- scale(rowSums(Data)) # standardized total score

# function for fitting models
fun <- function(x, a, b, c, d) {
  c + (d - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

# starting values for item 1
start <- startNLR(
  Data, GMAT[, "group"], model = "4PLcgdg",
  parameterization = "classic"
)[[1]][, 1:4]

# 2PL model for item 1
fit2PL <- nls(Data[, 1] ~ fun(zscore, a, b, c = 0, d = 1),
  algorithm = "port",
  start = start[1:2]
)
# NLR 3P model for item 1
fit3PL <- nls(Data[, 1] ~ fun(zscore, a, b, c, d = 1),
  algorithm = "port",
  start = start[1:3],
  lower = c(-Inf, -Inf, 0),
  upper = c(Inf, Inf, 1)
)
# NLR 4P model for item 1
fit4PL <- nls(Data[, 1] ~ fun(zscore, a, b, c, d),
  algorithm = "port",
  start = start,
  lower = c(-Inf, -Inf, 0, 0),
  upper = c(Inf, Inf, 1, 1)
)

# comparison
### AIC
AIC(fit2PL)
AIC(fit3PL)
AIC(fit4PL)
### BIC
BIC(fit2PL)
BIC(fit3PL)
BIC(fit4PL)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CUMULATIVE LOGIT ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(msm)
library(ShinyItemAnalysis)
library(VGAM)

# loading data
data(Science, package = "mirt")

# standardized total score calculation
zscore <- scale(rowSums(Science))
Science[, 1] <- factor(
  Science[, 1], levels = sort(unique(Science[, 1])), ordered = TRUE
)

# cumulative logit model for item 1
fit <- vglm(Science[, 1] ~ zscore,
            family = cumulative(reverse = TRUE, parallel = TRUE))

# coefficients under intercept/slope parametrization
coef(fit) # estimates
sqrt(diag(vcov(fit))) # SE

# IRT parametrization
# delta method
num_par <- length(coef(fit))
formula <- append(
  paste0("~ x", num_par),
  as.list(paste0("~ -x", 1:(num_par - 1), "/", "x", num_par))
)
formula <- lapply(formula, as.formula)
se <- deltamethod(
  formula,
  mean = coef(fit),
  cov = vcov(fit),
  ses = TRUE
)
# estimates and SE in IRT parametrization
cbind(c(coef(fit)[num_par], -coef(fit)[-num_par] / coef(fit)[num_par]), se)

# plot of estimated cumulative probabilities
plotCumulative(fit, type = "cumulative", matching.name = "Standardized total  score")
# plot of estimated category probabilities
plotCumulative(fit, type = "category", matching.name = "Standardized total score")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ADJACENT CATEGORY LOGIT ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(msm)
library(ShinyItemAnalysis)
library(VGAM)

# loading data
data(Science, package = "mirt")

# standardized total score calculation
zscore <- scale(rowSums(Science))
Science[, 1] <- factor(
  Science[, 1], levels = sort(unique(Science[, 1])), ordered = TRUE
)

# adjacent category logit model for item 1
fit <- vglm(Science[, 1] ~ zscore,
            family = acat(reverse = FALSE, parallel = TRUE))

# coefficients under intercept/slope parametrization
coef(fit) # estimates
sqrt(diag(vcov(fit))) # SE

# IRT parametrization
# delta method
num_par <- length(coef(fit))
formula <- append(
  paste0("~ x", num_par),
  as.list(paste0("~ -x", 1:(num_par - 1), "/", "x", num_par))
)
formula <- lapply(formula, as.formula)
se <- deltamethod(
  formula,
  mean = coef(fit),
  cov = vcov(fit),
  ses = TRUE
)
# estimates and SE in IRT parametrization
cbind(c(coef(fit)[num_par], -coef(fit)[-num_par] / coef(fit)[num_par]), se)

# plot of estimated category probabilities
plotAdjacent(fit, matching.name = "Standardized total score")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MULTINOMIAL ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(msm)
library(nnet)
library(ShinyItemAnalysis)

# loading data
data(GMAT, GMATtest, GMATkey, package = "difNLR")

# standardized total score calculation
zscore <- scale(rowSums(GMAT[, 1:20]))

# multinomial model for item 1
fit <- multinom(relevel(GMATtest[, 1], ref = paste(GMATkey[1])) ~ zscore)

# coefficients under intercept/slope parametrization
coef(fit) # estimates
sqrt(diag(vcov(fit))) # SE

# IRT parametrization
# delta method
subst_vcov <- function(vcov, cat) {
  ind <- grep(cat, colnames(vcov))
  vcov[ind, ind]
}
se <- t(sapply(
  rownames(coef(fit)),
  function(.x) {
    vcov_subset <- subst_vcov(vcov(fit), .x)
    msm::deltamethod(
      list(~ -x1 / x2, ~x2),
      mean = coef(fit)[.x, ],
      cov = vcov_subset,
      ses = TRUE
    )
  }
))

# estimates and SE in IRT parametrization
cbind(-coef(fit)[, 1] / coef(fit)[, 2], se[, 1], coef(fit)[, 2], se[, 2])

# plot of estimated category probabilities
plotMultinomial(fit, zscore, matching.name = "Standardized total score")

