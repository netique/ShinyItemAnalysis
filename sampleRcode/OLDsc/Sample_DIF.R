# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TOTAL SCORES ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)
library(moments)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# total score calculation wrt group
score <- rowSums(data)
score0 <- score[group == 0] # reference group
score1 <- score[group == 1] # focal group

# Summary of total score
rbind(
  c(
    length(score0), min(score0), max(score0), mean(score0), median(score0),
    sd(score0), skewness(score0), kurtosis(score0)
  ),
  c(
    length(score1), min(score1), max(score1), mean(score1), median(score1),
    sd(score1), skewness(score1), kurtosis(score1)
  )
)

df <- data.frame(score, group = as.factor(group))

# histogram of total scores wrt group
ggplot(data = df, aes(x = score, fill = group, col = group)) +
  geom_histogram(binwidth = 1, position = "dodge2", alpha = 0.75) +
  xlab("Total score") +
  ylab("Number of respondents") +
  scale_fill_manual(
    values = c("dodgerblue2", "goldenrod2"), labels = c("Reference", "Focal")
  ) +
  scale_colour_manual(
    values = c("dodgerblue2", "goldenrod2"), labels = c("Reference", "Focal")
  ) +
  theme_app() +
  theme(legend.position = "left")

# t-test to compare total scores
t.test(score0, score1)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DELTA PLOT ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(deltaplotR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, -22]

# delta scores with fixed threshold
(DS_fixed <- deltaplot(
  data = data, group = "group", focal.name = 1,
  thr = 1.5, purify = FALSE
))
# delta plot
diagplot(DS_fixed, thr.draw = TRUE)

# delta scores with normal threshold
(DS_normal <- deltaplot(
  data = data, group = "group", focal.name = 1,
  thr = "norm", purify = FALSE
))
# delta plot
diagplot(DS_normal, thr.draw = TRUE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MANTEL-HAENSZEL ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# Mantel-Haenszel test
(fit <- difMH(
  Data = data, group = group, focal.name = 1, match = "score",
  p.adjust.method = "none", purify = FALSE
))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** MANTEL-HAENSZEL - items ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# contingency table for item 1 and score 12
item <- 1
cut <- 12

df <- data.frame(data[, item], group)
colnames(df) <- c("Answer", "Group")
df$Answer <- relevel(factor(df$Answer, labels = c("Incorrect", "Correct")), "Correct")
df$Group <- factor(df$Group, labels = c("Reference Group", "Focal Group"))
score <- rowSums(data) # total score calculation
df <- df[score == 12, ] # responses of those with total score of 12
xtabs(~ Group + Answer, data = df)

# Mantel-Haenszel estimate of OR
(fit <- difMH(
  Data = data, group = group, focal.name = 1, match = "score",
  p.adjust.method = "none", purify = FALSE
))
fit$alphaMH

# D-DIF index calculation
-2.35 * log(fit$alphaMH)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC REGRESSION ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# logistic regression DIF detection method
(fit <- difLogistic(
  Data = data, group = group, focal.name = 1, match = "score",
  type = "both", p.adjust.method = "none", purify = FALSE
))

# loading data
data(LearningToLearn, package = "ShinyItemAnalysis")
data <- LearningToLearn[, 87:94] # item responses from Grade 9 from subscale 6
group <- LearningToLearn$track # school track - group membership variable
match <- scale(LearningToLearn$score_6) # standardized test score from Grade 6

# detecting differential item functioning in change (DIF-C) using
# the logistic regression DIF detection method
# and standardized total score from Grade 6 as the matching criterion
(fit <- difLogistic(
  Data = data, group = group, focal.name = "AS", match = match,
  type = "both", p.adjust.method = "none", purify = FALSE
))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** LOGISTIC REGRESSION - items ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difR)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# logistic regression DIF detection method
(fit <- difLogistic(
  Data = data, group = group, focal.name = 1, match = "score",
  type = "both", p.adjust.method = "none", purify = FALSE
))

# plot of characteristic curve for item 1
plotDIFLogistic(fit, item = 1, Data = data, group = group)

# estimated coefficients for item 1
fit$logitPar[1, ]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * GENERALIZED LOGISTIC ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difNLR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# generalized logistic regression DIF method
# using 3PL model with the same guessing parameter for both groups
(fit <- difNLR(
  Data = data, group = group, focal.name = 1, model = "3PLcg",
  match = "zscore", type = "all", p.adjust.method = "none", purify = FALSE
))

# loading data
data(LearningToLearn, package = "ShinyItemAnalysis")
data <- LearningToLearn[, 87:94] # item responses from Grade 9 from subscale 6
group <- LearningToLearn$track # school track - group membership variable
match <- scale(LearningToLearn$score_6) # standardized test score from Grade 6

# detecting differential item functioning in change (DIF-C) using
# the generalized logistic regression DIF method with 3PL model
# with the same guessing parameter for both groups
# and standardized total score from Grade 6 as the matching criterion
(fit <- difNLR(
  Data = data, group = group, focal.name = "AS", model = "3PLcg",
  match = match, type = "all", p.adjust.method = "none", purify = FALSE
))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** GENERALIZED LOGISTIC - items ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difNLR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# generalized logistic regression DIF method
# using 3PL model with the same guessing parameter for both groups
(fit <- difNLR(
  Data = data, group = group, focal.name = 1, model = "3PLcg",
  match = "zscore", type = "all", p.adjust.method = "none", purify = FALSE
))

# plot of characteristic curve of item 1
plot(fit, item = 1)

# estimated coefficients for item 1 with SE
coef(fit, SE = TRUE)[[1]]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT LORD ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difR)
library(ltm)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# 1PL IRT model
(fit1PL <- difLord(
  Data = data, group = group, focal.name = 1, model = "1PL",
  p.adjust.method = "none", purify = FALSE
))

# 2PL IRT model
(fit2PL <- difLord(
  Data = data, group = group, focal.name = 1, model = "2PL",
  p.adjust.method = "none", purify = FALSE
))

# 3PL IRT model with the same guessing for groups
guess <- itemParEst(data, model = "3PL")[, 3]
(fit3PL <- difLord(
  Data = data, group = group, focal.name = 1, model = "3PL",
  c = guess, p.adjust.method = "none", purify = FALSE
))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** IRT LORD - items ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difR)
library(ltm)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# 1PL IRT model
(fit1PL <- difLord(
  Data = data, group = group, focal.name = 1, model = "1PL",
  p.adjust.method = "none", purify = FALSE
))
# estimated coefficients for all items
(coef1PL <- fit1PL$itemParInit)
# plot of characteristic curve of item 1
plotDIFirt(parameters = coef1PL, item = 1, test = "Lord")

# 2PL IRT model
(fit2PL <- difLord(
  Data = data, group = group, focal.name = 1, model = "2PL",
  p.adjust.method = "none", purify = FALSE
))
# estimated coefficients for all items
(coef2PL <- fit2PL$itemParInit)
# plot of characteristic curve of item 1
plotDIFirt(parameters = coef2PL, item = 1, test = "Lord")

# 3PL IRT model with the same guessing for groups
guess <- itemParEst(data, model = "3PL")[, 3]
(fit3PL <- difLord(
  Data = data, group = group, focal.name = 1, model = "3PL",
  c = guess, p.adjust.method = "none", purify = FALSE
))
# estimated coefficients for all items
(coef3PL <- fit3PL$itemParInit)
# plot of characteristic curve of item 1
plotDIFirt(parameters = coef3PL, item = 1, test = "Lord")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT RAJU ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difR)
library(ltm)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# 1PL IRT model
(fit1PL <- difRaju(
  Data = data, group = group, focal.name = 1, model = "1PL",
  p.adjust.method = "none", purify = FALSE
))

# 2PL IRT model
(fit2PL <- difRaju(
  Data = data, group = group, focal.name = 1, model = "2PL",
  p.adjust.method = "none", purify = FALSE
))

# 3PL IRT model with the same guessing for groups
guess <- itemParEst(Data, model = "3PL")[, 3]
(fit3PL <- difRaju(
  Data = data, group = group, focal.name = 1, model = "3PL",
  c = guess, p.adjust.method = "none", purify = FALSE
))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** IRT RAJU - items ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difR)
library(ltm)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# 1PL IRT model
(fit1PL <- difRaju(
  Data = data, group = group, focal.name = 1, model = "1PL",
  p.adjust.method = "none", purify = FALSE
))
# estimated coefficients for all items
(coef1PL <- fit1PL$itemParInit)
# plot of characteristic curve of item 1
plotDIFirt(parameters = coef1PL, item = 1, test = "Raju")

# 2PL IRT model
(fit2PL <- difRaju(
  Data = data, group = group, focal.name = 1, model = "2PL",
  p.adjust.method = "none", purify = FALSE
))
# estimated coefficients for all items
(coef2PL <- fit2PL$itemParInit)
# plot of characteristic curve of item 1
plotDIFirt(parameters = coef2PL, item = 1, test = "Raju")

# 3PL IRT model with the same guessing for groups
guess <- itemParEst(data, model = "3PL")[, 3]
(fit3PL <- difRaju(
  Data = data, group = group, focal.name = 1, model = "3PL",
  c = guess, p.adjust.method = "none", purify = FALSE
))
# estimated coefficients for all items
(coef3PL <- fit3PL$itemParInit)
# plot of characteristic curve of item 1
plotDIFirt(parameters = coef3PL, item = 1, test = "Raju")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SIBTEST ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# SIBTEST (uniform DIF)
(fit_udif <- difSIBTEST(
  Data = data, group = group, focal.name = 1, type = "udif",
  p.adjust.method = "none", purify = FALSE
))

# Crossing-SIBTEST (non-uniform DIF)
(fit_nudif <- difSIBTEST(
  Data = data, group = group, focal.name = 1, type = "nudif",
  p.adjust.method = "none", purify = FALSE
))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CUMULATIVE LOGIT ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difNLR)

# loading data
data(dataMedicalgraded, package = "ShinyItemAnalysis")
data <- dataMedicalgraded[, 1:100]
group <- dataMedicalgraded[, 101]

# DIF with cumulative logit regression model
(fit <- difORD(
  Data = data, group = group, focal.name = 1, model = "cumulative",
  type = "both", match = "zscore", p.adjust.method = "none", purify = FALSE,
  parametrization = "classic"
))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** CUMULATIVE LOGIT - items ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difNLR)

# loading data
data(dataMedicalgraded, package = "ShinyItemAnalysis")
data <- dataMedicalgraded[, 1:100]
group <- dataMedicalgraded[, 101]

# DIF with cumulative logit regression model
(fit <- difORD(
  Data = data, group = group, focal.name = 1, model = "cumulative",
  type = "both", match = "zscore", p.adjust.method = "none", purify = FALSE,
  parametrization = "classic"
))

# plot of cumulative probabilities for item X2003
plot(fit, item = "X2003", plot.type = "cumulative")

# plot of category probabilities for item X2003
plot(fit, item = "X2003", plot.type = "category")

# estimated coefficients for all items with SE
coef(fit, SE = TRUE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ADJACENT CATEGORY LOGIT ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difNLR)

# loading data
data(dataMedicalgraded, package = "ShinyItemAnalysis")
data <- dataMedicalgraded[, 1:100]
group <- dataMedicalgraded[, 101]

# DIF with cumulative logit regression model
(fit <- difORD(
  Data = data, group = group, focal.name = 1, model = "adjacent",
  type = "both", match = "zscore", p.adjust.method = "none", purify = FALSE,
  parametrization = "classic"
))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ADJACENT CATEGORY LOGIT - items ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difNLR)

# loading data
data(dataMedicalgraded, package = "ShinyItemAnalysis")
data <- dataMedicalgraded[, 1:100]
group <- dataMedicalgraded[, 101]

# DIF with cumulative logit regression model
(fit <- difORD(
  Data = data, group = group, focal.name = 1, model = "cumulative",
  type = "both", match = "zscore", p.adjust.method = "none", purify = FALSE,
  parametrization = "classic"
))

# plot of characteristic curves for item X2003
plot(fit, item = "X2003")

# estimated coefficients for all items with SE
coef(fit, SE = TRUE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MULTINOMIAL ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difNLR)

# loading data
data(GMATtest, GMATkey, package = "difNLR")
data <- GMATtest[, 1:20]
group <- GMATtest[, "group"]
key <- GMATkey

# DDF with multinomial regression model
(fit <- ddfMLR(
  Data = data, group = group, focal.name = 1, key,
  type = "both", match = "zscore",
  p.adjust.method = "none", purify = FALSE,
  parametrization = "classic"
))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** MULTINOMIAL - items ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(difNLR)

# loading data
data(GMATtest, GMATkey, package = "difNLR")
data <- GMATtest[, 1:20]
group <- GMATtest[, "group"]
key <- GMATkey

# DDF with multinomial  regression model
(fit <- ddfMLR(
  Data = data, group = group, focal.name = 1, key,
  type = "both", match = "zscore",
  p.adjust.method = "none", purify = FALSE,
  parametrization = "classic"
))

# plot of characteristic curves for item 1
plot(fit, item = 1)

# estimated coefficients for all items with SE
coef(fit, SE = TRUE)
