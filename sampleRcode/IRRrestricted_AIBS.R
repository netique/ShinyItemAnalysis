library(ggplot2) # for graphics
library(ShinyItemAnalysis) # for graphics
library(lme4) # for mixed effect models
library(psych) # for ICC

#########################
# AIBS data preparation #
#########################
getwd()
load(file = "data/dataAIBS.rda")

#################
# Long format:
names(dataAIBS)
dataA <- dataAIBS[, c("ID", "ScoreA", "Score_avg", "Score_rank", "Score_rank2")]
dataB <- dataAIBS[, c("ID", "ScoreB", "Score_avg", "Score_rank", "Score_rank2")]
dataC <- dataAIBS[, c("ID", "ScoreC", "Score_avg", "Score_rank", "Score_rank2")]
colnames(dataA) <- colnames(dataB) <- colnames(dataC) <- c(
  "ID", "Score",
  "Score_avg", "Score_rank",
  "Score_rank2"
)
dataL <- rbind(dataA, dataB, dataC)
head(dataL, n = 3)


# JN approach -------------------------------------------------------------


select <- dplyr::select

d_long <- dataAIBS %>%
  select(ID, ScoreA, ScoreB, ScoreC, Score_rank, Score_rank2) %>%
  # rowwise() %>%
  # mutate(avg_score = mean(c_across(c(ScoreA, ScoreB, ScoreC)))) %>% # compute avg
  pivot_longer(c(ScoreA, ScoreB, ScoreC), names_to = "reviewer", values_to = "score")


# compute the mean rating directly while plotting
d_long %>% ggplot(aes(Score_rank2, score, group = ID)) +
  geom_line(col = "gray") +
  geom_point(shape = 1, size = 1.5, position = position_jitter(width = .2, height = 0)) +
  # a bit of horizontal jitter to show occluded reviewers
  stat_summary(fun = mean, fun.args = list(na.rm = TRUE), col = "red", shape = 5) +
  coord_cartesian(ylim = c(1, 5)) +
  theme_app()
# -------------------------------------------------------------------------



################
# Data summary #
################

summary(dataL)
hist(dataL$Score, main = "Histogram of Score", xlab = "Score")

####################
# Caterpillar plot #
####################

# Data for plotting
df <- data.frame(
  y = dataL$Score, g = dataL$ID,
  my = dataL$Score_avg, rank = dataL$Score_rank2
)
dfmy <- data.frame(my = dataAIBS$Score_avg, rank = dataAIBS$Score_rank2)

# Caterpillar plot - application rank
(g1a <- ggplot() +
  geom_point(data = df, aes(x = rank, y = y, group = g), shape = 1, size = 1.5) +
  geom_line(data = df, aes(x = rank, y = y, group = g), col = "gray") +
  geom_point(
    data = dfmy, aes(x = rank, y = my), col = "red",
    shape = 5, size = 1.5
  ) +
  ylab("Overall scientific merit") +
  xlab("AIBS application rank") +
  ylim(1, 5) +
  xlim(0, length(unique(df$g))) +
  theme_app(base_size = 12))

# TODO: prepare interactive visualization in SIA/Reliability/Restricted-range IRR
#       highlight proportion of top/bottom count/percentage (color the rest in grey)

################################
# IRR Estimation - full sample #
################################

model1 <- lmer(Score ~ 1 + (1 | ID), data = dataL[, c("ID", "Score")])
summary(model1)
as.data.frame(VarCorr(model1))
as.numeric(VarCorr(model1)) # Proposal (true score) variance
sigma(model1)^2 # Residual variance
as.numeric(VarCorr(model1)) / (as.numeric(VarCorr(model1)) + sigma(model1)^2) # IRR

##########################
# Bootstrapped CI of IRR
set.seed(1357)
N <- 1000 # takes less than a minute
model <- model1
bootstrap <- bootMer(
  model,
  function(mm) c(as.numeric(VarCorr(mm)), sigma(mm)^2), N
)
# Saves data frame with N rows and 2 columns (each for one var component)
bootTab <- bootstrap$t
colnames(bootTab) <- c(names(VarCorr(model)), "Residual")
head(bootTab, n = 3)
bIRR <- bootTab[, "ID"] / apply(bootTab[, ], 1, sum) # Vector of bootstrapped IRR
(IRRLCI <- quantile(bIRR, 0.025)) # Lower bound of 95% bootstrap confidence interval
(IRRUCI <- quantile(bIRR, 0.975)) # Upper bound of 95% bootstrap confidence interval

##########################
# Multiple-rater IRR
as.numeric(VarCorr(model1)) / (as.numeric(VarCorr(model1)) + sigma(model1)^2 / 3)

bIRR3 <- bootTab[, "ID"] / (bootTab[, "ID"] + bootTab[, "Residual"] / 3)
(IRR3LCI <- quantile(bIRR3, 0.025)) # Lower bound of 95% bootstrap confidence interval
(IRR3UCI <- quantile(bIRR3, 0.975)) # Upper bound of 95% bootstrap confidence interval

########################
# ICC in more models at once with psych:
psych::ICC(dataAIBS[, c("ScoreA", "ScoreB", "ScoreC")])
# ? How is CI calculated in psych, not exactly matching...

#############################
# IRR on restricted samples #
#############################
# TODO: prepare function IRRrestricted() for IRR estimate for selected proportion of proposals/respondents
# arguments: proportion (0-1, also can be a vector), or number (0 - #of rows)
#            direction: top/bottom
#            rank: calculated from mean score / inserted,
#            CI: should CI be calculated? TRUE/FALSE, N = number of bootstrapped samples
# TODO: shorten the code below by using the newly prepared function

# REML on top restricted samples
# Initialization of matrix with results
tabIRRrestr <- matrix(NA, ncol = 11, nrow = 72)
colnames(tabIRRrestr) <- c(
  "K", "Kperc", "Proj", "Res", "Total",
  "IRR1", "LCI", "UCI", "IRR3", "LCI3", "UCI3"
)

for (K in 2:72)
{
  # Fit model on restricted data
  model <- lmer(Score ~ 1 + (1 | ID), data = dataL[dataL$Score_rank2 <= K, ])

  # Decompose variance
  varAppl <- as.numeric(VarCorr(model)["ID"])
  varResid <- sigma(model)^2
  varTotal <- sum(varAppl, varResid, na.rm = T)

  # Calculate IRR
  (IRR <- varAppl / varTotal)

  # Calculate multiple-reviewer IRR (3 raters)
  (IRR3 <- varAppl / (varAppl + varResid / 3))

  # Calculate bootstrapped confidence interval
  set.seed(1357)
  N <- 10 # 00
  bootstrap <- bootMer(
    model,
    function(mm) c(as.numeric(VarCorr(mm)), sigma(mm)^2), N
  )
  # Saves data frame with N rows and 2 columns (each for one var component)
  bootTab <- bootstrap$t
  colnames(bootTab) <- c(names(VarCorr(model)), "Residual")
  head(bootTab, n = 3)

  bIRR <- bootTab[, "ID"] / apply(bootTab[, ], 1, sum) # Single-rater IRR
  (IRRLCI <- quantile(bIRR, 0.025)) # Lower bound of bootstrapped confidence interval
  (IRRUCI <- quantile(bIRR, 0.975)) # Upper bound of bootstrapped confidence interval

  bIRR3 <- bootTab[, "ID"] / (bootTab[, "ID"] + bootTab[, "Residual"] / 3) # Multiple-rater IRR
  (IRR3LCI <- quantile(bIRR3, 0.025)) # Lower bound of bootstrapped confidence interval
  (IRR3UCI <- quantile(bIRR3, 0.975)) # Upper bound of bootstrapped confidence interval

  # save
  tabIRRrestr[K, 1] <- K # Number of top proposals considered
  tabIRRrestr[K, 2] <- K / 72 # Ratio of top proposals considered
  tabIRRrestr[K, 3] <- varAppl # Application variance
  tabIRRrestr[K, 4] <- varResid # Residual variance
  tabIRRrestr[K, 5] <- varTotal # Total variance
  tabIRRrestr[K, 6] <- IRR # REML estimate of single-rater IRR
  tabIRRrestr[K, 7] <- IRRLCI # Lower bound of bootstrapped 95% confidence interval
  tabIRRrestr[K, 8] <- IRRUCI # Upper bound of bootstrapped 95% confidence interval
  tabIRRrestr[K, 9] <- IRR3 # REML estimate of multiple-rater IRR
  tabIRRrestr[K, 10] <- IRR3LCI # Lower bound of bootstrapped 95% confidence interval
  tabIRRrestr[K, 11] <- IRR3UCI # Upper bound of bootstrapped 95% confidence interval
}
tabIRRrestr <- as.data.frame(tabIRRrestr) # Save as a data frame

####################
# REML on restricted samples bottom
# Initialization of matrix with results
tabIRRrestrBOTTOM <- matrix(NA, ncol = 11, nrow = 72)
colnames(tabIRRrestrBOTTOM) <- c(
  "K", "Kperc", "Proj", "Res", "Total",
  "IRR1", "LCI", "UCI", "IRR3", "LCI3", "UCI3"
)

for (K in 2:72)
{
  # Fit model on restricted data
  model <- lmer(Score ~ 1 + (1 | ID), data = dataL[dataL$Score_rank2 > 72 - K, ])

  # Decompose variance
  varAppl <- as.numeric(VarCorr(model)["ID"])
  varResid <- sigma(model)^2
  varTotal <- sum(varAppl, varResid, na.rm = T)

  # Calculate IRR
  (IRR <- varAppl / varTotal)

  # Calculate multiple-reviewer IRR (3 raters)
  (IRR3 <- varAppl / (varAppl + varResid / 3))

  # Calculate bootstrapped confidence interval
  set.seed(1357)
  N <- 10 # 00
  bootstrap <- bootMer(
    model,
    function(mm) c(as.numeric(VarCorr(mm)), sigma(mm)^2), N
  )
  # Saves data frame with N rows and 2 columns (each for one var component)
  bootTab <- bootstrap$t
  colnames(bootTab) <- c(names(VarCorr(model)), "Residual")
  head(bootTab, n = 3)

  bIRR <- bootTab[, "ID"] / apply(bootTab[, ], 1, sum) # Single-rater IRR
  (IRRLCI <- quantile(bIRR, 0.025)) # Lower bound of bootstrapped confidence interval
  (IRRUCI <- quantile(bIRR, 0.975)) # Upper bound of bootstrapped confidence interval

  bIRR3 <- bootTab[, "ID"] / (bootTab[, "ID"] + bootTab[, "Residual"] / 3) # Multiple-rater IRR
  (IRR3LCI <- quantile(bIRR3, 0.025)) # Lower bound of bootstrapped confidence interval
  (IRR3UCI <- quantile(bIRR3, 0.975)) # Upper bound of bootstrapped confidence interval

  # save
  tabIRRrestrBOTTOM[K, 1] <- K # Number of bottom proposals considered
  tabIRRrestrBOTTOM[K, 2] <- K / 72 # Ratio of bottom proposals considered
  tabIRRrestrBOTTOM[K, 3] <- varAppl # Application variance
  tabIRRrestrBOTTOM[K, 4] <- varResid # Residual variance
  tabIRRrestrBOTTOM[K, 5] <- varTotal # Total variance
  tabIRRrestrBOTTOM[K, 6] <- IRR # REML estimate of single-rater IRR
  tabIRRrestrBOTTOM[K, 7] <- IRRLCI # Lower bound of bootstrapped 95% confidence interval
  tabIRRrestrBOTTOM[K, 8] <- IRRUCI # Upper bound of bootstrapped 95% confidence interval
  tabIRRrestrBOTTOM[K, 9] <- IRR3 # REML estimate of multiple-rater IRR
  tabIRRrestrBOTTOM[K, 10] <- IRR3LCI # Lower bound of bootstrapped 95% confidence interval
  tabIRRrestrBOTTOM[K, 11] <- IRR3UCI # Upper bound of bootstrapped 95% confidence interval
}
tabIRRrestrBOTTOM <- as.data.frame(tabIRRrestrBOTTOM) # Save as a data frame

###############################
## Display of restricted IRR ##
###############################
## Todo: prepare interactive visualization in SIA/Reliability/Restricted-range IRR
## Todo: prepare plot() method for IRRrestricted() function
##       should take into account arguments top/bottom (x-axis label), number/proportion
##       demonstration on vector of proportions c(0.2, 0.4, 0.6, 0.8)
## here demonstrated for two types of estimates (REML, Bayesian), only needed for one (REML)
## caterpillar plot with highlighted proportions should also be allowed (type = caterpillar?)

tabIRRrestr <- read.csv("sampleRcode/IRRfiles/tabIRRrestrTOP_reml_seed1357_rank2.csv")
tabIRRrestrBOTTOM <- read.csv("sampleRcode/IRRfiles/tabIRRrestrBOTTOM_reml_seed1357_rank2_AIBS.csv")
tabIRRrestrBRMS <- read.csv("sampleRcode/IRRfiles/tabIRRrestrTOP_Ht25_seed135_rank2.csv")
tabIRRrestrBRMSbottom <- read.csv("sampleRcode/IRRfiles/tabIRRrestrBOTTOM_Ht25_seed135_rank2_AIBS.csv")


(g2a <- ggplot() +
  geom_point(data = tabIRRrestr, aes(x = K, y = IRR1, col = "REML")) +
  geom_errorbar(data = tabIRRrestr, aes(
    x = K, y = IRR1,
    ymax = UCI, ymin = LCI, col = "REML"
  ), width = 0) +
  geom_point(data = tabIRRrestrBRMS, aes(x = K + 0.3, y = IRR1, col = "MCMC")) +
  geom_errorbar(data = tabIRRrestrBRMS, aes(
    x = K + 0.3, y = IRR1,
    ymax = UCI, ymin = LCI, col = "MCMC"
  ), width = 0) +
  scale_color_manual("",
    breaks = c("REML", "MCMC"),
    labels = c("REML estimate", "MCMC estimate"), values = c("black", "blue")
  ) +
  xlab("Number of top AIBS proposals") +
  ylab("IRR") +
  ylim(0, 1) +
  ggtitle("  ") +
  theme_app(base_size = 12) +
  theme(legend.position = c(0.97, 0.97), legend.justification = c(1, 1)))

(g2a2 <- ggplot() +
  geom_point(data = tabIRRrestr, aes(x = 100 * Kperc, y = IRR1, col = "REML")) +
  geom_errorbar(data = tabIRRrestr, aes(
    x = 100 * Kperc, y = IRR1,
    ymax = UCI, ymin = LCI, col = "REML"
  ), width = 0) +
  geom_point(data = tabIRRrestrBRMS, aes(x = 100 * Kperc + 0.5, y = IRR1, col = "MCMC")) +
  geom_errorbar(data = tabIRRrestrBRMS, aes(
    x = 100 * Kperc + 0.5, y = IRR1,
    ymax = UCI, ymin = LCI, col = "MCMC"
  ), width = 0) +
  scale_color_manual("",
    breaks = c("REML", "MCMC"),
    labels = c("REML estimate", "MCMC estimate"), values = c("black", "blue")
  ) +
  xlab("Percentage of top AIBS proposals") +
  ylab("IRR") +
  ylim(0, 1) +
  ggtitle("  ") +
  theme_app(base_size = 12) +
  theme(legend.position = c(0.97, 0.97), legend.justification = c(1, 1)))
