# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SPEARMAN-BROWN FORMULA ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(psychometric)

# loading data
data(HCI, package = "ShinyItemAnalysis")
data <- HCI[, 1:20]

# reliability of original data
rel.original <- psychometric::alpha(data)
# number of items in original data
items.original <- ncol(data)

# number of items in new data
items.new <- 30
# ratio of tests lengths
m <- items.new / items.original
# determining reliability
SBrel(Nlength = m, rxx = rel.original)

# desired reliability
rel.new <- 0.8
# determining test length
(m.new <- SBlength(rxxp = rel.new, rxx = rel.original))
# number of required items
m.new * items.original

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SPLIT HALF METHOD ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(psych)

# loading data
data(HCI, package = "ShinyItemAnalysis")

# first-last splitting
df1 <- HCI[, 1:10]
df2 <- HCI[, 11:20]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearman-Brown formula to estimate reliability
(rel.x <- 2 * cor.x / (1 + cor.x))

# even-odd splitting
df1 <- HCI[, seq(1, 20, 2)]
df2 <- HCI[, seq(2, 20, 2)]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearman-Brown formula to estimate reliability
(rel.x <- 2 * cor.x / (1 + cor.x))

# random splitting
samp <- sample(1:20, 10)
df1 <- HCI[, samp]
df2 <- HCI[, setdiff(1:20, samp)]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearman-Brown formula to estimate reliability
(rel.x <- 2 * cor.x / (1 + cor.x))

# minimum of 10,000 split-halves (Revelle's beta)
split <- psych::splitHalf(HCI[, 1:20], raw = TRUE)
items1 <- which(split$minAB[, "A"] == 1)
items2 <- which(split$minAB[, "B"] == 1)
df1 <- HCI[, items1]
df2 <- HCI[, items2]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearman-Brown formula to estimate reliability
(rel.x <- 2 * cor.x / (1 + cor.x))

# calculation of CI
z.r <- 0.5 * log((1 + cor.x) / (1 - cor.x))
n <- length(ts1)
z.low <- z.r - 1.96 * sqrt(1 / (n - 3))
z.upp <- z.r + 1.96 * sqrt(1 / (n - 3))

cor.low <- (exp(2 * z.low) - 1) / (exp(2 * z.low) + 1)
cor.upp <- (exp(2 * z.upp) - 1) / (exp(2 * z.upp) + 1)

rel.x <- 2 * cor.x / (1 + cor.x)
rel.low <- 2 * cor.low / (1 + cor.low)
rel.upp <- 2 * cor.upp / (1 + cor.upp)

# average 10,000 split-halves
split <- psych::splitHalf(HCI[, 1:20], raw = TRUE)
(rel.x <- mean(split$raw))

# average all split-halves
split <- psych::splitHalf(HCI[, 1:20], raw = TRUE, brute = TRUE)
(rel.x <- mean(split$raw))

# calculation of CI
n <- length(split$raw)
rel.low <- rel.x - 1.96 * sd(split$raw) / sqrt(n)
rel.upp <- rel.x + 1.96 * sd(split$raw) / sqrt(n)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CRONBACH'S ALPHA ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(psychometric)

# loading data
data(HCI, package = "ShinyItemAnalysis")
data <- HCI[, 1:20]

# Cronbach's alpha with confidence interval
a <- psychometric::alpha(data)
psychometric::alpha.CI(a, N = nrow(data), k = ncol(data), level = 0.95)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * INTRA-CLASS CORRELATION ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(psych)
library(tidyverse)

# loading and formatting data
data(AIBS, package = "ShinyItemAnalysis")
AIBSwide <- AIBS %>%
  pivot_wider(ID, values_from = Score, names_from = RevCode) %>%
  select(-ID)
head(AIBSwide)

ICC(AIBSwide)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * RESTRICTED-RANGE RELIABILITY ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)
library(purrr)
library(ShinyItemAnalysis)

# loading data
data(AIBS, package = "ShinyItemAnalysis")

# estimate reliability with ICC for complete AIBS dataset
ICCrestricted(Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj")

# estimate range-restricted ICC
ICCrestricted(Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj",
              sel = 0.90, dir = "top")

# caterpillar plot
AIBS %>%
  ggplot(aes(x = ScoreRankAdj, y = Score, group = ID)) +
  geom_line(col = "gray") +
  geom_point(shape = 1, size = 1.5) +
  stat_summary(fun = mean, fun.args = list(na.rm = TRUE), geom = "point",
               col = "red", shape = 5, size = 2.5, stroke = .35) +
  labs(x = "Ratee rank", y = "Rating (score)") +
  coord_cartesian(ylim = c(1, 5)) +
  theme_app()

# estimate all possible top-restricted subsets
all_top_restricted <- map_dfr(2:72,
  ~ ICCrestricted(Data = AIBS, case = "ID", var = "Score",
                  rank = "ScoreRankAdj", sel = .x, nsim = 10))
all_top_restricted

# or alternatively, in base R:
base_way <- lapply(2:72, function(x) {
  ICCrestricted(Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj",
                sel = x, nsim = 10)})
do.call(rbind.data.frame, base_way)

# plot
all_top_restricted %>%
  ggplot(aes(prop_sel, ICC1, ymin = ICC1_LCI, ymax = ICC1_UCI)) +
  geom_pointrange() + scale_x_continuous(labels = scales::percent) +
  labs(x = ("Proportion of top ratees"), y = "Reliability") +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
  theme_app()

