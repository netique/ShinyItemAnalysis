### Supplementary code for
### McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinkova, P., Cliff, W., Michael, J., ... & Wright, A. (2017).
### Development and validation of the homeostasis concept inventory. CBE—Life Sciences Education, 16(2), ar35.
###
###
### Last updated on Dec 14, 2020

# R packages
library(lme4)
library(lmerTest)
library(psych)
library(psychometric)
library(ltm)
library(difR)
library(ggplot2)
library(data.table)
library(mirt)
library(tidyr)
library(ShinyItemAnalysis)

# DATA
# load(file = "dataHCI.RData")
# load(file = "dataGrads.RData")
# load(file = "dataTestretest.RData")
# load(file = "dataPrePost.RData")
# data(dataHCI)

data("HCIgrads")
data("HCItestretest")
data("HCIprepost")
data("HCIdata")

summary(HCIgrads)
summary(HCItestretest)
summary(HCIprepost)
summary(HCIdata)

# attach(dataHCI)
attach(HCIdata) # unnecessary, data already attached

# variable names
varsA <- paste("A", 1:20, sep = "")
varsQR <- paste("QR", 1:20, sep = "")

# CRITERION VALIDITY
# graduate students (10), MEAN = 14.5, SD = 3.27
mean(HCIgrads$total)
sd(HCIgrads$total)

# undergraduate students (669), MEAN = 12.13, SD = 3.65
mean(HCIdata$total)
sd(HCIdata$total)
# comparison: total score graduate vs undergraduate,
# two-sample t-test, alternative greater, p-value 0.024
t.test(HCIgrads$total, HCIdata$total, alternative = "greater")

# comparison: total score pre vs post,
# paired t-test, p-value 0.010
t.test(HCIprepost$score.pre, HCIprepost$score.post, paired = TRUE)
# difference post-pre
dif <- HCIprepost$score.post - HCIprepost$score.pre
t.test(dif)
mean(dif, na.rm = TRUE)
sd(dif, na.rm = TRUE)
# comparison: exposed (16) vs not exposed (45), difference pre vs post
# two sample t-test, alternative greater, p-value 0.048
test <- HCItestretest[(HCItestretest$test == "test"), ]
retest <- HCItestretest[(HCItestretest$test == "retest"), ]
difTestretest <- retest$total - test$total
t.test(dif, difTestretest, alternative = "greater")
mean(difTestretest)
sd(difTestretest)

# LINEAR MIXED EFFECT MODEL; using lme4 and lmerTest packages
# some variables not provided in the HCIdata
# summary(lmer1 <- lmer(total ~ as.factor(yearc5) + gender + major +
#                        profsch + ethn4 + age3 + EnglishF + level +
#                        NprerH + typeS + typeSCH + (1|survey),
#                      data = HCIdata))
# anova(lmer1)

# some variables not provided in the HCIdata
# remove nonsignifficant: NprerH, typeS, typeSCH, level, profSCH (one by one, here all at once)
# summary(lmer2 <- lmer(total ~ as.factor(yearc5) + gender + major + ethn4 +
#                        age3 + EnglishF + (1|survey),
#                      data = HCIdata))
# anova(lmer2)

# some variables not provided in the HCIdata
# remove age3 (optimal mixed effect model)
# summary(lmer3 <- lmer(total ~ as.factor(yearc5) + gender + major + ethn4 +
#                        EnglishF + (1|survey),
#                      data = HCIdata))
# anova(lmer3)
#
# BIC(lmer1, lmer2, lmer3)

# optimal fixed effects model (selection not shown here)
lmF <- lm(total ~ gender + major + as.factor(yearc5) + minority + EnglishF + typeSCH, data = HCIdata)
summary(lmF)

# BIC(lmer1, lmer2, lmer3, lmF) # mixed-effect model fits better using BIC

# CONSTRUCT VALIDITY, UNIDIMENSIONALITY OF THE INSTRUMENT
# tetrachoric correlation heat-map
corP <- polychoric(HCIdata[, varsQR]) # unnecessary, as the plot_corr can compute it in one call
round(corP$rho, 2)

# Exploratory factor analysis
# Optimal number of factors
VSS(HCIdata[, varsQR])
# BIC supports unidimensionality of the measure (1-factor solution)
# RMSEA 0.04 acceptable for 1-factor model

(FA1 <- fa(HCIdata[, varsQR], nfactors = 1))
(FA2 <- fa(HCIdata[, varsQR], nfactors = 2))
(FA3 <- fa(HCIdata[, varsQR], nfactors = 3))

fa.diagram(FA1)
fa.diagram(FA2)
fa.diagram(FA3)

# Higher order factor solution
(om.h <- psych::omega(HCIdata[, varsQR], sl = FALSE))

# Scree plot
pca <- princomp(HCIdata[, varsQR], cor = TRUE)
plot(pca$sdev^2, type = "b", pch = 16, xlab = "Component number", ylab = "Eigenvalue")

round(
  cbind(
    "Variance" = pca$sdev^2, "%" = 100 * pca$sdev^2 / sum(pca$sdev^2),
    "Cumulative %" = 100 * cumsum(pca$sdev^2) / sum(pca$sdev^2)
  ),
  d = 2
)

# RELIABILITY
# Pearson correlation coefficient, 0.77, CI = (0.62, 0.87)
cor.test(test$total, retest$total)

# Cronbach's alpha, 0.72, CI = (0.69, 0.75); using psych and psychometric packages
psych::alpha(HCIdata[, varsQR])
psychometric::alpha(HCIdata[, varsQR])

# ITEM ANALYSIS
# difficulties and discriminations; using psychometric package
item.exam(HCIdata[, varsQR], discr = TRUE)

# IRT models - 1PL, 2PL, 3PL; using ltm package
fit1PL <- rasch(HCIdata[, varsQR])
fit2PL <- ltm(HCIdata[, varsQR] ~ z1)
fit3PL <- tpm(HCIdata[, varsQR])

# IRT models comparison, 3PL model is the best
anova(fit1PL, fit2PL)
anova(fit2PL, fit3PL)

# Item parameters for the optimal 3PL model
summary(fit3PL)

# item fit statistics for 3PL model
# calculation takes longer here, skipped
# item.fit(fit3PL, simulate.p.value = TRUE)

# IRT models with the mirt package
fit1PLmirt <- mirt(HCIdata[, varsQR],
  model = 1, itemtype = "2PL",
  constrain = list((1:length(varsQR)) + seq(0, (length(varsQR) - 1) * 3, 3)), SE = TRUE
)
fit2PLmirt <- mirt(HCIdata[, varsQR], model = 1, itemtype = "2PL", SE = TRUE)
fit3PLmirt <- mirt(HCIdata[, varsQR], model = 1, itemtype = "3PL", SE = TRUE)

# IRT models comparison, 3PL model selected as final model
anova(fit1PLmirt, fit2PLmirt)
anova(fit2PLmirt, fit3PLmirt)

# item coefficients under the optimal model
coef(fit3PLmirt, simplify = TRUE, IRTpars = TRUE)

# item fit statistics for 3PL model
itemfit(fit3PLmirt)

# FAIRNESS
# DIF by gender
data <- HCIdata[, varsQR]
group <- HCIdata[, "gender"]
# remove students who did not specify gender
group[HCIdata[, "gender"] == "none"] <- NA
data <- data[complete.cases(group), ]
group <- abs(c(na.omit(group)) - 2)
# logistic regression DIF method; using difR package
difLogistic(data, group, focal.name = 1, p.adjust.method = "BH")

# DIF by English language status
group <- HCIdata[, "EnglishF"]
group <- as.numeric(group == "yes")
data <- HCIdata[, varsQR]
# logistic regression DIF method; using difR package
difLogistic(data, group, focal.name = 1, p.adjust.method = "BH")

# DIF by Ethnic group
group <- HCIdata[, "ethn4"]
summary(group)
data <- HCIdata[, varsQR]

score <- apply(data, 1, sum)
LRval <- pval <- c()
# logistic regression DIF method with several focal groups
for (i in 1:20) {
  # null model
  fit0 <- glm(data[, i] ~ score, family = "binomial")
  # alternative model
  fit1 <- glm(data[, i] ~ score + group + score * group, family = "binomial")
  LRtest <- anova(fit0, fit1, test = "LRT")
  # LR-test statistics
  LRval <- c(LRval, LRtest$Deviance[2])
  # p-values
  pval <- c(pval, LRtest$`Pr(>Chi)`[2])
}
# adjusted p-values; using Benjamini-Hochberg correction
# none of items detected as DIF
LRval
p.adjust(pval, method = "BH")

# ABSTRACT VS APPLIED
item <- 1:20
i.type <- c(
  "Abstract", "Abstract", "Abstract", "Abstract", "Applied", "Applied",
  "Abstract", "Applied", "Applied", "Applied", "Applied",
  "Applied", "Abstract", "Applied", "Applied", "Abstract",
  "Applied", "Abstract", "Abstract", "Applied"
)
i.type <- as.factor(i.type)
difc <- item.exam(HCIdata[, varsQR], discr = TRUE)[, "Difficulty"]
i.types <- data.frame(item, i.type, difc)
# comparison: difficulty of abstract vs applied questions
# two sample t-test, p = 0.132
t.test(difc ~ i.type)
mean(difc[i.type == "Abstract"])
sd(difc[i.type == "Abstract"])
mean(difc[i.type == "Applied"])
sd(difc[i.type == "Applied"])

# FIGURES
# settings for graphics
theme1 <- theme_bw() + theme(
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  legend.title = element_blank(),
  legend.position = c(0, 1),
  legend.justification = c(0, 1),
  legend.background = element_rect(fill = "transparent"),
  legend.key = element_rect(colour = "white")
)

# Figure 1: boxplots
# graduate vs undergraduate
df <- data.frame(
  score = c(HCIgrads$total, HCIdata$total),
  group = c(
    rep("Graduate", nrow(HCIgrads)),
    rep("Undergraduate", nrow(HCIdata))
  )
)
df$group <- relevel(as.factor(df$group), "Undergraduate")
p1 <- ggplot(df, aes(x = group, y = score)) +
  geom_boxplot(aes(
    x = group, y = score,
    fill = group
  ),
  alpha = 0.7
  ) +
  geom_point(position = position_jitter(width = 0.2)) +
  xlab("") +
  ylab("Total score on HCI") +
  scale_fill_manual(values = c("#B79F00", "#F564E3"))
p1 + theme1 + theme(legend.position = "none")

# pre vs post
df <- pivot_longer(HCIprepost,
  cols = c(score.pre, score.post),
  names_to = "variable"
)
df$variable <- as.factor(df$variable)
levels(df$variable) <- c("Pre", "Post")
p2 <- ggplot(df, aes(x = variable, y = value)) +
  geom_boxplot(aes(
    x = variable, y = value,
    fill = variable
  ),
  alpha = 0.7
  ) +
  geom_point(position = position_jitter(width = 0.2)) +
  xlab("") +
  ylab("Total score on HCI") +
  scale_fill_manual(values = c("#00FF00FF", "#0080FFFF"))
p2 + theme1 + theme(legend.position = "none")

# exposed vs not exposed
df <- data.frame(
  value = c(dif, difTestretest),
  variable = c(
    rep("Exposed", length(dif)),
    rep("Not exposed", length(difTestretest))
  )
)
p3 <- ggplot(df, aes(x = variable, y = value)) +
  geom_boxplot(aes(
    x = variable, y = value,
    fill = variable
  ),
  alpha = 0.7
  ) +
  geom_point(position = position_jitter(width = 0.2)) +
  xlab("") +
  ylab("Difference in total score on HCI") +
  scale_fill_manual(values = c("#FF8000FF", "#FF00FFFF"))
p3 + theme1 + theme(legend.position = "none")

# Figure 2: density plots
# relevel of variables
levels(HCIdata$typeS) <- c("Allied", "Science", "Mixed")
HCIdata$typeS <- relevel(HCIdata$typeS, "Science")
HCIdata$major <- as.factor(HCIdata$major)
levels(HCIdata$major) <- c("Other", "Science")
HCIdata$major <- relevel(HCIdata$major, "Science")

# planned majors
hist0 <- ggplot(HCIdata, aes(total, fill = major)) +
  geom_density(aes(
    y = ..density..,
    color = major,
    linetype = major
  ),
  position = "identity",
  alpha = 0.5,
  size = 1
  ) +
  xlab("Total score on HCI") +
  ylab("Density") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.16))
hist0 + theme1
# course type
hist1 <- ggplot(HCIdata, aes(total, fill = typeS)) +
  geom_density(aes(
    y = ..density..,
    color = typeS,
    linetype = typeS
  ),
  position = "identity",
  alpha = 0.5,
  size = 1
  ) +
  xlab("Total score on HCI") +
  ylab("Density") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.16))
hist1 + theme1
# institution type
hist2 <- ggplot(HCIdata, aes(total, fill = typeSCH)) +
  geom_density(aes(
    y = ..density..,
    color = typeSCH,
    linetype = typeSCH
  ),
  position = "identity",
  alpha = 0.5,
  size = 1
  ) +
  xlab("Total score on HCI") +
  ylab("Density") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.16))
hist2 + theme1

# Figure 3: difficulty and discrimination
DDplot(HCIdata[, varsQR])

# Figure 4: Wright map
# 1PL IRT model
fit1PL <- rasch(HCIdata[, varsQR])
ltm::factor.scores(fit1PL)
# estimated ability
thetas1PL <- ltm::factor.scores(fit1PL)$score.dat[, "z1"]
# estimated difficulty
diffclt1PL <- summary(fit1PL)$coef[1:20, "value"]
# ordered difficulty
diffclt1PLordered <- diffclt1PL[order(diffclt1PL)]

# WrightMap::wrightMap(
#   thetas = thetas1PL,
#   thresholds = diffclt1PLordered,
#   item.side = itemClassic,
#   item.prop = 0.5,
#   label.items = order(diffclt1PL),
#   thr.lab.text = order(diffclt1PL),
#   dim.names = "",
#   oma = c(0, 2, 0, 2)
# )
ggWrightMap(
  theta = thetas1PL,
  b = diffclt1PLordered
)

# Figure 5: IRT models
# ICC
ICC <- plot(fit3PL,
  type = "ICC"
)
# IIC
IIC <- plot(fit3PL,
  type = "IIC",
  ylab = "Item information"
)
# TIF
TIF <- plot(fit3PL,
  type = "IIC",
  item = 0,
  ylab = "Test information",
  main = "Test Information Function"
)

# Figure 6: tetrachoric correlation heatmap

# use pre-computed corr. matrix and clus. meth. "complete", same as in corrplot
plot_corr(corP$rho, cor = "none", clust_method = "com") # clust_meth. can be abbrev.

# compute the matrix and visualize it directly in one call
plot_corr(HCIdata[, varsQR], cor = "poly", clust_method = "complete")

# square tiles
plot_corr(HCIdata[, varsQR], cor = "poly", clust_method = "complete", shape = "square")

# to mimic corrplot in the paper, few things need to be tweaked a bit
plot_corr(HCIdata[, varsQR], cor = "poly", clust_method = "complete", shape = "sq") +
  scale_fill_gradient2(
    limits = c(-.1, 1),
    breaks = seq(-.1, 1, length.out = 12),
    guide = guide_colorbar(
      barheight = .8, barwidth = .0275,
      default.unit = "npc",
      title = NULL, frame.colour = "black", ticks.colour = "black"
    )
  ) +
  theme(axis.text = element_text(colour = "red", size = 12))

corrplot::corrplot(corP$rho, method = "shade", ord = "hclust", rect.col = "black", cl.lim = c(-0.1, 1), cl.length = 12)


# Figure 7: abstract vs applied
AA_box <- ggplot(i.types, aes(x = i.type, y = difc)) +
  geom_boxplot(aes(
    x = i.type, y = difc,
    fill = i.type
  ),
  alpha = 0.7
  ) +
  geom_point(position = position_jitter(width = 0.2)) +
  xlab("") +
  ylab("Difficulty") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_fill_manual(values = c("gold", "#FF00FFFF"))
AA_box + theme1 + theme(legend.position = "none")

# Try it in interactive ShinyItemAnalysis app
# - under tab Data pick Select dataset: HCI
# - follow other tabs to perform the analysis
startShinyItemAnalysis()
