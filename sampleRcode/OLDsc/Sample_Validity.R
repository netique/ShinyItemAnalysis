# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CORRELATION STRUCTURE ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggdendro)
library(ggplot2)
library(psych)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]

# polychoric correlation matrix
(corP <- polychoric(data))

# correlation heat map with 3 clusters using Ward method
plot_corr(data, cor = "poly", clust_method = "ward.D", n_clust = 3)

# dendrogram
hc <- hclust(as.dist(1 - corP$rho), method = "ward.D") # hierarchical clustering
ggdendrogram(hc) # dendrogram

# scree plot
ev <- eigen(corP$rho)$values # eigen values
df <- data.frame(comp = 1:length(ev), ev)

ggplot(df, aes(x = comp, y = ev)) +
  geom_point() +
  geom_line() +
  ylab("Eigen value") +
  xlab("Component number") +
  theme_app()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * FACTOR ANALYSIS ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library(psych)
library(ggplot2)

# loading data
data(HCI, package = "ShinyItemAnalysis")
data <- HCI[, 1:20]

# scree plot, parallel analysis
(fa_paral <- fa_parallel(data))
plot(fa_paral)
as.data.frame(fa_paral)

# scree plot based on eigen()
(ev <- eigen(corP$rho)$values) # eigen values

df <- data.frame(comp = 1:length(ev), ev)

ggplot(df, aes(x = comp, y = ev)) +
  geom_point() +
  geom_line() +
  ylab("Eigen value") +
  xlab("Component number") +
  theme_app()

# scree plot based on FA
(ev_psych <- psych::fa(data)$values) # Eigen values of the common factor solution
(ev_psych <- psych::fa(corP$rho)$values) # Eigen values of the common factor solution using the polychoric correlation


psych::fa(data)$e.value # Eigen values of the original matrix
eigen(cor(data))$values

df_psych <- data.frame(comp = 1:length(ev_psych), comp = ev_psych)

ggplot(df, aes(x = comp, y = ev_psych)) +
  geom_point() +
  geom_line() +
  ylab("Eigen value") +
  xlab("Component number") +
  theme_app()

# EFA for 1, 2, and 3 factors
(FA1 <- psych::fa(data, nfactors = 1))
(FA2 <- psych::fa(data, nfactors = 2))
(FA3 <- psych::fa(data, nfactors = 3))

# Model fit for different number of factors
VSS(data)

# Path diagrams
fa.diagram(FA1)
fa.diagram(FA2)
fa.diagram(FA3)

# Higher order factor solution
(om.h <- omega(data, sl = FALSE))



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CRITERION VALIDITY ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
score <- rowSums(data) # total score calculation
criterion <- GMAT[, "criterion"] # criterion variable

# number of respondents in each criterion level
size <- as.factor(criterion)
levels(size) <- table(as.factor(criterion))
size <- as.numeric(paste(size))
df <- data.frame(score, criterion, size)

# descriptive plots
### boxplot, for discrete criterion
ggplot(df, aes(y = score, x = as.factor(criterion), fill = as.factor(criterion))) +
  geom_boxplot() +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  scale_fill_brewer(palette = "Blues") +
  xlab("Criterion group") +
  ylab("Total score") +
  coord_flip() +
  theme_app()
### scatterplot, for continuous criterion
ggplot(df, aes(x = score, y = criterion)) +
  geom_point() +
  ylab("Criterion variable") +
  xlab("Total score") +
  geom_smooth(
    method = lm,
    se = FALSE,
    color = "red"
  ) +
  theme_app()

# test for association between total score and criterion variable
cor.test(criterion, score, method = "pearson", exact = FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** CRITERION VALIDITY - items ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ShinyItemAnalysis)

# loading data
data(GMAT, GMATtest, GMATkey, package = "difNLR")
data <- GMATtest[, 1:20]
data_binary <- GMAT[, 1:20]
key <- GMATkey
criterion <- GMAT[, "criterion"]

# item difficulty / criterion validity plot
DDplot(data_binary, criterion = criterion, val_type = "simple")

# distractor plot for item 1 and 3 groups
plotDistractorAnalysis(data, key, num.groups = 3, item = 1, criterion = criterion)

# test for association between total score and criterion variable for item 1
cor.test(criterion, data_binary[, 1], method = "pearson", exact = FALSE)

