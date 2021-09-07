#-----------------------------------------------------------------
# Factor analysis: draft
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(ShinyItemAnalysis)
library(corrplot)
library(tidyverse)
library(psych)
library(lavaan)
library(semPlot)

select <- dplyr::select

#-----------------------------------------------------------------
# EFA on HCI data
#-----------------------------------------------------------------

data(HCI, package = "ShinyItemAnalysis")
names(HCI)
data <- HCI[, 1:20]

#---------------------------
# Optimal number of factors
#---------------------------

# Scree plot
# - either looking for an elbow, taking factors to the left of the elbow
# - or accounting only for components with eigenvazlue > 1

# Scree plot for HCI (but binary data!)
pca <- princomp(data, cor = TRUE)
plot(pca$sdev^2, type = "b", pch = 16, xlab = "Component number", ylab = "Eigenvalue")
abline(h = 1, col = "red")
# elbow supports unidimensionality
# !!! The size of eigenvalues does not correspond with scree plot from parallel analysis (see below), something is wrong?
# !!? What is implemented in SIA? - its principal component analysis

round(
  cbind(
    "Variance" = pca$sdev^2, "%" = 100 * pca$sdev^2 / sum(pca$sdev^2),
    "Cumulative %" = 100 * cumsum(pca$sdev^2) / sum(pca$sdev^2)
  ),
  d = 2
)

# Parallel analysis:
HCI_paraller <- fa.parallel(HCI[,1:20],
                            cor = "poly",
                            fa = "fa",
                            sim = FALSE,
                            error.bars = TRUE
)

# VSS() function includes: very simple structure (Revelle), and also other criteria
VSS(HCI)
## Very Simple Structure
## Call: vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm,
##           n.obs = n.obs, plot = plot, title = title, use = use, cor = cor)
## VSS complexity 1 achieves a maximimum of 0.39  with  2  factors
## VSS complexity 2 achieves a maximimum of 0.45  with  3  factors
##
## The Velicer MAP achieves a minimum of 0.01  with  1  factors
## BIC achieves a minimum of  -972.48  with  1  factors
## Sample Size adjusted BIC achieves a minimum of  -333.99  with  2  factors
##
## Statistics by number of factors
## vss1 vss2    map dof chisq    prob sqresid  fit RMSEA  BIC SABIC complex eChisq  SRMR eCRMS eBIC
## 1 0.39 0.00 0.0051 209   382 3.3e-12      18 0.39 0.036 -972  -309     1.0    601 0.045 0.047 -753
## 2 0.39 0.44 0.0076 188   287 4.3e-06      17 0.44 0.028 -931  -334     1.2    423 0.038 0.042 -795
## 3 0.31 0.45 0.0102 168   232 7.9e-04      16 0.47 0.024 -856  -323     1.6    325 0.033 0.039 -763
## 4 0.30 0.44 0.0134 149   175 7.1e-02      15 0.50 0.016 -790  -317     1.9    241 0.028 0.035 -724
## 5 0.28 0.42 0.0169 131   126 6.0e-01      14 0.52 0.000 -722  -306     2.0    169 0.024 0.031 -680
## 6 0.29 0.43 0.0205 114    99 8.4e-01      13 0.55 0.000 -640  -278     2.1    122 0.020 0.029 -617
## 7 0.28 0.41 0.0255  98    75 9.6e-01      13 0.56 0.000 -560  -249     2.3     90 0.017 0.027 -545
## 8 0.28 0.40 0.0304  83    60 9.7e-01      12 0.58 0.000 -478  -214     2.5     67 0.015 0.025 -471

# BIC supports unidimensionality of the measure (1-factor solution)
# RMSEA 0.04 acceptable for 1-factor model

#---------------------------
# EFA for 1, 2, and 3 factors
#---------------------------

(FA1 <- fa(data, nfactors = 1))
(FA2 <- fa(data, nfactors = 2))
(FA3 <- fa(data, nfactors = 3))

# Path diagrams
fa.diagram(FA1)
fa.diagram(FA2)
fa.diagram(FA3)

#---------------------------
# Higher order factor solution
#---------------------------

(om.h <- omega(data, sl = FALSE))


#-----------------------------------------------------------------
# Exploratory and confirmatory FA on "Big Five" personality data
#-----------------------------------------------------------------

# load the data (from package "psych")
data(bfi, package = "psych")

# explore the variables
bfi %>% names()

# make dataframe without item-only data
bfi_items <- bfi %>% select(-c(gender, education, age))

#---------------------------
# Number of factors
#---------------------------

# Parallel analysis for optimal number of factors
# eigenvalue of 1 is highlighted as for Kaiser rule
bfi_paraller <- fa.parallel(bfi_items,
  cor = "poly", # use cor = "poly" for ordinal items with small number of anchor points
  fa = "fa",
  sim = T, # just resample actual data
  error.bars = TRUE
)
# THIS TAKES QUITE LONG!! I am afraid this wouldn't be feasible inside SIA!
# -> reimplemented in SIA, much much faster, see ?fa_parallel

#---------------------------
# EFA using the number of factors proposed above
#---------------------------

bfi_efa <- fa(bfi_items, nfactors = bfi_paraller$nfact, cor = "cor")
bfi_efa

# Path diagram
fa.diagram(bfi_efa)
# nothing really loads on MR6, right?

#---------------------------
# EFA using the number of factors = 5
#---------------------------

bfi_efa5 <- fa(bfi_items, nfactors = 5, cor = "cor")

bfi_efa5 # printing the object is much more informative than the summary method

# Path diagram
fa.diagram(bfi_efa5)

# compare EFA models, see anova.psych() for details
?anova.psych
anova(bfi_efa, bfi_efa5)
# not sure about how to read this

# psych can output the diagram as DOT diagram language
dot_out <- capture.output(fa.graph(bfi_efa5)) # fa.graph is meant to write the DOT diagramm specification into file, capture it into an R object instead

# using DiagrammeR, we are able to render it directly in R,
# see the grVizOutput for Shiny-friendly usage
DiagrammeR::grViz(dot_out)


#---------------------------
# CFA using "lavaan" package
#---------------------------

# describe the expected factor stucture (factor names are arbitrary)
#  the =~ operator means that RHS loads onto LHS
cfa_model <- "
agreeableness =~ A1 + A2 + A3 + A4 + A5
conscientiousness =~ C1 + C2 + C3 + C4 + C5
extraversion =~ E1 + E2 + E3 + E4 + E5
neuroticism =~ N1 + N2 + N3 + N4 + N5
openness =~ O1 + O2 + O3 + O4 + O5
"

# fit the model specified above with cfa function of lavaan
# ordered = TRUE declares the ordinal nature of the data
cfa_fit <- cfa(cfa_model, ordered = TRUE, data = bfi_items)

# get summary with fit measures, standardized solution, confidence intervals
# and determination coef. for dependent variables
summary(cfa_fit, fit.measures = TRUE, standardized = TRUE, ci = TRUE, rsquare = TRUE)

# inspect modification indices
modificationIndices(cfa_fit, sort. = TRUE)

# visualize the model with semPlot package (can be tricky)
semPaths(cfa_fit,
  what = "std", layout = "circle",
  intercepts = FALSE, thresholds = FALSE
)

#-----------------------------------------------------------------
# IRT by EFA of Tetrachoric/Polychoric correlations
#-----------------------------------------------------------------

# Also see Kamata & Bauer, 2008:
#   A Note on the Relation Between Factor Analytic and Item Response Theory Models
#   https://doi.org/10.1080/10705510701758406
#
# Also see section 8.3. in Revelle
# http://www.personality-project.org/r/book/Chapter8.pdf
#
# Also see Psychometrika paper
# Takane & deLeeuw (1987): On the relationship between item response theory and factor analysis of discretized variables
# https://doi.org/10.1007/BF02294363

?psych::irt.fa

#################
library(ltm)

#####
# ltm
fit_ltm <- ltm(data ~ z1)
summary(fit_ltm)

plot(fit_ltm)
plot(fit_ltm, type = "IIC")

########
fit_irtfa1 <- irt.fa(data)
summary(fit_irtfa1)
fit_irtfa1

fit_irtfa1$irt # coeficients based on FA
round(coefficients(fit_ltm) / 1.702, 3) # almost identical coefficients from ltm

fit_irtfa1$fa

plot(fit_irtfa1, type = "ICC")
plot(fit_irtfa1, type = "IIC")
plot(fit_irtfa1, type = "test")
