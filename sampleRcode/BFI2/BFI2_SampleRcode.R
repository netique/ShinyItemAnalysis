## Analysis following the paper
## Hrebickova et al (2020). Big five inventory 2 (BFI-2): Hierarchicky model s 15 subskalami. [Big five inventory 2 (BFI-2): Hierarchical model, In Czech]. Ceskoslovenska psychologie LXIV(4) https://dostal.vyzkum-psychologie.cz/soubory/BFI2.pdf
## Soto, C. J., & John, O. P. (2017). The next Big Five Inventory (BFI-2): Developing and assessing a hierarchical model with 15 facets to enhance bandwidth, fidelity, and predictive power. Journal of Personality and Social Psychology, 113, 117-143.

library(semTools)
library(mirt)
library(ShinyItemAnalysis)
library(tidyverse)
library(lavaan)

options(digits = 4)


data(BFI2, package = "ShinyItemAnalysis")
?BFI2

head(BFI2)

items <- names(BFI2)[grepl("i", names(BFI2))]

#items_raw <- BFI2[, paste0("BFI", 1:60)]
# TODO: define items for BFI-2-S, BFI-2XS domains and BFI-2, BFI2-S facets, relevel where needed


# items -----------------------------------------------------------

# names derived from Soto, C. J., & John, O. P. (2017)
# N - Negative Emotionality (aka neuroticism)
# E - extraversion
# O - Open-Mindedness (more used - opennes to experience)
# C - consciousness
# A - agreeability

items_lookup <- tibble(
  E_scb_sociability = c(1, 16, 31, 46),
  E_asr_assertiveness = c(6, 21, 36, 51),
  E_enl_energy_level = c(11, 26, 41, 56),
  A_cmp_compassion = c(2, 17, 32, 47),
  A_rsp_respectfulness = c(7, 22, 37, 52),
  A_trs_trust = c(12, 27, 42, 57),
  C_org_organization = c(3, 18, 33, 48),
  C_prd_productiveness = c(8, 23, 38, 53),
  C_rsp_responsibility = c(13, 28, 43, 58),
  N_anx_anxiety = c(4, 19, 34, 49),
  N_dep_depression = c(9, 24, 39, 54),
  N_emt_emotional_volatility = c(14, 29, 44, 59),
  O_int_intellectual_curiosity = c(10, 25, 40, 55),
  O_aes_aesthetic_sensitivity = c(5, 20, 35, 50),
  O_crt_creative_imagination = c(15, 30, 45, 60)
)

# items_lookup <- tibble(
#   facet = factor(c(
#     "E_sociability",
#     "E_assertiveness",
#     "E_energy_level",
#     "A_compassion",
#     "A_respectfulness",
#     "A_trust",
#     "C_organization",
#     "C_productiveness",
#     "C_responsibility",
#     "N_anxiety",
#     "N_depression",
#     "N_emotional_volatility",
#     "O_intellectual_curiosity",
#     "O_aesthetic_sensitivity",
#     "O_creative_imagination"
#   )),
#   item_number = list(
#     c(1, 16, 31, 46),
#     c(6, 21, 36, 51),
#     c(11, 26, 41, 56),
#     c(2, 17, 32, 47),
#     c(7, 22, 37, 52),
#     c(12, 27, 42, 57),
#     c(3, 18, 33, 48),
#     c(8, 23, 38, 53),
#     c(13, 28, 43, 58),
#     c(4, 19, 34, 49),
#     c(9, 24, 39, 54),
#     c(14, 29, 44, 59),
#     c(10, 25, 40, 55),
#     c(5, 20, 35, 50),
#     c(15, 30, 45, 60)
#   )
# )


# table with reversed items identified
#
#reversed <- read_csv("sampleRcode/BFI2/BFI2_reversed_items.csv")
#
#items_overview <- items_lookup %>%
#  map(~ tibble(item_number = .x)) %>%
#  bind_rows(.id = "facet") %>%
#  mutate(domain = str_extract(facet, "."), # extract first letter as domain ID
#         domain = fct_inorder(domain),
#         facet = fct_inorder(facet)) %>% # retain order from the paper
#  relocate(item_number, domain) %>%
#  left_join(reversed)
#
#rename_vector <- items_overview %>%
#  mutate(
#    item_code = paste0("BFI", item_number),
#    facet_code = str_extract(facet, "(?<=_).{3}"),
#    item_name = paste0(
#      "i",
#      domain,
#      facet_code,
#      str_pad(item_number, 2, pad = 0),
#      ifelse(is_reversed, "r", "")
#    )
#  ) %>%
#  select(item_name, item_code) %>% deframe %>%
#  write_rds("sampleRcode/BFI2/bfi2_rename_vector.rds")
#
#
#
#BFI2 %>% rename(!!!rename_vector) %>%

# recode items ------------------------------------------------------------


# update: BFI2 in SIA is already recoded!!
# range 1-5 points so reverse with 6 - score

#items <- items_raw %>% modify_if(reversed$is_reversed, ~ 6 - .x)
#

# descriptives
# domains - its unclear how the mean was computed, but this returns the closest results

#items_overview %>%
#  group_by(domain) %>%
#  summarize(idx = list(map_dbl(item_number, ~.x))) %>%
#  transmute(domain,
#    mean = map_dbl(idx, ~ items[, .x] %>%
#      rowMeans() %>%
#      mean()),
#    sd = map_dbl(idx, ~ items[, .x] %>%
#      rowMeans() %>%
#      sd())
#  )

# facets
#items_overview %>%
#  group_by(facet) %>%
#  summarize(idx = list(map_dbl(item_number, ~.x))) %>%
#  transmute(facet,
#    mean = map_dbl(idx, ~ items[, .x] %>%
#      rowMeans() %>%
#      mean()),
#    sd = map_dbl(idx, ~ items[, .x] %>%
#      rowMeans() %>%
#      sd())
#  )

# -------------------------------------------
dim(BFI2) # 1733 participants, OK
summary(BFI2)

table(BFI2$Gender)
prop.table(table(BFI2$Gender)) # 42.1% men, 57.9% women, OK

summary(BFI2$Age) # 15 to 26 years, mean 20.06

# --------------------------------------------
# Test-retest - probably not available in this dataset?
# Average BFI-2 test-retest reliability estimated over a 6 month period was r = 0.86 for domains
#    and r = 0.80 for facets.

# BFI2 %>% count(ID) %>% pull(n) %>% unique() # probably not, IDs are unique

# --------------------------------------------
# Internal consistency
# Internal consistency of BFI2, BFI-2-S, BFI-2XS domains and BFI-2, BFI2-S facets was estimated using Cronbach's alpha
#   coefficient and ordinal McDonald's omega coefficient.
# The BFI-2 domains showed good internal consistency, ranging from 0.81 to 0.89.
# Internal consistency of individual facets ranged from 0.56 to 0.83 (M = 0.74).

psych::alpha(BFI2[,items])
# alpha 0.79 0.81 0.82

psychometric::alpha(BFI2[,items]) # 0.8062

omega_fit <- psych::omega(BFI2[,items], nfactors = 5, poly = TRUE, covar = T)
# general factor loadings < .2 are hidden from the diagram by default,
# but the loadings for all items are estimated as they supposed to

# Alpha                  0.89 differences caused by polychoric corr.
# G.6:                   0.93
# Omega Hierarchical:    0.26
# Omega H asymptotic:    0.29
# Omega Total            0.91

# try fit the model with lavaan and estimate omegas with semTools
omega_fit$model$lavaan
omega_lavaan <- cfa(omega_fit$model$lavaan, items, ordered = T, std.lv = T)
# I get this error message:
#Error in lavData(data = data, group = group, cluster = cluster, ov.names = OV.NAMES,  :
#                   lavaan ERROR: data= argument is not a data.fame, but of class ‘character’
omega_lavaan %>% semTools::reliability(return.total = T)
# lavaan ERROR: data= argument is not a data.fame, but of class ‘character’

# --------------------------------------------
# Internal structure:
# The structure of the BFI-2 at the level of items was explored using Principal Component Analysis with Varimax rotation;
# structures of domains were confirmed using Confirmatory Factor Analyses.
# p. 442: CFA with WLSMV as in Soto (2017), using lavaan

# All items of the BFI-2 showed factor loadings 0.30 or higher on intended factor. The BFI-2 hierarchical structure
# with 15 facets was confirmed using CFA. Short versions BFI-2-S and BFI-2-XS captured 91% and 77% of the domains of the full version of
# BFI-2 inventory

# PCA ---------------------------------------------------------------------
# seems it was done on raw, not recoded scores, only N was reversed
pca_fit <- BFI2[, items] %>% # keep the order in track
  psych::pca(nfactors = 5, cor = "poly", rotate = "varimax")
pca_fit

# guess domains
pca_loadings <- pca_fit$loadings %>%
  unclass() %>%
  as_tibble() %>%
  set_names(c("N", "C", "E", "O", "A")) %>%
  relocate("E", "A", "C", "N", "O") # set order from the paper


#pca_from_paper <- read_csv2("sampleRcode/BFI2/PCA_loadings_from_paper.csv") %>%
#  select(1:6)
## Warning message:
## Missing column names filled in: 'X7' [7], 'X8' [8]
#pca_from_paper


bind_cols(pca_from_paper, pca_loadings) %>%
  set_names(
    "item",
    paste0(
      c("E", "A", "C", "N", "O"),
      c(rep("_orig", 5), rep("_new", 5))
    )
  ) %>%
  pivot_longer(-item, names_to = c("domain", "fit"), names_sep = "_") %>%
  pivot_wider(names_from = fit) %>%
  ggplot(aes(orig, new)) + geom_point() + geom_smooth() + facet_wrap(~ domain)

# note the loadings differ because random sample of n = 700 was used in the paper
# any set.seed() to identify the IDs in the sample? - JN: unfortunately, no seed

# EFA --------------------------------------------------------------------------
FA5 <- psych::fa(BFI2[,items], nfactors = 5)
FA5
psych::fa.diagram(FA5)

# CFA - "three subscales models" -----------------------------------------------
# not working - any code that should be retained / added below?
# build models by domain
cfa_models <- items_overview %>%
  mutate(item_code = paste0("BFI", item_number)) %>% # from number to item name
  group_by(domain, facet) %>%
  summarize(items = str_flatten(item_code, collapse = " + ")) %>%
  mutate(.after = facet, op = "=~") %>% # add operator
  unite("model", c(facet, op, items), sep = " ") %>% # make one big string form three cols
  summarise(model = str_flatten(model, "\n"))
cfa_models

# again, authors used 1000 sample, so we conduct simple "bootstrap"
# randomly picking 1000 respondents each run (N = 100)
bs <- map(
  1:3,
  ~ {

    # fit those models using ordered (polych matrix), and WLSMV (the defualt)
    cfa_fitted <- cfa_models %>% mutate(fit = map(
      model, ~ cfa(.x, items %>% slice_sample(n = 1000),
        std.lv = TRUE, estimator = "WLSMV", ordered = T
      )
    ))

    # fit indices
    cfa_fitted %>% transmute(domain,
      chisq = map_dbl(fit, ~ .x %>% fitmeasures("chisq")),
      df = map_dbl(fit, ~ .x %>% fitmeasures("df")),
      cfi = map_dbl(fit, ~ .x %>% fitmeasures("cfi")),
      tli = map_dbl(fit, ~ .x %>% fitmeasures("tli")),
      rmsea = map_dbl(fit, ~ .x %>% fitmeasures("rmsea"))
    )
  }
)
bs

# quick graphical overview -- seems simillar to the original (df is always = 51)
bs %>%
  bind_rows(.id = "run") %>%
  pivot_longer(-c(run, domain)) %>%
  ggplot(aes(value, col = domain)) +
  geom_density( size = 1) +
  facet_wrap(~name, scales = "free")


# -------------------------------------------------------------------------

# CFA with acquiescence bias correction
# aquiescence factors
acq_factors <- items_overview %>%
  mutate(item_code = paste0("1*BFI", item_number)) %>% group_by(domain) %>% # acq loadings fixed to 1
  summarise(items = str_flatten(item_code, collapse = " + ")) %>%
  mutate(facet = "acq", op = "=~")
acq_factors

acq_orthogon <- items_overview %>%
  distinct(across(c(domain, facet))) %>%
  mutate(items = "0*acq", op = "~~")
acq_orthogon

# merge into domains
cfa_acq_models <- items_overview %>%
  mutate(item_code = paste0("BFI", item_number)) %>% # from number to item name
  group_by(domain, facet) %>%
  summarize(items = str_flatten(item_code, collapse = " + ")) %>%
  mutate(.after = facet, op = "=~") %>%
  bind_rows(acq_factors) %>%
  bind_rows(acq_orthogon) %>%
  unite("model", c(facet, op, items), sep = " ") %>% # make one big string form three cols
  summarise(model = str_flatten(model, "\n"))



# fit those models using ordered (polych matrix), and WLSMV (the defualt)
cfa_acq_fitted <- cfa_acq_models %>% mutate(fit = map(
  model, ~ cfa(.x, items %>% slice_sample(n = 1000),
               std.lv = TRUE, estimator = "WLSMV", ordered = T
  )
))

# fit indices
cfa_acq_fitted %>% transmute(domain,
                         chisq = map_dbl(fit, ~ .x %>% fitmeasures("chisq")),
                         df = map_dbl(fit, ~ .x %>% fitmeasures("df")),
                         cfi = map_dbl(fit, ~ .x %>% fitmeasures("cfi")),
                         tli = map_dbl(fit, ~ .x %>% fitmeasures("tli")),
                         rmsea = map_dbl(fit, ~ .x %>% fitmeasures("rmsea"))
)

# cannot converge???
# Error: Problem with `mutate()` input `chisq`.
# x lavaan ERROR: fit measures not available if model did not converge
# i Input `chisq` is `map_dbl(fit, ~.x %>% fitmeasures("chisq"))`.
# Run `rlang::last_error()` to see where the error occurred.
rlang::last_error()
#<error/dplyr:::mutate_error>
#  Problem with `mutate()` input `chisq`.
#x lavaan ERROR: fit measures not available if model did not converge
#i Input `chisq` is `map_dbl(fit, ~.x %>% fitmeasures("chisq"))`.
#Backtrace:
#  1. `%>%`(...)
#16. base::.handleSimpleError(...)
#17. dplyr:::h(simpleError(msg, call))
#Run `rlang::last_trace()` to see the full context.




# structure seesm OK, constrains too
semPlot::semPaths(cfa_acq_fitted$fit[[3]], whatLabels = "par")

# -------------------------
# CFA Extraversion (performs the best)

# single domain
E_SD <- '
E =~ i1 + i6 + i11 + i16 + i21 + i26 + i31 + i36 + i41 + i46 + i51 + i56'
fit_E_SD <- cfa(E_SD, BFI2, ordered = TRUE)
fitMeasures(fit_E_SD)

# 3 facets
E_FAC <- '
E_F1 =~ i1 + i16 + i31 + i46
E_F2 =~ i6 + i21 + i36 + i51
E_F3 =~ i11 + i26 + i41 + i56'
fit_E_FAC <- cfa(E_FAC, BFI2, ordered = TRUE)
fitMeasures(fit_E_FAC)

anova(fit_E_SD, fit_E_FAC)
BIC(fit_E_SD, fit_E_FAC)
AIC(fit_E_SD, fit_E_FAC)


# -------------------------
# Other analyses: IRT, multidimensional IRT w/ mirt

mirt_models <- items_overview %>%
  mutate(item_code = paste0("BFI", item_number)) %>% # from number to item name
  group_by(domain, facet) %>%
  summarize(items = str_flatten(item_code, collapse = " + ")) %>%
  mutate(.after = facet, op = "=") %>% # add operator
  unite("model", c(facet, op, items), sep = " ") %>% # make one big string form three cols
  summarise(model = str_flatten(model, "\n"))

mod1 <- mirt_models %>% filter(domain == "E") %>% pull(model) %>% mirt.model()
items1 <- mirt_models %>% filter(domain == "E") %>% pull(model) %>% str_extract_all("BFI\\d+") %>% unlist
mirt_E <- mirt::mirt(items[, items1], mod1)
summary(mirt_E)

plot(mirt_E)
itemfit(mirt_E)
M2(mirt_E)


# check SPSS file with provided data (for lavaan fits)

bfi2<- read.table("sampleRcode/BFI2/data_all15-26_acqui.csv", sep = ";", dec = ",", header = T)


# only items
orig_items <- bfi2 %>% select(matches("BFI\\d"))

# which items are reverse-coded?
orig_reversed <- orig_items %>% names %>% str_subset("r$") %>% parse_number() %>% sort
paper_reversed <- reversed %>% filter(is_reversed) %>% pull(item_number)

identical(orig_reversed, paper_reversed) # OK

# subset only final (i.e. all reversed, without raw items) for comparison
orig_items_recoded <- orig_items %>%
  select(-orig_reversed) %>%
  rename_with(~ str_remove(.x, "r$")) %>%
  relocate(paste0("BFI", seq(1, 60))) %>%
  mutate(across(where(is.integer), as.numeric))


# when both sorted by "public" ID, there are no differences
waldo::compare(
  items %>% add_column(ID = BFI2$ID) %>% arrange(ID),
  orig_items_recoded %>% add_column(ID = bfi2$ď.żid_all) %>% arrange(ID)
)


#--------------
colnames(BFI2)[grep("iE", colnames(BFI2))]
colnames(BFI2)[grep("iN", colnames(BFI2))]
colnames(BFI2)[grep("iO", colnames(BFI2))]
colnames(BFI2)[grep("iC", colnames(BFI2))]
colnames(BFI2)[grep("iA", colnames(BFI2))]

BFI2_model <- '
 E =~ iEscb01 + iEasr06 + iEenl11r + iEscb16r + iEasr21 + iEenl26r +
 iEscb31r + iEasr36r + iEenl41 + iEscb46 + iEasr51r + iEenl56
 N =~ iNanx04r + iNdep09r + iNemt14 + iNanx19 + iNdep24r + iNemt29r +
 iNanx34 + iNdep39 + iNemt44r + iNanx49r + iNdep54 + iNemt59
 O =~ iOaes05r + iOint10 + iOcrt15 + iOaes20 + iOint25r + iOcrt30r +
 iOaes35 + iOint40 + iOcrt45r + iOaes50r + iOint55r + iOcrt60
 C =~ iCorg03r + iCprd08r + iCrsp13 + iCorg18 + iCprd23r + iCrsp28r +
 iCorg33 + iCprd38 + iCrsp43 + iCorg48r + iCprd53 + iCrsp58r
 A =~ iAcmp02 + iArsp07 + iAtrs12r + iAcmp17r + iArsp22r + iAtrs27 +
 iAcmp32 + iArsp37r + iAtrs42r + iAcmp47r + iArsp52 + iAtrs57
'

# shorter model definition done programmatically and in base R
# (can be made prettier and shorter using tidyverse):
BFI2_model_shorter <- paste(lapply(
  c("E", "N", "O", "C", "A"),
  function(x) paste(x, "=~", paste(grep(paste0("i", x), colnames(BFI2), value = TRUE), collapse = " + "))
), collapse = "\n ")
# nice!

# show in console
cat(BFI2_model_shorter)

BFI2_model_without_superfluous_spaces <- "E =~ iEscb01 + iEasr06 + iEenl11r + iEscb16r + iEasr21 + iEenl26r + iEscb31r + iEasr36r + iEenl41 + iEscb46 + iEasr51r + iEenl56
 N =~ iNanx04r + iNdep09r + iNemt14 + iNanx19 + iNdep24r + iNemt29r + iNanx34 + iNdep39 + iNemt44r + iNanx49r + iNdep54 + iNemt59
 O =~ iOaes05r + iOint10 + iOcrt15 + iOaes20 + iOint25r + iOcrt30r + iOaes35 + iOint40 + iOcrt45r + iOaes50r + iOint55r + iOcrt60
 C =~ iCorg03r + iCprd08r + iCrsp13 + iCorg18 + iCprd23r + iCrsp28r + iCorg33 + iCprd38 + iCrsp43 + iCorg48r + iCprd53 + iCrsp58r
 A =~ iAcmp02 + iArsp07 + iAtrs12r + iAcmp17r + iArsp22r + iAtrs27 + iAcmp32 + iArsp37r + iAtrs42r + iAcmp47r + iArsp52 + iAtrs57"

# test
identical(BFI2_model_without_superfluous_spaces, BFI2_model_shorter)
# nice!

CFA_BFI2 <- lavaan::cfa(BFI2_model, data = BFI2[,5:64])
summary(CFA_BFI2, fit.measures = TRUE, standardized = TRUE)
semPlot::semPaths(CFA_BFI2)
#--------------

#--------------
BFI2E_model <- '
 scb =~ iEscb01 + iEscb16r + iEscb31r + iEscb46
 asr =~ iEasr06 + iEasr21 + iEasr36r + iEasr51r
 enl =~ iEenl11r + iEenl26r + iEenl41 + iEenl56
 '
CFA_BFI2E <- lavaan::cfa(BFI2E_model, data = BFI2[,grep("iE", colnames(BFI2))])
summary(CFA_BFI2E, fit.measures = TRUE, standardized = TRUE)
semPlot::semPaths(CFA_BFI2E)
#--------------

#--------------
BFI2E_model2 <- '
 # latent variable definitions
 scb =~ iEscb01 + iEscb16r + iEscb31r + iEscb46
 asr =~ iEasr06 + iEasr21 + iEasr36r + iEasr51r
 enl =~ iEenl11r + iEenl26r + iEenl41 + iEenl56

 # regressions
 scb ~ Gender
 asr ~ Gender
 enl ~ Gender

 # residual covariances
 iEscb01 ~~ iEenl56
 '
CFA_BFI2E2 <- lavaan::cfa(BFI2E_model2, data = BFI2)
summary(CFA_BFI2E2, fit.measures = TRUE,
        standardized = TRUE)
semPlot::semPaths(CFA_BFI2E2)
#--------------

#-----------------------------------------------------------------
# 3.4.3.2. Confirmatory factor analysis
#-----------------------------------------------------------------

#--------------
data(BFI2, package = "ShinyItemAnalysis")
head(BFI2, n = 2)
summary(BFI2)
#--------------

#--------------
model_EN <- 'E =~ i1 + i6 + i11 + i16 + i21 + i26 +
                  i31 + i36 + i41 + i46 + i51 + i56
             N =~ i4 + i9 + i14 + i19 + i24 + i29 +
                  i34 + i39 + i44 + i49 + i54 + i59'
fit_EN <- lavaan::cfa(model_EN, data = BFI2)
#--------------

#--------------
lavaan::parTable(fit_EN)
##    id lhs op rhs user block group free ustart exo label plabel start ...
## 1   1   E =~  i1    1     1     1    0      1   0         .p1. 1.000 ...
## 2   2   E =~  i6    1     1     1    1     NA   0         .p2. 0.813 ...
## ...
## 51 51   E ~~   N    0     1     1   49     NA   0        .p51. 0.000 ...
summary(fit_EN)
summary(fit_EN, fit.measures = TRUE, standardized = TRUE)
#--------------

#--------------
parameterEstimates(fit_EN)
##    lhs op rhs    est    se       z pvalue ci.lower ci.upper
## 1    E =~  i1  1.000 0.000      NA     NA    1.000    1.000
## 2    E =~  i6  0.969 0.041  23.611      0    0.889    1.049
## ...
## 13   N =~  i4  1.000 0.000      NA     NA    1.000    1.000
## 14   N =~  i9  0.833 0.039  21.509      0    0.757    0.909
## ...
## 25  i1 ~~  i1  0.617 0.023  26.337      0    0.571    0.663
## 26  i6 ~~  i6  0.647 0.024  26.664      0    0.599    0.694
## ...
## 49   E ~~   E  0.489 0.033  14.870      0    0.425    0.554
## 50   N ~~   N  0.587 0.039  15.191      0    0.512    0.663
## 51   E ~~   N -0.196 0.017 -11.514      0   -0.229   -0.162

parameterEstimates(fit_EN, ci = FALSE, standardized = TRUE)
##    lhs op rhs    est    se       z pvalue std.lv std.all std.nox
## 1    E =~  i1  1.000 0.000      NA     NA  0.699   0.665   0.665
## 2    E =~  i6  0.969 0.041  23.611      0  0.678   0.644   0.644
## 3    E =~ i11  0.470 0.041  11.423      0  0.329   0.296   0.296
## 4    E =~ i16  1.420 0.052  27.066      0  0.993   0.757   0.757
## 5    E =~ i21  1.150 0.047  24.647      0  0.804   0.677   0.677
## 6    E =~ i26  0.510 0.034  14.790      0  0.357   0.387   0.387
## 7    E =~ i31  1.265 0.050  25.257      0  0.885   0.697   0.697
## 8    E =~ i36  0.547 0.039  14.137      0  0.382   0.369   0.369
## 9    E =~ i41  0.813 0.039  20.583      0  0.568   0.552   0.552
## 10   E =~ i46  1.126 0.045  25.099      0  0.787   0.692   0.692
## 11   E =~ i51  1.052 0.045  23.559      0  0.736   0.643   0.643
## 12   E =~ i56  0.669 0.036  18.468      0  0.468   0.491   0.491
## 13   N =~  i4  1.000 0.000      NA     NA  0.766   0.672   0.672
## 14   N =~  i9  0.833 0.039  21.509      0  0.639   0.569   0.569
## 15   N =~ i14  1.069 0.044  24.397      0  0.819   0.653   0.653
## 16   N =~ i19  0.817 0.037  22.095      0  0.626   0.585   0.585
## 17   N =~ i24  0.807 0.039  20.685      0  0.618   0.545   0.545
## 18   N =~ i29  1.171 0.042  27.874      0  0.898   0.761   0.761
## 19   N =~ i34  0.828 0.038  21.852      0  0.634   0.578   0.578
## 20   N =~ i39  1.155 0.043  26.963      0  0.885   0.732   0.732
## 21   N =~ i44  0.730 0.038  19.372      0  0.560   0.508   0.508
## 22   N =~ i49  0.836 0.039  21.611      0  0.641   0.571   0.571
## 23   N =~ i54  1.176 0.044  26.900      0  0.901   0.730   0.730
## 24   N =~ i59  0.974 0.041  23.968      0  0.747   0.641   0.641
#--------------

#--------------
model_ENs <- 'E =~ NA*i1 + i6 + i11 + i16 + i21 + i26 +
                   i31 + i36 + i41 + i46 + i51 + i56
              N =~ NA*i4 + i9 + i14 + i19 + i24 + i29 +
                   i34 + i39 + i44 + i49 + i54 + i59
              E ~~ 1*E
              N ~~ 1*N'
fit_ENs <- lavaan::cfa(model_ENs, data = BFI2)
parameterEstimates(fit_ENs, ci = FALSE, standardized = TRUE)
##    lhs op rhs    est    se       z pvalue std.lv std.all std.nox
## 1    E =~  i1  0.699 0.024  29.740      0  0.699   0.665   0.665
## 2    E =~  i6  0.678 0.024  28.552      0  0.678   0.644   0.644
## ...
#--------------

#--------------
lavaan::inspect(fit_EN)
## $lambda
##      E  N
## i1   0  0
## i6   1  0
## i11  2  0
## ...
## $theta
##     i1 i6 i11 i16 i21 i26 i31 i36 i41 i46 i51 i56 i4 i9 i14 i19 i24 i29 i34 i39 i44 i49 i54 i59
## i1  23
## i6   0 24
## i11  0  0 25
## ...
## $psi
##    E  N
## E 47
## N 49 48
lavaan::lavInspect(fit_EN, what = "est")$theta
lavaan::lavInspect(fit_EN, what = "est")$lambda
lavaan::lavInspect(fit_EN, what = "std")$lambda
lavaan::lavInspect(fit_EN, what = "est")$psi
lavaan::lavInspect(fit_EN, what = "std")$psi
#--------------

#--------------
semPlot::semPaths(fit_EN, what = "std.est", rotation = 4, edge.label.cex = 0.6)
semPlot::semPaths(fit_ENs, what = "est", rotation = 4, edge.label.cex = 0.6)
#--------------

#-------------- Save plot
CairoPNG(file = "figures/chapter3/validity_semPlot_cfa.png", width = 6, height = 4, dpi = 300, pointsize = 12, unit = "in")
semPlot::semPaths(fit_ENs, what = "est", rotation = 4, edge.label.cex = 0.7)
dev.off()
#--------------

#--------------
FS <- predict(fit_EN)
head(FS, n = 3)
##            E       N
## [1,]  0.5944  0.2344
## [2,]  0.6298 -0.6944
## [3,] -1.4920  1.6955
#--------------

#-----------------------------------------------------------------
# Hierarchical CFA
#-----------------------------------------------------------------

#--------------
model_EN_hier <- 'Escb =~ i1 + i16 + i31 + i46
                  Easr =~ i6 + i21 + i36 + i51
                  Eenl =~ i11 + i26 + i41 + i56
                  Nanx =~ i4 + i19 + i34 + i49
                  Ndep =~ i9 + i24 + i39 + i54
                  Nemt =~ i14 + i29 + i44 + i59
                  E =~ Escb + Easr + Eenl
                  N =~ Nanx + Ndep + Nemt'
fit_EN_hier <- lavaan::cfa(model_EN_hier, data = BFI2)
#--------------

#--------------
summary(fit_EN_hier, fit.measures = TRUE, standardized = TRUE)
lavaan::parTable(fit_EN_hier)
lavaan::parameterEstimates(fit_EN_hier)
semPlot::semPaths(fit_EN_hier, what = "std.est", rotation = 4, edge.label.cex = 1.5)
#--------------

#-------------- Save plot
CairoPNG(file = "figures/chapter3/validity_semPlot_cfaH.png", width = 6, height = 4, dpi = 300, pointsize = 12, unit = "in")
semPlot::semPaths(fit_EN_hier, what = "est", rotation = 4, edge.label.cex = 0.7)
dev.off()
#--------------

#--------------
FSh <- predict(fit_EN_hier)
head(FSh, n = 3)
##         Escb    Easr    Eenl    Nanx    Ndep    Nemt       E         N
## [1,]  0.4401  0.6603  0.5546 -0.1261 -0.1289  0.8747  0.5131  0.004086
## [2,]  0.7308  0.4984  0.3823 -0.5643 -0.7703 -0.5638  0.6025 -0.683164
## [3,] -1.5153 -1.4035 -0.5333  1.7516  1.3825  1.4776 -1.2671  1.667109
#--------------

#--------------
fitMeasures(fit_EN, c("cfi", "tli", "rmsea", "bic"))
##      cfi    rmsea      bic
## 7.78e-01 9.20e-02 1.15e+05
fitMeasures(fit_EN_hier, c("cfi", "tli", "rmsea", "bic"))
##       cfi     rmsea       bic
## 8.800e-01 6.900e-02 1.133e+05
#--------------
