library(mirt)
library(ShinyItemAnalysis)
library(msm)
library(nnet)

# loading data
data(HCItest, HCI, package = "ShinyItemAnalysis")
HCInumeric <- HCItest[, 1:20]
HCInumeric[] <- sapply(HCInumeric, as.numeric)

# model
fit <- mirt(HCInumeric, model = 1, itemtype = "nominal", SE = TRUE)

# item response curves
plot(fit, type = "trace")
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
# mirt default model
coef(fit, simplify = TRUE) # intercept-slope parametrization ak(a1*theta) + dk, ak0=0, ak(K-1)=K-1, d0=0
coef(fit, printSE = TRUE) # SE printed
# Notes:
# see mirt package documentation (PDF)https://cran.r-project.org/web/packages/mirt/mirt.pdf, p. 111
# This parametrization is useful for generalizations to multidimensional models
# mirt PDF, p. 111: More specific scoring function may be included by passing a suitable list or matrices to the gpcm_mats input argument.

# Bock's original parametrization
coef(fit, IRTpars = TRUE, simplify = TRUE)
coef(fit, IRTpars = TRUE, printSE = TRUE) # Intercept-slope parametrization, SE not printed
# Notes:
# See Eq. (3.3) in https://www.routledgehandbooks.com/doi/10.4324/9780203861264.ch3
# This is intercept-slope parametrization ak*theta + dk, with additional constraints Sum(ak) = 0, Sum(dk) = 0

# Other cases (to be) implemented in SIA:
# Different constraints (e.g., zero parameter for correct answer)... see below
# "IRT parametrization" ak(theta - b) needs to be implemented manually... see below
# SE calculation for intercept-slope parametrization and for IRT parametrization... see below

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(HCI[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)


# ---------------------------------------------
# code from sc/bock, not sure if needed (now solved at very bottom), moved here
# ---------------------------------------------
# intercept/slope parametrization --> IRT parametriaztion by hand
tab_a <- apply(coef(fit_NRM_mirt, IRTpars = FALSE, simplify = TRUE)$items, 1,
               function(x) x[1] * x[c(2:5, 10)])
tab_a <- apply(tab_a, 2, function(x) scale(x, center = TRUE, scale = FALSE))
tab_a

tab_c <- apply(coef(fit_NRM_mirt, IRTpars = FALSE, simplify = TRUE)$items[, c(6:9, 11)], 1,
               function(x) scale(x, center = TRUE, scale = FALSE))
tab_c
# ---------------

# ---------------------------------------------
# INTERCEPT/SLOPE PARAMETRIZATION IN APP
# ---------------------------------------------
data("HCIkey")
data("HCItest")

# RELEVELING DATA
data <- HCItest[, 1:20]
key <- unlist(HCIkey)

m <- ncol(data)

levels_data_original <- lapply(1:m, function(i) levels(factor(unlist(data[, i]))))
lev <- c(unlist(levels_data_original), levels(key)) # all levels in data and key
lev <- unique(lev) # all unique levels
lev_num <- as.numeric(as.factor(lev)) - 1 # change them to numbers

# new numeric levels for key
levels_key_num <- sapply(
  1:length(levels(key)),
  function(i) lev_num[levels(key)[i] == lev]
)
# new numeric levels for dataset
levels_data_num <- lapply(1:m, function(i) {
  sapply(
    1:length(levels(factor(unlist(data[, i])))),
    function(j) lev_num[levels(factor(unlist(data[, i])))[j] == lev]
  )
})

# creating new numeric key
key_num <- key
levels(key_num) <- levels_key_num
key_num <- as.numeric(paste(key_num))

# creating new numeric dataset
data_num <- data.frame(data)
for (i in 1:m) {
  levels(data_num[, i]) <- levels_data_num[[i]]
  data_num[, i] <- as.numeric(paste(data_num[, i]))
}

# FITTING MODEL, SETTING STARTING VALUES AND CONSTRAINTS

# starting values
sv <- mirt(data_num, 1, "nominal", pars = "values", verbose = FALSE, SE = TRUE)

# starting values of discrimination for distractors need to be lower than
# for the correct answer (fixed at 0, see below)
sv$value[grepl("ak", sv$name)] <- -0.5
sv$est[grepl("ak", sv$name)] <- TRUE

# we don't want to estimate ak parameter for the correct answer
# they are fixed at 0, the same for parameters d
for (i in 1:m) {
  item_name <- colnames(data_num)[i]
  tmp <- sv[sv$item == item_name, ]
  tmp$est <- TRUE
  tmp[tmp$name == paste0("ak", key_num[i]), "value"] <- 0
  tmp[tmp$name == paste0("ak", key_num[i]), "est"] <- FALSE
  tmp[tmp$name == paste0("d", key_num[i]), "value"] <- 0
  tmp[tmp$name == paste0("d", key_num[i]), "est"] <- FALSE
  sv[sv$item == item_name, ] <- tmp
}

# we don't want to estimate a1 parameter for any item as it multiplies all
# category specific slopes
sv[sv$name == "a1", "value"] <- 1
sv[sv$name == "a1", "est"] <- FALSE
sv
# group    item   class name parnum value lbound ubound   est prior.type prior_1 prior_2
# 1    all  Item.1 nominal   a1      1   1.0   -Inf    Inf FALSE       none     NaN     NaN
# 2    all  Item.1 nominal  ak0      2  -0.5   -Inf    Inf  TRUE       none     NaN     NaN
# 3    all  Item.1 nominal  ak1      3  -0.5   -Inf    Inf  TRUE       none     NaN     NaN
# 4    all  Item.1 nominal  ak2      4  -0.5   -Inf    Inf  TRUE       none     NaN     NaN
# 5    all  Item.1 nominal  ak3      5  -0.5   -Inf    Inf  TRUE       none     NaN     NaN
# 6    all  Item.1 nominal   d0      6   0.0   -Inf    Inf  TRUE       none     NaN     NaN
# 7    all  Item.1 nominal   d1      7   0.0   -Inf    Inf  TRUE       none     NaN     NaN
# 8    all  Item.1 nominal   d2      8   0.0   -Inf    Inf  TRUE       none     NaN     NaN
# 9    all  Item.1 nominal   d3      9   0.0   -Inf    Inf  TRUE       none     NaN     NaN
# 10   all  Item.2 nominal   a1     10   1.0   -Inf    Inf FALSE       none     NaN     NaN
# 11   all  Item.2 nominal  ak0     11  -0.5   -Inf    Inf  TRUE       none     NaN     NaN
# 12   all  Item.2 nominal  ak1     12  -0.5   -Inf    Inf  TRUE       none     NaN     NaN
# 13   all  Item.2 nominal  ak2     13  -0.5   -Inf    Inf  TRUE       none     NaN     NaN
# 14   all  Item.2 nominal   d0     14   0.0   -Inf    Inf  TRUE       none     NaN     NaN
# 15   all  Item.2 nominal   d1     15   0.0   -Inf    Inf  TRUE       none     NaN     NaN
# 16   all  Item.2 nominal   d2     16   0.0   -Inf    Inf  TRUE       none     NaN     NaN

fit <- mirt(data_num, model = 1, itemtype = "nominal", pars = sv, SE = TRUE)
coef(fit, simplify = TRUE)
plot(fit, type = "trace")
# $items
#         a1    ak0    ak1    ak2    ak3     d0     d1     d2     d3    ak4     d4
# Item.1   1 -1.374 -0.407 -0.997  0.000 -3.315 -2.029 -1.632  0.000     NA     NA
# Item.2   1 -0.984  0.000 -0.445     NA -1.897  0.000 -2.039     NA     NA     NA
# Item.3   1  0.000 -2.090 -1.363     NA  0.000 -3.716 -2.805     NA     NA     NA
# Item.4   1 -2.963 -2.049 -0.252  0.000 -5.397 -3.774  0.320  0.000     NA     NA
# Item.5   1 -0.805  0.000 -0.852 -0.669 -1.440  0.000 -1.091 -0.336     NA     NA
# Item.6   1 -1.592 -0.908  0.000     NA -1.647  0.549  0.000     NA     NA     NA
# Item.7   1 -0.537 -0.190  0.000     NA -1.673 -0.463  0.000     NA     NA     NA
# Item.8   1 -1.036 -1.180  0.000     NA -1.611 -1.994  0.000     NA     NA     NA
# Item.9   1 -0.334 -1.171 -2.877  0.000  0.115 -2.026 -5.367  0.000     NA     NA
# Item.10  1  0.000 -0.742 -0.798     NA  0.000 -1.381 -1.397     NA     NA     NA
# Item.11  1  0.000 -1.480 -0.735     NA  0.000 -2.889 -1.822     NA     NA     NA
# Item.12  1 -0.945 -1.078 -1.252  0.000 -1.830 -3.606 -2.838  0.000 -0.731 -0.790
# Item.13  1  0.000 -1.517 -1.230 -0.972  0.000 -2.442 -1.666 -1.270     NA     NA
# Item.14  1  0.000 -1.121 -1.004 -1.586  0.000 -2.942 -2.066 -2.993     NA     NA
# Item.15  1 -0.978 -1.219  0.000 -0.487 -1.299 -0.756  0.000 -0.700     NA     NA
# Item.16  1  0.000 -1.098 -1.583 -0.756  0.000 -1.003 -2.385 -2.287     NA     NA
# Item.17  1  0.077 -0.294  0.000 -0.124  0.037 -0.126  0.000 -0.855     NA     NA
# Item.18  1 -1.810 -1.866  0.000 -1.953 -3.235 -2.851  0.000 -3.892     NA     NA
# Item.19  1 -1.352 -1.686  0.000 -1.117 -2.932 -3.152  0.000 -3.081 -1.597 -3.634
# Item.20  1 -1.627 -1.727 -0.766  0.000 -4.550 -3.370 -1.331  0.000     NA     NA


# mirt default parametrization with new constraints (a1 fixed to 1, correct option not estimated)
coef(fit, printSE = TRUE)

# intercept-slope parametrization by Bock
# Note: SE is the same
coef(fit, IRTpars = TRUE) # centered intercept-slope par.
# not centered intercept-slope par.
c <- as.data.frame(coef(fit, simplify = TRUE))
c <- c[, c(2:(length(c) - 2))]
c <- c[, order(names(c))] # alphabetical order
c #not centered

###################################
#IRT parametrization a*(theta - b)
n <- ncol(c)/2
c[(n+1):ncol(c)] <- t(apply(c, 1, function (x) - x[(n+1):ncol(c)]/x[1:n]))
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
c[is.nan(c)] <- 0
name1 <- c(rep("a",n),rep("b",n))
name2 <- rep(1:n,2)
names(c) <- paste(name1, name2, sep = "")
c

#item 5
plot(fit, type = "trace", which.items = 5)
key_num[5] + 1
c[5,"b1"]
c[5,"b3"]
c[5,"b4"]


###  delta method for SE
par <- extract.mirt(fit, 'parvec')
vcov <- vcov(fit)
colnames(vcov)
mod2values(fit)


delta_method_NRM <- function(x) {

ind <- mod2values(fit)$parnum[mod2values(fit)$item == paste("Item.",x,sep="")
                       & mod2values(fit)$est == TRUE]
ind <- paste0('\\b',ind,'\\b')
i <- which(grepl(paste(paste(".",ind,sep=""), collapse="|"), colnames(vcov)))

mean <- par[i]
v <- vcov[i, i]

#prepare formula
a <- paste(rep("x",length(i)/2),1:(length(i)/2),sep="")
b <- paste(rep("x",length(i)/2),((length(i)/2)+1):length(i),sep="")
b <- paste("-",b,"/",a,sep="")
formula <- c(a,b)
formula <- paste("~",formula)
formula <- sapply(formula,as.formula)
SE <- deltamethod(formula, mean, v)

name1 <- c(rep("a",length(i)/2+1),rep("b",length(i)/2+1))
name2 <- rep(1:((length(i)/2)+1),2)
names_SE <- paste(name1,name2,sep="")
names_SE <- names_SE[-c(key_num[x]+1,(key_num[x]+1)+(length(i)+2)/2)]
names(SE) <- names_SE
SE

df <- c[x,]
df <- df[ , colSums(is.na(df)) == 0]
df <- rbind(df,rep(NA,length(i)))
df[2,match(names(SE), names(df))] <- SE
df
rownames(df)[2] <- "SE"

print(df, digits = 4)
}

#item1
delta_method_NRM(1)
coef(fit,printSE = TRUE)$Item.1

#item 13
delta_method_NRM(13)
coef(fit,printSE = TRUE)$Item.13


##############################
# check with multinomial model

data(HCItest, HCIkey, package = "ShinyItemAnalysis")
key <- unlist(HCIkey)
zscore <- scale(rowSums(HCI[, 1:20])) # Z-score

mult <- function(x) {
# re-leveling item
HCItest[, x] <- relevel(HCItest[, x], ref = paste(key[x]))
#multinomial model
fit.mult <<- multinom(HCItest[, x] ~ zscore)
}

mult(13)
coef(fit.mult)
sqrt(diag(vcov(fit.mult)))
coef(fit,printSE = TRUE)$Item.13

# IRT parametrization
# delta method
subst_vcov <- function(vcov, cat) {
  ind <- grep(cat, colnames(vcov))
  vcov[ind, ind]
}
se <- t(sapply(
  rownames(coef(fit.mult)),
  function(.x) {
    vcov_subset <- subst_vcov(vcov(fit.mult), .x)
    msm::deltamethod(
      list(~ -x1 / x2, ~x2),
      mean = coef(fit.mult)[.x, ],
      cov = vcov_subset,
      ses = TRUE
    )
  }
))

# estimates and SE in IRT parametrization
cbind(-coef(fit.mult)[, 1] / coef(fit.mult)[, 2], se[, 1], coef(fit.mult)[, 2], se[, 2])

delta_method_NRM(13)

# plot of estimated category probabilities
plotMultinomial(fit.mult, zscore, matching.name = "Standardized total score")
