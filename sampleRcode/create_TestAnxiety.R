# Data comes from supplementary files to Ch7 of the
# Analysis of Multivariate Social Science Data (Bartholomew, Steele, Moustaki, Galbrath)

n <-  max(count.fields("SampleRcode/data/anxiety.txt"))
TestAnxietyCor <- data.matrix(read.table("SampleRcode/data/anxiety.txt", fill=TRUE, col.names=1:n))

# fill in the upper diagonal to obtain the symmetric matrix
TestAnxietyCor[upper.tri(TestAnxietyCor)] <- 0
TestAnxietyCor <- TestAnxietyCor + t(TestAnxietyCor) - diag(diag(TestAnxietyCor))

colnames(TestAnxietyCor) <- paste0("i", 1:20)
rownames(TestAnxietyCor) <- paste0("i", 1:20)
round(TestAnxietyCor,2)

save(TestAnxietyCor, file = "data/TestAnxietyCor.rda")

#--------------------
# Sample R code

# corrplot
ShinyItemAnalysis::plot_corr(TestAnxietyCor, cor = "none") # change wording of the cor argument?
corrplot::corrplot(TestAnxietyCor)

fa_parallel(TestAnxietyCor, cor = "none", n_obs = 335) # change wording of the cor argument?
plot(fa_parallel(TestAnxietyCor, cor = "none", n_obs = 335))
psych::fa.parallel(TestAnxietyCor) # plots seem to look differently, set.seed?

#--------------
# FA unrotated:
FA2_tAnxiety <- psych::fa(TestAnxietyCor, nfactors = 2,
                          n.obs = 335, rotate = "none")
FA2_tAnxiety
## Factor Analysis using method =  minres
## Call: psych::fa(r = TestAnxietyCor, nfactors = 2, n.obs = 335, rotate = "none")
## Standardized loadings (pattern matrix) based upon correlation matrix
##      MR1   MR2   h2   u2 com
## i1  0.62 -0.08 0.39 0.61 1.0
## i2  0.62 -0.17 0.41 0.59 1.1
## i3  0.54  0.24 0.35 0.65 1.4
## i4  0.65  0.09 0.44 0.56 1.0
## i5  0.52  0.49 0.50 0.50 2.0
## ...
#--------------

#--------------
# communalities are sum of squared loadings:
FA2_tAnxiety$communalities
apply(FA2_tAnxiety$loadings^2, 1, sum)
#--------------

#--------------
plot(FA2_tAnxiety, xlim = c(-.5,1), ylim = c(-.5, 1))

# label unrotated axes
# TODO: please move i2 and i1 to lower index
text(x = 0.95, y = -0.05, expression(paste(hat(alpha), "i1")))
text(x = -0.05, y = 0.95, expression(paste(hat(alpha), "i2")))
#--------------

# FA oblimin rotation
FA2_tAnxiety_obl <- psych::fa(TestAnxietyCor, nfactors = 2,
                              n.obs = 335, rotate = "oblimin")
print(FA2_tAnxiety_obl$loadings, cutoff = 0.4)
## Loadings:
##        MR1    MR2
## i1   0.565
## i2   0.662
## i3          0.473
## i4   0.400
## i5          0.795
## ...
#--------------
FA2_tAnxiety_obl$rot.mat
##         [,1]   [,2]
## [1,]  0.7725 0.3022_obl
## [2,] -1.0996 1.3094
FA2_tAnxiety$loadings %*% FA2_tAnxiety_obl$rot.mat
#--------------

#--------------
FA2_tAnxiety_obl$rot.mat
solve(FA2_tAnxiety_obl$rot.mat)

# rotated oblique axes
lines(c(0, solve(FA2_tAnxiety_obl$rot.mat)[1,1]), c(0,solve(FA2_tAnxiety_obl$rot.mat)[1,2]), lty = 3)
lines(c(0, solve(FA2_tAnxiety_obl$rot.mat)[2,1]), c(0,solve(FA2_tAnxiety_obl$rot.mat)[2,2]), lty = 3)

# label rotated axes
# TODO: please move i2r and i1r to lower index
text(x = 0.75, y = 0.6, labels = expression(paste(hat(alpha), "i2r")))
text(x = 0.9, y = - 0.25, labels = expression(paste(hat(alpha), "i1r")))

#--------------

#-------------- Save plot
CairoPNG(file = "validity_EFA_tAnxiety_unrot.png", width = 4, height = 4, dpi = 300, pointsize = 10, unit = "in")
par(mgp = c(2.1, 0.7, 0), mar = c(3.4, 3.4, 1.3, 0.9), cex.axis = 1.2,
    cex.lab = 1.2, ann = FALSE, lwd = 0.6)
plot(FA2_tAnxiety, xlim = c(-.5,1), ylim = c(-.5, 1))
text(x = 0.95, y = -0.05, expression(paste(hat(alpha), "i1")))
text(x = -0.05, y = 0.95, expression(paste(hat(alpha), "i2")))
lines(c(0, solve(FA2_tAnxiety_obl$rot.mat)[1,1]), c(0,solve(FA2_tAnxiety_obl$rot.mat)[1,2]), lty = 3)
lines(c(0, solve(FA2_tAnxiety_obl$rot.mat)[2,1]), c(0,solve(FA2_tAnxiety_obl$rot.mat)[2,2]), lty = 3)
text(x = 0.75, y = 0.6, labels = expression(paste(hat(alpha), "i2r")))
text(x = 0.9, y = - 0.25, labels = expression(paste(hat(alpha), "i1r")))
dev.off()
#--------------

#--------------
# rotated loadings
# points(loadings(FA2_tAnxiety_obl), pch = 16, col = "grey")
plot(FA2_tAnxiety_obl, xlim = c(-.5,1), ylim = c(-.5, 1))
text(x = 0.95, y = -0.05, expression(paste(hat(alpha), "i1r")))
text(x = -0.07, y = 0.95, expression(paste(hat(alpha), "i2r")))
# TODO: please move i2r and i1r to lower index
#--------------

#-------------- Save plot
CairoPNG(file = "figures/chapter3/validity_EFA_tAnxiety_rot.png", width = 4, height = 4, dpi = 300, pointsize = 10, unit = "in")
par(mgp = c(2.1, 0.7, 0), mar = c(3.4, 3.4, 1.3, 0.9), cex.axis = 1.2,
    cex.lab = 1.2, ann = FALSE, lwd = 0.6)
plot(FA2_tAnxiety_obl, xlim = c(-.5,1), ylim = c(-.5, 1))
text(x = 0.95, y = -0.05, expression(paste(hat(alpha), "i1r")))
text(x = -0.07, y = 0.95, expression(paste(hat(alpha), "i2r")))
dev.off()
#--------------

#--------------
# with factanal() and GPArotation()
library(GPArotation)
?GPArotation::rotations
(FA2b_tAnxiety <- factanal(covmat = TestAnxietyCor, factors = 2,
                           rotation = "none", n.obs = 335))
(FA2b_tAnxiety_obl <- factanal(covmat = TestAnxietyCor, factors = 2,
                               rotation = "oblimin", n.obs = 335))
update(FA2b_tAnxiety, rotation = "oblimin")
#--------------

library(lavaan)
#--------------
print(FA2_tAnxiety_obl$loadings, cutoff = 0.4)
tAnxiety_model <- '
 physio =~ i1 + i2 + i4 + i8 + i9 + i10 + i11 + i12 + i13 + i15 +
           i18 + i19 + i20
 psycho =~ i3 + i5 + i6 + i7 + i14 + i17'
CFA_tAnxiety <- lavaan::cfa(tAnxiety_model, sample.cov = TestAnxietyCor,
                            sample.nobs = 335)
semPlot::semPaths(CFA_tAnxiety)
#--------------
