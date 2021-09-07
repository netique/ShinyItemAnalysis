bfi2<- read.table("sampleRcode/BFI2/data_all15-26_acqui_CFA.csv", sep = ";", dec = ",", header = T)
bfi2<- read.table("sampleRcode/BFI2/data_all15-26_acqui.csv", sep = ";", dec = ",", header = T)

library(lavaan)
library(psych)


E_SD <- '
E =~ BFI1 + BFI6 + BFI11r + BFI16r + BFI21 + BFI26r + BFI31r + BFI36r + BFI41 + BFI46 + BFI51r + BFI56'
fit_E_SD <- cfa(E_SD,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_E_SD)


E_SD_AQ <- '
E =~ BFI1 + BFI6 + BFI11 + BFI16 + BFI21 + BFI26 + BFI31 + BFI36 + BFI41 + BFI46 + BFI51 + BFI56
AQ =~ BFI1 + 1*BFI6 + 1*BFI11 + 1*BFI16 + 1*BFI21 + 1*BFI26 + 1*BFI31 + 1*BFI36 + 1*BFI41 + 1*BFI46 + 1*BFI51 + 1*BFI56
E~~0*AQ'
fit_E_SD_AQ <- cfa(E_SD_AQ,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_E_SD_AQ)


E_PN <- '
E_P =~ BFI1 + BFI6 + BFI21 + BFI41 + BFI46 + BFI56
E_N =~ BFI11r + BFI16r + BFI26r + BFI31r + BFI36r + BFI51r'
fit_E_PN <- cfa(E_PN,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_E_PN)


E_FAC <- '
E_F1 =~ BFI1 + BFI16r + BFI31r + BFI46
E_F2 =~ BFI6 + BFI21 + BFI36r + BFI51r
E_F3 =~ BFI11r + BFI26r + BFI41 + BFI56'
fit_E_FAC <- cfa(E_FAC,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_E_FAC)


E_FAC_AQ <- '
E_F1 =~ BFI1 + BFI16 + BFI31 + BFI46
E_F2 =~ BFI6 + BFI21 + BFI36 + BFI51
E_F3 =~ BFI11 + BFI26 + BFI41 + BFI56
AQ =~ 1*BFI1 + 1*BFI6 + 1*BFI11 + 1*BFI16 + 1*BFI21 + 1*BFI26 + 1*BFI31 + 1*BFI36 + 1*BFI41 + 1*BFI46 + 1*BFI51 + 1*BFI56
E_F1~~0*AQ
E_F2~~0*AQ
E_F3~~0*AQ'
fit_E_FAC_AQ <- cfa(E_FAC_AQ,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_E_FAC_AQ)




A_SD <- '
A =~ BFI2 + BFI7 + BFI12 + BFI17 + BFI22 + BFI27 + BFI32 + BFI37 + BFI42 + BFI47 + BFI52 + BFI57'
fit_A_SD <- cfa(A_SD,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_A_SD)


A_SD_AQ <- '
A =~ BFI2 + BFI7 + BFI12 + BFI17 + BFI22 + BFI27 + BFI32 + BFI37 + BFI42 + BFI47 + BFI52 + BFI57
AQ =~ BFI2 + 1*BFI7 + 1*BFI12 + 1*BFI17 + 1*BFI22 + 1*BFI27 + 1*BFI32 + 1*BFI37 + 1*BFI42 + 1*BFI47 + 1*BFI52 + 1*BFI57
A~~0*AQ'
fit_A_SD_AQ <- cfa(A_SD_AQ,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_A_SD_AQ)


A_PN <- '
A_P =~ BFI2 + BFI7 + BFI27 + BFI32 + BFI52 + BFI57
A_N =~ BFI12r + BFI17r + BFI22r + BFI37r + BFI42r + BFI47r'
fit_A_PN <- cfa(A_PN,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_A_PN)


A_FAC <- '
A_F1 =~ BFI2 + BFI17r + BFI32 + BFI47r
A_F2 =~ BFI7 + BFI22r + BFI37r + BFI52
A_F3 =~ BFI12r + BFI27 + BFI42r + BFI57'
fit_A_FAC <- cfa(A_FAC,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_A_FAC)


A_FAC_AQ <- '
A_F1 =~ BFI2 + BFI17 + BFI32 + BFI47
A_F2 =~ BFI7 + BFI22 + BFI37 + BFI52
A_F3 =~ BFI12 + BFI27 + BFI42 + BFI57
AQ =~ 1*BFI2 + 1*BFI7 + 1*BFI12 + 1*BFI17 + 1*BFI22 + 1*BFI27 + 1*BFI32 + 1*BFI37 + 1*BFI42 + 1*BFI47 + 1*BFI52 + 1*BFI57
A_F1~~0*AQ
A_F2~~0*AQ
A_F3~~0*AQ'
fit_A_FAC_AQ <- cfa(A_FAC_AQ,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_A_FAC_AQ)







C_SD <- '
C =~ BFI3r + BFI8r + BFI13 + BFI18 + BFI23r + BFI28r + BFI33 + BFI38 + BFI43 + BFI48r + BFI53 + BFI58r'
fit_C_SD <- cfa(C_SD,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_C_SD)


C_SD_AQ <- '
C =~ BFI3 + BFI8 + BFI13 + BFI18 + BFI23 + BFI28 + BFI33 + BFI38 + BFI43 + BFI48 + BFI53 + BFI58
AQ =~ 1*BFI3 + 1*BFI8 + 1*BFI13 + 1*BFI18 + 1*BFI23 + 1*BFI28 + 1*BFI33 + 1*BFI38 + 1*BFI43 + 1*BFI48 + 1*BFI53 + 1*BFI58
C~~0*AQ'
fit_C_SD_AQ <- cfa(C_SD_AQ,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_C_SD_AQ)


C_PN <- '
C_P =~  BFI13 + BFI18 + BFI33 + BFI38 + BFI43 + BFI53
C_N =~ BFI3r + BFI8r + BFI23r + BFI28r + BFI48r + BFI58r'
fit_C_PN <- cfa(C_PN,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_C_PN)


C_FAC <- '
C_F1 =~  BFI3r + BFI18 + BFI33 + BFI48r
C_F2 =~  BFI8r + BFI23r + BFI38 + BFI53
C_F3 =~ BFI13 + BFI28r + BFI43 + BFI58r'
fit_C_FAC <- cfa(C_FAC,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_C_FAC)


C_FAC_AQ <- '
C_F1 =~   BFI3 + BFI18 + BFI33 + BFI48
C_F2 =~ BFI8 + BFI23 + BFI38 + BFI53
C_F3 =~ BFI13 + BFI28 + BFI43 + BFI58
AQ =~ 1*BFI3 + 1*BFI8 + 1*BFI13 + 1*BFI18 + 1*BFI23 + 1*BFI28 + 1*BFI33 + 1*BFI38 + 1*BFI43 + 1*BFI48 + 1*BFI53 + 1*BFI58
C_F1~~0*AQ
C_F2~~0*AQ
C_F3~~0*AQ'
fit_C_FAC_AQ <- cfa(C_FAC_AQ,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_C_FAC_AQ)









NE_SD <- '
NE =~ BFI4r + BFI9r + BFI14 + BFI19 + BFI24r + BFI29r + BFI34 + BFI39 + BFI44r + BFI49r + BFI54 + BFI59'
fit_NE_SD <- cfa(NE_SD,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_NE_SD)


NE_SD_AQ <- '
NE =~ BFI4 + BFI9 + BFI14 + BFI19 + BFI24 + BFI29 + BFI34 + BFI39 + BFI44 + BFI49 + BFI54 + BFI59
AQ =~ 1*BFI4 + 1*BFI9 + 1*BFI14 + 1*BFI19 + 1*BFI24 + 1*BFI29 + 1*BFI34 + 1*BFI39 + 1*BFI44 + 1*BFI49 + 1*BFI54 + 1*BFI59
NE~~0*AQ'
fit_NE_SD_AQ <- cfa(NE_SD_AQ,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_NE_SD_AQ)


NE_PN <- '
NE_P =~ BFI14 + BFI19 + BFI34 + BFI39 + BFI54 + BFI59
NE_N =~ BFI4r + BFI9r + BFI24r + BFI29r + BFI44r + BFI49r'
fit_NE_PN <- cfa(NE_PN,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_NE_PN)


NE_FAC <- '
NE_F1 =~  BFI4r + BFI19 + BFI34 + BFI49r
NE_F2 =~  BFI9r + BFI24r + BFI39 + BFI54
NE_F3 =~ BFI14 + BFI29r + BFI44r + BFI59'
fit_NE_FAC <- cfa(NE_FAC,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_NE_FAC)


NE_FAC_AQ <- '
NE_F1 =~ BFI4 + BFI19 + BFI34 + BFI49
NE_F2 =~ BFI9 + BFI24 + BFI39 + BFI54
NE_F3 =~ BFI14 + BFI29 + BFI44 + BFI59
AQ =~ 1*BFI4 + 1*BFI9 + 1*BFI14 + 1*BFI19 + 1*BFI24 + 1*BFI29 + 1*BFI34 + 1*BFI39 + 1*BFI44 + 1*BFI49 + 1*BFI54 + 1*BFI59
NE_F1~~0*AQ
NE_F2~~0*AQ
NE_F3~~0*AQ'
fit_NE_FAC_AQ <- cfa(NE_FAC_AQ,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_NE_FAC_AQ)





O_SD <- '
O =~ BFI5r + BFI10 + BFI15 + BFI20 + BFI25r + BFI30r + BFI35 + BFI40 + BFI45r + BFI50r + BFI55r + BFI60'
fit_O_SD <- cfa(O_SD,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_O_SD)


O_SD_AQ <- '
O =~ BFI5 + BFI10 + BFI15 + BFI20 + BFI25 + BFI30 + BFI35 + BFI40 + BFI45 + BFI50 + BFI55 + BFI60
AQ =~ 1*BFI5 + 1*BFI10 + 1*BFI15 + 1*BFI20 + 1*BFI25 + 1*BFI30 + 1*BFI35 + 1*BFI40 + 1*BFI45 + 1*BFI50 + 1*BFI55 + 1*BFI60
O~~0*AQ'
fit_O_SD_AQ <- cfa(O_SD_AQ,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_O_SD_AQ)


O_PN <- '
O_P =~ BFI10 + BFI15 + BFI20 + BFI35 + BFI40 + BFI60
O_N =~ BFI5r + BFI25r + BFI30r + BFI45r + BFI50r + BFI55r'
fit_O_PN <- cfa(O_PN,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_O_PN)


O_FAC <- '
O_F1 =~  BFI10 + BFI25r + BFI40 + BFI55r
O_F2 =~  BFI5r + BFI20 + BFI35 + BFI50r
O_F3 =~ BFI15 + BFI30r + BFI45r + BFI60'
fit_O_FAC <- cfa(O_FAC,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_O_FAC)


O_FAC_AQ <- '
O_F1 =~  BFI10 + BFI25 + BFI40 + BFI55
O_F2 =~  BFI5 + BFI20 + BFI35 + BFI50
O_F3 =~ BFI15 + BFI30 + BFI45 + BFI60
AQ =~ 1*BFI5 + 1*BFI10 + 1*BFI15 + 1*BFI20 + 1*BFI25 + 1*BFI30 + 1*BFI35 + 1*BFI40 + 1*BFI45 + 1*BFI50 + 1*BFI55 + 1*BFI60
O_F1~~0*AQ
O_F2~~0*AQ
O_F3~~0*AQ'
fit_O_FAC_AQ <- cfa(O_FAC_AQ,bfi2,ordered=names(bfi2[,8:97]))
fitMeasures(fit_O_FAC_AQ)






bfi2<- read.table("data_all15-26_acqui.csv", sep = ";", dec = ",", header = T)

library(lavaan)
library(psych)




library(userfriendlyscience)

BFI_E <- scaleStructure(dat=bfi2, items = c('BFI1', 'BFI6', 'BFI11r', 'BFI16r', 'BFI21', 'BFI26r', 'BFI31r', 'BFI36r', 'BFI41', 'BFI46', 'BFI51r', 'BFI56'),
               digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_A <- scaleStructure(dat=bfi2, items = c('BFI2', 'BFI7', 'BFI12r', 'BFI17r', 'BFI22r', 'BFI27', 'BFI32', 'BFI37r', 'BFI42r', 'BFI47r', 'BFI52', 'BFI57'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_C <- scaleStructure(dat=bfi2, items = c('BFI3r', 'BFI8r', 'BFI13', 'BFI18', 'BFI23r', 'BFI28r', 'BFI33', 'BFI38', 'BFI43', 'BFI48r', 'BFI53', 'BFI58r'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_NE <- scaleStructure(dat=bfi2, items = c('BFI4r', 'BFI9r', 'BFI14', 'BFI19', 'BFI24r', 'BFI29r', 'BFI34', 'BFI39', 'BFI44r', 'BFI49r', 'BFI54', 'BFI59'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_O <- scaleStructure(dat=bfi2, items = c('BFI5r', 'BFI10', 'BFI15', 'BFI20', 'BFI25r', 'BFI30r', 'BFI35', 'BFI40', 'BFI45r', 'BFI50r', 'BFI55r', 'BFI60'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)


BFI_sociability <- scaleStructure(dat=bfi2, items = c('BFI1', 'BFI16r', 'BFI31r', 'BFI46'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_assertiveness <- scaleStructure(dat=bfi2, items = c('BFI6', 'BFI21', 'BFI36r', 'BFI51r'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_energy <- scaleStructure(dat=bfi2, items = c('BFI11r', 'BFI26r', 'BFI41', 'BFI56'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_compassion <- scaleStructure(dat=bfi2, items = c('BFI2', 'BFI17r', 'BFI32', 'BFI47r'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_respectfulness <- scaleStructure(dat=bfi2, items = c('BFI7', 'BFI22r', 'BFI37r', 'BFI52'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_trust <- scaleStructure(dat=bfi2, items = c('BFI12r', 'BFI27', 'BFI42r', 'BFI57'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_organization <- scaleStructure(dat=bfi2, items = c('BFI3r', 'BFI18', 'BFI33', 'BFI48r'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_productiveness <- scaleStructure(dat=bfi2, items = c('BFI8r', 'BFI23r', 'BFI38', 'BFI53'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_responsibility <- scaleStructure(dat=bfi2, items = c('BFI13', 'BFI28r', 'BFI43', 'BFI58r'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_anxiety <- scaleStructure(dat=bfi2, items = c('BFI4r', 'BFI19', 'BFI34', 'BFI49r'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_depression <- scaleStructure(dat=bfi2, items = c('BFI9r', 'BFI24r', 'BFI39', 'BFI54'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_emotional_volatility <- scaleStructure(dat=bfi2, items = c('BFI14', 'BFI29r', 'BFI44r', 'BFI59'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_intelectual_curiosity <- scaleStructure(dat=bfi2, items = c('BFI10', 'BFI25r', 'BFI40', 'BFI55r'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_aesthetic_sensitivity <- scaleStructure(dat=bfi2, items = c('BFI5r', 'BFI20', 'BFI35', 'BFI50r'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_creative_imagination <- scaleStructure(dat=bfi2, items = c('BFI15', 'BFI30r', 'BFI45r', 'BFI60'),
                    digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFI_E
BFI_A
BFI_C
BFI_NE
BFI_O

BFI_sociability
BFI_assertiveness
BFI_energy
BFI_compassion
BFI_respectfulness
BFI_trust
BFI_organization
BFI_productiveness
BFI_responsibility
BFI_anxiety
BFI_depression
BFI_emotional_volatility
BFI_intelectual_curiosity
BFI_aesthetic_sensitivity
BFI_creative_imagination




BFIs_E <- scaleStructure(dat=bfi2, items = c('BFI1', 'BFI16r', 'BFI21', 'BFI26r', 'BFI41', 'BFI51r'),
                        digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFIs_A <- scaleStructure(dat=bfi2, items = c('BFI2', 'BFI7', 'BFI12r', 'BFI37r', 'BFI47r', 'BFI57'),
                        digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFIs_C <- scaleStructure(dat=bfi2, items = c('BFI3r', 'BFI23r', 'BFI28r', 'BFI33', 'BFI43', 'BFI53'),
                        digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFIs_NE <- scaleStructure(dat=bfi2, items = c('BFI4r', 'BFI24r', 'BFI29r', 'BFI34', 'BFI54', 'BFI59'),
                        digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFIs_O <- scaleStructure(dat=bfi2, items = c('BFI5r', 'BFI20', 'BFI30r', 'BFI40', 'BFI55r', 'BFI60'),
                        digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)



soc <- data.frame(bfi2$BFI1,bfi2$BFI16r)
soc.pc <- polychoric(soc)$rho[1,2]
BFIs_sociability <- 2*soc.pc/(1+soc.pc)



ass <- data.frame(bfi2$BFI21,bfi2$BFI51r)
ass.pc <- polychoric(ass)$rho[1,2]
BFIs_assertiveness <- 2*ass.pc/(1+ass.pc)
BFIs_assertiveness


ene <- data.frame(bfi2$BFI26r,bfi2$BFI41)
ene.pc <- polychoric(ene)$rho[1,2]
BFIs_energy <- 2*ene.pc/(1+ene.pc)
BFIs_energy



com <- data.frame(bfi2$BFI2,bfi2$BFI47r)
com.pc <- polychoric(com)$rho[1,2]
BFIs_compassion <- 2*com.pc/(1+com.pc)
BFIs_compassion


res <- data.frame(bfi2$BFI7,bfi2$BFI37r)
res.pc <- polychoric(res)$rho[1,2]
BFIs_respectfulness <- 2*res.pc/(1+res.pc)
BFIs_respectfulness



tru <- data.frame(bfi2$BFI12r,bfi2$BFI57)
tru.pc <- polychoric(tru)$rho[1,2]
BFIs_trust <- 2*tru.pc/(1+tru.pc)
BFIs_trust



org <- data.frame(bfi2$BFI3r,bfi2$BFI33)
org.pc <- polychoric(org)$rho[1,2]
BFIs_organization <- 2*org.pc/(1+org.pc)
BFIs_organization


pro <- data.frame(bfi2$BFI23r,bfi2$BFI53)
pro.pc <- polychoric(pro)$rho[1,2]
BFIs_productiveness <- 2*pro.pc/(1+pro.pc)
BFIs_productiveness




res <- data.frame(bfi2$BFI28r,bfi2$BFI43)
res.pc <- polychoric(res)$rho[1,2]
BFIs_responsibility <- 2*res.pc/(1+res.pc)
BFIs_responsibility



anx <- data.frame(bfi2$BFI4r,bfi2$BFI34)
anx.pc <- polychoric(anx)$rho[1,2]
BFIs_anxiety  <- 2*anx.pc/(1+anx.pc)
BFIs_anxiety




dep <- data.frame(bfi2$BFI24r,bfi2$BFI54)
dep.pc <- polychoric(dep)$rho[1,2]
BFIs_depression  <- 2*dep.pc/(1+dep.pc)
BFIs_depression





vol <- data.frame(bfi2$BFI29r,bfi2$BFI59)
vol.pc <- polychoric(vol)$rho[1,2]
BFIs_emotional_volatility <- 2*vol.pc/(1+vol.pc)
BFIs_emotional_volatility





int <- data.frame(bfi2$BFI40,bfi2$BFI55r)
int.pc <- polychoric(int)$rho[1,2]
BFIs_intelectual_curiosity <- 2*int.pc/(1+int.pc)
BFIs_intelectual_curiosity





aes <- data.frame(bfi2$BFI5r,bfi2$BFI20)
aes.pc <- polychoric(aes)$rho[1,2]
BFIs_aesthetic_sensitivity <- 2*aes.pc/(1+aes.pc)
BFIs_aesthetic_sensitivity



BFIs_creative_imagination <- scaleStructure(dat=bfi2, items = c('BFI30r', 'BFI60'),
                                           digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

cre <- data.frame(bfi2$BFI30r,bfi2$BFI60)
cre.pc <- polychoric(cre)$rho[1,2]
BFIs_creative_imagination  <- 2*cre.pc/(1+cre.pc)
BFIs_creative_imagination


BFIs_E
BFIs_A
BFIs_C
BFIs_NE
BFIs_O

BFIs_sociability
BFIs_assertiveness
BFIs_energy
BFIs_compassion
BFIs_respectfulness
BFIs_trust
BFIs_organization
BFIs_productiveness
BFIs_responsibility
BFIs_anxiety
BFIs_depression
BFIs_emotional_volatility
BFIs_intelectual_curiosity
BFIs_aesthetic_sensitivity
BFIs_creative_imagination






BFIxs_E <- scaleStructure(dat=bfi2, items = c('BFI16r', 'BFI21', 'BFI41'),
                         digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFIxs_A <- scaleStructure(dat=bfi2, items = c('BFI2', 'BFI37r', 'BFI57'),
                         digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFIxs_C <- scaleStructure(dat=bfi2, items = c('BFI3r', 'BFI23r', 'BFI43'),
                         digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFIxs_NE <- scaleStructure(dat=bfi2, items = c('BFI29r', 'BFI34', 'BFI54'),
                          digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)

BFIxs_O <- scaleStructure(dat=bfi2, items = c('BFI20', 'BFI55r', 'BFI60'),
                         digits = 2, ci = TRUE, interval.type="normal-theory", conf.level=.95,silent=FALSE, samples=1000, bootstrapSeed = NULL, omega.psych = TRUE, poly = TRUE)



BFIxs_E
BFIxs_A
BFIxs_C
BFIxs_NE
BFIxs_O
