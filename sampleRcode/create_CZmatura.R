# load libraries
library(openxlsx)
library(RCurl) # to read data directly from URL
# from https://stackoverflow.com/questions/21738463/importing-excel-file-using-url-using-read-xls

###############################################
# Maturita matematika
# https://vysledky.cermat.cz/statistika/
# nahore se nastavuje rok a termin (jaro/podzim)
# polozkova data jsou zcela dole

# jaro2020
# https://vysledky.cermat.cz/statistika/FileOpen.aspx?guid=da32fe78-1d8a-4507-9dae-55bc882814dd

# jaro2019
# https://vysledky.cermat.cz/statistika/FileOpen.aspx?guid=1143f995-602d-4077-92c1-7864eb336a75

# podzim2020
# https://vysledky.cermat.cz/statistika/FileOpen.aspx?guid=00a26eb7-c5b4-4dbd-86f1-2bc4be290ab8

#------------------
# READ DATA
# M jaro 2019
url <- "https://vysledky.cermat.cz/statistika/FileOpen.aspx?guid=1143f995-602d-4077-92c1-7864eb336a75"
CZmatura <- read.xlsx(url)
summary(CZmatura)
head(CZmatura)

#------------------
# data names
names(CZmatura)
names(CZmatura)[1:5] <- c("Project", "Exam", "SchType", "SchTypeN", "FirstAtt")

#------------------
# data format
CZmatura$Project <- as.factor(CZmatura$Project)
CZmatura$Exam <- as.factor(CZmatura$Exam)
CZmatura$SchType <- as.factor(CZmatura$SchType)
CZmatura$SchTypeN <- as.factor(CZmatura$SchTypeN)

#CZmatura$prvomaturant <- as.factor(CZmatura$prvomaturant)
CZmatura$SchTypeGY <- as.numeric(CZmatura$SchType %in% c("GY4", "GY6", "GY8"))

#------------------
# responses
itemR <- names(CZmatura)[6:42] # colnames(CZmatura)[grepl("o", colnames(CZmatura))] is not working
itemR
summary(CZmatura[,itemR])
# fix format
cbind(CZmatura$o1, as.numeric(CZmatura$o1))[1:10,]
for (i in 1:19)
  CZmatura[,itemR[i]] <- as.numeric(CZmatura[,itemR[i]])
for (i in 1:19)
  print(table(CZmatura[,itemR[i]]))

# replace 9 with NA
for (i in 1:length(itemR))
  CZmatura[CZmatura[,itemR[i]] == 9,itemR[i]] <- NA
summary(CZmatura[,itemR])

#------------------
# scored items
itemS <- colnames(CZmatura)[grepl("b", colnames(CZmatura))]
itemS
summary(CZmatura[,itemS]) # no NAs

#------------------
# total score
CZmatura$Total <- rowSums(CZmatura[,itemS])
summary(CZmatura$Total)

#------------------
# IRT score and SE
library(mirt)
fitGPCM <- mirt(data = CZmatura[,itemS],
                model = 1,
                SE = TRUE,
                itemtype = 'gpcm')

# model
fitGPCM

# coefficients
coef(fitGPCM, simplify = T)
coef(fitGPCM, IRTpars = T, simplify = T)

# ICCs
plot(fitGPCM, type = 'trace')
plot(fitGPCM, type = 'itemscore')

# latent scores and their SE:
# fsGPCM_all <- fscores(fitGPCM)
fsGPCM_all_se <- fscores(fitGPCM, full.scores = TRUE,
                         full.scores.SE = TRUE)
length(fsGPCM_all)
dim(fsGPCM_all_se)
fsGPCM_all[1:5]
summary(fsGPCM_all)

fsGPCM_all_se[1:5,] # AROUND 0.3
summary(fsGPCM_all_se[,2])
hist(fsGPCM_all)

CZmatura$IRTscore <- fsGPCM_all_se[,1]
CZmatura$IRTscoreSE <- fsGPCM_all_se[,2]

# correlation of IRTscore and total score
cor(CZmatura$Total, CZmatura$IRTscore) # 0.971
plot(CZmatura$Total, CZmatura$IRTscore)


#------------------
# Random sample of 2000 respondents
# random seed for drawing a reproducible random sample
set.seed(123)
samp <- sample(1:nrow(CZmatura), 2000)

#------------------
# save as data.frame, remove Project, Exam, SchTypeN, reorder
CZmatura <- as.data.frame(CZmatura[,c(#"Project", "Exam",
  "SchType", "FirstAtt", "SchTypeGY",
                                      itemR, itemS, "Total",
                                      "IRTscore", "IRTscoreSE")])
CZmaturaS <- CZmatura[samp,]
rownames(CZmaturaS) <- NULL

summary(CZmatura)
summary(CZmaturaS)
head(CZmatura)
head(CZmaturaS)

#-------------------------
# save as .Rnw and .csv
save(CZmatura, file = "data/CZmatura.rda")
save(CZmaturaS, file = "data/CZmaturaS.rda")

write.csv(CZmatura, file = "CZmaturitaJaro2019.csv")
write.csv(CZmatura, file = "CZmaturitaJaro2019sample.csv")

#-------------------------
# save data for SIA
write.csv(CZmaturaS[,itemS], file = "sampleRcode/data/SIA/CZmaturaS2019sample.csv", row.names = FALSE)
write.csv(CZmaturaS[,"SchTypeGY"], file = "sampleRcode/data/SIA/CZmaturaS2019sampleGroup.csv", row.names = FALSE)

write.csv(CZmatura[,itemS], file = "sampleRcode/data/SIA/CZmaturaS2019.csv", row.names = FALSE)
write.csv(CZmatura[,"SchTypeGY"], file = "sampleRcode/data/SIA/CZmaturaS2019Group.csv", row.names = FALSE)


############################################################
# TODO: save also other matura files to allow for equating
#       rename files, e.g., to Mm19s (Matura - math - 2019 - spring)
# TODO for SIA package: remove Project, Exam, rename SchType?, only sample?
# TODO: save key, or perhaps include in the data description the correct answers
# in future, this will allow using multinomial models for multiple-choice items
############################################################


#------------------
# Item-specific IRT model

# key:
# https://maturita.cermat.cz/files/files/testy-zadani-klice/MA_jaro_2019_DT_klic.pdf

sapply(CZmatura[,itemS], max) # maximal item scores
##  b1   b2 b3.1 b3.2   b4   b5   b6   b7 b8.1 b8.2 b8.3 b9.1 b9.2  b10  b11
##   1    1    1    1    2    2    1    2    1    1    1    1    1    1    1
##  b12  b13  b14  b15  b16  b17  b18  b19  b20  b21  b22  b23  b24  b25  b26
##    1    1    3    2    2    2    2    2    2    2    2    2    2    4    3

CZmatura$b3 <- CZmatura$b3.1 + CZmatura$b3.2
CZmatura$b8 <- CZmatura$b8.1 + CZmatura$b8.2
CZmatura$b9 <- CZmatura$b9.1 + CZmatura$b9.2

CZmaturaSc2 <- CZmatura[, paste0("b", 1:26)]
head(CZmaturaSc2, n = 2)

# items 17-24 (multiple-choice, originally scored as 0 or 2 pts)
CZmaturaSc2[, 17:24] <- as.numeric(CZmaturaSc2[, 17:24] == 2)

# save for SIA
write.csv(CZmaturaSc2, file = "sampleRcode/data/SIA/CZmaturaS2019sc2.csv", row.names = FALSE)

# item type
maxscore <- sapply(CZmaturaSc2, max)
maxscore
##  b1  b2  b3  b4  b5  b6  b7  b8  b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22
##   1   1   2   2   2   1   2   2   2   1   1   1   1   3   2   2   1   1   1   1   1   1
##  b23 b24 b25 b26
##    1   1   4   3

itemtype <- ifelse(maxscore == 1, "2PL", "gpcm")
itemtype[17:24] <- "3PL"
itemtype

fitIRTmix <- mirt(data = CZmaturaSc2,
                  model = 1,
                  SE = TRUE,
                  itemtype = itemtype)
# model
fitIRTmix

# coefficients
coef(fitIRTmix, simplify = T)
coef(fitIRTmix, IRTpars = T, simplify = T)

# ICCs
plot(fitIRTmix, type = 'trace')
plot(fitIRTmix, type = 'itemscore')

# latent scores and their SE:
fsIRTmix_all_se <- fscores(fitIRTmix, full.scores = TRUE,
                        full.scores.SE = TRUE)
dim(fsIRTmix_all_se)
fsIRTmix_all_se[1:5,]
summary(fsIRTmix_all_se[,2])
hist(fsIRTmix_all_se[,1])

CZmatura$IRTscore2 <- fsIRTmix_all_se[,1]
CZmatura$IRTscore2SE <- fsIRTmix_all_se[,2]

# correlation of IRTscore and total score
cor(CZmatura$Total, CZmatura$IRTscore2) # 0.9814034
plot(CZmatura$Total, CZmatura$IRTscore2)

cor(CZmatura$IRTscore2, CZmatura$IRTscore) # 0.9912929
plot(CZmatura$IRTscore2, CZmatura$IRTscore)

