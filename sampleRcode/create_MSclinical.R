load("sampleRcode/data/dataPROM.rda")
names(dataPROM)
names(dataPROM) <- c("LCLA", "MI", "MAS", "BBS", "T", "DD", "DM", "PRs", "KH", "NHPT", "T25FW", "PASAT3","EDSS")
head(dataPROM, n = 3)
rownames(dataPROM) <- NULL

MSclinical <- dataPROM
save(MSclinical, file = "data/MSclinical.rda")
