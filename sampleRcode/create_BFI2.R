
# References:
# Hrebickova et al (2020). Big five inventory 2 (BFI-2): Hierarchicky model s 15
# subskalami. [Big five inventory 2 (BFI-2): Hierarchical model, In Czech].
# Ceskoslovenska psychologie LXIV(4)
#
# Soto, C. J., & John, O. P. (2017). The next Big Five Inventory (BFI-2): Developing and
# assessing a hierarchical model with 15 facets to enhance bandwidth, fidelity, and
# predictive power. Journal of Personality and Social Psychology, 113, 117-143.
# (includes original English wording, and scoring key in Appendix)

dataspss <- haven::read_spss("SampleRcode/data/data_BFI2_hrebickova_martinkova.sav")

# remove all SPSS attributes, output bare data.frame
dataspss <- haven::zap_label(dataspss)
dataspss <- haven::zap_labels(dataspss)
dataspss <- haven::zap_formats(dataspss)
dataspss <- haven::zap_widths(dataspss)

# reverse-code needed (we will save only reverse-coded items)

names(dataspss)

# item codes that needs to be reverted
reverse_coded <- paste0("BFI", c(
  3, 4, 5, 8, 9, 11, 12, 16, 17, 22, 23, 24, 25, 26, 28, 29, 30,
  31, 36, 37, 42, 44, 45, 47, 48, 49, 50, 51, 55, 58
))

dataspss[, reverse_coded] <- 6 - dataspss[, reverse_coded]

## rename items according to doc
# rename_vector <- readRDS("sampleRcode/BFI2/bfi2_rename_vector.rds") # from BFI2_SampleCode.R
#
#dataspss <- dplyr::rename(dataspss, !!!rename_vector)
#
#BFI2 <- as.data.frame(dataspss)
#
#names(BFI2)[1:4] <- c("ID", "Gender", "Age", "Educ")

BFI2 <- as.data.frame(dataspss[,c(5:64,2:4)])
names(BFI2)[1:60] <- paste0("i", 1:60)
names(BFI2)[61:63] <- c("Gender", "Age", "Educ")
summary(BFI2)

##############################
# save as csv (for osf.io)
write.csv(BFI2, file = "SampleRcode/data/BFI2.csv", row.names = FALSE)

# save as rda (for SIA)
save(BFI2, file = "data/BFI2.rda")
