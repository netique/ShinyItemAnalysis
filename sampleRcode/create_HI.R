library(foreign)

dataspss <- read.spss("SampleRcode/data/vyska.vaha.sav")
str(dataspss)
vys <- names(dataspss)[grepl("vys", names(dataspss))]

HI <- dataspss[c(vys, "zena")]
HI <- data.frame(matrix(unlist(HI), ncol = length(HI)))

colnames(HI) <- c(vys, "Gender")
head(HI)

t.test(HI$vyska.cm ~ HI$Gender) # 1 = male, 2 = female
HI$Gender <- factor(HI$Gender, labels = c("M", "F"))

# new item names
# (Items 14–26 are reverse scored, col name starts with "Not")
new.colnames <- c(
  "ShortTrousers", "TallerThanM", "TallerThanF", "HeightForBasketball",
  "AskMeToReach", "CommentsTall", "ConcertObstructs", "ShortBed",
  "TopShelfEasy", "CrowdViewComf", "ShortBlanket", "BendToHug",
  "CarefullHead", "NotSmallerThanM", "NotStoolNeeded", "NotPlayDwarf",
  "NotSmallerThanW", "NotNoticeSmall", "NotOnTipToes", "NotClothChildSize",
  "NotBusLegsEnoughSpace", "NotFasterWalk", "NotAgeUnderestim",
  "NotWishLowerChair", "NotUpwardLook", "NotMirrorTooHigh"
)
length(new.colnames)

colnames(HI)[1:26] <- new.colnames
colnames(HI)[27] <- "HeightCM"
HI <- as.data.frame(HI)

head(HI)
summary(HI)

save(HI, file = "data/HI.rda")
