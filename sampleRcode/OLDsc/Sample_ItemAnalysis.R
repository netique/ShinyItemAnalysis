# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TRADITIONAL ITEM ANALYSIS ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(psych)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")
Data <- GMAT[, 1:20]

# difficulty and discrimination plot
DDplot(Data, discrim = 'ULI', k = 3, l = 1, u = 3)

# Cronbach alpha
psych::alpha(Data)

# traditional item analysis table
ItemAnalysis(Data)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DISTRACTORS ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(data.table)
library(ShinyItemAnalysis)

# loading data
data(GMATtest, GMATkey, package = "difNLR")
Data <- GMATtest[, 1:20]
key <- unlist(GMATkey)

# combinations - plot for item 1 and 3 groups
plotDistractorAnalysis(Data, key, num.group = 3, item = 1, multiple.answers = TRUE)

# distractors - plot for item 1 and 3 groups
plotDistractorAnalysis(Data, key, num.group = 3, item = 1, multiple.answers = FALSE)

# table with counts and margins - item 1 and 3 groups
DA <- DistractorAnalysis(Data, key, num.groups = 3)[[1]]
dcast(as.data.frame(DA), response ~ score.level, sum, margins = TRUE, value.var = "Freq")

# table with proportions - item 1 and 3 groups
DistractorAnalysis(Data, key, num.groups = 3, p.table = TRUE)[[1]]


