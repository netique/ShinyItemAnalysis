#########################
# AIBS data preparation #
#########################
# download the AIBS data (Gallo, 2021) from https://doi.org/10.6084/m9.figshare.12728087

#########################
# Load libraries
#########################

library(dplyr)
library(tidyr)

#########################
# Read data
#########################

AIBS_wide <- read.csv("sampleRcode/data/ReviewData.csv")
head(AIBS_wide, n = 2)

#########################
# Rename variables
#########################

colnames(AIBS_wide)
colnames(AIBS_wide) <- c(
  "ReviewYear", "ID", "PropType", "InvID", "OrgType", "PIgender", "PIrank", "PIdegree", "ScoreA", "ScoreB",
  "ScoreC", "InnovationA", "InnovationB", "InnovationC", "ApproachA", "ApproachB", "ApproachC", "InvestigA",
  "InvestigB", "InvestigC", "SignifA", "SignifB", "SignifC", "ImpactA", "ImpactB", "ImpactC", "RevExpA",
  "RevExpB", "RevExpC", "IDRevA", "InstRevA", "GenRevA", "RankRevA", "DegRevA",
  "IDRevB", "InstRevB", "GenRevB", "RankRevB", "DegRevB",
  "IDRevC", "InstRevC", "GenRevC", "RankRevC", "DegRevC"
  )

summary(AIBS_wide)
head(AIBS_wide, n = 2)

#########################
# Variable type update
#########################

AIBS_wide$ID <- as.factor(AIBS_wide$ID)
AIBS_wide$PropType <- as.factor(AIBS_wide$PropType)
levels(AIBS_wide$PropType)
levels(AIBS_wide$PropType) <- c("Pilot", "Standard", "Standard")
AIBS_wide$InvID <- as.factor(AIBS_wide$InvID)
AIBS_wide$OrgType <- as.factor(AIBS_wide$OrgType)
levels(AIBS_wide$OrgType)

AIBS_wide$ImpactA[AIBS_wide$ImpactA == "N/A"] <- NA
AIBS_wide$ImpactA <- as.numeric(as.character(AIBS_wide$ImpactA))
AIBS_wide$ImpactB[AIBS_wide$ImpactB == "N/A"] <- NA
AIBS_wide$ImpactB <- as.numeric(as.character(AIBS_wide$ImpactB))
AIBS_wide$ImpactC[AIBS_wide$ImpactC == "N/A"] <- NA
AIBS_wide$ImpactC <- as.numeric(as.character(AIBS_wide$ImpactC))

AIBS_wide$IDRevA <- as.factor(AIBS_wide$IDRevA)
AIBS_wide$IDRevB <- as.factor(AIBS_wide$IDRevB)
AIBS_wide$IDRevC <- as.factor(AIBS_wide$IDRevC)


#########################
# New variables
#########################

# Calculate the average score (the lower the better)
AIBS_wide[, "Score_avg"] <- apply(AIBS_wide[, c("ScoreA", "ScoreB", "ScoreC")], 1, mean)

# Avg increased by 0.001 * highest score (final decisions tend to be influenced by worse scores)
AIBS_wide[, "Score_avg2"] <- AIBS_wide[, "Score_avg"] + 0.0001 * apply(AIBS_wide[, c("ScoreA", "ScoreB", "ScoreC")], 1, max)

# Rank the proposals
AIBS_wide[, "Score_rank"] <- rank(AIBS_wide[, "Score_avg"], ties.method = "first")
AIBS_wide[, "Score_rank2"] <- rank(AIBS_wide[, "Score_avg2"], ties.method = "first")

head(AIBS_wide)
head(AIBS_wide[order(AIBS_wide$Score_rank2), ])

AIBS_wide <- as.data.frame(AIBS_wide)
summary(AIBS_wide)

#########################
# Long format
#########################

# select all cols ending with either A, B, or C, with:
# matches("[A|B|C]$", ignore.case = FALSE)

# first, ensure the selection is right
orig_names <- AIBS_wide %>% names()
matched_names <- AIBS_wide %>%
  select(matches("[A|B|C]", ignore.case = FALSE)) %>%
  names()

# matched names that are NOT in original names
setdiff(orig_names, matched_names)

# matched names that are in original names
intersect(orig_names, matched_names)

# pivot into long format, each applicant should have 3 rows as there are 3 "ratings"
AIBS <- AIBS_wide %>%
  pivot_longer(
    matches("[A|B|C]$", ignore.case = FALSE), # which variables to pivot
    names_pattern = "(.*)(.)$", # regex with 2 groups - first matches anything of any length, the second matches single character at the very end of the variable name
    names_to = c(".value", "rating") # grab the part of the variables preceeding A, B, or C with the ".value" sentient and use it for new variables names, "rating" stands for the new variable denoting if the row is applicant's rating A, B, or C
  ) %>%
  relocate(ID, rating) # get ID and rating ID first

AIBS <- as.data.frame(AIBS)
head(AIBS, n = 2)

colnames(AIBS)
cbind(colnames(AIBS), c("ID", "RevCode", "Year", "PropType", "PIID", "PIOrgType", "PIGender", "PIRank", "PIDegree",
  "ScoreAvg", "ScoreAvgAdj", "ScoreRank", "ScoreRankAdj", "Score", "Innovation", "Approach", "Investig", "Signif", "Impact",
  "RevExp", "RevID", "RevInst", "RevGender", "RevRank", "RevDegree"))
colnames(AIBS) <- c("ID", "RevCode", "Year", "PropType", "PIID", "PIOrgType", "PIGender", "PIRank", "PIDegree",
                   "ScoreAvg", "ScoreAvgAdj", "ScoreRank", "ScoreRankAdj", "Score", "Innovation", "Approach", "Investig", "Signif", "Impact",
                   "RevExp", "RevID", "RevInst", "RevGender", "RevRank", "RevDegree")

###############################################
# changing ordering of variables to correspond to PI and Rev
###############################################

AIBS <- as.data.frame(AIBS)[, c("ID", "Year", "PropType",
                                "PIID", "PIOrgType", "PIGender", "PIRank", "PIDegree",
                                "ScoreAvg", "ScoreAvgAdj", "ScoreRank", "ScoreRankAdj", "Score",
                                "Innovation", "Approach", "Investig", "Signif", "Impact",
                                "RevID", "RevExp", "RevInst", "RevGender", "RevRank", "RevDegree", "RevCode")]

summary(AIBS)

###############################################
# save AIBS.Rda
###############################################

save(AIBS, file = "data/AIBS.rda")
