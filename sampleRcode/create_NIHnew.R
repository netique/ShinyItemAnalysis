###################################
# NIH data preparation
###################################
# download the AIBS data (Erosheva et al., 2020) from https://doi.org/10.17605/OSF.IO/4D6RX

#########################
# Load libraries
#########################
library(tidyverse)

#########################
# Read data
#########################
NIHall <- read.csv("sampleRcode/data/NIH-public-data_Erosheva-et-al.csv")
n <- nrow(NIHall)
head(NIHall, n = 2)

#########################
# Variable type update
#########################

NIHall_factors <- c('GROUP_ID', 'PI_RACE', 'PI_ID', 'REVIEWER_ID',
                    'APPLICATION_ID', 'IRG', 'ADMIN_ORG', 'SRG',
                    'APPLICATION_TYPE', 'PI_GENDER', 'PI_ETHNICITY',
                    'CAREER_STAGE', 'DEG_CATEGORY', 'INSTITUTION_BIN')

# Change variables' class to factor and set theirs reference levels
NIHall <- NIHall %>% mutate(
  across(all_of(NIHall_factors), as.factor),
  GROUP_ID = fct_relevel(GROUP_ID, "Matched White", "Matched Black", "Random White", "All Black"),
  CAREER_STAGE = fct_relevel(CAREER_STAGE, "Experienced", "ESI", "Non-ESI NI"),
  PI_GENDER = fct_relevel(PI_GENDER, "Male", "Female"),
  PI_ETHNICITY = fct_relevel(PI_ETHNICITY, "Non-Hispanic", "Hispanic/Latino"),
  PI_RACE = fct_relevel(PI_RACE, "White", "Black"),
  DEG_CATEGORY = fct_relevel(DEG_CATEGORY, "PHD", "MD", "MD/PHD", "Others"),
  SRG = as.factor(paste(SRG, IRG, sep = "_"))
)

# Check criteria variables
criteria <- c('SIGNIFICANCE_INIT', 'INVESTIGATOR_INIT', 'INNOVATION_INIT',
              'APPROACH_INIT', 'ENVIRONMENT_INIT')
summary(NIHall[,criteria])

# Check overall scores, change to numeric format
summary(NIHall[,'IMPACT_INIT'])
summary(NIHall[,'IMPACT_FINAL'])
table(NIHall[,'IMPACT_FINAL'])
summary(as.numeric(NIHall$IMPACT_FINAL))
NIHall$IMPACT_FINAL_num <- as.numeric(NIHall$IMPACT_FINAL)

########################################
## Define random subsample of data
########################################

# Filter based on GROUP_ID
d_random <- NIHall %>% filter(GROUP_ID != "Matched White")
length(levels(d_random$APPLICATION_ID)) #4651

# Drop unused factor levels for all factor variables, sort by appl. ID
d_random <- d_random %>%
  mutate(across(where(is.factor), droplevels)) %>%
  arrange(APPLICATION_ID)

length(levels(d_random$APPLICATION_ID)) #3045

###########################################
# Define random sample of black applicants
###########################################

# In original data, we have 46,226 total black (1015) and white (45,211) applicants.
# Erosheva et al (2020) sampled 2030 random white, which is approximately ratio of 2030/45211=0.0449
# To obtain the same ratio of Black applicants as in original data,
#   we sample 0.0449*1015 = 46 random Black applications from Matched Black and All Black together.

set.seed(123)
blackIDs <- d_random %>% distinct(APPLICATION_ID, .keep_all = TRUE) %>% # get only distinct applications, i.e. one appl. per row
  filter(GROUP_ID != "Random White") %>%
  pull(APPLICATION_ID) %>%
  sample(46)

blackIDs
# [1] 2006 2250 841  2579 890  4612 4003 565  1406 1033 1124 58   1800 3225 2919 2924 3780 3471 481  4683 1621 3143 4886
#     1654 4091 138  2534 2084 4907 3777 939  4589 2881 2892 2717 4266 1797 4109 686  2673 2349 3044 3819 4433 4610 4093

dim(d_random) #[1] 8595   23
dim(d_random[(d_random$GROUP_ID == "Random White"),]) # [1] 5669   23

NIH <- d_random %>% filter(GROUP_ID == "Random White" | APPLICATION_ID %in% blackIDs)
dim(NIH) # [1] 5802   23

NIH$APPLICATION_ID[1:10]

############################################################
# Add mean score, adjusted mean score, rank, adjusted rank
############################################################

## How many times was an application rated
table(table(NIH$APPLICATION_ID))

# Add mean score by application_id, and adjusted mean score, increased by small ratio of highest score
NIH <- NIH %>%
  group_by(APPLICATION_ID) %>%
  mutate(
    IMPACT_INIT_AVG = mean(IMPACT_INIT, na.rm = TRUE),
    IMPACT_INIT_AVGADJ = mean(IMPACT_INIT) + 0.001 * max(IMPACT_INIT)
  ) # compute the mean & adj. mean for each appl.

# Add RANK based on simple mean
NIH <- NIH %>%
  arrange(IMPACT_INIT_AVG) %>% # sort by mean
  nest() %>% # make only one row from all appl.'s rows
  rowid_to_column("IMPACT_INIT_RANK") %>% # get ROW number and make it to separate column, effectively a rank, set a name
  unnest(cols = data) # unwrap those nested data into original form

# Add RANK based on adj. mean as above
NIH <- NIH %>%
  arrange(IMPACT_INIT_AVGADJ) %>%
  nest() %>%
  rowid_to_column("IMPACT_INIT_RANKADJ") %>%
  unnest(cols = data) %>%
  ungroup() # remove the grouping information, so further operations should not raise any unexpected behavior

head(NIH[,c("APPLICATION_ID", "IMPACT_INIT", "IMPACT_INIT_AVG", "IMPACT_INIT_RANK", "IMPACT_INIT_AVGADJ", "IMPACT_INIT_RANKADJ")])

plot(NIH$IMPACT_INIT_RANK, NIH$IMPACT_INIT_RANKADJ)
dim(NIHall) # [1] 13488    23
dim(NIH) # [1] 5802   27

summary(NIH$IMPACT_INIT_RANK) # 1 to 2076 (2030 + 46, correct)
summary(NIH$IMPACT_INIT_RANKADJ) # 1 to 2076 (2030 + 46, correct)
NIH$IMPACT_INIT_RANK[order(NIH$IMPACT_INIT_RANK)] # no numbers missing, correct

###############################################
# Rename, reorganize
###############################################

names(NIH)
names(NIH) <-
  c("ScoreRankAdj", #"IMPACT_INIT_RANKADJ",
    "ID", #"APPLICATION_ID",
    "ScoreRank", #"IMPACT_INIT_RANK",
    "GroupID", #"GROUP_ID",
    "Score", #"IMPACT_INIT"
    "Significance", #"SIGNIFICANCE_INIT",
    "Investigator", #"INVESTIGATOR_INIT",
    "Innovation", #"INNOVATION_INIT",
    "Approach", #"APPROACH_INIT",
    "Environment", #"ENVIRONMENT_INIT",
    "ScoreFinalChar", #"IMPACT_FINAL",
    "PIRace", #"PI_RACE",
    "PIID", #"PI_ID",
    "RevID", #"REVIEWER_ID",
    "IRG", #"IRG",
    "AdminOrg", #"ADMIN_ORG",
    "SRG", #"SRG",
    "PropType", #"APPLICATION_TYPE",
    "Ammend", #"AMENDED",
    "PIGender", #"PI_GENDER",
    "PIEthn", #"PI_ETHNICITY",
    "PICareerStage", #"CAREER_STAGE",
    "PIDegree", #"DEG_CATEGORY",
    "PIInst", #"INSTITUTION_BIN",
    "ScoreFinal", #"IMPACT_FINAL_num",
    "ScoreAvg", #"IMPACT_INIT_AVG",
    "ScoreAvgAdj" #"IMPACT_INIT_AVGADJ"
  )
NIH <- as.data.frame(NIH[,c(
  "ID", "Score",  "Significance", "Investigator", "Innovation", "Approach", "Environment",
  "PIRace", "PIID", "PIGender", "PIEthn", "PICareerStage", "PIDegree", "PIInst",
  "GroupID", "RevID", "IRG", "AdminOrg", "SRG", "PropType", "Ammend",
  "ScoreAvg", "ScoreAvgAdj",  "ScoreRankAdj", "ScoreRank",
  "ScoreFinal", "ScoreFinalChar"
)])
summary(NIH)


###############################################
# save NIH.rda
###############################################

save(NIH, file = "data/NIH.rda")
