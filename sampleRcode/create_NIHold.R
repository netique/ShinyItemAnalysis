###################################
# NIH data preparation
###################################

library(dplyr)


#########################
# Read data
#########################
NIHall <- read.csv("sampleRcode/data/NIH-public-data_Erosheva-et-al.csv")
n <- dim(NIHall)[1]
head(NIHall, n = 2)

#########################
# Variable type update: as in Erosheva et al. supplement
#########################

NIHall_factors <- c('GROUP_ID', 'PI_RACE', 'PI_ID', 'REVIEWER_ID',
                  'APPLICATION_ID', 'IRG', 'ADMIN_ORG', 'SRG',
                  'APPLICATION_TYPE', 'PI_GENDER', 'PI_ETHNICITY',
                  'CAREER_STAGE', 'DEG_CATEGORY', 'INSTITUTION_BIN')
NIHall[NIHall_factors] <- lapply(NIHall[NIHall_factors], as.factor)

# "White" is considered the default level of the race variable
NIHall$GROUP_ID <- factor(NIHall$GROUP_ID,
                          levels = c('Matched White',
                                   'Matched Black',
                                   'Random White',
                                   'All Black'))

# "Experienced" is the default career stage
NIHall$CAREER_STAGE <- factor(NIHall$CAREER_STAGE,
                              levels = c('Experienced',
                                       'ESI',
                                       'Non-ESI NI'))

# "Male" is considered the default gender in this study
NIHall$PI_GENDER <- factor(NIHall$PI_GENDER,
                           levels = c('Male', 'Female'))

# "Non-Hispanic" is considered the default ethnicity in this study
NIHall$PI_ETHNICITY <- factor(NIHall$PI_ETHNICITY,
                              levels = c('Non-Hispanic', 'Hispanic/Latino'))

# White is considered the default level of the race variable
NIHall$PI_RACE <- factor(NIHall$PI_RACE,
                         levels = c('White', 'Black'))

# PhD is considered the default degree
NIHall$DEG_CATEGORY <- factor(NIHall$DEG_CATEGORY,
                              levels = c('PHD', 'MD' ,'MD/PHD', 'Others'))

# recode SRG
NIHall <- NIHall %>% mutate(SRG = as.factor(paste(SRG, IRG, sep = '_')))

#########################
# Define variables: as in Erosheva et al. supplement
#########################

criteria <- c('SIGNIFICANCE_INIT', 'INVESTIGATOR_INIT', 'INNOVATION_INIT',
              'APPROACH_INIT', 'ENVIRONMENT_INIT')

summary(NIHall[,criteria])
summary(NIHall[,'IMPACT_INIT'])
summary(NIHall[,'IMPACT_FINAL'])
table(NIHall[,'IMPACT_FINAL'])
summary(as.numeric(NIHall$IMPACT_FINAL))
NIHall$IMPACT_FINAL_num <- as.numeric(NIHall$IMPACT_FINAL)

plot(NIHall$IMPACT_INIT, NIHall$IMPACT_FINAL_num)
cor(NIHall$IMPACT_INIT, NIHall$IMPACT_FINAL_num, use = "pairwise.complete.obs") #0.76

ID_clusters <- c('PI_ID', 'REVIEWER_ID')

org_clusters <- c('ADMIN_ORG', 'IRG', 'SRG')

matching <- c('APPLICATION_TYPE', 'AMENDED', 'PI_GENDER', 'PI_ETHNICITY',
              'CAREER_STAGE', 'DEG_CATEGORY', 'INSTITUTION_BIN', 'IRG')

# Application- and applicant-specific variables for the public data are all matching variables except for IRG, which is a structural variable
app_app_vars <- matching[1:7]

########################################
## define subsamples of data:  as in Erosheva et al. supplement
## Note: code for how matched samples were created is not available
########################################

# Filter based on GROUP_ID
# d_matched <- NIHall[NIHall$GROUP_ID %in% c('Matched White', 'Matched Black'),]
d_random <- NIHall[NIHall$GROUP_ID != 'Matched White',]



# TODO: move random sample creation here.



# Ensure factor variables have factor data type,
#  remove unnecessary GROUP_ID factor levels after filtering
# d_matched$GROUP_ID <- factor(d_matched$GROUP_ID)
d_random$GROUP_ID <- factor(d_random$GROUP_ID)
for (i in 1:(dim(NIHall)[2])) {
  if (is.factor(NIHall[,i][[1]])) {
#    d_matched[,i][[1]] <- factor(d_matched[,i][[1]])
    d_random[,i][[1]] <- factor(d_random[,i][[1]])
  }
}

#View(d_matched)
#View(d_random)

dim(NIHall)
#dim(d_matched)
dim(d_random)
#dim(d_matched)[1] + dim(d_random)[1]

#save(d_random, file = "dataNIHrandom_SciAdv.Rda")
#save(d_matched, file = "dataNIHmatched_SciAdv.Rda")

############################################################
# New: ADD MEAN SCORE, PROJECT RANK, etc. to d_random
###########################################################

## HOW MANY TIMES WAS AN APPLICATION RATED #
table(table(d_random$APPLICATION_ID))

d_random$APPLICATION_ID[1:20]
# [1] 196  196  196  3620 3620 3620 1270 1270 1270 2708 2708 2708 849  849  849  29   29
# [18] 29   3459 3459
# 4651 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 27 28 29 30 ... 4995

length(levels(d_random$APPLICATION_ID)) #4651

# Add mean score by application_id
d_random <- d_random %>%
  group_by(APPLICATION_ID) %>%
  mutate(IMPACT_INIT.avg = mean(IMPACT_INIT))
summary(d_random$IMPACT_INIT.avg)

d_random$APPLICATION_ID[1:20]
# [1] 196  196  196  3620 3620 3620 1270 1270 1270 2708 2708 2708 849  849  849  29   29
# [18] 29   3459 3459
# 4651 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 27 28 29 30 ... 4995
length(levels(d_random$APPLICATION_ID)) #4651

# Add rank by average score
d_random <- d_random %>%
  group_by(APPLICATION_ID) %>%
  summarise(IMPACT_INIT.avg = unique(IMPACT_INIT.avg)) %>%
  mutate(IMPACT_INIT.rank = rank(IMPACT_INIT.avg, ties.method = "first")) %>%
  select(APPLICATION_ID, IMPACT_INIT.rank) %>%
  left_join(d_random, by = "APPLICATION_ID")

d_random$APPLICATION_ID[1:20]
# [1] 2  2  2  3  3  4  4  4  5  5  5  6  6  6  7  7  7  8  8  11
# 4651 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 27 28 29 30 ... 4995
# Adding range changed the order!
length(levels(d_random$APPLICATION_ID)) #4651

# Add mean score by application_id, increased by small ratio of highest score
d_random <- d_random %>%
  group_by(APPLICATION_ID) %>%
  mutate(IMPACT_INIT.avg2 = mean(IMPACT_INIT) + 0.001 * max(IMPACT_INIT))
summary(d_random$IMPACT_INIT.avg2)

# Add rank by "increased" average score
d_random <- d_random %>%
  group_by(APPLICATION_ID) %>%
  summarise(IMPACT_INIT.avg2 = unique(IMPACT_INIT.avg2)) %>%
  mutate(IMPACT_INIT.rank2 = rank(IMPACT_INIT.avg2, ties.method = "first")) %>%
  select(APPLICATION_ID, IMPACT_INIT.rank2) %>%
  left_join(d_random, by = "APPLICATION_ID")


head(d_random[,c("APPLICATION_ID", "IMPACT_INIT", "IMPACT_INIT.avg", "IMPACT_INIT.rank", "IMPACT_INIT.avg2", "IMPACT_INIT.rank2")])
sum(d_random$IMPACT_INIT.rank != d_random$IMPACT_INIT.rank2) # 8238
sum(d_random$IMPACT_INIT.rank == d_random$IMPACT_INIT.rank2) # 357

d_random$APPLICATION_ID[1:20]
length(levels(d_random$APPLICATION_ID)) #4651

#save(d_random, file = "dataNIHrandom_Extended.Rda")


############################################################
# New: RANDOM SAMPLE FOR BLACK APPLICANTS
###########################################################
# mail 9/15/2020 EE:
# We have 46,226 total Black (1015) and White (45,211) applicants.
# We sample 2030 random white, which is approximately 2030/45,211=0.0449
# So we need to sample approximately 0.0449*1015=46 random Black applications from Matched Black and All Black together.

uniqueID <- !duplicated(d_random$APPLICATION_ID)
d_random_un <- d_random[uniqueID,]
table(d_random_un$GROUP_ID)

blackID <-  d_random_un$APPLICATION_ID[d_random_un$GROUP_ID != "Random White"]
length(blackID)

set.seed(123)
(blackIDs <- sample(blackID, 46))
# [1] 2006 2250 841  2579 890  4612 4003 565  1406 1033 1124 58   1800 3225 2919 2924 3780 3471 481  4683 1621 3143 4886
#     1654 4091 138  2534 2084 4907 3777 939  4589 2881 2892 2717 4266 1797 4109 686  2673 2349 3044 3819 4433 4610 4093

dim(d_random) #[1] 8595   27
dim(d_random[(d_random$GROUP_ID == "Random White"),]) # [1] 5669   27

d_random <- d_random[((d_random$GROUP_ID == "Random White") | (d_random$APPLICATION_ID %in% blackIDs)),]
dim(d_random) # [1] 5802   27

plot(d_random$IMPACT_INIT.rank, d_random$IMPACT_INIT.rank2)
boxplot(d_random$IMPACT_INIT.rank)
d_random$IMPACT_INIT.rank[order(d_random$IMPACT_INIT.rank)] # some ranks are missing (we have 1,2,4,5,7,...)
rankOLD <- d_random$IMPACT_INIT.rank
rankOLD2 <- d_random$IMPACT_INIT.rank2

# !!!! this needs to be run 2x (or original values need to be removed)
# Correct rank by average score
d_random <- d_random %>%
  group_by(APPLICATION_ID) %>%
  summarise(IMPACT_INIT.avg = unique(IMPACT_INIT.avg)) %>%
  mutate(IMPACT_INIT.rank = rank(IMPACT_INIT.avg, ties.method = "first")) %>%
  select(APPLICATION_ID, IMPACT_INIT.rank) %>%
  left_join(d_random, by = "APPLICATION_ID")

# Correct rank by "increased" average score
d_random <- d_random %>%
  group_by(APPLICATION_ID) %>%
  summarise(IMPACT_INIT.avg2 = unique(IMPACT_INIT.avg2)) %>%
  mutate(IMPACT_INIT.rank2 = rank(IMPACT_INIT.avg2, ties.method = "first")) %>%
  select(APPLICATION_ID, IMPACT_INIT.rank2) %>%
  left_join(d_random, by = "APPLICATION_ID")

summary(d_random$IMPACT_INIT.rank) # 1 to 2076 (2030 + 46, correct)
summary(d_random$IMPACT_INIT.rank2) # 1 to 2076 (2030 + 46, correct)
d_random$IMPACT_INIT.rank[order(d_random$IMPACT_INIT.rank)] # no numbers missing, correct

###############################################
# save NIH.Rda
###############################################
# save(d_random, file = "dataNIHrandom_Extended_sample.Rda")

# TODO: save as data frame, check analyses come out the same

NIH <- d_random
save(NIH, file = "data/NIH.rda")

#########################
# TODO:
# - create csv files to test upload when rater data upload is implemented
#########################
