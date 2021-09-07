###################################################################################
# Supplementary code for                                                          #
#   Kolek, L., Šisler, V., Martinková, P., & Brom, C. (2021). Can video games     #
#   change attitudes towards history? Results from a laboratory experiment        #
#   measuring short- and long-term effects. Journal of Computer Assisted Learning,#
#   1–22. https://doi.org/10.1111/jcal.12575                                      #
#                                                                                 #
# adjusted from Supplementary material                                            #
# accessible at https://doi.org/10.6084/m9.figshare.14690934                      #
###################################################################################

###################
# load libraries
###################

library(ShinyItemAnalysis)
library(difNLR)
library(nlme)
library(lmerTest)
library(effsize)
library(psychometric)
library(corrplot)
library(ggplot2)
library(ggpubr)
# devtools::install_github("cardiomoon/ggiraphExtra") # ggPredict()
library(ggiraphExtra)
library(tidyr)
library(cowplot)
library(AICcmodavg)
library(here)
library(usethis)



###############################
# load, explore and prepare data
###############################

data <- read.csv(here("sampleRcode/data/AttitudesExpulsion.csv"), na.strings = "NA")

head(data)
summary(data)
names(data)
summary(data[, 1:50])

# relevel gender
data$Gender <- relevel(as.factor(data$Gender), 2)

data$Group <- as.factor(data$Group)
colnames(data)[which(names(data) == "IDan")] <- "ID"
# View(data)

# save for ShinyItemAnalysis package
AttitudesExpulsion <- data
usethis::use_data(AttitudesExpulsion)

# save(AttitudesExpulsion, file = "AttitudesExpulsion.rda")
# load(file = "AttitudesExpulsion.rda")

# data(AttitudesExpulsion, package = "ShinyItemAnalysis")
# data <- AttitudesExpulsion

###############################
