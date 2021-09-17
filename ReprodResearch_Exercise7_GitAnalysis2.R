############################# RAW DATA ################################
library(dplyr)
library(data.table)

# Adding the dataframe Verhaltensdaten.txt to R
df <- read.table ("~/Desktop/Gittest/Analysis_ElianePflugi/Verhaltensdaten.txt", sep="\t", header = TRUE)
count(df)
df


################################# PREPROCESSING ################################
#removing participants with "remove" in subject filter
df2 <- df [!(df$subject_filter=="remove"),]
count(df2)
df2

# calculating the average for the independent variables attention and working memory
### attention
df2$mac_2b_mean <- rowMeans(df2[,c("mac_2b_s1", "mac_2b_s2")])

### WM
df2$D2_mean <- rowMeans(df2[,c("D2_KL_Tag1", "D2_KL_Tag2")])

# checking the correlation of the dependent variables with cor(df2[, c ("", "", "", "")], use = pairwise.complete)
### iaps_SD consists of two measuring days
### should the avrage be calculated for that too? At least we did last time so i presume we do it again
df2$iaps_SD <- rowMeans(df2[,c("iaps_hit_SD_day1", "iaps_hit_SD_day2")])

# checking the correlation of the dependent variables
cor(df2[,c ("iaps_SD", "iaps_hit_LD", "words_hit_IR", "words_hit_SD")], use = "pairwise.complete")



############################### MAIN ANALYSIS #######################################
######################### association of attention & WM ############################

# UV1 -> D2_mean
# UV2 -> mac_2b_mean

# AV1 -> iaps_SD
# AV2 -> iaps_hit_LD
# AV3 -> words_hit_IR
# AV4 -> words_hit_SD

# calculating the association of attention & WM
summary(lm(iaps_SD ~ D2_mean + mac_2b_mean, data = df2))

# additional analysis for validating / investigating the results
summary(lm(iaps_hit_LD ~ D2_mean + mac_2b_mean, data = df2))
summary(lm(words_hit_IR ~ D2_mean + mac_2b_mean, data = df2))
summary(lm(words_hit_SD ~ D2_mean + mac_2b_mean, data = df2))

# the results found can be generalized over time and to another task






