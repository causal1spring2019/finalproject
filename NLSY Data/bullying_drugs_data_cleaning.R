# causal 1 group project

load("/Volumes/homefolders/Kara/mediation_long_clean.Rdata")

## collapse incident hard drug use into every hard drug use variable
library(dplyr)
drugs = long_all %>% group_by(PUBID_1997) %>% summarize(ever_new_user = any(newuser==1), allNA = all(is.na(newuser)))

# code people with all NAs for drug use as NA
drugs$ever_new_user2 = ifelse(is.na(drugs$ever_new_user), 0, 1)
drugs$allNA2 = ifelse(drugs$allNA==TRUE, 1, 0)
drugs$ever_new_user2 = ifelse(drugs$allNA2==1, NA, drugs$ever_new_user2)

# keep only new variable and ID; merge with full data
drugs_short = subset(drugs, select=c(1,4))
full = merge(long_all, drugs_short, by="PUBID_1997", all=TRUE)

#keep first observation for each person
short = full %>% group_by(PUBID_1997) %>% filter(row_number() == 1)

# keep only vars of interest for bullying --> drug use analysis
final_data = subset(short, select=c(1,3,18:19,21:26,28:33,37,38,40:43))

# drop if exposure or outcome are NA
final_data = subset(final_data, !is.na(final_data$bullied_bf_12_1997) & !is.na(final_data$ever_new_user2))

# save dataset
save(final_data, file = "~/Documents/GitHub/finalproject/NLSYdata.Rdata")


# joint distribution of exposure and outcome
table(final_data$bullied_bf_12_1997,final_data$ever_new_user2, useNA="ifany")
table(final_data$bullied_bf_12_1997)
table(final_data$ever_new_user2)
