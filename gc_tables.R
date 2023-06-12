# Extract select tables from the gotham culture report.

# Author: Adam
# Version: 2023-05-05

# Packages
packs <- c("tidyverse", "tabulizer", "ggplot2")
lapply(packs, require, character.only = TRUE)

# Parameters
# Not Applicable

# ============================================================================

# Q 37_4: I feel valued and supported by Division Leadership --------------
#extract table
leadership_value_raw <- 
  extract_areas(".\\SPF Report 31 Mar 2023.pdf", pages = 38:39, "data.frame")
leadership_value_raw
leadership_value0 <- rbind(leadership_value_raw[[1]], leadership_value_raw[[2]])

#make a clean work groups vector
(multiple_rows <- leadership_value0[, 1][leadership_value0[, 2] == ""])
leadership_value0[, 1]
(workgroups <- leadership_value0[, 1][leadership_value0[, 2] != ""])
workgroups[3] <- paste(workgroups[3], multiple_rows[1], multiple_rows[2])
workgroups[4] <- paste(workgroups[4], multiple_rows[3], multiple_rows[4])
workgroups[7] <- paste(workgroups[7], multiple_rows[5])
workgroups[11] <- paste(workgroups[11], multiple_rows[6])
workgroups[19] <- paste(workgroups[19], multiple_rows[7])
workgroups[20] <- paste(workgroups[20], multiple_rows[8])
workgroups[21] <- paste(workgroups[21], multiple_rows[9], multiple_rows[10])
workgroups[22] <- paste(workgroups[22], multiple_rows[11])

#categories for agree/disagree questions
agree_cats <- c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree")

#Build and save table
leadership_value <- leadership_value0[leadership_value0[, 2] != "", ]
leadership_value[, 1] <- workgroups
colnames(leadership_value) <- c("Workgroup", agree_cats)
saveRDS(leadership_value, ".\\gCtables\\leadership_value.rds")

# Q 15: Decision makers explain rationale behind decisions --------------
#extract table
rationale_raw <- 
  extract_areas(".\\SPF Report 31 Mar 2023.pdf", pages = 16:17, "data.frame")
rationale_raw
rationale0 <- rbind(rationale_raw[[1]], rationale_raw[[2]][, -2])

#Build and save table
rationale <- rationale0[rationale0[, 2] != "", ]
rationale[, 1] <- workgroups
colnames(rationale) <- c("Workgroup", agree_cats)
saveRDS(rationale, ".\\gCtables\\rationale.rds")

# Q 33: ADF&G provides me with opportunities to develop my skills and prepare me for advancement  --------------
#extract table
skills_raw <- 
  extract_areas(".\\SPF Report 31 Mar 2023.pdf", pages = 32:33, "data.frame")
skills_raw
skills0 <- rbind(skills_raw[[1]], skills_raw[[2]][, -c(2:3)])

#Build and save table
skills <- skills0[skills0[, 2] != "", ]
skills[, 1] <- workgroups
colnames(skills) <- c("Workgroup", agree_cats)
saveRDS(skills, ".\\gCtables\\skills.rds")

# Q 51: I generally have the flexibility I need with my work schedule  --------------
#extract table
flexibility_raw <-  extract_areas(".\\SPF Report 31 Mar 2023.pdf", pages = 56:57, "data.frame")
flexibility_raw
flexibility0 <- rbind(flexibility_raw[[1]], flexibility_raw[[2]])

#Build and save table
flexibility <- flexibility0[flexibility0[, 2] != "", ]
flexibility[, 1] <- workgroups
colnames(flexibility) <- c("Workgroup", agree_cats)
saveRDS(flexibility, ".\\gCtables\\flexibility.rds")
