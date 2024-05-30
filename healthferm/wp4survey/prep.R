library(haven)
library(tidyverse)
library(plotly)


pbff <- read_sav(paste0(path,"data_raw/3325_Data_240212_FINAL new id[87].sav"))


# Combine Weight and Weight_New into Weight
pbff <- pbff %>%
  mutate(Weight = coalesce(Weight, Weight_New))

# Combine Height and Height_New into Height
pbff <- pbff %>%
  mutate(Height = coalesce(Height, Height_New)) %>%
# Transform values reported in m to cm
  mutate(Height = ifelse(Height<3, Height*100, Height))

# Replace missing value in Sex
pbff <- pbff %>%
  mutate(Sex = replace_na(Sex, 3))
# Convert the Sex variable to a factor with descriptive labels
pbff$Sex <- as_factor(pbff$Sex)

# Create descriptive variable for education level
pbff <- pbff %>%
  # mutate(edu_desc = as_factor(lvledu))
  mutate(edu_desc = lvledu)
pbff$edu_desc <- factor(pbff$edu_desc, levels = 1:5, labels = c("Primary", "Secondary", "Other post-secondary", "Undergraduate", "Graduate"))

# Create descriptive variable for SES
pbff <- pbff %>%
  mutate(ses_desc = as_factor(SES))

# Create descriptive variable for area
pbff <- pbff %>%
  mutate(area_desc = as_factor(area))

# Replace NA with 0 for number of children
pbff <- pbff %>%
  mutate(nbchildren = replace_na(nbchildren, 0))

# Create dataframes for each submission group
pbff_t1 <- pbff %>%
  filter(data_source == 1)
pbff_t2 <- pbff %>%
  filter(data_source == 2)

# Data submission period
pbff_t1_mindate <- min(pbff_t1$submitdate) %>% format("%Y-%m-%d")
pbff_t1_maxdate <- max(pbff_t1$submitdate) %>% format("%Y-%m-%d")
pbff_t2_mindate <- min(pbff_t2$submitdate) %>% format("%Y-%m-%d")
pbff_t2_maxdate <- max(pbff_t2$submitdate) %>% format("%Y-%m-%d")

write_csv(pbff, "data_csv/pbff.csv")

