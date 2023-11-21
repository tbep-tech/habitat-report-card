library(tidyverse)
library(here)

# simple version of all restoration projects --------------------------------------------------

# from https://github.com/tbep-tech/TBEP_Habitat_Restoration
pth <- 'https://raw.githubusercontent.com/tbep-tech/TBEP_Habitat_Restoration/main/Habitat_Restoration_Clean.csv'

rstdatall <- read.csv(pth, stringsAsFactors = F) %>% 
  select(
    Year = Federal_Fiscal_Year,
    Category = PrimaryHabitat,
    Activity = GeneralActivity,
    Acres,
    Miles,
    Feet
  ) %>%
  mutate(
    Acres = as.numeric(Acres),
    Miles = case_when(
      is.na(Miles) & !is.na(Feet) ~ Feet / 5280,
      T ~ Miles
    ),
    Category = ifelse(Category == '', NA, Category),
    Activity = ifelse(Activity == '', NA, Activity)
  ) %>% 
  select(Year, Category, Activity, Acres, Miles)

save(rstdatall, file = here('data/rstdatall.RData'))
