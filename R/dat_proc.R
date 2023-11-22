library(tidyverse)
library(here)

# simple version of all restoration projects --------------------------------------------------

# from https://github.com/tbep-tech/TBEP_Habitat_Restoration
pth <- 'https://raw.githubusercontent.com/tbep-tech/TBEP_Habitat_Restoration/main/Habitat_Restoration_Clean.csv'

rstdatall <- read.csv(pth, stringsAsFactors = F) %>% 
  select(
    Year = Federal_Fiscal_Year,, 
    Lat = Latitude, 
    Lon = Longitude,
    Primary = PrimaryHabitat,
    General = GeneralHabitat,
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
    General = case_when(
      General == 'estuarine' ~ 'Estuarine', 
      grepl('^Upland', General) ~ 'Uplands',
      grepl('^Mix|^Other', General) ~ 'Mixed', 
      T ~ General
    ), 
    General = ifelse(General == '', NA, General),
    Primary = ifelse(Primary == '', NA, Primary),
    Activity = ifelse(Activity == '', NA, Activity)
  ) %>% 
  select(Year, Lat, Lon, Primary, General, Activity, Acres, Miles)

save(rstdatall, file = here('data/rstdatall.RData'))
