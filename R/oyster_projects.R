library(tidyverse)
library(here)
library(dplyr)

# search all restoration projects --------------------------------------------------

# from https://github.com/tbep-tech/TBEP_Habitat_Restoration
pth <- 'https://raw.githubusercontent.com/tbep-tech/TBEP_Habitat_Restoration/main/restoration.csv'

rstdata <- read.csv(pth, stringsAsFactors = F)%>%
  mutate(Year = Federal_Fiscal_Year,
         Partner = Lead_Implementer,
         Lat = Latitude, 
         Lon = Longitude,
         Restoration_Technique,
         Primary = PrimaryHabitat,
         General = GeneralHabitat,
         Activity = GeneralActivity,
    Miles = case_when(
      Activity %in% 'Maintenance' & Restoration_Technique %in% 'Debris Removal' ~ NA_real_,
      T ~ Miles
    ),
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
  ) 

# Check for "oyster" in any column-----------------------------------------------
result <- apply(rstdata, 2, function(x) grepl(c("oyster","Oyster"), x, ignore.case = TRUE))
# Find the rows containing "oyster" in any column
rows_with_oyster <- which(rowSums(result) > 0)
# Display the rows where "oyster" appears
oyster <- rstdata[rows_with_oyster, ]%>%
  filter(General %in% c("Estuarine","Mix (estuarine and freshwater)"), Activity=="Restoration")
test <- oyster%>%
  filter(Restoration_Technique=='Planting')

#filter by PrimaryHabitat (for years >=2006)------------------------------------
oyster2 <- rstdata[rows_with_oyster, ]%>%
  filter(Primary %in% c("Oyster Bars","Hard Bottom","Mangrove Forests","Living Shorelines","Artificial Reefs",
        "Low-Salinity Salt Marsh", "Intertidal Estuarine (Other)"), Activity=='Restoration')

#combine dataframes and delete duplicates---------------------------------------------------
oyster_all <- distinct(rbind(oyster, oyster2))%>%
  #deleting projects that are not oyster-related
  filter(!(View_Detail %in% c("18677", "18959","21999")) & Project_Name != "Autism inspired Plants!")%>%
  arrange(Year)%>%
  select(Year, Partner, Lat, Lon, Project_Name, Project_Description, Habitat_Type, 
                      Restoration_Technique, Project_Benefits, Primary, General, Activity, Acres, Miles)

save(oyster_all, file = here('data/subsets/oysterprojects.RData'))
write.csv(oyster_all, here("data/subsets/oysterprojects.csv"), row.names = FALSE)
