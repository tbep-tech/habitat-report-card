library(tidyverse)
library(here)
library(googlesheets4)
library(googledrive)

# auth google drive
drive_auth(email = 'mbeck@tbep.org')
gs4_auth(token = drive_token())

# HMPU compiled table -------------------------------------------------------------------------

# curated table, pull data prior to 2006 to join with GPRA
# https://docs.google.com/spreadsheets/d/1IkGidfU4SQJ_ZtOEqfn8licNCuWGlq_MtGuWWuIL1Js/edit#gid=2008440809
rstdat <- read_sheet('1IkGidfU4SQJ_ZtOEqfn8licNCuWGlq_MtGuWWuIL1Js', sheet = 'FullDatabase')

rstdat <- rstdat %>% 
  select(
    `Federal Fiscal Year` = `Year Reported`, 
    `HMPU habitat 1` = `HMPU Habitat Type for Restoration`, 
    Acres, 
    `Activity Name` = `Basic Activity (Enhance/Rest)`, 
    `Miles` = `Linear Miles`,
    `Feet` = `Linear Feet`
  ) %>% 
  filter(`Federal Fiscal Year` < 2006)

# GPRA raw data
# https://docs.google.com/spreadsheets/d/1QFX5XXpTgAr4u9wrxa_TUOuF5roUqEhJzZEDUiHi3SE/edit?usp=sharing
rstdat2 <- read_sheet('1QFX5XXpTgAr4u9wrxa_TUOuF5roUqEhJzZEDUiHi3SE')

# GPRA to HMPU lookup table
# https://docs.google.com/spreadsheets/d/1jYbODoAHXPqYC69fanINDoRkKmMl6oY9qltqdkC6UfM/edit#gid=741038946
lkup <- read_sheet('1jYbODoAHXPqYC69fanINDoRkKmMl6oY9qltqdkC6UfM')

# GRPA manually assigned lookup table
# https://docs.google.com/spreadsheets/d/1bZtu4P1ji4pdSRcAnWSksHl1AJ-b3pFqxXedSdSHwBY/edit#gid=0
lkupmanual <- read_sheet('1bZtu4P1ji4pdSRcAnWSksHl1AJ-b3pFqxXedSdSHwBY') %>% 
  select(`View Detail`, `HMPU habitat 1`)

# create input for table function
rstdat2 <- rstdat2 %>% 
  left_join(lkup, by = c('Habitat Type', 'Restoration Technique')) %>% 
  left_join(lkupmanual, by = 'View Detail') %>% 
  unite('HMPU habitat 1', `HMPU habitat 1.x`, `HMPU habitat 1.y`, na.rm = T)

rstdatall <- bind_rows(rstdat, rstdat2) %>% 
  select(
    Year = `Federal Fiscal Year`, 
    Category = `HMPU habitat 1`, 
    Acres, 
    Activity = `Activity Name`, 
    `Linear Miles` = `Miles`,
    `Linear Ft` = `Feet`
  ) %>% 
  rowwise() %>% 
  mutate(
    Miles = sum(`Linear Miles`,  `Linear Ft` / 5280, na.rm = T),
    Activity = case_when(
      Activity %in% c('Establishment', 'Reestablishment', 'Restoration') ~ 'Restoration', 
      Activity %in% c('Enhancement', 'Maintenance', 'Protection', 'Rehabilitation') ~ 'Enhancement'
    ), 
    Category = ifelse(Category == '', NA, Category)
  ) %>% 
  ungroup() %>% 
  select(Year, Category, Activity, Acres, Miles)

save(rstdatall, file = here('data/rstdatall.RData'))
