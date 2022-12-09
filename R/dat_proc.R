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

# create input for table function
rstdat2 <- rstdat2 %>% 
  left_join(lkup, by = c('Habitat Type', 'Restoration Technique'))

rstdatall <- bind_rows(rstdat, rstdat2) %>% 
  select(
    Year = `Federal Fiscal Year`, 
    Category = `HMPU habitat 1`, 
    Acres, 
    Activity = `Activity Name`, 
    `Linear Miles` = `Miles`,
    `Linear Ft` = `Feet`
  ) %>% 
  mutate(
    Activity = case_when(
      Activity %in% c('Establishment', 'Reestablishment', 'Restoration') ~ 'Restoration', 
      Activity %in% c('Enhancement', 'Maintenance', 'Protection', 'Rehabilitation') ~ 'Enhancement'
    )
  )

save(rstdatall, file = here('data/rstdatall.RData'))
