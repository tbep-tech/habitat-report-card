# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(mapedit)
library(mapview)

source(here('R/funcs.R'))

# Load data ---------------------------------------------------

load(url('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/acres.RData'))
load(url('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/subtacres.RData'))
load(url('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/trgs.RData'))

trgshr <- trgs |> 
  select(HMPU_TARGETS, Target2030)

pth <- 'https://raw.githubusercontent.com/tbep-tech/TBEP_Habitat_Restoration/main/restoration.csv'

subtdcat <- c('Seagrasses', 'Oyster Bars', 'Tidal Flats')
nodatcat <- c('Artificial Reefs', 'Hard Bottom', 'Tidal Tributaries', 'Living Shorelines')
updatcat <- c('Mangrove Forests', 'Salt Barrens', 'Salt Marshes', 'Coastal Uplands', 
  'Non-Forested Freshwater Wetlands', 'Forested Freshwater Wetlands', 
  'Native Uplands')

subtdyr <- 2024
nodatyr <- 2017
updatyr <- 2023

# starting restoration data for 2023 (inter/supratidal) and 2024 (subtidal)
# manually add 2017 for not tracked
strdat <- bind_rows(
    filter(acres, name == updatyr),
    filter(subtacres, name == subtdyr)
  ) |>
  ungroup() |> 
  select(-name) |> 
  rbind(
    tibble(
      HMPU_TARGETS = nodatcat,
      Acres = c(166, 423, 387, 11.3)
    )
  ) |> 
  rename(start = Acres) |> 
  filter(HMPU_TARGETS %in% c(trgshr$HMPU_TARGETS))

#Bringing in restoration data archive and performing QA
restdat <- read.csv(pth, stringsAsFactors = F) |>
  #fixing mistake in restoration table, Perico Bayou oyster restoration in 2019, 300 acres is incorrect, 300ftX3.5ft~1000sqft or 0.02 acres
  mutate(Acres=if_else(View_Detail==19688, 0.02, Acres))

# currrent restoration data since 2017
curdat <- restdat |>
  filter(GeneralActivity == 'Restoration' & Federal_Fiscal_Year >= nodatyr) |>
  select(
    Year = Federal_Fiscal_Year,
    HMPU_TARGETS = PrimaryHabitat,
    Acres,
    Miles,
    Feet
  ) |>
  mutate(
    Acres = as.numeric(Acres),
    Miles = case_when(
      is.na(Miles) & !is.na(Feet) ~ Feet / 5280,
      T ~ Miles
    ),
    HMPU_TARGETS = case_when(
      HMPU_TARGETS == "Low-Salinity Salt Marsh" ~ "Salt Marshes",
      HMPU_TARGETS == "Non-forested Freshwater Wetlands" ~ "Non-Forested Freshwater Wetlands",
      HMPU_TARGETS == "Uplands (Non-coastal)" ~ "Native Uplands",
      HMPU_TARGETS == "Intertidal Estuarine (Other)" ~ "Total Intertidal",
      HMPU_TARGETS == '' ~ NA_character_,
      TRUE ~ HMPU_TARGETS
    )
  ) |> 
  filter(HMPU_TARGETS != 'Total Intertidal') |> 
  summarise(
    Acres = sum(Acres, na.rm=TRUE),
    Miles = sum(Miles, na.rm=TRUE),
    .by=c(Year, HMPU_TARGETS)
  ) 

# current restoration progress based on 2023 for inter/supratidal, 2024 for subtidal, 2017 for categories not in lu data
prgdat <- curdat |> 
  tidyr::complete(Year, HMPU_TARGETS = strdat$HMPU_TARGETS, fill=list(Acres=0, Miles=0)) |> 
  mutate(
    progress = case_when(
      HMPU_TARGETS %in% c('Tidal Tributaries', 'Living Shorelines') ~ Miles, 
      TRUE ~ Acres
    )
  ) |> 
  select(-Miles, -Acres) |> 
  filter(
    (HMPU_TARGETS %in% nodatcat & Year >= nodatyr) |
    (HMPU_TARGETS %in% updatcat & Year >= updatyr) |
    (HMPU_TARGETS %in% subtdcat & Year >= subtdyr)
  ) |> 
  arrange(HMPU_TARGETS, Year) |> 
  mutate(
    progress = cumsum(progress),
    .by = HMPU_TARGETS
  ) |> 
  full_join(strdat, by="HMPU_TARGETS") |> 
  mutate(
    progress = progress + start
  ) |> 
  select(-start)

#Current year progress met, 
curmet <- prgdat |>
  left_join(trgshr, by="HMPU_TARGETS") |> 
  filter(Year == max(Year)) |>
  mutate(
    curmet = progress >= Target2030 
  )

# 2030 estimate based on last five years, likely to meet as T/F
tnddat <- prgdat |> 
  filter(Year <= max(Year) & Year > (max(Year) - 5)) |> 
  nest(data = c(Year, progress)) |> 
  mutate(
    est2030 = purrr::map_dbl(data, ~{
      mod <- lm(progress ~ Year, data = .x)
      round(predict(mod, newdata = data.frame(Year = 2030)), 1)
    })
  ) |> 
  unnest('est2030') |> 
  left_join(trgshr, by="HMPU_TARGETS") |> 
  mutate(
    estmet = est2030 >= Target2030
  )

#Combine current year target met and 2030 estimate
rsttrg <- curmet |>
  select(-Target2030) |>
  full_join(tnddat, by="HMPU_TARGETS") |>
  select(HMPU_TARGETS,Target2030,Year,everything() & -data)
