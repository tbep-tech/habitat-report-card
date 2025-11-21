# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(mapedit)
library(mapview)

source(here('R/funcs.R'))
crtyr="2025" #most recent year of restoration data available
yrstogo="5" #years to go in current restoration plan (2030-crtyr)

# simple version of all restoration projects --------------------------------------------------

# from https://github.com/tbep-tech/TBEP_Habitat_Restoration
pth <- 'https://raw.githubusercontent.com/tbep-tech/TBEP_Habitat_Restoration/main/restoration.csv'

rstdatall <- read.csv(pth, stringsAsFactors = F) %>% 
  select(
    Year = Federal_Fiscal_Year,
    Partner = Lead_Implementer,
    Lat = Latitude, 
    Lon = Longitude,
    Restoration_Technique,
    Primary = PrimaryHabitat,
    General = GeneralHabitat,
    Activity = GeneralActivity,
    Acres,
    Miles,
    Feet
  ) %>%
  mutate(
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
  ) %>% 
  select(Year, Partner, Lat, Lon, Primary, General, Activity, Acres, Miles)

save(rstdatall, file = here('data/rstdatall.RData'))

#Calculating % annual restoration effort needed to meet 2030 targets------------
# Download targets file
download.file(
  url = "https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/trgs.RData",
  destfile = here("Data/trgs.RData")
)
load(url('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/trgs.RData'))


#summarize habitat restoration by habitat type for 2020-current year;
data_filtered <- rstdatall%>%
  filter(Year>= 2020 & Year <= 2025,
         Activity=="Restoration")%>%
  mutate(HMPU_TARGETS=Primary)%>%
  mutate(HMPU_TARGETS = case_when(
    HMPU_TARGETS == "Low-Salinity Salt Marsh" ~ "Salt Marshes",
    HMPU_TARGETS == "Non-forested Freshwater Wetlands" ~ "Non-Forested Freshwater Wetlands",
    HMPU_TARGETS == "Uplands (Non-coastal)" ~ "Native Uplands",
    HMPU_TARGETS == "Intertidal Estuarine (Other)" ~ "Total Intertidal",
    TRUE ~ HMPU_TARGETS
  ))

# Summary by habitat type
habitat_summary <- data_filtered %>%
  group_by(HMPU_TARGETS) %>%
  summarise(
    sum_acres=sum(Acres,na.rm=TRUE),
    sum_miles=sum(Miles,na.rm=TRUE)
  )

habitat_crtyr <-data_filtered %>%
  group_by(HMPU_TARGETS)%>%
  filter(Year==crtyr,
         Activity=="Restoration")

# current year summary by habitat type
habitat_summary_crtyr <- habitat_crtyr %>%
  group_by(HMPU_TARGETS) %>%
  summarise(
    sum_acres_crt=sum(Acres,na.rm=TRUE),
    sum_miles_crt=sum(Miles,na.rm=TRUE)
  )

# Keep all records from habitat_summary
restored <- habitat_summary %>%
  left_join(rsttargets, by = "HMPU_TARGETS")%>%
  left_join(habitat_summary_crtyr, by = "HMPU_TARGETS")

#Find acres/miles to go based on 2025 LULC

restore_pct <- restored %>%
  mutate(AcrestoGo=Target2030-(sum_acres+acres_LULC),
         MilestoGo=Target2030-sum_miles)
  
# alternative approach ---------------------------------------------------

load(url('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/acres.RData'))
load(url('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/subtacres.RData'))
load(url('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/trgs.RData'))

trgshr <- trgs |> 
  select(HMPU_TARGETS, Target2030)

pth <- 'https://raw.githubusercontent.com/tbep-tech/TBEP_Habitat_Restoration/main/restoration.csv'

# starting restoration data for 2017 (inter/supratidal) and 2018 (subtidal)
strdat <- bind_rows(
    filter(acres, name == 2017),
    filter(subtacres, name == 2018)
  ) |>
  ungroup() |> 
  select(-name) |> 
  rbind(
    tibble(
      HMPU_TARGETS = c('Artificial Reefs', 'Hard Bottom', 'Tidal Tributaries', 'Living Shorelines'),
      Acres = c(166, 423, 387, 11.3)
    )
  ) |> 
  rename(start = Acres) |> 
  filter(HMPU_TARGETS %in% c(trgshr$HMPU_TARGETS))

# currrent restoration data since 2017
curdat <- read.csv(pth, stringsAsFactors = F) |> 
  filter(GeneralActivity == 'Restoration' & Federal_Fiscal_Year >= 2017) |>
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

# current restoration progress based on 2017 starting for inter/supratidal, 2018 for subtidal
prgdat <- curdat |> 
  tidyr::complete(Year, HMPU_TARGETS = strdat$HMPU_TARGETS, fill=list(Acres=0, Miles=0)) |> 
  mutate(
    progress = case_when(
      HMPU_TARGETS %in% c('Tidal Tributaries', 'Living Shorelines') ~ Miles, 
      TRUE ~ Acres
    )
  ) |> 
  select(-Miles, -Acres) |> 
  filter(!(HMPU_TARGETS %in% c('Artificial Reefs', 'Oyster Bars', 'Hard Bottom', 'Seagrasses', 'Tidal Flats') & Year == 2017)) |> 
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

# 2030 estimate based on last five years, likel to meet as T/F
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

