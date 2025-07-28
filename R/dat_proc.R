# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)

source(here('R/funcs.R'))

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

# restoration map shapefile -------------------------------------------------------------------

source('https://raw.githubusercontent.com/tbep-tech/hmpu-workflow/master/R/funcs.R')

fl <- 'https://raw.githubusercontent.com/tbep-tech/hmpu-workflow/master/data/restorelyr.RData'

download.file(fl, here('data/restorelyr.RData'))

data(restorelyr)

restmap <- restdat_fun(restorelyr)

st_write(restmap, here('data/restmap.shp'))

# oyster hsi ----------------------------------------------------------------------------------

pth <- 'T:/05_GIS/OYSTER_HSI/TampaBay_OysterIndex.gdb'

oysterhsi <- st_read(pth, layer = 'TB_OHSI_Polygon') |> 
  st_transform(crs = 6443)

save(oysterhsi, file = here('data/oysterhsi.RData'), compress = 'xz')

# SWFWMD oyster coverage by bay segment -------------------------------------------------------

data('swfwmdtbseg', package = 'tbeptools')
toint <- swfwmdtbseg |> 
  st_transform(crs = 6443)

oysdat <- tibble(
    yr = c(2014, 2016, 2018, 2020, 2022, 2024)
  ) |> 
  mutate(
    dat = purrr::map(yr, function(x){

      dat <- rdataload(paste0('sgdat', x))
      
      cat(x, '\n')
      
      out <- dat |> 
        dplyr::filter(FLUCCSCODE == '6540') |> 
        st_intersection(toint) |>
        select(-FLUCCSCODE) |>
        mutate(
          acres = as.numeric(units::set_units(st_area(geometry), 'acre'))
        )
      
      return(out)
      
    })
  ) |> 
  unnest('dat') |> 
  summarise(
    acres = sum(acres, na.rm = TRUE),
    .by = c('yr', 'segment')
  )
  
save(oysdat, file = here('data/oysdat.RData'), compress = 'xz')

# 2024 oyster union with OHSI 14, 15 by bay segment -------------------------------------------

load(file = here('data/oysterhsi.RData'))

data(swfwmdtbseg, package = 'tbeptools')

levs <- c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay',
          'Lower Tampa Bay', 'Boca Ciega Bay', 'Terra Ceia Bay', 'Manatee River')

toint <- swfwmdtbseg |> 
  st_transform(st_crs(oysterhsi))

oydat <- oysterhsi |> 
  select(
    osi = OSI_VALUE
  ) |> 
  filter(osi %in% c(14, 15)) |> 
  st_intersection(toint) |>
  mutate(
    acres = as.numeric(units::set_units(st_area(Shape), 'acre'))
  ) 

sgdat2024 <- rdataload('sgdat2024')
oydat2024 <- sgdat2024 |> 
  dplyr::filter(FLUCCSCODE == '6540') |> 
  st_intersection(toint)

a <- oydat2024 
b <- oydat |> 
  select(osi, segment) |> 
  rename(geometry = Shape)

st_agr(a) = "constant"
st_agr(b) = "constant"

# some clean up stuff for slivers
a <- a |> 
  st_set_precision(1e5) |> 
  st_make_valid() |> 
  st_buffer(dist = 0)
b <- b |> 
  st_set_precision(1e5) |> 
  st_make_valid() |> 
  st_buffer(dist = 0)
aunion <- a |> 
  st_union() |> 
  st_set_precision(1e5) |> 
  st_make_valid() |> 
  st_buffer(dist = 0)
bunion <- b |> 
  st_union() |> 
  st_set_precision(1e5) |> 
  st_make_valid() |> 
  st_buffer(dist = 0)

# dat <- st_union(oydat2024, oydat)
op1 <- st_difference(a, bunion)
op2 <- st_difference(b, aunion)
op3 <- st_intersection(a, b) |> 
  select(-segment.1)

oysunion <- bind_rows(op1, op2, op3) |> 
  mutate(
    FLUCCSCODE = case_when(
      is.na(FLUCCSCODE) ~ 'Not Oyster', 
      FLUCCSCODE == '6540' ~ 'Oyster',
    ), 
    osi = case_when(
      is.na(osi) ~ 'Not 14 or 15', 
      osi == 14 ~ '14', 
      osi == 15 ~ '15'
    ),
    acres = as.numeric(units::set_units(st_area(geometry), 'acre'))
  )

rownames(oysunion) <- 1:nrow(oysunion)

save(oysunion, file = here('data/oysunion.RData'))
