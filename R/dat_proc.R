# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(mapedit)
library(mapview)

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

# opportunity areas shapefile --------------------------------------------

# copied by hand, this didnt work (couldn't read in file after copied)
rt <- 'https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/shapefiles/oppmap'
fls <- paste0(rt, c('.shp', '.shx', '.dbf', '.prj'))

purrr::walk(fls, function(x){
  download.file(x, here('data/', basename(x)))
})

# oyster hsi ----------------------------------------------------------------------------------

pth <- 'T:/05_GIS/OYSTER_HSI/TampaBay_OysterIndex.gdb'

oysterhsi <- st_read(pth, layer = 'TB_OHSI_Polygon') |> 
  st_transform(crs = 6443)

save(oysterhsi, file = here('data/oysterhsi.RData'), compress = 'xz')

# manually correct swfwmdtbseg layer  ---------------------------------------------------------

# manatee river portion is incomplete
data('swfwmdtbseg', package = 'tbeptools')

swfwmdtbsegcor <- editFeatures(swfwmdtbseg)

save(swfwmdtbsegcor, file = here('data/swfwmdtbsegcor.RData'), compress = 'xz')

# SWFWMD oyster coverage by bay segment -------------------------------------------------------

load(file = here('data/swfwmdtbsegcor.RData'))

toint <- swfwmdtbsegcor |> 
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

# 2024 oyster coverage ------------------------------------------------------------------------

# extra oyster data
load(file = url('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/oyse.RData'))

# 2024 data, add extra
sgdat2024 <- rdataload('sgdat2024')
oydat2024 <- sgdat2024 |> 
  dplyr::filter(FLUCCSCODE == '6540') |> 
  mutate(
    FLUCCSCODE = 'Oyster'
  ) 
oyse <- oyse |> 
  mutate(FLUCCSCODE = 'Oyster') |> 
  select(FLUCCSCODE, geometry = x)
oydat2024 <- oydat2024 |> 
  bind_rows(oyse)

save(oydat2024, file = here('data/oydat2024.RData'), compress = 'xz')

# 2024 oyster union with OHSI 14, 15 by bay segment -------------------------------------------

load(file = here('data/oysterhsi.RData'))
load(file = here('data/oydat2024.RData'))
load(file = here('data/swfwmdtbsegcor.RData'))

levs <- c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay',
          'Lower Tampa Bay', 'Boca Ciega Bay', 'Terra Ceia Bay', 'Manatee River')

toint <- swfwmdtbsegcor |> 
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

oydat2024 <- oydat2024 |> 
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
      T ~ FLUCCSCODE
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

# 2022 max oyster coverage --------------------------------------------------------------------

load(file = here('data/swfwmdtbsegcor.RData'))  

toint <- swfwmdtbsegcor |> 
  st_transform(crs = 6443)

# extra oyster data, original FWC update from early 2024
oyse <- rdataload('oyse', dataurl = 'https://github.com/tbep-tech/hmpu-workflow/raw/e34cc5a9503a0acede0eab9567d7bc9f4e6b4511/data/')

sgdat2022 <- rdataload('sgdat2022')
oydat2022 <- sgdat2022 |> 
  dplyr::filter(FLUCCSCODE == '6540') |> 
  mutate(
    FLUCCSCODE = 'Oyster'
  )
oyse <- oyse |> 
  mutate(FLUCCSCODE = 'Oyster') |> 
  select(FLUCCSCODE, geometry = x)
oydat2022 <- oydat2022 |> 
  bind_rows(oyse) |> 
  st_intersection(toint) |>
  mutate(
    acres = as.numeric(units::set_units(st_area(geometry), 'acre'))
  ) 

save(oydat2022, file = here('data/oydat2022.RData'), compress = 'xz')

