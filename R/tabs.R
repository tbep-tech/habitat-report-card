library(tidyverse)
library(here)
library(reactable)
library(googlesheets4)
library(googledrive)
library(showtext)
library(htmltools)

# auth google drive
drive_auth(email = 'mbeck@tbep.org')
gs4_auth(token = drive_token())

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

source(here('R/funcs.R'))

# HMPU compiled table -------------------------------------------------------------------------

# https://docs.google.com/spreadsheets/d/1IkGidfU4SQJ_ZtOEqfn8licNCuWGlq_MtGuWWuIL1Js/edit#gid=2008440809
rstdat <- read_sheet('1IkGidfU4SQJ_ZtOEqfn8licNCuWGlq_MtGuWWuIL1Js', sheet = 'FullDatabase')

taball <- rstdat_tab(rstdat, yrrng = c(1971, 2021), family = fml)

tab2021 <- rstdat_tab(rstdat, yrrng = 2021, family = fml)

save(taball, file = here('tabs/taball.RData'))
save(tab2021, file = here('tabs/tab2021.RData'))

# GPRA raw table ------------------------------------------------------------------------------

# https://docs.google.com/spreadsheets/d/1QFX5XXpTgAr4u9wrxa_TUOuF5roUqEhJzZEDUiHi3SE/edit?usp=sharing
rstdat2 <- read_sheet('1QFX5XXpTgAr4u9wrxa_TUOuF5roUqEhJzZEDUiHi3SE')

taball_2 <- rstdat_tab2(rstdat2, yrrng = c(2006, 2022), family = fml)

tab2022_2 <- rstdat_tab2(rstdat2, yrrng = 2022, family = fml)

save(taball_2, file = here('tabs/taball_2.RData'))
save(tab2022_2, file = here('tabs/tab2022_2.RData'))