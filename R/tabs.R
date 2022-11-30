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

# https://docs.google.com/spreadsheets/d/1IkGidfU4SQJ_ZtOEqfn8licNCuWGlq_MtGuWWuIL1Js/edit#gid=2008440809
rstdat <- read_sheet('1IkGidfU4SQJ_ZtOEqfn8licNCuWGlq_MtGuWWuIL1Js', sheet = 'FullDatabase')

taball <- rstdat_tab(rstdat, yrrng = c(1971, 2021), family = fml)

tab2021 <- rstdat_tab(rstdat, yrrng = 2021, family = fml)

save(taball, file = here('tabs/taball.RData'))
save(tab2021, file = here('tabs/tab2021.RData'))
