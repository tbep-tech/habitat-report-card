library(tidyverse)
library(here)
library(reactable)
library(showtext)
library(htmltools)
library(sf)
library(units)
library(flextable)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

source(here('R/funcs.R'))

load(file = here('data/rstdatall.RData'))

# maximum year
cur <- max(rstdatall$Year)


# compiled habitat tables ---------------------------------------------------------------------

# all years
pritaball <- tab_fun(rstdatall, yrrng = c(2006, cur), family = fml, rowgrp = 'Primary')
gentaball <- tab_fun(rstdatall, yrrng = c(1971, cur), family = fml, rowgrp = 'General')

# current year
pritabcur <- tab_fun(rstdatall, yrrng = cur, family = fml, rowgrp = 'Primary')
gentabcur <- tab_fun(rstdatall, yrrng = cur, family = fml, rowgrp = 'General')

save(pritaball, file = here('tabs/pritaball.RData'))
save(gentaball, file = here('tabs/gentaball.RData'))
save(pritabcur, file = here('tabs/pritabcur.RData'))
save(gentabcur, file = here('tabs/gentabcur.RData'))
