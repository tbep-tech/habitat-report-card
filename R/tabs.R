library(tidyverse)
library(here)
library(reactable)
library(htmltools)
library(sf)
library(units)
library(flextable)

source(here('R/funcs.R'))

load(file = here('data/rstdatall.RData'))

# maximum year
cur <- max(rstdatall$Year)

# compiled habitat tables ---------------------------------------------------------------------

# general and primary
gentab <- tab_fun(rstdatall, yrrng = c(1971, cur), rowgrp = 'General')
pritab <- tab_fun(rstdatall, yrrng = c(2006, cur), rowgrp = 'Primary')

save(gentab, file = here('tabs/gentab.RData'))
save(pritab, file = here('tabs/pritab.RData'))


