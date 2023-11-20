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

cur <- 2023

# HMPU compiled table -------------------------------------------------------------------------

# all years
taball <- rstdat_tab(rstdatall, yrrng = c(2006, cur), family = fml)

# current year
tabcur <- rstdat_tab(rstdatall, yrrng = cur, family = fml)

save(taball, file = here('tabs/taball.RData'))
save(tabcur, file = here('tabs/tabcur.RData'))

# hmpu targets/goals tables by subtidal, not subtidal -----------------------------------------

source(url('https://raw.githubusercontent.com/tbep-tech/hmpu-workflow/master/R/funcs.R'))

fluccs <- read.csv(url('https://raw.githubusercontent.com/tbep-tech/hmpu-workflow/master/data/FLUCCShabsclass.csv'), stringsAsFactors = F)

lulcfl <- 'lulc2020'
subtfl <- 'sgdat2022'

# inputs
lulc <- rdataload(lulcfl)
subt <- rdataload(subtfl)
hard <- rdataload('hard')
arti <- rdataload('arti')
tidt <- rdataload('tidt')
livs <- rdataload('livs')
coastal <- rdataload('coastal')
strata <- rdataload('strata')
trgs <- rdataload('trgs')
restorelyr <- rdataload('restorelyr')

# make table
cap <- ''
tabsub <- target_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, restorelyr, trgs, 
                     cap, stratsel = 'Subtidal', typ = 'targets')
tabint <- target_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, restorelyr, trgs, 
                        cap, stratsel = 'Intertidal', typ = 'targets')
tabsup <- target_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, restorelyr, trgs, 
                     cap, stratsel = 'Supratidal', typ = 'targets')
# save_as_html(tab, path = 'docs/target_table.html', title = 'Target Table')

