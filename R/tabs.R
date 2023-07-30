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

# HMPU compiled table -------------------------------------------------------------------------

# all years
taball <- rstdat_tab(rstdatall, yrrng = c(2006, 2022), family = fml)

# 2022
tab2022 <- rstdat_tab(rstdatall, yrrng = 2022, family = fml)

save(taball, file = here('tabs/taball.RData'))
save(tab2022, file = here('tabs/tab2022.RData'))


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
cap <- 'Summary of the Recommended 2030 Targets and 2050 Goals'
tabsub <- target_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, restorelyr, trgs, 
                     cap, stratsel = 'Subtidal')
tabnotsub <- target_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, restorelyr, trgs, 
                        cap, stratsel = 'Not Subtidal')

# save_as_html(tab, path = 'docs/target_table.html', title = 'Target Table')

