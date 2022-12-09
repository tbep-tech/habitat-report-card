library(tidyverse)
library(here)
library(reactable)
library(showtext)
library(htmltools)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

source(here('R/funcs.R'))

load(file = here('data/rstdatall.RData'))

# HMPU compiled table -------------------------------------------------------------------------

# all years
taball <- rstdat_tab(rstdatall, yrrng = c(1971, 2022), family = fml)

# 2022
tab2022 <- rstdat_tab(rstdatall, yrrng = 2022, family = fml)

save(taball, file = here('tabs/taball.RData'))
save(tab2022, file = here('tabs/tab2022.RData'))
