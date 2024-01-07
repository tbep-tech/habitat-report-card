library(tidyverse)
library(here)
library(reactable)
library(htmltools)
library(htmlwidgets)
library(sf)
library(units)
library(flextable)
library(webshot2)

source(here('R/funcs.R'))

load(file = here('data/rstdatall.RData'))

# maximum year
cur <- max(rstdatall$Year)

# compiled habitat tables ---------------------------------------------------------------------

# general and primary
gentab <- tab_fun(rstdatall, yrrng = c(1971, cur), rowgrp = 'General')
pritab <- tab_fun(rstdatall, yrrng = c(2006, cur), rowgrp = 'Primary')

# add css
gentab$dependencies <- list(
  htmlDependency(
    name = "styles",
    version = "4.3.0",
    src = here('docs'),
    stylesheet = 'styles.css')
)
pritab$dependencies <- list(
  htmlDependency(
    name = "styles",
    version = "4.3.0",
    src = here('docs'),
    stylesheet = 'styles.css')
)

# save as html, then png, remove html, then trim png with imagemagick
saveWidget(pritab, here('pritab.html'), selfcontained = TRUE)
webshot(url = here('pritab.html'), file = here('docs/tabs/pritab.png'), zoom = 4)
file.remove(here('pritab.html'))
system('magick convert "docs/tabs/pritab.png" -trim docs/tabs/pritab.png')

saveWidget(gentab, here('gentab.html'), selfcontained = TRUE)
webshot(url = here('gentab.html'), file = here('docs/tabs/gentab.png'), zoom = 4, delay = 1)
file.remove(here('gentab.html'))
system('magick convert "docs/tabs/gentab.png" -trim docs/tabs/gentab.png')

# progress table ------------------------------------------------------------------------------

# save as png from url, then trim png with imagemagick
webshot(url = 'https://tbep-tech.github.io/hmpu-workflow/target_table_simple.html', file = here('docs/tabs/target_table_simple.png'), zoom = 4)
system('magick convert "docs/tabs/target_table_simple.png" -trim docs/tabs/target_table_simple.png')

