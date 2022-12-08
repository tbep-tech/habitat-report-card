library(tidyverse)
library(here)
library(googlesheets4)
library(googledrive)
library(showtext)
library(patchwork)

# auth google drive
drive_auth(email = 'mbeck@tbep.org')
gs4_auth(token = drive_token())

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

source(here('R/funcs.R'))

# cumulative effort ---------------------------------------------------------------------------

# https://docs.google.com/spreadsheets/d/1QFX5XXpTgAr4u9wrxa_TUOuF5roUqEhJzZEDUiHi3SE/edit?usp=sharing
rstdat2 <- read_sheet('1QFX5XXpTgAr4u9wrxa_TUOuF5roUqEhJzZEDUiHi3SE')

# data prep
rstsum <- rstdat2 %>% 
  select(
    Year = `Federal Fiscal Year`, 
    Category = `Habitat Type`, 
    Acres, 
    Activity = `Activity Name`, 
    `Linear Miles` = `Miles`,
    `Linear Ft` = `Feet`
  ) %>% 
  filter(!is.na(Activity)) %>% 
  rowwise() %>% 
  mutate(
    Miles = sum(`Linear Miles`,  `Linear Ft` / 5280, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(Year, Activity) %>% 
  summarise(
    tot= n(),
    Acres = sum(Acres, na.rm = T),
    Miles = sum(Miles, na.rm = T),
    .groups = 'drop'
  ) %>% 
  group_by(Activity) %>% 
  mutate(
    cumtot = cumsum(tot),
    cumacres = cumsum(Acres),
    cummiles = cumsum(Miles)
  ) %>% 
  ungroup()

thm <- theme_minimal() + 
  theme(
    legend.position = 'top', 
    axis.title.x = element_blank(), 
    panel.grid.minor.y = element_blank()
  )

toplo1 <- rstsum

p1 <- ggplot(toplo1, aes(x = Year, y = cumtot, fill = Activity)) + 
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  scale_fill_manual(values = colorspace::sequential_hcl(3, palette = 'Hawaii')) +
  labs(
    y = 'Cumulative projects', 
    fill = NULL,
  )

p2 <- ggplot(toplo1, aes(x = Year, y = cumacres, fill = Activity)) + 
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  scale_fill_manual(values = colorspace::sequential_hcl(3, palette = 'Hawaii')) +
  labs(
    y = 'Cumulative acres', 
    fill = NULL,
  )

p3 <- ggplot(toplo1, aes(x = Year, y = cummiles, fill = Activity)) + 
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  scale_fill_manual(values = colorspace::sequential_hcl(3, palette = 'Hawaii')) +
  labs(
    y = 'Cumulative miles', 
    fill = NULL,
  )

pout <- p1 + p2 + p3 + plot_layout(ncol = 1, guides = 'collect') & thm

jpeg(here('figs/cumulative.jpg'), height = 7, width = 7, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()
