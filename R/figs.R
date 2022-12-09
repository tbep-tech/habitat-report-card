library(tidyverse)
library(here)
library(showtext)
library(patchwork)

load(file = here('data/rstdatall.RData'))

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

source(here('R/funcs.R'))

# cumulative effort ---------------------------------------------------------------------------

# data prep
rstsum <- rstdatall %>% 
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
  scale_fill_manual(values = colorspace::sequential_hcl(2, palette = 'Hawaii')) +
  labs(
    y = 'Cumulative projects', 
    fill = NULL,
  ) + 
  thm

p2 <- ggplot(toplo1, aes(x = Year, y = cumacres, fill = Activity)) + 
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colorspace::sequential_hcl(2, palette = 'Hawaii')) +
  labs(
    y = 'Cumulative acres', 
    fill = NULL,
  ) + 
  thm

p3 <- ggplot(toplo1, aes(x = Year, y = cummiles, fill = Activity)) + 
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colorspace::sequential_hcl(2, palette = 'Hawaii')) +
  labs(
    y = 'Cumulative miles', 
    fill = NULL,
  ) + 
  thm

pout <- p1 + p2 + p3 + plot_layout(ncol = 1, guides = 'collect') & thm

png(here('figs/cumulative.png'), height = 7, width = 7, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()
