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
  filter(Year >= 2006) %>% 
  filter(!is.na(Activity)) %>%
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
  scale_fill_manual(values = colorspace::sequential_hcl(2, palette = 'Hawaii')) +
  labs(
    y = 'Cumulative projects', 
    fill = NULL,
  ) 

p2 <- ggplot(toplo1, aes(x = Year, y = cumacres, fill = Activity)) + 
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  scale_fill_manual(values = colorspace::sequential_hcl(2, palette = 'Hawaii')) +
  labs(
    y = 'Cumulative acres', 
    fill = NULL,
  ) 

p3 <- ggplot(toplo1, aes(x = Year, y = cummiles, fill = Activity)) + 
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  scale_fill_manual(values = colorspace::sequential_hcl(2, palette = 'Hawaii')) +
  labs(
    y = 'Cumulative miles', 
    fill = NULL,
  ) 

pout <- p1 + p2 + p3 + plot_layout(ncol = 1, guides = 'collect') & thm

png(here('docs/figs/cumulative.png'), height = 7, width = 7, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()

# cumulative effort by habitat ----------------------------------------------------------------

levs <- c("Artificial Reefs", "Coastal Uplands", "Forested Freshwater Wetlands", 
          "Hard Bottom", "Intertidal Estuarine (Other)", "Living Shorelines", 
          "Low-Salinity Salt Marsh", "Mangrove Forests", "Non-forested Freshwater Wetlands", 
          "Oyster Bars", "Seagrasses", "Tidal Tributaries", "Uplands (Non-coastal)")

# data prep
rstsum <- rstdatall %>% 
  arrange(Year, Category) %>% 
  filter(!is.na(Category)) %>%
  filter(Year >= 2006) %>% 
  mutate(
    Category = factor(Category, levels = levs)
  ) %>% 
  select(-Activity) %>% 
  mutate(prj = 1) %>% 
  tidyr::complete(Year, Category, fill = list(Acres = 0, Miles = 0, prj = 0)) %>% 
  group_by(Year, Category) %>% 
  summarise(
    tot = sum(prj),
    Acres = sum(Acres, na.rm = T),
    Miles = sum(Miles, na.rm = T),
    .groups = 'drop'
  ) %>% 
  group_by(Category) %>% 
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
ncol <- length(unique(toplo1$Category))

colfun <- colorRampPalette(brewer.pal(8, "Accent"))

p1 <- ggplot(toplo1, aes(x = Year, y = cumtot, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(length(unique(toplo1$Category)))) +
  labs(
    y = 'Cumulative projects', 
    fill = NULL,
  ) 

p2 <- ggplot(toplo1, aes(x = Year, y = cumacres, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(length(unique(toplo1$Category)))) +
  labs(
    y = 'Cumulative acres', 
    fill = NULL,
  ) 

p3 <- ggplot(toplo1, aes(x = Year, y = cummiles, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(length(unique(toplo1$Category)))) +
  labs(
    y = 'Cumulative miles', 
    fill = NULL,
  ) 

pout <- p1 + p2 + p3 + plot_layout(ncol = 1, guides = 'collect') & thm & guides(fill = guide_legend(nrow = 5))

png(here('docs/figs/cumulativehmp.png'), height = 8, width = 7, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()

p1 <- ggplot(toplo1, aes(x = Year, y = tot, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year)), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_area(position = 'fill', alpha = 0.8) + 
  scale_fill_manual(values = colfun(length(unique(toplo1$Category)))) +
  labs(
    y = 'Projects', 
    fill = NULL,
  ) 

p2 <- ggplot(toplo1, aes(x = Year, y = Acres, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year)), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_area(position = 'fill', alpha = 0.8) + 
  scale_fill_manual(values = colfun(length(unique(toplo1$Category)))) +
  labs(
    y = 'Acres', 
    fill = NULL,
  ) 

p3 <- ggplot(toplo1, aes(x = Year, y = Miles, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year)), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_area(position = 'fill', alpha = 0.8) + 
  scale_fill_manual(values = colfun(length(unique(toplo1$Category)))) +
  labs(
    y = 'Miles', 
    fill = NULL,
  ) 

pout <- p1 + p2 + p3 + plot_layout(ncol = 1, guides = 'collect') & thm & guides(fill = guide_legend(nrow = 5))

png(here('docs/figs/prophmp.png'), height = 8, width = 7, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()