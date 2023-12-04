library(tidyverse)
library(here)
library(showtext)
library(patchwork)
library(plotrix)
library(RColorBrewer)
library(scales)
library(tbeptools)
library(ggspatial)
library(sf)

load(file = here('data/rstdatall.RData'))

# get font
font_add_google("Roboto")
fml <- "Roboto"
# showtext_auto()

source(here('R/funcs.R'))

# maximum year
cur <- max(rstdatall$Year)

# cumulative effort ---------------------------------------------------------------------------

# data prep
rstsum <- rstdatall %>% 
  filter(Year >= 2006) %>% 
  filter(!is.na(Activity)) %>%
  filter(Activity != 'Protection') %>%
  group_by(Year, Activity) %>% 
  summarise(
    tot= n(),
    Acres = sum(Acres, na.rm = T),
    Miles = sum(Miles, na.rm = T),
    .groups = 'drop'
  ) %>% 
  complete(Year, Activity, fill = list(tot = 0, Acres = 0, Miles = 0)) %>%
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

# cumulative projects, whole database ---------------------------------------------------------

# data prep
rstsum <- rstdatall %>% 
  group_by(Year) %>% 
  summarise(
    tot= n(), 
    .groups = 'drop'
  ) %>% 
  mutate(
    cumtot = cumsum(tot)
  ) 

thm <- theme_minimal() + 
  theme(
    legend.position = 'top', 
    axis.title.x = element_blank(), 
    panel.grid.minor.y = element_blank()
  )

toplo1 <- rstsum

p1 <- ggplot(toplo1, aes(x = Year, y = cumtot)) + 
  # scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_line() + 
  geom_point() +
  labs(
    y = 'Cumulative projects', 
    subtitle = paste(max(toplo1$cumtot), 'projects')
  ) + 
  thm

png(here('docs/figs/cumulativehmpall.png'), height = 3, width = 7, family = 'serif', units = 'in', res = 500)
print(p1)
dev.off()

# cumulative effort by habitat ----------------------------------------------------------------

levs <- c("Artificial Reefs", "Coastal Uplands", "Forested Freshwater Wetlands", 
          "Hard Bottom", "Intertidal Estuarine (Other)", "Living Shorelines", 
          "Low-Salinity Salt Marsh", "Mangrove Forests", "Non-forested Freshwater Wetlands", 
          "Oyster Bars", "Salt Barrens", "Seagrasses", "Tidal Tributaries", "Uplands (Non-coastal)")

# data prep
rstsum <- rstdatall %>% 
  arrange(Year, Primary) %>% 
  filter(!is.na(Primary)) %>%
  filter(Year >= 2006) %>% 
  filter(Primary %in% levs) %>% 
  mutate(
    Primary = factor(Primary, levels = levs)
  ) %>% 
  select(-Activity) %>% 
  mutate(prj = 1) %>% 
  tidyr::complete(Year, Primary, fill = list(Acres = 0, Miles = 0, prj = 0)) %>% 
  group_by(Year, Primary) %>% 
  summarise(
    tot = sum(prj),
    Acres = sum(Acres, na.rm = T),
    Miles = sum(Miles, na.rm = T),
    .groups = 'drop'
  ) %>% 
  group_by(Primary) %>% 
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
ncol <- length(levels(toplo1$Primary))

colfun <- colorRampPalette(brewer.pal(8, "Accent"))

p1 <- ggplot(toplo1, aes(x = Year, y = cumtot, fill = Primary)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Cumulative projects', 
    fill = NULL,
  ) 

p2 <- ggplot(toplo1, aes(x = Year, y = cumacres, fill = Primary)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Cumulative acres', 
    fill = NULL,
  ) 

p3 <- ggplot(toplo1, aes(x = Year, y = cummiles, fill = Primary)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Cumulative miles', 
    fill = NULL,
  ) 

pout <- p1 + p2 + p3 + plot_layout(ncol = 1, guides = 'collect') & thm & guides(fill = guide_legend(nrow = 5))

png(here('docs/figs/cumulativehmp.png'), height = 8, width = 7, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()

p1 <- ggplot(toplo1, aes(x = Year, y = tot, fill = Primary)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Projects', 
    fill = NULL,
  ) 

p2 <- ggplot(toplo1, aes(x = Year, y = Acres, fill = Primary)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Acres', 
    fill = NULL,
  ) 

p3 <- ggplot(toplo1, aes(x = Year, y = Miles, fill = Primary)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Miles', 
    fill = NULL,
  ) 

pout <- p1 + p2 + p3 + plot_layout(ncol = 1, guides = 'collect') & thm & guides(fill = guide_legend(nrow = 5))

png(here('docs/figs/totalhmp.png'), height = 8, width = 7, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()

toplo2 <- toplo1 %>% 
  filter(Year >= 2020)
ncol <- length(levels(toplo2$Primary))

p1 <- ggplot(toplo2, aes(x = Year, y = tot, fill = Primary)) + 
  geom_bar(position = 'stack', stat = 'identity', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Projects', 
    fill = NULL,
  ) 

p2 <- ggplot(toplo2, aes(x = Year, y = Acres, fill = Primary)) + 
  geom_bar(position = 'stack', stat = 'identity', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Acres', 
    fill = NULL,
  ) 

p3 <- ggplot(toplo2, aes(x = Year, y = Miles, fill = Primary)) + 
  geom_bar(position = 'stack', stat = 'identity', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Miles', 
    fill = NULL,
  ) 

pout <- p1 + p2 + p3 + plot_layout(ncol = 1, guides = 'collect') & thm & guides(fill = guide_legend(nrow = 5))

png(here('docs/figs/totalhmprecent.png'), height = 8, width = 7, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()

# pie charts ----------------------------------------------------------------------------------

levs <- c("Artificial Reefs", "Coastal Uplands", "Forested Freshwater Wetlands", 
          "Hard Bottom", "Intertidal Estuarine (Other)", "Living Shorelines", 
          "Low-Salinity Salt Marsh", "Mangrove Forests", "Non-forested Freshwater Wetlands", 
          "Oyster Bars", "Salt Barrens", "Seagrasses", "Tidal Tributaries", "Uplands (Non-coastal)")
cols <- colorRampPalette(brewer.pal(8, "Accent"))(length(levs))

# data prep
rstsum <- rstdatall %>% 
  arrange(Year, Primary) %>% 
  filter(Primary %in% levs) %>%
  filter(Year >= 2006) %>% 
  filter(!is.na(Primary)) %>% 
  mutate(
    Primary = factor(Primary, levels = levs)
  ) %>% 
  select(-Activity) %>% 
  mutate(prj = 1) %>% 
  tidyr::complete(Year, Primary, fill = list(Acres = 0, Miles = 0, prj = 0)) %>% 
  group_by(Year, Primary) %>% 
  summarise(
    tot = sum(prj),
    Acres = sum(Acres, na.rm = T),
    Miles = sum(Miles, na.rm = T),
    .groups = 'drop'
  ) %>% 
  group_by(Year) %>% 
  mutate(
    alltot = sum(tot),
    allacres = sum(Acres), 
    allmiles = sum(Miles)
  ) 

toplo <- rstsum %>% 
  filter(Year >= (cur - 2)) %>% 
  mutate(
    allacres2 = formatC(round(allacres, 1), format= "f", big.mark = ",", digits = 1), 
    allmiles2 = formatC(round(allmiles, 1), format= "f", big.mark = ",", digits = 1)
  ) %>% 
  unite('Yeartot', Year, alltot, sep = ': ', remove = F) %>% 
  unite('Yearacres', Year, allacres2, sep = ': ', remove = F) %>% 
  unite('Yearmiles', Year, allmiles2, sep = ': ', remove = F)

p <- ggplot(toplo, aes(x = alltot/2, y = tot, fill = Primary, width = alltot)) +
  geom_bar(position = "fill", stat="identity", color = 'black') +
  facet_wrap(~ Yeartot, strip.position = 'bottom') + 
  coord_polar("y") +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(
    legend.title = element_blank(), 
    legend.position = 'top', 
    strip.text = element_text(size = 12)
    ) +
  guides(fill = guide_legend(nrow = 5))

png(here('docs/figs/totalpie.png'), height = 4, width = 7, family = 'serif', units = 'in', res = 500)
print(p)
dev.off()

p <- ggplot(toplo, aes(x = allmiles/2, y = Miles, fill = Primary, width = allmiles)) +
  geom_bar(position = "fill", stat="identity", color = 'black') +
  facet_wrap(~ Yearmiles, strip.position = 'bottom') + 
  coord_polar("y") +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(
    legend.title = element_blank(), 
    legend.position = 'top', 
    strip.text = element_text(size = 12)
  ) +
  guides(fill = guide_legend(nrow = 5))

png(here('docs/figs/milespie.png'), height = 4, width = 7, family = 'serif', units = 'in', res = 500)
print(p)
dev.off()

# cur only ------------------------------------------------------------------------------------

toplo <- rstsum %>% 
  filter(Year == cur) %>% 
  mutate(
    Primary = reorder(Primary, tot),
    acreslab = formatC(round(Acres, 1), big.mark = ",", format = 'f', digits = 1), 
    acreslab = gsub('\\.0$', '', acreslab)
  )

thm <- theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.y = element_blank(), 
    strip.placement = 'none', 
    legend.position = 'none',
    strip.background = element_blank(), 
    strip.text.y = element_text(angle = 90, size = 10), 
    strip.text.x = element_blank(),
    axis.title.x = element_text(size = 11), 
    panel.background = element_rect(fill = alpha('grey', 0.1), color = NA)
  )

p1 <- ggplot(toplo, aes(x = tot, y = Primary, fill = Primary)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = tot), hjust = 0, nudge_x = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(toplo$tot) * 1.18)) +
  scale_fill_manual(values = cols) +
  thm + 
  labs(
    y = NULL, 
    x = 'Total projects'
  )
  
p2 <- ggplot(toplo, aes(x = Acres, y = Primary, fill = Primary)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = acreslab), hjust = 0, nudge_x = 1000) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(toplo$Acres) * 1.55), labels = comma) +
  scale_fill_manual(values = cols) +
  thm + 
  theme(axis.text.y = element_blank()) +
  labs(
    y = NULL, 
    x = 'Total Acres'
  )

p3 <- ggplot(toplo, aes(x = Miles, y = Primary, fill = Primary)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = round(Miles, 2)), hjust = 0, nudge_x = 0.005) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(toplo$Miles) * 1.3)) +
  scale_fill_manual(values = cols) +
  thm + 
  theme(axis.text.y = element_blank()) +
  labs(
    y = NULL, 
    x = 'Total Miles'
  )


p <- p1 + p2 + p3 + plot_layout(ncol = 3)


png(here('docs/figs/barcur.png'), height = 5, width = 8, family = 'serif', units = 'in', res = 500)
print(p)
dev.off()

# map -----------------------------------------------------------------------------------------

tomap <- rstdatall %>% 
  st_as_sf(coords = c('Lon', 'Lat'), crs = 4326) %>% 
  .[tbshed, ] %>% 
  mutate(
    filcol = case_when(
      Year == cur ~ as.character(cur), 
      T ~ 'Past projects'
    )
  )

m <- ggplot() + 
  ggspatial::annotation_map_tile(zoom = 9, type = 'cartolight', cachedir = system.file("rosm.cache", package = "ggspatial")) + 
  annotation_north_arrow(location = 'tl', style = north_arrow_orienteering(fill = c('black', 'black'), text_col = NA)) +
  annotation_scale(location = 'br', text_cex = 1.5,) +
  geom_sf(data= tbshed, fill = 'grey', color = 'darkgrey', inherit.aes = F, alpha = 0.5, linewidth = 1.5) +
  geom_sf(data = tomap, aes(fill = filcol), color = 'black', pch = 21, inherit.aes = F, size = 4) +
  scale_fill_manual(values = c('#00806E', '#958984')) +
  theme(
    panel.grid = element_blank(), 
    axis.title = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = NA, color = 'black'), 
    legend.position = c(1, 1), 
    legend.justification = c(1, 0.85), 
    legend.background = element_rect(fill = NA, color = NA)
  ) + 
  labs(
    subtitle = paste0('Project locations in the watershed (1971-', cur, ')'), 
    fill = NULL
  )

png(here('docs/figs/map.png'), height = 5, width = 8, units = 'in', res = 500)
print(m)
dev.off()

# HMP report cards ----------------------------------------------------------------------------

p1 <- show_hmpreport(acres = acres, subtacres = subtacres, hmptrgs = hmptrgs, typ = 'targets',
                     strata = 'Subtidal', ycollapse = T, xang = 45)
p2 <- show_hmpreport(acres = acres, subtacres = subtacres, hmptrgs = hmptrgs, typ = 'targets',
                     strata = c('Intertidal', 'Supratidal'), ycollapse = T, xang = 45)

p <- p1 + p2 + plot_layout(ncol = 2, guides = 'collect', widths = c(0.6, 1)) & labs(title = NULL)

png(here('docs/figs/hmpreport.png'), height = 6, width = 7, units = 'in', res = 500)
print(p)
dev.off()

