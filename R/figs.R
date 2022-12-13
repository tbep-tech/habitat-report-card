library(tidyverse)
library(here)
library(showtext)
library(patchwork)
library(plotrix)
library(RColorBrewer)
library(scales)

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
  arrange(Year, Category) %>% 
  filter(!is.na(Category)) %>%
  filter(Year >= 2006) %>% 
  filter(!is.na(Category)) %>% 
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
ncol <- length(levels(toplo1$Category))

colfun <- colorRampPalette(brewer.pal(8, "Accent"))

p1 <- ggplot(toplo1, aes(x = Year, y = cumtot, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Cumulative projects', 
    fill = NULL,
  ) 

p2 <- ggplot(toplo1, aes(x = Year, y = cumacres, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Cumulative acres', 
    fill = NULL,
  ) 

p3 <- ggplot(toplo1, aes(x = Year, y = cummiles, fill = Category)) + 
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

p1 <- ggplot(toplo1, aes(x = Year, y = tot, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year)), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_area(position = 'fill', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Projects', 
    fill = NULL,
  ) 

p2 <- ggplot(toplo1, aes(x = Year, y = Acres, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year)), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_area(position = 'fill', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Acres', 
    fill = NULL,
  ) 

p3 <- ggplot(toplo1, aes(x = Year, y = Miles, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year)), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_area(position = 'fill', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Miles', 
    fill = NULL,
  ) 

pout <- p1 + p2 + p3 + plot_layout(ncol = 1, guides = 'collect') & thm & guides(fill = guide_legend(nrow = 5))

png(here('docs/figs/prophmp.png'), height = 8, width = 7, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()

p1 <- ggplot(toplo1, aes(x = Year, y = tot, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Projects', 
    fill = NULL,
  ) 

p2 <- ggplot(toplo1, aes(x = Year, y = Acres, fill = Category)) + 
  scale_x_continuous(breaks = seq(min(toplo1$Year), max(toplo1$Year))) +
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Acres', 
    fill = NULL,
  ) 

p3 <- ggplot(toplo1, aes(x = Year, y = Miles, fill = Category)) + 
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
ncol <- length(levels(toplo2$Category))

p1 <- ggplot(toplo2, aes(x = Year, y = tot, fill = Category)) + 
  geom_bar(position = 'stack', stat = 'identity', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Projects', 
    fill = NULL,
  ) 

p2 <- ggplot(toplo2, aes(x = Year, y = Acres, fill = Category)) + 
  geom_bar(position = 'stack', stat = 'identity', alpha = 0.8) + 
  scale_fill_manual(values = colfun(ncol)) +
  labs(
    y = 'Acres', 
    fill = NULL,
  ) 

p3 <- ggplot(toplo2, aes(x = Year, y = Miles, fill = Category)) + 
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
  arrange(Year, Category) %>% 
  filter(!is.na(Category)) %>%
  filter(Year >= 2006) %>% 
  filter(!is.na(Category)) %>% 
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
  group_by(Year) %>% 
  mutate(
    alltot = sum(tot),
    allacres = sum(Acres), 
    allmiles = sum(Miles)
  ) 


cols <- qualitative_hcl(length(unique(tnanndat$SOURCE)), palette = "Dynamic")

toplo <- rstsum %>% 
  filter(Year >= 2020) %>% 
  mutate(
    allacres2 = formatC(round(allacres, 1), format= "f", big.mark = ",", digits = 1), 
    allmiles2 = formatC(round(allmiles, 1), format= "f", big.mark = ",", digits = 1)
  ) %>% 
  unite('Yeartot', Year, alltot, sep = ': ', remove = F) %>% 
  unite('Yearacres', Year, allacres2, sep = ': ', remove = F) %>% 
  unite('Yearmiles', Year, allmiles2, sep = ': ', remove = F)

p <- ggplot(toplo, aes(x = alltot/2, y = tot, fill = Category, width = alltot)) +
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

p <- ggplot(toplo, aes(x = allacres/2, y = Acres, fill = Category, width = allacres)) +
  geom_bar(position = "fill", stat="identity", color = 'black') +
  facet_wrap(~ Yearacres, strip.position = 'bottom') + 
  coord_polar("y") +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(
    legend.title = element_blank(), 
    legend.position = 'top', 
    strip.text = element_text(size = 12)
  ) +
  guides(fill = guide_legend(nrow = 5))

png(here('docs/figs/acrespie.png'), height = 4, width = 7, family = 'serif', units = 'in', res = 500)
print(p)
dev.off()

p <- ggplot(toplo, aes(x = allmiles/2, y = Miles, fill = Category, width = allmiles)) +
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

# 2022 only -----------------------------------------------------------------------------------

toplo <- rstsum %>% 
  filter(Year == 2022) %>% 
  mutate(
    Category = reorder(Category, tot),
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

p1 <- ggplot(toplo, aes(x = tot, y = Category, fill = Category)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = tot), hjust = 0, nudge_x = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(toplo$tot) * 1.18)) +
  scale_fill_manual(values = cols) +
  thm + 
  labs(
    y = NULL, 
    x = 'Total projects'
  )
  
p2 <- ggplot(toplo, aes(x = Acres, y = Category, fill = Category)) + 
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

p3 <- ggplot(toplo, aes(x = Miles, y = Category, fill = Category)) + 
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


png(here('docs/figs/bar2022.png'), height = 5, width = 8, family = 'serif', units = 'in', res = 500)
print(p)
dev.off()
