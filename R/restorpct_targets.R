# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(readr)
library(patchwork)

# Load data ---------------------------------------------------

load(url('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/acres.RData'))
load(url('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/subtacres.RData'))
load(url('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/trgs.RData'))

trgshr <- trgs |> 
  select(HMPU_TARGETS, Target2030)

pth <- 'https://raw.githubusercontent.com/tbep-tech/TBEP_Habitat_Restoration/main/restoration.csv'

subtdcat <- c('Seagrasses', 'Oyster Bars', 'Tidal Flats')
nodatcat <- c('Artificial Reefs', 'Hard Bottom', 'Tidal Tributaries', 'Living Shorelines')
updatcat <- c('Mangrove Forests', 'Salt Barrens', 'Salt Marshes', 'Coastal Uplands', 
  'Non-Forested Freshwater Wetlands', 'Forested Freshwater Wetlands', 
  'Native Uplands')

subtdyr <- 2024
nodatyr <- 2017
updatyr <- 2023

# starting restoration data for 2023 (inter/supratidal) and 2024 (subtidal)
# manually add 2017 for not tracked
strdat <- bind_rows(
    filter(acres, name == updatyr),
    filter(subtacres, name == subtdyr)
  ) |>
  ungroup() |> 
  select(-name) |> 
  rbind(
    tibble(
      HMPU_TARGETS = nodatcat,
      Acres = c(166, 423, 387, 11.3)
    )
  ) |> 
  rename(start = Acres) |> 
  filter(HMPU_TARGETS %in% c(trgshr$HMPU_TARGETS))

#Bringing in restoration data archive and performing QA
restdat <- read.csv(pth, stringsAsFactors = F) |>
  #fixing mistake in restoration table, Perico Bayou oyster restoration in 2019, 300 acres is incorrect, 300ftX3.5ft~1000sqft or 0.02 acres
  mutate(Acres=if_else(View_Detail==19688, 0.02, Acres))

# currrent restoration data since 2017
curdat <- restdat |>
  filter(GeneralActivity == 'Restoration' & Federal_Fiscal_Year >= nodatyr) |>
  select(
    Year = Federal_Fiscal_Year,
    HMPU_TARGETS = PrimaryHabitat,
    Acres,
    Miles,
    Feet
  ) |>
  mutate(
    Acres = as.numeric(Acres),
    Miles = case_when(
      is.na(Miles) & !is.na(Feet) ~ Feet / 5280,
      T ~ Miles
    ),
    HMPU_TARGETS = case_when(
      HMPU_TARGETS == "Low-Salinity Salt Marsh" ~ "Salt Marshes",
      HMPU_TARGETS == "Non-forested Freshwater Wetlands" ~ "Non-Forested Freshwater Wetlands",
      HMPU_TARGETS == "Uplands (Non-coastal)" ~ "Native Uplands",
      HMPU_TARGETS == "Intertidal Estuarine (Other)" ~ "Total Intertidal",
      HMPU_TARGETS == '' ~ NA_character_,
      TRUE ~ HMPU_TARGETS
    )
  ) |> 
  filter(HMPU_TARGETS != 'Total Intertidal') |> 
  summarise(
    Acres = sum(Acres, na.rm=TRUE),
    Miles = sum(Miles, na.rm=TRUE),
    .by=c(Year, HMPU_TARGETS)
  ) 

# current restoration progress based on 2023 for inter/supratidal, 2024 for subtidal, 2017 for categories not in lu data
prgdat <- curdat |> 
  tidyr::complete(Year, HMPU_TARGETS = strdat$HMPU_TARGETS, fill=list(Acres=0, Miles=0)) |> 
  mutate(
    progress = case_when(
      HMPU_TARGETS %in% c('Tidal Tributaries', 'Living Shorelines') ~ Miles, 
      TRUE ~ Acres
    )
  ) |> 
  select(-Miles, -Acres) |> 
  filter(
    (HMPU_TARGETS %in% nodatcat & Year >= nodatyr) |
    (HMPU_TARGETS %in% updatcat & Year >= updatyr) |
    (HMPU_TARGETS %in% subtdcat & Year >= subtdyr)
  ) |> 
  arrange(HMPU_TARGETS, Year) |> 
  mutate(
    progress = cumsum(progress),
    .by = HMPU_TARGETS
  ) |> 
  full_join(strdat, by="HMPU_TARGETS") |> 
  mutate(
    progress = progress + start
  ) |> 
  select(-start)
save(prgdat, file = here("docs", "prgdat.RData"))


#Current year progress met, 
curmet <- prgdat |>
  left_join(trgshr, by="HMPU_TARGETS") |> 
  filter(Year == max(Year)) |>
  mutate(
    curmet = progress >= Target2030 
  )

# 2030 estimate based on last five years, likely to meet as T/F
tnddat <- prgdat |> 
  filter(Year <= max(Year) & Year > (max(Year) - 5)) |> 
  nest(data = c(Year, progress)) |> 
  mutate(
    est2030 = purrr::map_dbl(data, ~{
      mod <- lm(progress ~ Year, data = .x)
      round(predict(mod, newdata = data.frame(Year = 2030)), 1)
    })
  ) |> 
  unnest('est2030') |> 
  left_join(trgshr, by="HMPU_TARGETS") |> 
  mutate(
    estmet = est2030 >= Target2030
  )

#Combine current year target met and 2030 estimate
rsttrg <- curmet |>
  select(-Target2030) |>
  full_join(tnddat, by="HMPU_TARGETS") |>
  select(HMPU_TARGETS,Target2030,Year,everything() & -data)|>
  mutate(ToGo=Target2030-progress)

write.csv(rsttrg,here("docs","restoration_targets.csv"))

# restoration report card figure --------------------------------------------------------------------------

# yearly status

# for each habitat and year: target met that year, and 2030 estimate from a linear fit
# to the last five years up to and including that year (same approach as tnddat)
est2030 <- function(yr, prg) {
  if (length(unique(yr)) < 2) return(tail(prg, 1))   # one year only, no trend yet
  round(predict(lm(prg ~ yr), newdata = data.frame(yr = 2030)), 1)
}

rptdat <- prgdat |>
  left_join(trgshr, by = 'HMPU_TARGETS') |>
  arrange(HMPU_TARGETS, Year) |>
  mutate(
    est2030 = map_dbl(Year, \(y) {
      sel <- Year <= y & Year > (y - 5)
      est2030(Year[sel], progress[sel])
    }),
    .by = HMPU_TARGETS
  ) |>
  mutate(
    curmet = progress >= Target2030,
    estmet = est2030 >= Target2030
  ) |>
  filter(Year >= 2023)   # show 2023 on; earlier years are still used for the trends above

# plot

sub <- c('Tidal Flats', 'Seagrasses', 'Oyster Bars', 'Artificial Reefs', 'Hard Bottom')
int <- c('Mangrove Forests', 'Salt Barrens', 'Salt Marshes', 'Tidal Tributaries', 'Living Shorelines')
sup <- c('Coastal Uplands', 'Non-Forested Freshwater Wetlands',
         'Forested Freshwater Wetlands', 'Native Uplands')

# habitats flagged with an asterisk on the axis labels
astr <- c('Artificial Reefs', 'Hard Bottom', 'Living Shorelines', 'Tidal Tributaries')
habtlab <- function(h) ifelse(h %in% astr, paste0(h, '*'), h)

cols <- c('2030 target not met' = '#CC3231', '2030 target met' = '#2DC938')
trnds <- c('Trending above 2030 target' = 1, 'Trending below 2030 target' = 2)

# x = habitat, y = year; arrow up-right if trending above target, down-right if below
prep <- function(d, habs) {
  d <- filter(d, HMPU_TARGETS %in% habs)
  yrs <- sort(unique(d$Year))
  d |>
    mutate(
      x = match(HMPU_TARGETS, habs),
      y = match(Year, yrs),
      trend = factor(ifelse(estmet, names(trnds)[1], names(trnds)[2]), levels = names(trnds)),
      met = factor(ifelse(curmet, '2030 target met', '2030 target not met'), levels = names(cols)),
      y0 = ifelse(estmet, y + 0.3, y - 0.3),   # y axis is reversed below
      y1 = ifelse(estmet, y - 0.3, y + 0.3)
    )
}

# legend key that draws a black up or down arrow
draw_key_trend <- function(data, params, size) {
  up <- data$shape == 1
  grid::segmentsGrob(0.15, ifelse(up, 0.15, 0.85), 0.85, ifelse(up, 0.85, 0.15),
                     arrow = arrow(type = 'closed', length = unit(0.08, 'inches')),
                     gp = grid::gpar(col = 'black', fill = 'black', lwd = 5, linejoin = 'mitre', lineend = 'round'))
}

arrw <- arrow(type = 'closed', length = unit(0.1, 'inches'), angle = 30)

lyrs <- list(
  geom_segment(aes(x = x - 0.3, xend = x + 0.3, y = y0, yend = y1, color = met),
               linewidth = 2, lineend = 'round', linejoin = 'mitre', arrow = arrw,
               key_glyph = 'rect'),
  geom_point(aes(x = x, y = y, shape = trend), alpha = 0, key_glyph = draw_key_trend),
  scale_shape_manual(values = trnds, drop = FALSE, name = NULL),
  scale_color_manual(values = cols, drop = FALSE, name = NULL)
)

yscl <- function(d) scale_y_reverse(breaks = sort(unique(d$y)), labels = sort(unique(d$Year)),
                                    expand = expansion(add = 0.5))

thm <- theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = 'black', linewidth = 1),
    panel.spacing = unit(0, 'lines'),
    axis.title = element_blank(),
    axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text = element_text(size = 14),
    plot.margin = margin(5, 60, 5, 5)
  )

# subtidal panel
dsub <- prep(rptdat, sub)
p1 <- ggplot(dsub) +
  lyrs +
  scale_x_continuous(breaks = seq_along(sub), labels = habtlab(sub), limits = c(0.5, length(sub) + 0.5),
                     position = 'top', expand = c(0, 0)) +
  yscl(dsub) +
  facet_grid(~'Subtidal', switch = 'x') +
  thm

# intertidal + supratidal panel
habs2 <- c(int, sup)
dint <- prep(rptdat, habs2) |>
  mutate(cat = factor(ifelse(HMPU_TARGETS %in% int, 'Intertidal', 'Supratidal'),
                      levels = c('Intertidal', 'Supratidal')))
p2 <- ggplot(dint) +
  lyrs +
  scale_x_continuous(breaks = seq_along(habs2), labels = habtlab(habs2),
                     position = 'top', expand = expansion(add = 0.5)) +
  yscl(dint) +
  facet_grid(~cat, scales = 'free_x', space = 'free_x', switch = 'x') +
  thm

p <- p1 + p2 +
  plot_layout(widths = c(length(sub), length(habs2)), guides = 'collect') &
  theme(legend.position = 'bottom', legend.text = element_text(size = 14),
        legend.key.size = unit(0.35, 'inches'))

p

png(here('docs/figs/hmpreport_rest.png'), height = 6.5, width = 13, units = 'in', res = 500)
print(p)
dev.off()
