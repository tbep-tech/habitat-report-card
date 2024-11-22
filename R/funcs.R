# gpra database projects table by habitat type (rowgrp)
tab_fun <- function(dat, yrrng, fntsz = 14, family = NULL, rowgrp = c('Primary', 'General')){

  rowgrp <- match.arg(rowgrp)
  
  dat <- dat %>% 
    rename(rowgrp = !!rowgrp) |> 
    filter()
  
  # habitat categories
  allhab <- dat %>% 
    pull(`rowgrp`) %>% 
    unique() %>% 
    sort() %>%
    tibble(rowgrp = .)
  
  collevs <- c('Restoration', 'Maintenance', 'Protection')
  collabs <- c('Restoration (Ac / Mi)', 'Maintenance (Ac / Mi)', 'Protection (Ac / Mi)')
  
  habmin <- 105
  
  # only unique step for primary habitat categories
  if(rowgrp == 'Primary'){
    
    allhab <- allhab %>% 
      pull(rowgrp) %>% 
      .[!. %in% 'Other'] %>% 
      c(., 'Other') %>% 
      tibble(rowgrp = . )

    dat <- dat %>% 
      filter(Activity != 'Protection')
    
    collevs <- grep('Restoration|Maintenance', collevs, value = T)
    collabs <- grep('Restoration|Maintenance', collabs, value = T)
    
    habmin <- 235
    
  }
  
  # prep data by year selection
  totab1 <- tab_prep(dat, yrrng = yrrng[2], allhab, collevs, collabs)
  totab2 <- tab_prep(dat, yrrng = yrrng, allhab, collevs, collabs)
  totab <- list(totab1, totab2) %>% 
    enframe(name = 'Year') %>% 
    mutate(
      Year = case_when(
        Year == 1 ~ paste0(yrrng[2]), 
        Year == 2 ~ paste0(yrrng[1], '-', yrrng[2])
      )
    ) %>% 
    unnest('value') %>% 
    flextable::as_grouped_data('Year') %>%
    mutate(
      rowgrp = ifelse(is.na(rowgrp), Year, rowgrp)
    ) %>% 
    select(-Year) %>% 
    as_tibble()
  
  # index for bold rows
  bld <- grep(paste(c(yrrng, 'Total'), collapse = '|'), totab$rowgrp)
  
  # table
  tab <- reactable(
    totab, 
    columns = list(
      rowgrp = colDef(name = 'Habitat', minWidth = habmin, maxWidth = habmin, align = 'left', class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
      tot = colDef(name = 'Total projects', minWidth = 70, maxWidth = 70)
    ),
    defaultColDef = colDef(
      headerStyle= list(fontSize = fntsz, fontFamily = family),
      minWidth = 105,
      maxWidth = 105,
      resizable = F,
      align = 'center',
      style = function(value, index) {
        if (index %in% bld)
          list(fontWeight = "bold", fontSize = fntsz, fontFamily = family)
        else
          list(fontSize = fntsz, fontFamily = family)
      }
    ),
    defaultPageSize = nrow(totab),
    showPageSizeOptions = F,
    sortable = F,
    highlight = T,
    wrap = T
  )
  
  out <- tab
  
  return(out)
  
}

# internal function to tab_fun to subset by a year or range of years
tab_prep <- function(dat, yrrng, allhab, collevs, collabs){
  
  if(length(yrrng) == 1)
    yrrng <- rep(yrrng, 2)
  
  # data prep
  rstsum <- dat %>% 
    filter(Year <= yrrng[2] & Year >= yrrng[1]) %>% 
    filter(!is.na(Activity)) %>% 
    summarise(
      tot= n(),
      Acres = sum(Acres, na.rm = T), 
      Miles = sum(Miles, na.rm = T),
      .by = c('rowgrp', 'Activity')
    ) %>% 
    mutate(
      tot = sum(tot), 
      .by = rowgrp
    ) %>%
    mutate(
      rowgrp = factor(rowgrp, levels = allhab$rowgrp)
    ) %>% 
    complete(rowgrp, Activity, fill = list(tot = 0, Acres = 0, Miles = 0)) %>% 
    mutate(
      tot = max(tot), 
      .by = rowgrp
    ) %>% 
    mutate(
      rowgrp = as.character(rowgrp)
    )
  
  # total projects
  totproj <- rstsum %>% 
    select(rowgrp, tot) %>% 
    unique() %>% 
    pull(tot) %>% 
    sum() %>% 
    tibble(
      rowgrp = 'Total',
      tot = .
    )
  
  # totals from rstsum
  tots <- rstsum %>% 
    pivot_longer(cols = c('Acres', 'Miles'), names_to = 'var', values_to = 'val') %>%
    summarise(
      val = sum(val, na.rm = T),
      .by = c(Activity, var)
    ) %>%
    mutate(
      val = case_when(
        val < 1 & val > 0 ~ '< 1', 
        T ~ format(round(val, 0), big.mark = ',', trim = T)
      ), 
      Activity = factor(Activity, levels = collevs, labels = collabs)
    ) %>% 
    pivot_wider(names_from = 'var', values_from = 'val', values_fill = '0', names_expand = T) %>%
    unite('val', Acres, Miles, sep = ' / ') %>% 
    pivot_wider(names_from = 'Activity', values_from = 'val', values_fill = '0 / 0', names_expand = T) %>% 
    bind_cols(totproj, .)
  
  # combine all    
  totab <- rstsum %>% 
    mutate(
      Acres = case_when(
        Acres < 1 & Acres > 0 ~ '< 1', 
        T ~ format(round(Acres, 0), big.mark = ',', trim = T)
      ),
      Miles = case_when(
        Miles < 1 & Miles > 0 ~ '< 1', 
        T ~ format(round(Miles, 0), big.mark = ',', trim = T)
      )
    ) %>%
    unite('val', Acres, Miles, sep = ' / ') %>% 
    mutate(
      Activity = factor(Activity, levels = collevs, labels = collabs)
    ) %>% 
    pivot_wider(names_from = 'Activity', values_from = 'val', values_fill = '0 / 0', names_expand = T) %>% 
    select(rowgrp, tot, all_of(collabs)) %>% 
    left_join(allhab, ., by = 'rowgrp')  %>% 
    bind_rows(tots) %>% 
    mutate(tot = as.character(tot))
  
  return(totab)
  
}

# get datasets from hmpu-workflow repo
# try simple load, download if fail
rdataload <- function(x){
  
  fl <- paste0(x, '.RData')
  dataurl <- 'https://github.com/tbep-tech/hmpu-workflow/raw/master/data/'
  flurl <- paste0(dataurl, fl)
  
  # try simple load
  ld <- try(load(url(flurl)), silent = T)
  
  # return x if load worked
  if(!inherits(ld, 'try-error')){
    out <- get(x)
  }
  
  # download x if load failed
  if(inherits(ld, 'try-error')){
    
    fl <- paste(tempdir(), fl, sep = '/')
    download.file(flurl, destfile = fl, quiet = T)
    load(file = fl)
    out <- get(x)
    suppressMessages(file.remove(fl))
    
  }
  
  return(out)
    
}

# get summaries for inline text
summ_fun <- function(dat, yrrng = NULL){
  
  if(!is.null(yrrng)){
    if(length(yrrng) == 1)
      yrrng <- rep(yrrng, 2)
  } else { 
    yrrng <- range(dat$Year)
  }
  
  # data prep
  rstsum <- dat %>% 
    filter(Year <= yrrng[2] & Year >= yrrng[1]) %>% 
    filter(!is.na(Activity)) %>% 
    summarise(
      pro = n(),
      acr = sum(Acres, na.rm = T), 
      mil = sum(Miles, na.rm = T),
      par = length(unique(Partner))
    ) %>% 
    mutate(
      acr = case_when(
        acr < 1 & acr > 0 ~ '< 1', 
        T ~ format(round(acr, 0), big.mark = ',', trim = T)
      ),
      mil = case_when(
        mil < 1 & mil > 0 ~ '< 1', 
        T ~ format(round(mil, 2), big.mark = ',', trim = T)
      )
    )
  
  return(rstsum)
  
}