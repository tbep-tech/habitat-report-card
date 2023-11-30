# gpra database projects table by habitat type (rowgrp)
tab_fun <- function(dat, yrrng, fntsz = 14, family, rowgrp = c('Primary', 'General')){

  rowgrp <- match.arg(rowgrp)
  
  dat <- dat %>% 
    rename(rowgrp = !!rowgrp)
  
  if(length(yrrng) == 1)
    yrrng <- rep(yrrng, 2)
  
  # habitat categories
  allhab <- dat %>% 
    pull(`rowgrp`) %>% 
    unique() %>% 
    sort() %>%
    tibble(rowgrp = .)
  
  collevs <- c('Restoration', 'Enhancement', 'Protection')
  collabs <- c('Restoration (Ac / Mi)', 'Enhancement (Ac / Mi)', 'Protection (Ac / Mi)')
  
  habmin <- 80
  
  # only unique step for primary habitat categories
  if(rowgrp == 'Primary'){
    
    allhab <- allhab %>% 
      pull(rowgrp) %>% 
      .[!. %in% 'Other'] %>% 
      c(., 'Other') %>% 
      tibble(rowgrp = . )

    dat <- dat %>% 
      filter(Activity != 'Protection')
    
    collevs <- grep('Restoration|Enhancement', collevs, value = T)
    collabs <- grep('Restoration|Enhancement', collabs, value = T)
    
    habmin <- 180
    
  }
    
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
  
  # yrrng
  yrs <- yrrng %>% 
    unique %>% 
    paste(., collapse = '-')
  
  # table
  tab <- reactable(
    totab, 
    columns = list(
      rowgrp = colDef(name = 'Habitat', minWidth = habmin, class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
      tot = colDef(name = 'Total projects', minWidth = 70)
    ),
    defaultColDef = colDef(
      headerStyle= list(fontSize = fntsz, fontFamily = family),
      minWidth = 100,
      resizable = TRUE,
      style = function(value, index) {
        if (index == nrow(totab))
          list(fontWeight = "bold", fontSize = fntsz, fontFamily = family)
        else
          list(fontSize = fntsz, fontFamily = family)
      }
    ),
    defaultPageSize = nrow(totab),
    showPageSizeOptions = F,
    highlight = T,
    wrap = T
  )
  
  # add title
  ttl <- paste0('Projects in Tampa Bay by ', tolower(rowgrp), ' habitat (', yrs, ')')
  out <-  htmlwidgets::prependContent(tab, h5(class = "title", ttl))
  
  return(out)
  
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
