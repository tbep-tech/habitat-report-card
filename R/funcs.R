# gpra database projects table
rstdat_tab <- function(dat, yrrng, fntsz = 14, family){

  if(length(yrrng) == 1)
    yrrng <- rep(yrrng, 2)

  # habitat categories
  allhab <- dat %>% 
    pull(`Category`) %>% 
    unique() %>% 
    sort() %>% 
    .[!. %in% 'Other'] %>% 
    c(., 'Other') %>% 
    tibble(Category = . )

  # data prep
  rstsum <- dat %>% 
    filter(Year <= yrrng[2] & Year >= yrrng[1]) %>% 
    filter(!is.na(Activity)) %>% 
    filter(Activity != 'Protection') %>% 
    summarise(
      tot= n(),
      Acres = sum(Acres, na.rm = T), 
      Miles = sum(Miles, na.rm = T),
      .by = c('Category', 'Activity')
    ) %>% 
    mutate(
      tot = sum(tot), 
      .by = Category
    ) %>%
    mutate(
      Category = factor(Category, levels = allhab$Category)
    ) %>% 
    complete(Category, Activity, fill = list(tot = 0, Acres = 0, Miles = 0)) %>% 
    mutate(
      tot = max(tot), 
      .by = Category
    ) %>% 
    mutate(
      Category = as.character(Category)
    )
    
  # total projects
  totproj <- rstsum %>% 
    select(Category, tot) %>% 
    unique() %>% 
    pull(tot) %>% 
    sum() %>% 
    tibble(
      Category = 'Total',
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
      Activity = factor(Activity, levels = c('Restoration', 'Enhancement'),
                        labels = c('Restoration (Acres / Miles)', 'Enhancement (Acres / Miles)')
      )
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
      Activity = factor(Activity, levels = c('Restoration', 'Enhancement'),
                        labels = c('Restoration (Acres / Miles)', 'Enhancement (Acres / Miles)')
      )
    ) %>% 
    pivot_wider(names_from = 'Activity', values_from = 'val', values_fill = '0 / 0', names_expand = T) %>% 
    select(Category, tot, `Restoration (Acres / Miles)`, `Enhancement (Acres / Miles)`) %>% 
    left_join(allhab, ., by = 'Category')  %>% 
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
      Category = colDef(name = 'Habitat', minWidth = 180, class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
      tot = colDef(name = 'Total projects', minWidth = 80)
    ),
    defaultColDef = colDef(
      headerStyle= list(fontSize = fntsz, fontFamily = family),
      minWidth = 150,
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
    wrap = F
  )
  
  # add title
  ttl <- paste0('Restoration and enhancement projects in Tampa Bay (', yrs, ')')
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