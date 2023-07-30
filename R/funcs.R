# gpra database projects table
rstdat_tab <- function(dat, yrrng, fntsz = 14, family){

  if(length(yrrng) == 1)
    yrrng <- rep(yrrng, 2)

  allhab <- dat %>% 
    pull(`Category`) %>% 
    unique() %>% 
    sort() %>% 
    c(., 'Mix/undocumented') %>% 
    tibble(Category = . )
  
  # data prep
  rstsum <- dat %>% 
    filter(Year <= yrrng[2] & Year >= yrrng[1]) %>% 
    group_by(Category, Activity) %>% 
    summarise(
      tot= n(),
      Acres = sum(Acres, na.rm = T), 
      Miles = sum(Miles, na.rm = T),
      .groups = 'drop'
    ) %>% 
    group_by(Category) %>% 
    mutate(
      tot = sum(tot)
    ) %>% 
    ungroup() %>% 
    pivot_longer(c('Acres', 'Miles'), names_to = 'var', values_to = 'val') %>% 
    unite('var', Activity, var, sep = ', ') %>% 
    pivot_wider(names_from = 'var', values_from = 'val', values_fill = 0) %>% 
    select(Category, tot, `Restoration, Acres`, `Restoration, Miles`, `Enhancement, Acres`, `Enhancement, Miles`) %>% 
    mutate(
      Category = ifelse(is.na(Category), 'Mix/undocumented', Category)
    ) %>% 
    left_join(allhab, ., by = 'Category') %>% 
    rowwise() %>% 
    mutate_all(function(x) ifelse(is.na(x), 0, x))

  # yrrng
  yrs <- yrrng %>% 
    unique %>% 
    paste(., collapse = '-')
  
  # table
  tab <- reactable(
    rstsum, 
    columns = list(
      Category = colDef(name = 'Habitat', footer = 'Total',  minWidth = 200, class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
      tot = colDef(name = 'Total projects', minWidth = 120, 
                   format = colFormat(digits = 0))
    ),
    defaultColDef = colDef(
      footer = function(values){
        if(!is.numeric(values))
          return()

        if(any(!values %% 1 == 0))
          formatC(sum(round(values, 1)), format= "f", big.mark = ",", digits = 1)
        else
          formatC(sum(values), format= "f", big.mark = ",", digits = 0)
        
      },
      headerStyle= list(fontSize = fntsz, fontFamily = family),
      footerStyle = list(fontWeight = "bold", fontSize = fntsz, fontFamily = family),
      format = colFormat(digits = 1, separators = TRUE), 
      minWidth = 150,
      style = list(fontSize = fntsz, fontFamily = family),
      resizable = TRUE
    ),
    defaultPageSize = nrow(rstsum),
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