# hmpu database projects table
rstdat_tab <- function(rstdat, yrrng, fntsz = 14, family){

  if(length(yrrng) == 1)
    yrrng <- rep(yrrng, 2)
  
  # data prep
  rstsum <- rstdat %>% 
    select(
      Year = `Year Reported`, 
      Category = `Habitat Type (basic ESA categories)(existing databases)`, 
      Acres, 
      Activity = `Basic Activity (Enhance/Rest)`, 
      `Linear Miles` = `Linear Miles`,
      `Linear Ft` = `Linear Feet`
    ) %>% 
    rowwise() %>% 
    mutate(
      Category = case_when(
        Category == 'estuarine' ~ 'Estuarine', 
        Category == 'Upland' ~ 'Uplands',
        grepl('^Mix', Category) ~ 'Mixed', 
        T ~ Category
      ), 
      Miles = sum(`Linear Miles`,  `Linear Ft` / 5280, na.rm = T)
    ) %>% 
    ungroup() %>% 
    filter(Year <= yrrng[2] & Year >= yrrng[1]) %>% 
    group_by(Category, Activity) %>% 
    summarise(
      tot= n(),
      Acres = sum(Acres, na.rm = T), 
      Miles = sum(Miles, na.rm = T),
      .groups = 'drop'
    ) %>% 
    filter(!is.na(Category)) %>% 
    group_by(Category) %>% 
    mutate(
      tot = sum(tot)
    ) %>% 
    pivot_longer(c('Acres', 'Miles'), names_to = 'var', values_to = 'val') %>% 
    unite('var', Activity, var, sep = ', ') %>% 
    pivot_wider(names_from = 'var', values_from = 'val') %>% 
    select(Category, tot, `Restoration, Acres`, `Restoration, Miles`, `Enhancement, Acres`, `Enhancement, Miles`)
  
  # yrrng
  yrs <- yrrng %>% 
    unique %>% 
    paste(., collapse = '-')
  
  # table
  tab <- reactable(
    rstsum, 
    columns = list(
      Category = colDef(name = 'Habitat', footer = 'Total',  minWidth = 50, class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
      tot = colDef(name = 'Total projects', minWidth = 50)
    ),
    defaultColDef = colDef(
      footer = function(values){
        if(!is.numeric(values))
          return()
        
        formatC(round(sum(values), 0), format= "d", big.mark = ",")
        
      },
      headerStyle= list(fontSize = fntsz, fontFamily = family),
      footerStyle = list(fontWeight = "bold", fontSize = fntsz, fontFamily = family),
      format = colFormat(digits = 0, separators = TRUE), 
      minWidth = 80,
      style = list(fontSize = fntsz, fontFamily = family),
      resizable = TRUE
    ),
    showPageSizeOptions = F,
    highlight = T,
    wrap = F
  )
  
  # add title
  ttl <- paste0('Enhancement and restoration projects in Tampa Bay (', yrs, ')')
  out <-  htmlwidgets::prependContent(tab, h5(class = "title", ttl))
  
  return(out)
  
}