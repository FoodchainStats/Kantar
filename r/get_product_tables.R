
findheader_total <- function(headerlist_total) {
  rownumbers <- c()
  for (i in 1:length(headerlist_total)) {
    x <- cells_total %>%
      filter(character==headerlist_total[i]) %>%
      select(row)
    rownumbers[i] <- x[[1]]
  }
  rownumbers[length(headerlist_total)+1] <- max(cells_total %>% select(row))+1
  return(rownumbers)
}

findheader <- function(header="") {
  rownumbers <- c()
  for (s in 1:length(product_sheets)) {
    x <- cells %>%
      filter(sheet==product_sheets[s]) %>%
      filter(character==header) %>%
      select(row)
    rownumbers[s] <- x[[1]]
  }
  return(rownumbers)
}

allheadermins <- function(headerlist=headerlist) {
  allmins <- list()
  for (i in 1:length(headerlist)) {
    allmins[[i]] <- findheader(headerlist[i])
  }
  max <- c()
  for (s in 1:length(product_sheets)) {
    max[s] <- max(cells %>% filter(sheet==product_sheets[s]) %>% select(row))
  }
  allmins[[12]] <- max+1
  return(allmins)
}

kpis_total <- function(cells_total){
  cells_total %>% 
    dplyr::filter(row <=14) %>% 
    unpivotr::behead("N", week_ending) %>% 
    unpivotr::behead("NNW", section) %>%
    unpivotr::behead("W", kpi) %>%
    dplyr::mutate(attribute = "All data") %>% 
    dplyr::mutate(type = "Total") %>% 
    dplyr::mutate(subtype = NA) %>% 
    dplyr::mutate(product = "All") %>%
    select(product,section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
    tidyr::drop_na(type)  
}

allproductskpis <- function(cells,FUN=kpis) {
  x <- cells %>% 
    group_by(sheet) %>% 
    nest() %>% 
    mutate(data = map(data, FUN)) %>%
    unnest(cols=c(data)) %>%
    rename(product = sheet)
  return(x)
}

allproductsother <- function(cells,indent,FUN) {
  x <- cells %>% 
    group_by(sheet) %>% 
    nest() %>% 
    mutate(data = map(data, ~FUN(.,indent))) %>%
    unnest(cols=c(data)) %>%
    rename(product = sheet)
  return(x)
}

kpis <- function(cells){
  cells %>% 
  dplyr::filter(row <=6) %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>%
  unpivotr::behead("W", kpi) %>%
  dplyr::mutate(attribute = "All data") %>% 
  dplyr::mutate(type = "Total") %>% 
  dplyr::mutate(subtype = NA) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na(type)  
}

lifestage <- function(cells, indent){
  cells %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>% 
  unpivotr::behead("NNW", attribute) %>% 
  unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "kpi") %>% 
  unpivotr::behead("W", type) %>% 
  dplyr::mutate(subtype = NA) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na(type)
}

region <- function(cells, indent){
  cells %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>% 
  unpivotr::behead("NNW", attribute) %>% 
  unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "kpi") %>% 
  unpivotr::behead("W", type) %>% 
  dplyr::mutate(subtype = NA) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na(type)
}

socclass_x_region <- function(cells, indent){
  cells %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>% 
  unpivotr::behead("NNW", attribute) %>% 
  unpivotr::behead("NNW", kpi) %>% 
  unpivotr::behead_if(indent[local_format_id] == 1, direction = "WNW", name = "type") %>% 
  unpivotr::behead("W", subtype) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na()
}

retailer_x_region <- function(cells, indent){
  cells %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>% 
  unpivotr::behead("NNW", attribute) %>% 
  unpivotr::behead("NNW", kpi) %>% 
  unpivotr::behead_if(indent[local_format_id] == 1, direction = "WNW", name = "type") %>% 
  unpivotr::behead("W", subtype) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na()
}

rural_urban <- function(cells,indent) {
  cells %>% 
    unpivotr::behead("N", week_ending) %>%
    unpivotr::behead("NNW", section) %>% 
    unpivotr::behead("NNW", attribute) %>% 
    unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "kpi") %>% 
    unpivotr::behead("W", type) %>% 
    dplyr::mutate(subtype = NA) %>% 
    select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
    tidyr::drop_na(type)
}

social_class <- function(cells, indent){
  cells %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>% 
  unpivotr::behead("NNW", attribute) %>% 
  unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "kpi") %>% 
  unpivotr::behead("W", type) %>% 
  dplyr::mutate(subtype = NA) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na(type)
}

vulnerable_groups <- function(cells, indent){
  cells %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>% 
  unpivotr::behead("NNW", attribute) %>% 
  unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "kpi") %>% 
  unpivotr::behead("W", type) %>% 
  dplyr::mutate(subtype = NA) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na(type)
}

channel <- function(cells, indent){
  cells %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>%
  unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "kpi") %>% 
  unpivotr::behead("W", type) %>% 
  dplyr::mutate(attribute = "All data") %>% 
  dplyr::mutate(subtype = NA) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na(type)  
}

retailer <- function(cells, indent){
  cells %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>%
  unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "kpi") %>% 
  unpivotr::behead("W", type) %>% 
  dplyr::mutate(attribute = "All data") %>% 
  dplyr::mutate(subtype = NA) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na(type)  
}

retailer_x_channel <- function(cells, indent){
  cells %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>% 
  unpivotr::behead("NNW", kpi) %>% 
  unpivotr::behead_if(indent[local_format_id] == 1, direction = "WNW", name = "type") %>% 
  unpivotr::behead("W", subtype) %>% 
  dplyr::mutate(attribute = "All data") %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na()
}

top_ten_manufacturers <- function(cells, indent){
  cells %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>%
  unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "kpi") %>% 
  unpivotr::behead("W", type) %>% 
  dplyr::mutate(attribute = "All data") %>% 
  dplyr::mutate(subtype = NA) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na(type)
}

alcoholabv <- function(cells, indent){
  cells %>% 
    unpivotr::behead("N", week_ending) %>% 
    unpivotr::behead("NNW", section) %>% 
    unpivotr::behead("NNW", attribute) %>% 
    unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "kpi") %>% 
    unpivotr::behead("W", type) %>% 
    dplyr::mutate(subtype = NA) %>% 
    select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
    tidyr::drop_na(type)
}
