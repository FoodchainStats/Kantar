
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
  dplyr::filter(row == 1 | row >=8 & row <= 45) %>% 
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
  dplyr::filter(row == 1 | row == 8 | row >=47 & row <= 99) %>% 
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
  dplyr::filter(row == 1 | row == 8 | row >=101 & row <= 441) %>% 
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
  dplyr::filter(row == 1 | row == 8 | row >=443 & row <= 1311) %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>% 
  unpivotr::behead("NNW", attribute) %>% 
  unpivotr::behead("NNW", kpi) %>% 
  unpivotr::behead_if(indent[local_format_id] == 1, direction = "WNW", name = "type") %>% 
  unpivotr::behead("W", subtype) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na()
}


rural_urban <- function(cells, indent){
  cells %>% 
  dplyr::filter(row == 1 | row == 8 | row >=1313 & row <= 1329) %>% 
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
  dplyr::filter(row == 1 | row == 8 | row >=1331 & row <= 1359) %>% 
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
  dplyr::filter(row == 1 | row == 8 | row >=1361 & row <= 1377) %>% 
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
  dplyr::filter(row == 1 | row >= 1379 & row <=1407) %>% 
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
  dplyr::filter(row == 1 | row >= 1409 & row <=1481) %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>%
  unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "kpi") %>% 
  unpivotr::behead("W", type) %>% 
  dplyr::mutate(attribute = "All data") %>% 
  dplyr::mutate(subtype = NA) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na(type)  
}

retailer_x_region <- function(cells, indent){
  cells %>% 
  dplyr::filter(row == 1 | row >= 1483 & row <= 1707) %>% 
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
  dplyr::filter(row == 1 | row >= 1709 & row <=1753) %>% 
  unpivotr::behead("N", week_ending) %>% 
  unpivotr::behead("NNW", section) %>%
  unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "kpi") %>% 
  unpivotr::behead("W", type) %>% 
  dplyr::mutate(attribute = "All data") %>% 
  dplyr::mutate(subtype = NA) %>% 
  select(section, attribute, kpi, type, subtype, week_ending, value = numeric) %>% 
  tidyr::drop_na(type)
}