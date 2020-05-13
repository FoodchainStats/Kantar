pacman::p_load(tidyverse, tidyxl, unpivotr, here)

source(here("r", "get_product_tables.R"))


# Test on a single sheet workbook
cells <- xlsx_cells(here("data", "raw", "kantar_product.xlsx"))
formats <- xlsx_formats(here("data", "raw", "kantar_product.xlsx"))
indent <- formats$local$alignment$indent

kpis(cells)


# Test on whole workbook - total tab
sheets <- tidyxl::xlsx_sheet_names(here::here("data", "raw", "2020_05_03 Kantar Take Home Weekly Data Raw.xlsx"))
product_sheets <- sheets[2:length(sheets)]
cells <- tidyxl::xlsx_cells(here::here("data", "raw", "2020_05_03 Kantar Take Home Weekly Data Raw.xlsx"),
                            sheets = product_sheets,
                            include_blank_cells = FALSE)

kpi_table <- 
  cells %>% 
  group_by(sheet) %>% 
  nest() %>% 
  mutate(data = map(data, kpis)) %>% 
  unnest() %>% 
  rename(product = sheet)
