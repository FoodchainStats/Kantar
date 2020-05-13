pacman::p_load(tidyverse, tidyxl, unpivotr, here)

source(here("r", "get_product_tables.R"))

cells <- xlsx_cells(here("data", "kantar_product.xlsx"))
formats <- xlsx_formats(here("data", "kantar_product.xlsx"))
indent <- formats$local$alignment$indent


kpis(cells)
