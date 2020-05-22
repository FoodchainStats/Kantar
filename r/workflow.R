rm(list=ls())
pacman::p_load(tidyverse, tidyxl, unpivotr, here,lubridate)

source(here::here("r", "get_product_tables.R"))


# Test on a single sheet workbook
# cells_test <- xlsx_cells(here("data", "raw", "kantar_product.xlsx"))
# formats_test <- xlsx_formats(here("data", "raw", "kantar_product.xlsx"))
# indent_test <- formats_test$local$alignment$indent
# kpi_test <- kpis(cells_test)
# lifestage_test <- lifestage(cells_test,indent_test)
# rural_test <- rural_urban(cells_test,indent_test)


# Test on whole workbook - products sheets
sheets <- tidyxl::xlsx_sheet_names(here::here("data", "raw", "2020_05_03 Kantar Take Home Weekly Data Raw.xlsx"))
product_sheets <- sheets[2:length(sheets)]
cells <- tidyxl::xlsx_cells(here::here("data", "raw", "2020_05_03 Kantar Take Home Weekly Data Raw.xlsx"),
                            sheets = product_sheets,
                            include_blank_cells = FALSE)
cells_total <- tidyxl::xlsx_cells(here::here("data", "raw", "2020_05_03 Kantar Take Home Weekly Data Raw.xlsx"),
                            sheets = sheets[1],
                            include_blank_cells = FALSE)
formats <- xlsx_formats(here::here("data", "raw", "2020_05_03 Kantar Take Home Weekly Data Raw.xlsx")) 
indent <- formats$local$alignment$indent

# set up for product sheets
functionlist <- c("lifestage",
                "region",
                "socclass_x_region",
                "retailer_x_region",
                "rural_urban",
                "social_class",
                "vulnerable_groups",
                "channel",
                "retailer",
                "retailer_x_channel",
                "top_ten_manufacturers")
headerlist <- c("Lifestage",
                 "Region",
                 "Social Class by Region",
                 "Retailer by Region",
                 "Rural / Urban Split",
                 "Social Class",
                 "Vulnerable Groups",
                 "Channel",
                 "Retailer",
                 "Retailer by Channel",
                 "Top Ten Manufacturers")
header_mins <- allheadermins(headerlist)

# create tables from all product sheets ( 1 KPI table and 11 detail tables)
kpi_table <- allproductskpis(cells)  %>% mutate(week_ending=lubridate::ymd(strptime(gsub("w/e ","",week_ending),format="%Y-%m-%d")))
for (f in functionlist) {
  min_for_f <- header_mins[[match(f,functionlist)]]
  max_for_f <- header_mins[[match(f,functionlist)+1]]-1
  cells_for_f <- cells %>% 
    mutate(min=min_for_f[match(sheet,product_sheets)],max=max_for_f[match(sheet,product_sheets)]) %>%
    filter(row==1 | row==8 | (row>=min & row<=max)) %>%
    select(-min,-max)
  x <- allproductsother(cells_for_f,indent,FUN=get(f)) %>% mutate(week_ending=lubridate::ymd(strptime(gsub("w/e ","",week_ending),format="%Y-%m-%d")))
  assign(paste0(f,"_table"),x)
  print(paste0(f,"_table created"))
}

# set up for total sheet
functionlist_total <- c("lifestage",
                  "region",
                  "socclass_x_region",
                  "retailer_x_region",
                  "rural_urban",
                  "social_class",
                  "vulnerable_groups",
                  "channel",
                  "retailer",
                  "retailer_x_channel",
                  "alcoholabv")
headerlist_total <- c("Lifestage",
                "Region",
                "Social Class by Region",
                "Retailer by Region",
                "Rural / Urban Split",
                "Social Class",
                "Vulnerable Groups",
                "Channel",
                "Retailer",
                "Retailer by Channel",
                "Alcohol ABV")
header_mins <- findheader_total(headerlist_total)

# create tables from total sheet ( 1 KPI table and 11 detail tables)
kpi_total <- kpis_total(cells_total) %>% mutate(week_ending=lubridate::ymd(strptime(gsub("w/e ","",week_ending),format="%Y-%m-%d")))
for (f in functionlist_total) {
f <- "socclass_x_region"
  min_for_f <- header_mins[match(f,functionlist_total)]
  max_for_f <- header_mins[match(f,functionlist_total)+1]-1
  cells_for_f <- cells_total %>%
    filter(row==1 | row==16 | (row>=min_for_f & row<=max_for_f))
  x <- lapply(list(cells_for_f),indent,FUN=get(f))
  x <- as.data.frame(x) %>% mutate(week_ending=lubridate::ymd(strptime(gsub("w/e ","",week_ending),format="%Y-%m-%d")))
  assign(paste0(f,"_total"),x)
  print(paste0(f,"_total created"))
}
  
  
# save output
date <- max(kpi_table$week_ending)
#list of total tables
out_total <- mget(paste0(c("kpi",functionlist_total),"_total"))
write_rds(out_total,here::here("data",paste0("all_total_",date,".rds")))
#seperate tables for the product sheets
for (f in paste0(c("kpi",functionlist),"_table")) {
  write_rds(get(f),here::here("data",paste0(f,"_products_",date,".rds")))
  print(paste0(f,"_products_date.rds saved"))
  }
