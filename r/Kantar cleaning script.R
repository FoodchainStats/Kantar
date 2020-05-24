##### Load packages ------------------------------------------------------
rm(list=ls())
pacman::p_load(tidyverse, janitor, readxl, openxlsx, data.table, plotly, flexdashboard, DT, shiny,viridis)
setwd("S:\\Analysis\\Covid-19\\Kantar")
######### Import most recent dataset ---------------------------------------------------------------

## Importing category subcategory lookup
cat_lookup <- read.csv("data/Category Subcategory grid with units.csv", stringsAsFactors = F)

path <- "data/2020_05_10 Kantar Take Home Weekly Data Raw.xlsx"
sheetnames <- excel_sheets(path)
mylist <- lapply(excel_sheets(path), read_excel, path = path)

# name the dataframes
names(mylist) <- sheetnames

# creating column containing the dataframe name
mylist <- Map(cbind, mylist, Category = names(mylist))

# Bring the dataframes to the global environment (no longer needed but useful to see raw data)
#list2env(mylist ,.GlobalEnv)

######## Manipulating data ---------------------------------------------------------------
################## Function to manipulate data into one big dataframe --------------------------------------------------------------


#df <- `Total Take Home Food & Drink `
for(i in 1:length(mylist)){
#for(i in c(1,2,7)){
  # getting sheet into dataframe
  df <- mylist[[i]]
  
  category_df_initial <- as.data.frame(df) %>%
  rename(X1 = 1)%>%
  filter(!is.na(X1))%>%
  # also removing blank columns ie columns beginning with '...'
    select(-starts_with("..."))
 
  # first mutate option - for sheets with alcohol and top 10 manufacturer info (ie. alcohol category sheets)
  if(length(which(grepl("Manufacturer", category_df_initial$X1))) > 0 & length(which(grepl("Alcohol", category_df_initial$X1))) > 0){
   category_df <- category_df_initial %>%
  mutate(COUNT_NA = rowSums(is.na(category_df_initial)),
    Group = ifelse(COUNT_NA == ncol(category_df_initial) - 2 | COUNT_NA == ncol(category_df_initial) - 1 | COUNT_NA == ncol(category_df_initial), NA,
           ifelse(row_number() < which(grepl("Lifestage", category_df_initial$X1)), "Overall",
         ifelse(grepl("Family|Retired|Dependents|Nesters", X1) | 
                  (grepl("Total", X1) & between(row_number(), which(grepl("Lifestage", category_df_initial$X1)), which(grepl("Region", category_df_initial$X1))[1])), "Lifestage",
         ifelse(between(row_number(), which(grepl("Region", category_df_initial$X1))[1], which(grepl("Social Class by Region", category_df_initial$X1))), "Region",
                ifelse((X1 %in% c("AB", "C1", "C2", "D", "E") | grepl("Total", X1))
                        & between(row_number(), which(grepl("Social Class by Region", category_df_initial$X1)), which(grepl("Retailer by Region", category_df_initial$X1))), "Social Class by Region",
                 ifelse(between(row_number(), which(grepl("Retailer by Region", category_df_initial$X1)), which(grepl("Rural / Urban", category_df_initial$X1))), "Retailer by Region",
                 ifelse(X1 %in% c("Rural", "Urban") | 
                         (grepl("Total", X1) & between(row_number(), which(grepl("Rural / Urban", category_df_initial$X1)), which(grepl("Social Class", category_df_initial$X1))[2])), "Rural / Urban Split",
                 ifelse((X1 %in% c("AB", "C1", "C2", "D", "E") | grepl("Total", X1)) 
                        & between(row_number(), which(grepl("Social Class", category_df_initial$X1))[2], which(grepl("Vulnerable Groups", category_df_initial$X1))), "Social Class",
                        ifelse(X1 %in% c("Main Shopper 70+", "DE Household with Children") | 
                                 (grepl("Total", X1) & between(row_number(), which(grepl("Vulnerable Groups", category_df_initial$X1)), which(grepl("Channel", category_df_initial$X1))[1])), "Vulnerable Groups",
                  ifelse(X1 %in% c("Supermarket", "Discounter", "Convenience", "Internet", "High Street", "High street") | 
                           (grepl("Total", X1) & between(row_number(), which(grepl("Channel", category_df_initial$X1))[1], 
                                                         which(grepl("Retailer", category_df_initial$X1) & !grepl("Region|Channel", category_df_initial$X1)))), "Channel", 
                         ifelse(between(row_number(), which(grepl("Retailer", category_df_initial$X1) & !grepl("Region|Channel", category_df_initial$X1)), 
                                        which(grepl("Retailer by Channel", category_df_initial$X1))), "Retailer",
                         ifelse(between(row_number(), which(grepl("Retailer by Channel", category_df_initial$X1)), which(grepl("Manufacturer", category_df_initial$X1))[1]), "Retailer by Channel",
                         ifelse(between(row_number(), which(grepl("Manufacturer", category_df_initial$X1)), which(grepl("Alcohol ABV", category_df_initial$X1))[1]), "Top 10 Manufacturers",
                                "Alcohol ABV"))))))))))))),
         # first removing 'nutritional' from variable names
         X1 = gsub(" \\(Nutritional\\)", "", X1),
  # then editing some variable names for consistency
  X1 = ifelse(grepl("trips per household", X1, ignore.case = T), "Trips Per Household", 
              ifelse(grepl("volume per trip", X1, ignore.case = T), "Volume Per Trip", 
                     ifelse(grepl("Carb", X1), "Carbohydrates",
                            ifelse(grepl("Saturate", X1), "Saturated Fat", 
                            ifelse(grepl("fibre", X1), "Fibre", 
                                   ifelse(X1 == "Fat", "Total Fat", X1)))))))%>%
  # filtering out group name title rows
  filter(!trimws(X1) %in% c("KPIs", "Demographics", "Lifestage", "Region", "Social Class by Region", "Retailer by Region", "Rural / Urban Split", 
                            "Social Class", "Vulnerable Groups", "Channel", "Retailer by Channel", "Retailer", "Alcohol ABV", "Top 10 Manufacturers", "Top Ten Manufacturers"))%>%
  # flag if a further regional/channel breakdown
  mutate(further_breakdown = ifelse(is.na(Group) & !grepl("Volume|Household|Penetration|Sugar|Calories|Fat|Saturate|Fibre|Protein|Carb|Sodium", X1), 1, 
                                    ifelse(is.na(Group) & grepl("Volume|Household|Penetration|Sugar|Calories|Fat|Saturate|Fibre|Protein|Carb|Sodium", X1),2, 0)))   
  # if it's the title sheet there's no top 10 manufacturer info
  }else if(length(which(grepl("Manufacturer", category_df_initial$X1))) == 0 & length(which(grepl("Alcohol", category_df_initial$X1))) > 0){
      category_df <- category_df_initial %>%
  mutate(COUNT_NA = rowSums(is.na(category_df_initial)),
    Group = ifelse(COUNT_NA == ncol(category_df_initial) - 2 | COUNT_NA == ncol(category_df_initial) - 1 | COUNT_NA == ncol(category_df_initial), NA,
           ifelse(row_number() < which(grepl("Lifestage", category_df_initial$X1)), "Overall",
         ifelse(grepl("Family|Retired|Dependents|Nesters", X1) | 
                  (grepl("Total", X1) & between(row_number(), which(grepl("Lifestage", category_df_initial$X1)), which(grepl("Region", category_df_initial$X1))[1])), "Lifestage",
         ifelse(between(row_number(), which(grepl("Region", category_df_initial$X1))[1], which(grepl("Social Class by Region", category_df_initial$X1))), "Region",
                ifelse((X1 %in% c("AB", "C1", "C2", "D", "E") | grepl("Total", X1))
                        & between(row_number(), which(grepl("Social Class by Region", category_df_initial$X1)), which(grepl("Retailer by Region", category_df_initial$X1))), "Social Class by Region",
                 ifelse(between(row_number(), which(grepl("Retailer by Region", category_df_initial$X1)), which(grepl("Rural / Urban", category_df_initial$X1))), "Retailer by Region",
                 ifelse(X1 %in% c("Rural", "Urban") | 
                         (grepl("Total", X1) & between(row_number(), which(grepl("Rural / Urban", category_df_initial$X1)), which(grepl("Social Class", category_df_initial$X1))[2])), "Rural / Urban Split",
                 ifelse((X1 %in% c("AB", "C1", "C2", "D", "E") | grepl("Total", X1)) 
                        & between(row_number(), which(grepl("Social Class", category_df_initial$X1))[2], which(grepl("Vulnerable Groups", category_df_initial$X1))), "Social Class",
                        ifelse(X1 %in% c("Main Shopper 70+", "DE Household with Children") | 
                                 (grepl("Total", X1) & between(row_number(), which(grepl("Vulnerable Groups", category_df_initial$X1)), which(grepl("Channel", category_df_initial$X1))[1])), "Vulnerable Groups",
                  ifelse(X1 %in% c("Supermarket", "Discounter", "Convenience", "Internet", "High Street", "High street") | 
                           (grepl("Total", X1) & between(row_number(), which(grepl("Channel", category_df_initial$X1))[1], 
                                                         which(grepl("Retailer", category_df_initial$X1) & !grepl("Region|Channel", category_df_initial$X1)))), "Channel", 
                         ifelse(between(row_number(), which(grepl("Retailer", category_df_initial$X1) & !grepl("Region|Channel", category_df_initial$X1)), 
                                        which(grepl("Retailer by Channel", category_df_initial$X1))), "Retailer",
                         ifelse(between(row_number(), which(grepl("Retailer by Channel", category_df_initial$X1)), which(grepl("Alcohol ABV", category_df_initial$X1))[1]), "Retailer by Channel",
                                "Alcohol ABV")))))))))))),
         # first removing 'nutritional' from variable names
         X1 = gsub(" \\(Nutritional\\)", "", X1),
  # then editing some variable names for consistency
  X1 = ifelse(grepl("trips per household", X1, ignore.case = T), "Trips Per Household", 
              ifelse(grepl("volume per trip", X1, ignore.case = T), "Volume Per Trip", 
                     ifelse(grepl("Carb", X1), "Carbohydrates",
                            ifelse(grepl("Saturate", X1), "Saturated Fat", 
                            ifelse(grepl("fibre", X1), "Fibre", 
                                   ifelse(X1 == "Fat", "Total Fat", X1)))))))%>%
  # filtering out group name title rows
  filter(!trimws(X1) %in% c("KPIs", "Demographics", "Lifestage", "Region", "Social Class by Region", "Retailer by Region", "Rural / Urban Split", 
                            "Social Class", "Vulnerable Groups", "Channel", "Retailer by Channel", "Retailer", "Alcohol ABV", "Top 10 Manufacturers", "Top Ten Manufacturers"))%>%
  # flag if a further regional/channel breakdown
  mutate(further_breakdown = ifelse(is.na(Group) & !grepl("Volume|Household|Penetration|Sugar|Calories|Fat|Saturate|Fibre|Protein|Carb|Sodium", X1), 1, 
                                    ifelse(is.na(Group) & grepl("Volume|Household|Penetration|Sugar|Calories|Fat|Saturate|Fibre|Protein|Carb|Sodium", X1),2, 0)))   
  # all other categories (non-alcohol) just have top 10 manufacturer info
      }else{
       category_df <- category_df_initial %>%
  mutate(COUNT_NA = rowSums(is.na(category_df_initial)),
    Group = ifelse(COUNT_NA == ncol(category_df_initial) - 2 | COUNT_NA == ncol(category_df_initial) - 1 | COUNT_NA == ncol(category_df_initial), NA,
           ifelse(row_number() < which(grepl("Lifestage", category_df_initial$X1)), "Overall",
         ifelse(grepl("Family|Retired|Dependents|Nesters", X1) | 
                  (grepl("Total", X1) & between(row_number(), which(grepl("Lifestage", category_df_initial$X1)), which(grepl("Region", category_df_initial$X1))[1])), "Lifestage",
         ifelse(between(row_number(), which(grepl("Region", category_df_initial$X1))[1], which(grepl("Social Class by Region", category_df_initial$X1))), "Region",
                ifelse((X1 %in% c("AB", "C1", "C2", "D", "E") | grepl("Total", X1))
                        & between(row_number(), which(grepl("Social Class by Region", category_df_initial$X1)), which(grepl("Retailer by Region", category_df_initial$X1))), "Social Class by Region",
                 ifelse(between(row_number(), which(grepl("Retailer by Region", category_df_initial$X1)), which(grepl("Rural / Urban", category_df_initial$X1))), "Retailer by Region",
                 ifelse(X1 %in% c("Rural", "Urban") | 
                         (grepl("Total", X1) & between(row_number(), which(grepl("Rural / Urban", category_df_initial$X1)), which(grepl("Social Class", category_df_initial$X1))[2])), "Rural / Urban Split",
                 ifelse((X1 %in% c("AB", "C1", "C2", "D", "E") | grepl("Total", X1)) 
                        & between(row_number(), which(grepl("Social Class", category_df_initial$X1))[2], which(grepl("Vulnerable Groups", category_df_initial$X1))), "Social Class",
                        ifelse(X1 %in% c("Main Shopper 70+", "DE Household with Children") | 
                                 (grepl("Total", X1) & between(row_number(), which(grepl("Vulnerable Groups", category_df_initial$X1)), which(grepl("Channel", category_df_initial$X1))[1])), "Vulnerable Groups",
                  ifelse(X1 %in% c("Supermarket", "Discounter", "Convenience", "Internet", "High Street", "High street") | 
                           (grepl("Total", X1) & between(row_number(), which(grepl("Channel", category_df_initial$X1))[1], 
                                                         which(grepl("Retailer", category_df_initial$X1) & !grepl("Region|Channel", category_df_initial$X1)))), "Channel", 
                         ifelse(between(row_number(), which(grepl("Retailer", category_df_initial$X1) & !grepl("Region|Channel", category_df_initial$X1)), 
                                        which(grepl("Retailer by Channel", category_df_initial$X1))), "Retailer",
                         ifelse(between(row_number(), which(grepl("Retailer by Channel", category_df_initial$X1)), which(grepl("Manufacturer", category_df_initial$X1))[1]), "Retailer by Channel",
                                "Top 10 Manufacturers")))))))))))),
         # first removing 'nutritional' from variable names
         X1 = gsub(" \\(Nutritional\\)", "", X1),
  # then editing some variable names for consistency
  X1 = ifelse(grepl("trips per household", X1, ignore.case = T), "Trips Per Household", 
              ifelse(grepl("volume per trip", X1, ignore.case = T), "Volume Per Trip", 
                     ifelse(grepl("Carb", X1), "Carbohydrates",
                            ifelse(grepl("Saturate", X1), "Saturated Fat", 
                            ifelse(grepl("fibre", X1), "Fibre", 
                                   ifelse(X1 == "Fat", "Total Fat", X1)))))))%>%
  # filtering out group name title rows
  filter(!trimws(X1) %in% c("KPIs", "Demographics", "Lifestage", "Region", "Social Class by Region", "Retailer by Region", "Rural / Urban Split", 
                            "Social Class", "Vulnerable Groups", "Channel", "Retailer by Channel", "Retailer", "Alcohol ABV", "Top 10 Manufacturers", "Top Ten Manufacturers"))%>%
  # flag if a further regional/channel breakdown
  mutate(further_breakdown = ifelse(is.na(Group) & !grepl("Volume|Household|Penetration|Sugar|Calories|Fat|Saturate|Fibre|Protein|Carb|Sodium", X1), 1, 
                                    ifelse(is.na(Group) & grepl("Volume|Household|Penetration|Sugar|Calories|Fat|Saturate|Fibre|Protein|Carb|Sodium", X1),2, 0)))     
    
   }
    

    
   # then if further breakdown == 1 paste the name of the breakdown group in the additional name column by creating a function
         #---------------------------------------         
           current_name <- NA
for(name in 1:nrow(category_df)){
  # if it's a further breakdown, populate with further breakdown name
  if(category_df$further_breakdown[name] == 1){
    current_name <- category_df[name,1]
  }
  # if it's an indicator name, populate with NA
  if(category_df$further_breakdown[name] == 2){
    current_name <- NA
  }
  category_df$additional_name[name] <- current_name  
  }
rm(current_name, name)

# then join additional name onto X1 name and remove unneeded columns
category_df <- category_df %>%
  mutate(X1 = ifelse(further_breakdown != 1 & !is.na(additional_name) & !is.na(Group), paste0(additional_name, " - ", X1), X1))%>%
  select(-further_breakdown, -additional_name)

 # getting indicator information
  current_indicator <- NA
for(ind in 1:nrow(category_df)){
  if(is.na(category_df[ind,2]) == TRUE & grepl("Volume|Household|Penetration|Sugar|Calories|Fat|Saturate|Fibre|Protein|Carb|Sodium", category_df[ind,1])){
    current_indicator <- category_df[ind,1]
  }
  category_df$Indicator[ind] <- current_indicator  
  }
rm(current_indicator, ind)

# removing unneeded rows
category_df <- category_df %>%
  filter(!is.na(Group))%>%
  # cleaning up variable names
  mutate(Indicator = ifelse(is.na(Indicator), X1, Indicator),
         Sub_Group = ifelse(Indicator != X1, X1, "Overall"))%>%
  select(-X1)

# binding onto main dataset
if(i == 1){
cleaned_data <- category_df
}else{
  cleaned_data <- bind_rows(cleaned_data, category_df)
}

}

# Joining category/sub-category info and units
cleaned_data <- left_join(cleaned_data, cat_lookup, by = "Category")%>%
  # temporary fix for fresh fish
  mutate(Parent_Category = ifelse(Category %in% c("Chilled Prepared Fish", "Shellfish", "Wet Smoked Fish"), "Fresh Fish", Parent_Category),
         KGs = ifelse(Parent_Category == "Fresh Fish", "Y", KGs),
         Litres = ifelse(Parent_Category == "Fresh Fish", "0", Litres),
         Servings.Number.in.Pack = ifelse(Parent_Category == "Fresh Fish", "0", Servings.Number.in.Pack),
         Units = ifelse(Parent_Category == "Fresh Fish", "0", Units))%>%
  select(-Comments)

# removing the COUNT_NA column
# also renaming difficult to interpret category names eg. fabs 
cleaned_data <- cleaned_data %>%
  mutate(Category = ifelse(Category == "Household & Cleaning Prds", "Household+Cleaning Products",
           ifelse(grepl("Pickle\\+Tbl", Category), "Pickles, Table Sauces + Condiments",
         ifelse(grepl("Fabs", Category), "Flavoured Alcoholic Beverages", ifelse(grepl("Svry", Category), gsub("Svry", "Savoury", Category),
          ifelse(grepl("Frt", Category), gsub("Frt", "Fruit", Category), ifelse(grepl("Prods", Category), gsub("Prods", "Products", Category),
         ifelse(grepl("Prds", Category), gsub("Prds", "Products", Category), 
           ifelse(grepl("Carbohydrts\\+Sncks", Category), gsub("Carbohydrts\\+Sncks", "Carbohydrates+Snacks", Category), Category)))))))),
         Parent_Category = ifelse(Parent_Category == "Household & Cleaning Prds", "Household+Cleaning Products",
                                  ifelse(grepl("Pickle\\+Tbl", Parent_Category), "Pickles, Table Sauces + Condiments",
         ifelse(grepl("Fabs", Parent_Category), "Flavoured Alcoholic Beverages", ifelse(grepl("Svry", Parent_Category), gsub("Svry", "Savoury", Parent_Category),
          ifelse(grepl("Frt", Parent_Category), gsub("Frt", "Fruit", Parent_Category), ifelse(grepl("Prods", Parent_Category), gsub("Prods", "Products", Parent_Category),
         ifelse(grepl("Prds", Parent_Category), gsub("Prds", "Products", Parent_Category), 
           ifelse(grepl("Carbohydrts\\+Sncks", Parent_Category), gsub("Carbohydrts\\+Sncks", "Carbohydrates+Snacks", Parent_Category), Parent_Category)))))))))%>%
  mutate(Category = trimws(Category),
         Parent_Category = trimws(Parent_Category))%>%
  select(-COUNT_NA)%>%
# dividing volume indicators by 1000 to get tonnes, and kcals by 1000000 to get million kcals
  # TEMPORARY FIX FOR BABY MILK (CURRENTLY EQUIVALENT POWDERED GRAMS), DIVIDING BY 1000000 (KANTAR SHOULD CHANGE THIS IN FURUTRE RELEASES)
    modify_at(
    vars(starts_with("w/e")),
    ~ if_else(!cleaned_data$Indicator %in% c("Penetration", "Trips Per Household", "Volume Per Trip", "Calories") & cleaned_data$Category == "Baby Milk", . /1000000,
              if_else(cleaned_data$Indicator == "Volume Per Trip" & cleaned_data$Category == "Baby Milk", . /1000,
              ifelse(!cleaned_data$Indicator %in% c("Penetration", "Trips Per Household", "Volume Per Trip", "Calories"), . /1000,
              if_else(cleaned_data$Indicator == "Calories", . /1000000, .)))))

# creating variables to arrange in correct order ie. total at top, then by category/subcategory alphabetically
# forcing non-grocery products to the bottom
# then need to re-arrange demographic group order
cleaned_data <- cleaned_data %>%
  mutate(arrange_index_1 = ifelse(Parent_Category == "Total Food & Drink", "A", 
                                  ifelse(Parent_Category == "Baby Milk", "Z", 
                                         ifelse(Parent_Category %in% c("Household+Cleaning Products", "Healthcare", "Oral-Care", "Other Toiletries"),
                                                "Y", Parent_Category))),
         arrange_index_2 = ifelse(Category == Parent_Category, "A", Category),
         arrange_index_3 = ifelse(Parent_Category == "Household+Cleaning Products" & Category == "Household+Cleaning Products", "A",
                                  ifelse(Parent_Category == "Household+Cleaning Products" & Category != "Household+Cleaning Products", Category, NA)),
         demo_index = ifelse(Group == "Overall", 1, ifelse(Group == "Social Class", 2, ifelse(Group == "Lifestage", 3,
                      ifelse(Group == "Vulnerable Groups", 4, ifelse(Group == "Region", 5, ifelse(Group == "Rural / Urban Split", 6, 7)))))))%>%
  arrange(arrange_index_1, arrange_index_3, arrange_index_2, demo_index, Group)%>%
  select(-arrange_index_1, -arrange_index_2, -arrange_index_3, -demo_index)


# removing certain groups info for now
cleaned_data_filtered <- filter(cleaned_data, Group!="Social Class by Region" & Group!="Top 10 Manufacturers" & Group!="Retailer by Region" & Group!="Retailer by Channel")

################################ Assigning consistent colours to groups / categories ------------------------------------
# phe pallette
pal <- viridis(20)

######## by groups
group_cols <- cleaned_data_filtered %>%
  select(Group, Sub_Group)%>%
  distinct() %>%
  filter(!grepl("Total", Sub_Group)) %>% # as we'll give them a default colour of grey ((#A4AEB5))
  group_by(Group) %>%
 mutate(Count = row_number())%>%
  ungroup()%>%
  # for social class by region just assign social class (or region?!) numbers as they'll never all be presented together
mutate(Count = ifelse(Group == "Social Class by Region" & grepl("- AB", Sub_Group), 1,
                      ifelse(Group == "Social Class by Region" & grepl("- C1", Sub_Group), 2,
                      ifelse(Group == "Social Class by Region" & grepl("- C2", Sub_Group), 3,
                      ifelse(Group == "Social Class by Region" & grepl("- D", Sub_Group), 4,
                      ifelse(Group == "Social Class by Region" & grepl("- E", Sub_Group), 5, Count))))))

# getting maximum number of colours needed
# remove default grey from pallette (#A4AEB5) - this is for 'Total'...note it doesn't seem to appear so leaving this step
group_pal  <- as.data.frame(pal) %>%
  #as.data.frame(colorRampPalette(pal)(max(group_cols$Count))) %>%
  rename(group_colour = 1)%>%
  filter(group_colour != "#A4AEB5")%>%
  mutate(group_colour = as.character(group_colour),
    Count = row_number())

# assigning colour to group
group_cols <- left_join(group_cols, group_pal, by = "Count")%>%
  select(Sub_Group, group_colour)
  
######## by categories and subcategories
# Parent category is given a colour
# Subcategory is given a line type number

category_cols <- cleaned_data_filtered %>%
  select(Parent_Category)%>%
  distinct() %>%
  filter(Parent_Category != "Total Food & Drink") %>% # as we'll give them a default colour of grey (#A4AEB5)
 mutate(Count = row_number())

category_lines <- cleaned_data_filtered %>%
  select(Category, Parent_Category)%>%
  distinct() %>%
  filter(Category != Parent_Category)%>% # as we'll give them a default line type of 1
  group_by(Parent_Category)%>%
 mutate(category_line = row_number()+1)%>%
  ungroup()%>%
  select(-Parent_Category)

# getting maximum number of colours needed (as more than 12 need to use the colour ramp function)
category_pal  <- as.data.frame(colorRampPalette(pal)(max(category_cols$Count))) %>%
  rename(category_colour = 1)%>%
  mutate(category_colour = as.character(category_colour),
    Count = row_number())

# assigning colour to category
category_cols <- left_join(category_cols, category_pal, by = "Count")%>%
  select(Parent_Category, category_colour)

##### joining colours to main data
cleaned_data_filtered <- left_join(cleaned_data_filtered, group_cols, by = "Sub_Group")%>%
  left_join(category_cols, by = "Parent_Category") %>%
  left_join(category_lines, by = "Category") %>%
  mutate(group_colour = ifelse(is.na(group_colour), "#A4AEB5", group_colour),
         category_colour = ifelse(is.na(category_colour), "#A4AEB5", category_colour),
         category_line = ifelse(is.na(category_line), 1, category_line))


############# Assigning an order column to groups/categories to ensure they appear in correct order --------------------------------

# NOTE IN SCRIPT THE Total Food & Drink ORDER IS MANIPULATED TO FORCE TO CORRECT POSITION

##### By group
order_group <- select(cleaned_data_filtered, Group, Sub_Group)%>%
  distinct() %>%
  group_by(Group) %>%
 mutate(group_order_col = row_number())%>%
  ungroup()%>%
  select(-Group)%>%
  distinct()

##### By category
order_category <- select(cleaned_data_filtered, Category, Parent_Category, category_line)%>%
  distinct() %>%
 mutate(category_order_col = row_number()) %>%
  select(-Parent_Category, -category_line)

# adding order to cleaned data 
cleaned_data_filtered <- left_join(cleaned_data_filtered, order_group, by = "Sub_Group")%>%
  left_join(order_category, by = "Category")


# moving grouping variables / colours to beginning of data
cleaned_data_filtered <- cleaned_data_filtered[,c(ncol(cleaned_data_filtered)-13, ncol(cleaned_data_filtered)-9, ncol(cleaned_data_filtered)-12, ncol(cleaned_data_filtered)-10, ncol(cleaned_data_filtered)-11, 
                                                  ncol(cleaned_data_filtered)-8, ncol(cleaned_data_filtered)-7, ncol(cleaned_data_filtered)-6, ncol(cleaned_data_filtered)-5, ncol(cleaned_data_filtered)-4, 
                                                  ncol(cleaned_data_filtered)-3, ncol(cleaned_data_filtered)-2, ncol(cleaned_data_filtered)-1, ncol(cleaned_data_filtered), 
                                                  1:(ncol(cleaned_data_filtered)-14))] 



#### writing this edited data as a csv file - THIS IS THE CSV FILE FED INTO THE DASHBOARD
fwrite(cleaned_data_filtered, "dashboard/Current_Week_cleaned_data.csv", row.names = F )


