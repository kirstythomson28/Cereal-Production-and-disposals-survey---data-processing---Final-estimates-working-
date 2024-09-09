## Final production tab ###
## Position in order: 2 / 11

library(readxl)
library(readr)
library(dplyr)
library(stringr)

#Load final production spreadsheet that has area, production and yield values for each crop type by season 
Final_production_all <- read_excel("Cereal Disposals - 2023 Crop year - Final Production .xlsx")


## Create summary of final production values ##
## (Table 1/3) ##
Final_production_all_clean <- Final_production_all %>%
  select(,1:5)%>%
  #Ensure region names match those you are working with# 
  mutate(Region = case_when(Region == "North Eastern" ~'UKM5',
                            Region == "Eastern" ~ 'UKM7',
                            Region == "Southern" ~ 'UKM9',
                            Region == "Highlands and Islands" ~ 'UKM6',
                            Region == "West Central" ~ 'UKM8')) %>%
  rename(area = "Areas (hectares)", 
         production = "Production (tonnes)", 
         yield = "Yield (tonnes/hectare)")


## Create summary of final production for each crop type by season for all regions combined ##
## (Table 2/3) ##
# Create Final_production_crop_season by grouping, summarizing, and calculating yield
Final_production_crop_season <- Final_production_all_clean %>%
  group_by(Crop) %>%
  summarize(
    area = sum(area),
    production = sum(production),
    yield = production / area
  )

## Create summary of final production for each crop type for all seasons and regions combined ##
## (Table 3/3) ##
# Create Final_production_crop_type by grouping, summarizing, and calculating yield
Final_production_crop_type <- Final_production_all_clean %>%
  group_by(Crop) %>%
  summarize(
    area = sum(area),
    production = sum(production),
    yield = production / area
  )


# Create a generalized crop name column
Final_production_crop_type <- Final_production_all_clean %>%
  mutate(
    General_Crop = case_when(
      str_starts(Crop, "Barley") ~ "Barley",
      str_starts(Crop, "Wheat") ~ "Wheat",
      str_starts(Crop, "Oats") ~ "Oats",
      str_starts(Crop, "OSRape") ~ "OSRape",
      TRUE ~ "Other"
    )
  )

Final_production_crop_type <- Final_production_crop_type %>%
  group_by(General_Crop) %>%
  summarize(
    area = sum(area),
    production = sum(production),
    yield = production / area
  )

# Calculate the sum of area and production for Barley, Oats, and Wheat
cereal_sums <- Final_production_crop_type %>%
  filter(General_Crop %in% c("Barley", "Wheat", "Oats")) %>%
  summarize(
    General_Crop = "Total_cereals",
    area = sum(area),
    production = sum(production),
    yield = production / area
  )

# Combine the summarized data with the new row
Final_production_crop_type <- bind_rows(Final_production_crop_type, cereal_sums) 

