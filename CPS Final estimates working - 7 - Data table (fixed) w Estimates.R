## Data table (fixed) w estimates tab ###
## Position in order: 7 / 11


column_names <- c("Opening_stock", "Merchants_for_Malting","Merchants_for_Feed", "Merchants_for_Milling",
                  "Merchants_for_Seed", "Merchants_for_Industrial", "Merchants_for_Other", "Farmers_in_Scotland" ,
                  "Farmers_outwith_Scotland", "Used_for_Seed" , "Used_for_Feed","Waste_Other", "Total_disposed", "Closing_stock")  

## This tab is recreating the pivot table previously used in Excel##
Data_table_fixed_est <- disposals.w.est %>%
  group_by(month, Region, Crop) %>%
  summarise(across(all_of(column_names), ~ sum(.x, na.rm = TRUE), .names = "Sum_{.col}")) %>%
  ungroup()

filtered_disposals_est <- disposals.w.est %>%
  filter(rowSums(select(., 8:21) > 0) > 0)

#Calculate the number of unique parish and holding for each month, Region, and Crop
unique_counts_est <- filtered_disposals_est %>%
  group_by(month, Region, Crop) %>%
  summarise(
    count_parish_holdings = n_distinct(CPH),
    .groups = 'drop'
  )


# Join the unique counts with Data_table_fixed
Data_table_fixed_est <- Data_table_fixed_est %>%
  left_join(unique_counts_est, by = c("month", "Region", "Crop"))

Data_table_fixed_est <- Data_table_fixed_est %>%
  mutate(across(everything(), ~replace(., is.na(.), 0)))


# 1. Calculate monthly totals (sum across all regions and crops for each month)
monthly_totals <- Data_table_fixed_est %>%
  group_by(month,Crop) %>%
  summarise(across(starts_with("Sum_"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(Region = "total",month = paste0(month, "_total"))

# 2. Calculate region totals (sum across all months and crops for each region)
region_totals_est <- Data_table_fixed_est %>%
  group_by(Region, Crop) %>%
  summarise(across(starts_with("Sum_"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(month = "total", Region = paste0(Region, "_total"))

# 3. Calculate grand totals 

grand_totals_est <- Data_table_fixed_est %>%
  group_by(Crop)%>%
  summarise(across(starts_with("Sum_"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(Region = "grand_total", month = "grand_total")%>%
  relocate(Region, .before = Crop)%>%
  relocate(month, .before = Region)

# 4. Combine the original data with the calculated totals
Data_table_fixed_with_est_and_totals <- bind_rows(Data_table_fixed_est, monthly_totals, region_totals_est, grand_totals_est)
# Replace NA values with 0 in column 'x'
Data_table_fixed_with_est_and_totals$count_parish_holdings[is.na(Data_table_fixed_with_est_and_totals$count_parish_holdings)] <- 0

# 5. arrange the data
Data_table_fixed_with_est_and_totals <- Data_table_fixed_with_est_and_totals %>%
  arrange(month, Region, Crop)

View(Data_table_fixed_with_est_and_totals)









