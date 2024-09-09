## Disposal Results w estimates tab ATTEMPT  ###
## Position in order: 8 / 11

## Second version of Disposal results w estimates tab ###
### This version is work in progress ##
##It is to complete the same calculation as attempt 1 but with much less code ##


library(dplyr)
library(purrr)
library(tidyr)

FPAC_subset <- Final_production_all_clean %>%
  select(Crop, Region, production) %>%
  # Combine all variations of Barley, Wheat, and Oats into a single category
  mutate(Crop = case_when(
    grepl("^Barley", Crop, ignore.case = TRUE) ~ "Barley",  # Combine all "Barley" variations into "Barley"
    grepl("^Wheat", Crop, ignore.case = TRUE) ~ "Wheat",    # Combine all "Wheat" variations into "Wheat"
    grepl("^Oats", Crop, ignore.case = TRUE) ~ "Oats",      # Combine all "Oats" variations into "Oats"
    TRUE ~ Crop                                             # Keep other values unchanged
  )) %>%
  filter(!(Crop %in% c("OSRape S", "OSRape W")))%>%
  # Group by Crop, Region, and other necessary columns, and sum the production values
  group_by(Crop, Region) %>% 
  summarize(production = sum(production, na.rm = TRUE), .groups = 'drop')%>%
  mutate(month = "Oct")

total_production_by_crop_oct <- FPAC_subset %>%
  filter(month == "Oct") %>%  # Filter rows for month = "Oct"
  group_by(Crop) %>%          # Group by Crop
  summarise(production = sum(production, na.rm = TRUE), # Summarize total production
            .groups = 'drop')%>%
  mutate(month = "Oct_total",
         Region = "total")

# Merge with Data_table_fixed_with_totals
DRT2 <- Data_table_fixed_with_est_and_totals %>%
  left_join(FPAC_subset, by = c("Crop", "Region", "month")) %>%
  left_join(total_production_by_crop_oct, by = c("Crop", "Region", "month")) %>%
  # Calculate Opening_stock based on the month
  mutate(Start_stock = ifelse(month == "Oct_total", production.y, production.x)) %>%
  select(-production.x, -production.y) %>% # Remove redundant production columns
  relocate(Start_stock, .before = Sum_Opening_stock)

# Define the regions of interest
regions_of_interest <- c("UKM5", "UKM6", "UKM7", "UKM8", "UKM9")

# Define the variables of interest
variables <- c("Merchants_for_Malting", "Merchants_for_Feed", "Merchants_for_Milling",  
               "Merchants_for_Seed", "Merchants_for_Industrial", "Merchants_for_Other",     
               "Farmers_in_Scotland", "Farmers_outwith_Scotland", "Used_for_Seed", 
               "Used_for_Feed", "Waste_Other")
variables2 <- c("Sum_Opening_stock","Sum_Merchants_for_Malting","Sum_Merchants_for_Feed","Sum_Merchants_for_Milling","Sum_Merchants_for_Seed",
                "Sum_Merchants_for_Industrial","Sum_Merchants_for_Other","Sum_Farmers_in_Scotland","Sum_Farmers_outwith_Scotland","Sum_Used_for_Seed",
                "Sum_Used_for_Feed","Sum_Waste_Other","Sum_Total_disposed","Sum_Closing_stock")
DRT2 <- DRT2 %>%
  mutate(Merchants_for_Malting = NA,
         Merchants_for_Feed = NA,
         Merchants_for_Milling = NA,
         Merchants_for_Seed = NA,
         Merchants_for_Industrial = NA,
         Merchants_for_Other = NA,
         Farmers_in_Scotland = NA,
         Farmers_outwith_Scotland = NA,
         Used_for_Seed = NA,
         Used_for_Feed = NA,
         Waste_Other = NA)

#------------------------------------------------------------------------------#

# Calculate the totals for each variable over regions UKM5:UKM9 for each crop
totals_by_crop <- DRT2 %>%
  filter(Region %in% regions_of_interest) %>%
  group_by(Crop, month) %>%
  summarize(across(
    all_of(variables2), 
    list(Sum = ~sum(.x, na.rm = TRUE)),
    .names = "Sum_{.col}"
  ),
  Sum_Sum_Opening_stock = sum(Sum_Opening_stock, na.rm = TRUE),
  Sum_Start_stock = sum(Start_stock, na.rm = TRUE),
  .groups = 'drop'
  )

# Perform the conditional computation for each variable
DRT2 <- DRT2 %>%
  # Join with the computed totals by crop
  left_join(totals_by_crop, by = c("Crop","month")) %>%
  group_by(month, Region, Crop) %>%
  # Create the 'condition' variable first
  mutate(condition = ifelse(count_parish_holdings > 4, 1, 0)) %>%
  # Use across to apply the conditional logic to each variable
  mutate(across(
    all_of(variables),
    ~ ifelse(
      condition == 1,
      (get(paste0("Sum_", cur_column())) / Sum_Opening_stock) * Start_stock,
      (get(paste0("Sum_Sum_", cur_column())) / Sum_Sum_Opening_stock) * Start_stock
    ),
    .names = "{.col}"
  )) %>%
  ungroup()

DRT2 <- DRT2 %>%
  # Calculate Total_disposed by summing across the specified variables
  rowwise() %>%
  mutate(Total_disposed = sum(c_across(all_of(variables)), na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate Closing_Stock
  mutate(Closing_Stock = Start_stock - Total_disposed)


oct_closing_stock <- DRT2 %>%
  filter(month == "Oct") %>%
  select(Crop, Region, Closing_Stock) %>%
  rename(Closing_Stock_Oct = Closing_Stock)

# Update Opening_stock for month = Jun with Closing_Stock from October
DRT2 <- DRT2 %>%
  left_join(oct_closing_stock, by = c("Crop", "Region")) %>%
  mutate(Start_stock = ifelse(month == "Jun", Closing_Stock_Oct, Start_stock)) %>%
  select(-Closing_Stock_Oct) # Drop the temporary column


# Perform the conditional computation for each variable
DRT2 <- DRT2 %>%
  group_by(month, Region, Crop) %>%
  # Create the 'condition' variable first
  mutate(condition = ifelse(count_parish_holdings > 4, 1, 0)) %>%
  # Use across to apply the conditional logic to each variable
  mutate(across(
    all_of(variables),
    ~ ifelse(
      condition == 1,
      (get(paste0("Sum_", cur_column())) / Sum_Opening_stock) * Start_stock,
      (get(paste0("Sum_Sum_", cur_column())) / Sum_Sum_Opening_stock) * Start_stock
    ),
    .names = "{.col}"
  )) %>%
  ungroup()


# Create the subset with only the relevant columns and filter for months Jun and Oct
subset_DRT2 <- DRT2 %>%
  select(Crop, month, Region, Start_stock, all_of(variables)) %>%  # Select relevant columns
  filter(month %in% c("Jun", "Oct")) %>%  # Filter for months Jun and Oct
  # Calculate Total_disposed by summing across the specified variables
  rowwise() %>% 
  mutate(Total_disposed = sum(c_across(all_of(variables)), na.rm = TRUE)) %>%
  ungroup() %>%  # Ungroup after rowwise calculations
  # Calculate Closing_Stock
  mutate(Closing_Stock = Start_stock - Total_disposed)

# Define the types of interest
specific_type <- "Specific"
overall_type <- "Overall"

# Add the 'type' variable to subset_DRT
subset_DRT2 <- subset_DRT2 %>%
  # Create a new column 'type' with values "Specific" and "Overall"
  mutate(type = case_when(
    month %in% c("Jun", "Oct") ~ specific_type,
    TRUE ~ overall_type
  )) %>%
  # Arrange the columns so that 'type' comes after 'month'
  select(Crop, month, type, everything())

# Calculate the overall totals for each crop and month, including Start_stock
overall_totals2 <- subset_DRT2 %>%
  # Filter rows with specific types only
  filter(type == "Specific") %>%
  # Group by Crop and month, and summarize the totals
  group_by(Crop, month) %>%
  summarize(across(all_of(variables), sum, na.rm = TRUE), 
            Total_disposed = sum(Total_disposed, na.rm = TRUE),
            Closing_Stock = sum(Closing_Stock, na.rm = TRUE),
            Start_stock = sum(Start_stock, na.rm = TRUE), # Include Start_stock
            .groups = 'drop') %>%
  # Add the 'type' column with value "Overall" and set Region to "Overall"
  mutate(type = "Overall", Region = "Overall") %>%
  # Arrange columns to match the structure of subset_DRT
  select(Crop, month, Region, type, everything())

# Add the overall totals to the original subset_DRT
subset_DRT_with_overall2 <- subset_DRT2 %>%
  bind_rows(overall_totals2)


# Calculate the overall totals for each crop across both months
overall_month_totals2 <- subset_DRT_with_overall2 %>%
  # Filter for rows with type = Overall to ensure we are summing the correct rows
  filter(type == "Overall") %>%
  # Group by Crop to calculate totals across months
  group_by(Crop) %>%
  summarize(across(all_of(variables), sum, na.rm = TRUE),
            Total_disposed = sum(Total_disposed, na.rm = TRUE),
            Closing_Stock = sum(Closing_Stock, na.rm = TRUE),
            Start_stock = sum(Start_stock, na.rm = TRUE),
            type = "Overall",
            month = "Overall",
            Region = "Overall",
            .groups = 'drop')

# Combine this summary with the existing data frame
final_subset_DRT2 <- subset_DRT_with_overall2 %>%
  bind_rows(overall_month_totals2)%>%
  select(-type)

# View the updated data frame
View(final_subset_DRT2)


# Assuming final_subset_DRT is your final dataset with rows for 'Overall'
# Create the scottish_average summary table
scottish_average2 <- final_subset_DRT2 %>%
  # Filter for rows where month and Region are 'Overall'
  filter(month == "Overall", Region == "Overall") %>%
  # Group by Crop to calculate the ratios
  group_by(Crop) %>%
  # Calculate the average for each variable divided by Total_disposed
  summarize(across(all_of(variables), ~ mean(.x / Total_disposed, na.rm = TRUE) * 100),
            .groups = 'drop') %>%
  # Format the variables as percentages with 0 decimal places
  mutate(across(all_of(variables), ~ sprintf("%.0f", .)))

# View the summary table
View(scottish_average2)

#------------------------------------------------------------------------------#





