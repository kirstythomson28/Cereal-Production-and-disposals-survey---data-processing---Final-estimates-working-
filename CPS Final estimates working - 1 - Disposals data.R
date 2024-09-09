## Disposals data ##
## Position in order: 1 / 11


library(readxl)

# Read in raw data from October and June surveys 

# UPDATE FILE LINK #
disposals <- read_excel("Cereal Disposals - 2024 - Data - Raw Data - October and June.xlsx")



### If you have noember and june survey results saved seperatly use the following
#code to combine them:
# Note you will need to remove the hashtag and update the link to files

#disposals_nov <- read_excel("Cereal Disposals - 2024 - Data - Raw Data - November.xlsx")
#disposals_june <- read_excel("Cereal Disposals - 2024 - Data - Raw Data - June.xlsx")

#disposals <- merge(disposals_nov, disposals_june, by = c("Crop", "Region", "Month"))



