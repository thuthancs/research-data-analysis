install.packages("dplyr")
install.packages("usmap")
install.packages("ggplot2")

library(dplyr)
library(usmap)
library(ggplot2)

# DATA PREP
# Create a mapping from old names to new names
name_mapping <- c("CO2limits" = "co2limits",
                  "CO2limitsOppose" = "co2limitsOppose",
                  "trustclimsciSST" = "trustclimscisst",
                  "trustclimsciSSTOppose" = "trustclimscisstOppose",
                  "supportRPS" = "supportrps", 
                  "supportRPSOppose" = "supportrpsOppose")

# Apply the renaming
howe_etal_2016_metadata$YCOM.VARIABLE.NAME <- 
  ifelse(howe_etal_2016_metadata$YCOM.VARIABLE.NAME %in% names(name_mapping),
         name_mapping[howe_etal_2016_metadata$YCOM.VARIABLE.NAME],
         howe_etal_2016_metadata$YCOM.VARIABLE.NAME)

# Extract a list of variables in 2016 metadata file
variable_list <- unique(howe_etal_2016_metadata$YCOM.VARIABLE.NAME)
variable_list <- howe_etal_2016_metadata[, "YCOM.VARIABLE.NAME"]
variable_list

# Subset the YCOM public data using the variable list from 2016 to 2024
ycom_subset <- ycom_publicdata_2010_2024[ycom_publicdata_2010_2024$variable %in% variable_list, ]
ycom_subset <- ycom_subset %>% select(GeoID, GeoName, GeoType, variable, x2016:x2024)
ycom_subset <- ycom_subset %>% filter(!is.na(x2024))
ycom_subset <- ycom_subset %>% mutate(percentage_diff = x2024 - x2016)

# EXPLORATORY DATA ANALYSIS
consensus_data <- ycom_subset %>% filter(variable == "consensusOppose")


