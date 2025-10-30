install.packages("dplyr")
install.packages("usmap")
install.packages("ggplot2")
install.packages("stringr")

library(dplyr)
library(usmap)
library(ggplot2)
library(stringr)


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
# Create a function to plot US maps for any variable
plot_variable_map <- function(data, variable_name, plot_title, 
                              show_labels = FALSE,
                              legend_name = "Difference") {
  # Filter and prepare data
  map_data <- data %>% 
    filter(variable == variable_name) %>%
    select(state = GeoName, percentage_diff)
  
  # Create the plot
  plot_usmap(data = map_data, values = "percentage_diff", 
             color = "white", labels = show_labels) +
    scale_fill_gradient2(
      low = "darkred",
      mid = "white",
      high = "darkgreen",
      midpoint = 0,
      name = legend_name,
      label = scales::comma,
      guide = guide_colorbar(barheight = 15, barwidth = 1.5)
    ) +
    labs(title = plot_title) +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5)
    )
}

# Loop through all variables in variable_list to generate the map for each variable
for (var in variable_list) {
  description <- howe_etal_2016_metadata$VARIABLE.DESCRIPTION[
    howe_etal_2016_metadata$YCOM.VARIABLE.NAME == var
  ]
  # Create a title using the variable name
  plot_title <- paste("Percentage Difference (2024 vs 2016) -", description)
  plot_title <- str_wrap(plot_title, width = 80)
  
  # Create and display the plot
  plot <- plot_variable_map(
    data = ycom_subset, 
    variable_name = var, 
    plot_title = plot_title,
    show_labels = TRUE
  )
  print(plot)
}
