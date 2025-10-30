###############################################################
# CLIMATE OPINION ANALYSIS — NATIONAL & STATE TREND PLOTS
# --------------------------------------------------------
# This script visualizes trends in public support for various 
# climate policies across U.S. states using data from 
# the Yale Climate Opinion Maps (YCOM) 2010–2024 dataset.
#
# Key outputs:
#   1. National-level trend lines for multiple policies (2016–2024)
#   2. State-level trend plots for each U.S. state
###############################################################


library(ggplot2)
library(dplyr)    # for data manipulation (filter, select, summarise, mutate)
library(tidyr)    # for reshaping data (pivot_longer)

# ---- Define human-readable labels for each variable ----
# This lookup vector converts dataset column codes (e.g., "drillanwr")
# into descriptive labels that appear in plot legends.
variable_labels <- c(
  "drillanwr" = "Drill in Arctic National Wildlife Refuge",
  "drilloffshore" = "Drill Offshore",
  "fundrenewables" = "Fund Renewables",
  "generaterenewable" = "Generate Renewables",
  "rebates" = "Tax Rebates",
  "reducetax" = "Reduce Taxes",
  "regulate" = "Regulate CO2",
  "supportrps" = "Support Renewable Portfolio Standards",
  "teachgw" = "Teach Global Warming"
)

# FUNCTION 1: PLOT NATIONAL AVERAGE POLICY TRENDS (2016–2024)
plot_variable_trends <- function(data, variables, plot_title = "Trends Over Time",
                                 highlight_vars = NULL) {
  
  # --- STEP 1: Filter relevant variables and reshape data ---
  # Convert wide data (columns x2016, x2017, etc.) into long format (rows per year).
  plot_data_long <- data %>%
    dplyr::filter(variable %in% variables) %>%              # keep only specified variables
    dplyr::select(GeoName, variable, x2016:x2024) %>%       # keep year columns
    tidyr::pivot_longer(                                   # convert columns x2016...x2024 to rows
      cols = starts_with("x"),
      names_to = "year",
      values_to = "percentage"
    ) %>%
    dplyr::mutate(
      year = as.numeric(gsub("x", "", year))               # remove "x" and convert to numeric
    )
  
  # --- STEP 2: Calculate national averages for each variable-year pair ---
  avg_data <- plot_data_long %>%
    dplyr::group_by(variable, year) %>%
    dplyr::summarise(
      avg_percentage = mean(percentage, na.rm = TRUE),     # average across all states
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Tag variables to highlight (e.g., oil drilling) for custom coloring
      highlight = ifelse(variable %in% highlight_vars, "Highlighted", "Normal")
    )
  
  # --- STEP 3: Create the ggplot visualization ---
  ggplot(avg_data, aes(x = year, y = avg_percentage,
                       shape = variable, group = variable)) +
    geom_line(aes(color = highlight), size = 1, na.rm = TRUE) +  # lines per policy
    geom_point(aes(color = highlight), size = 3, na.rm = TRUE) + # points per data year
    labs(
      title = plot_title,
      x = "Year",
      y = "Percentage (%)",
      shape = "Policy",  # legend title
      color = NULL
    ) +
    scale_color_manual(
      # Red for highlighted variables (e.g., oil drilling), gray for others
      values = c("Highlighted" = "red", "Normal" = "gray40"),
      guide = "none"
    ) +
    scale_shape_manual(
      # Assign distinct shapes and readable labels
      values = c(15, 16, 17, 18, 3, 4, 8, 10, 11),
      labels = variable_labels
    ) +
    scale_x_continuous(breaks = 2016:2024) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 8)
    ) +
    guides(shape = guide_legend(nrow = 3, byrow = TRUE))
}


# FUNCTION 2: PLOT POLICY TRENDS FOR A SPECIFIC STATE
plot_variable_trends_state <- function(data, variables, state_name, 
                                       plot_title = NULL,
                                       highlight_vars = NULL) {
  
  # --- STEP 1: Create a default title if none is given ---
  if (is.null(plot_title)) {
    plot_title <- paste("Public Support Trends in", state_name, "(2016–2024)")
  }
  
  # --- STEP 2: Filter data for the selected state ---
  plot_data_long <- data %>%
    dplyr::filter(variable %in% variables, GeoName == state_name) %>%
    dplyr::select(variable, x2016:x2024) %>%
    tidyr::pivot_longer(
      cols = starts_with("x"),
      names_to = "year",
      values_to = "percentage"
    ) %>%
    dplyr::mutate(
      year = as.numeric(gsub("x", "", year)),             # remove "x" prefix
      highlight = ifelse(variable %in% highlight_vars, "Highlighted", "Normal")
    )
  
  # --- STEP 3: Check if the state exists in the dataset ---
  if (nrow(plot_data_long) == 0) {
    stop(paste("State '", state_name, "' not found in the data."))
  }
  
  # --- STEP 4: Generate a ggplot for this specific state ---
  ggplot(plot_data_long, aes(x = year, y = percentage,
                             shape = variable, group = variable)) +
    geom_line(aes(color = highlight), size = 1, na.rm = TRUE) +
    geom_point(aes(color = highlight), size = 3, na.rm = TRUE) +
    labs(
      title = plot_title,
      x = "Year",
      y = "Percentage (%)",
      shape = "Policy",
      color = NULL
    ) +
    scale_color_manual(
      values = c("Highlighted" = "red", "Normal" = "gray40"),
      guide = "none"
    ) +
    scale_shape_manual(
      values = c(15, 16, 17, 18, 3, 4, 8, 10, 11),
      labels = variable_labels
    ) +
    scale_x_continuous(breaks = 2016:2024) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 8)
    ) +
    guides(shape = guide_legend(nrow = 3, byrow = TRUE))
}


# ---- Define variables to include in the analysis ----
variables_to_plot <- c(
  "fundrenewables", "generaterenewable", "regulate",
  "supportrps", "rebates", "reducetax", 
  "drillanwr", "drilloffshore", "teachgw"
)

# ---- NATIONAL TRENDS PLOT ----
# Generates a line chart showing national averages for each policy.
# Highlighted variables (in red) are those related to oil drilling.
plot_variable_trends(
  data = ycom_publicdata_2010_2024,
  variables = variables_to_plot,
  plot_title = "Policy Support Trends (2016–2024)",
  highlight_vars = c("drillanwr", "drilloffshore")
)


# ---- STATE-LEVEL TREND PLOTS ----
# Loop over each state to create individual trend plots.
# This uses the same structure but filters data by GeoName.
state_list <- ycom_publicdata_2010_2024 %>%
  dplyr::filter(GeoType == "state") %>%
  dplyr::pull(GeoName) %>%
  unique()

# Iterate through each state, generate and print a plot
for (state in state_list) {
  plot <- plot_variable_trends_state(
    data = ycom_publicdata_2010_2024,
    variables = variables_to_plot,
    state_name = state,
    highlight_vars = c("drillanwr", "drilloffshore", "teachgw")  # highlight 3 variables
  )
  print(plot)
}

