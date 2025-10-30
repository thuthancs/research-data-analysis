# Load required libraries
library(tidyverse)
library(reshape2)

# Assuming your data is loaded as 'climate_data'
# If reading from CSV:
# climate_data <- read.csv("your_file.csv")

# ==============================================================
# STEP 1: SPECIFY YOUR VARIABLES OF INTEREST
# ==============================================================

# Define the variables you want to analyze
selected_variables <- c(
  "worried",
  "timing",
  "harmplants",
  "futuregen",
  "devharm",
  "harmus",
  "personal",
  "regulate",
  "fundrenewables",
  "generaterenewable",
  "co2limits",
  "teachgw",
  "transitioneconomy",
  "taxdividend",
  "rebates",
  "reducetax",
  "supportrps"
)

# ==============================================================
# STEP 2: PREPARE DATA
# ==============================================================

# Filter for selected variables and reshape
climate_subset <- ycom_publicdata_2010_2024 %>%
  filter(variable %in% selected_variables)

# Reshape to long format
climate_long <- climate_subset %>%
  pivot_longer(cols = starts_with("x20"),
               names_to = "year",
               values_to = "value") %>%
  filter(!is.na(value))

# Create wide format with variables as columns for correlation
# Average across states and years for each variable
climate_wide <- climate_long %>%
  pivot_wider(names_from = variable,
              values_from = value,
              id_cols = c(GeoName, year),
              values_fn = mean)

# ==============================================================
# STEP 3: CALCULATE CORRELATION MATRIX
# ==============================================================

# Select only the variable columns
cor_data <- climate_wide %>%
  select(all_of(selected_variables))

# Calculate correlation matrix
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

# ==============================================================
# STEP 4: CREATE HEATMAP WITH GGPLOT2
# ==============================================================
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

# Apply upper triangle mask
upper_tri <- get_upper_tri(cor_matrix)

# Melt correlation matrix for ggplot
cor_melted <- melt(cor_matrix)

# Create the heatmap
p <- ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(
    low = "#3B4CC0", 
    high = "#B40426", 
    mid = "white",
    midpoint = 0, 
    limit = c(-1, 1), 
    space = "Lab",
    name = "Correlation"
  ) +
  geom_text(aes(label = round(value, 2)), 
            size = 3, 
            color = "black") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  labs(
    title = "Correlation Heatmap: Climate Opinion Variables",
    subtitle = "Based on state-level data from 2018-2024"
  ) +
  coord_fixed()

print(p)

