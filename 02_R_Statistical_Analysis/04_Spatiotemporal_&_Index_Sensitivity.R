



# ==============================================================================
# SCRIPT 04: SPATIOTEMPORAL DYNAMICS & INDEX SENSITIVITY
# Project: Andean Fire Ecosystem Dynamics (2017-2025)
# ==============================================================================

library(tidyverse)
library(scales)
library(trend)
library(ggplot2)

theme_set(theme_bw(base_size = 12, base_family = "sans") + 
            theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14, color = "#2c3e50"),
                  plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#7f8c8d"),
                  plot.background = element_rect(fill = "white", color = NA)))

# 1. MERGE FRAGMENTS
df_zone_raw <- read_csv("3_Zonal_Annual_Stats.csv")
df_zone_merged <- df_zone_raw %>%
  mutate(Zone = str_replace_all(Zone, "_", " ")) %>% group_by(Year, Zone) %>%
  summarise(Burned_Ha = sum(Burned_Ha, na.rm = TRUE), Total_Zone_Ha = sum(Total_Zone_Ha, na.rm = TRUE), NBR_Mean = mean(NBR_Mean, na.rm = TRUE), .groups = "drop")

# 2. BUBBLE PLOT (DYNAMIC SCALING)
df_bubble <- df_zone_merged %>% mutate(Zone = fct_reorder(Zone, Burned_Ha, .fun = sum, .desc = FALSE))

p_bubble <- ggplot(df_bubble, aes(x = Year, y = Zone)) +
  geom_point(aes(size = Burned_Ha, fill = NBR_Mean), shape = 21, color = "grey20", stroke = 0.8, alpha = 0.9) + 
  scale_fill_gradientn(colors = c("#c0392b", "#e67e22", "#f1c40f", "#2ecc71"), name = "Mean NBR") +
  scale_size_continuous(range = c(4, 22), name = "Burned Area\n(Hectares)", breaks = waiver(), labels = comma) +
  scale_x_continuous(breaks = unique(df_bubble$Year)) +
  labs(title = "Spatiotemporal Fire Dynamics (2017-2025)", subtitle = "Bubble size represents fire extent; Color indicates ecosystem integrity", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0),
        axis.text.x = element_text(face = "bold", color = "black"), axis.text.y = element_text(face = "bold", color = "#2c3e50"),
        panel.grid.major = element_line(color = "#8B8B83", linetype = "dashed", linewidth = 0.4), panel.grid.minor = element_blank(),
        legend.position = "right", plot.background = element_rect(fill = "white", color = NA)) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 5, order = 1), size = guide_legend(override.aes = list(fill = "gray70", color="gray20"), order = 2))

ggsave("Fig9_BubblePlot_Dynamics.png", p_bubble, width = 11, height = 7, dpi = 600)


# 3. ANNUAL IMPACT (DYNAMIC Y-AXIS FACETS)
# Calculate Zone Capacity, Spearman's rho, and Sen's Slope for each management zone
df_stats <- df_zone_merged %>% 
  arrange(Zone, Year) %>% # Chronological order is mandatory for Sen's Slope calculation
  group_by(Zone) %>% 
  summarise(
    Total_Area = max(Total_Zone_Ha, na.rm = TRUE),
    rho = suppressWarnings(cor(Year, Burned_Ha, method = "spearman")),
    p_val = suppressWarnings(cor.test(Year, Burned_Ha, method = "spearman")$p.value),
    sen = suppressWarnings(as.numeric(trend::sens.slope(Burned_Ha)$estimates)),
    .groups = "drop"
  ) %>%
  mutate(
    # Format p-value for publication-grade visualization
    p_label = ifelse(p_val < 0.001, "< 0.001", paste0("= ", round(p_val, 3))),
    
    # Generate the final statistical annotation text
    Stats_Label = paste0("Spearman \u03c1: ", round(rho, 2), " (p ", p_label, ")\n",
                         "Sen's Slope: ", round(sen, 2), " ha/yr")
  )

# Define custom palette for the zones
custom_colors <- c("#E63946", "#1D3557", "#2A9D8F", "#F4A261", "#7B2CBF")

p_impact <- ggplot(df_zone_merged, aes(x = Year, y = Burned_Ha, fill = Zone)) +
  geom_col(alpha = 0.85, color = "black", linewidth = 0.4, width = 0.7) +
  
  # Non-parametric LOESS trend line to visualize fire progression over the years
  geom_smooth(method = "loess", color = "black", linetype = "dashed", se = FALSE, linewidth = 0.5, alpha = 0.5) +
  
  # Annotation 1: Total Zone Capacity (Top-Left)
  geom_text(data = df_stats, aes(x = -Inf, y = Inf, label = paste("Zone Capacity:", comma(Total_Area), "ha")), 
            vjust = 1.5, hjust = -0.05, size = 3.5, fontface = "italic", color = "gray30", inherit.aes = FALSE) +
  
  # Annotation 2: Statistical Results - Sen's Slope & Spearman (Top-Right)
  geom_text(data = df_stats, aes(x = Inf, y = Inf, label = Stats_Label), 
            vjust = 1.5, hjust = 1.05, size = 3.5, fontface = "bold", color = "black", inherit.aes = FALSE) +
  
  facet_wrap(~Zone, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = custom_colors) + 
  scale_x_continuous(breaks = seq(2017, 2025, by = 2)) +
  
  # Increase Y-axis expansion (25%) to prevent tall bars from overlapping the text annotations
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.25))) +
  
  labs(title = "Annual Fire Severity Dynamics", 
       subtitle = "Burned area scaling dynamically per zone with Non-Parametric Trends", 
       x = "Observation Year", y = "Burned Area (Hectares)") +
  
  theme(strip.background = element_rect(fill = "gray95", color = "gray20"), 
        strip.text = element_text(face = "bold"), 
        legend.position = "none", 
        panel.grid.minor = element_blank())

# Export the plot at high resolution (600 DPI) for Q1 Journal submission
ggsave("Fig10_Annual_Impact_Dynamics_Stats.png", p_impact, width = 11, height = 8.5, dpi = 600)


# 4. SENSITIVITY COMPARISON 
# Create the data frame with the categorical summary
df_comp <- data.frame(
  Index = c(rep("NBR (Burn Severity & Structure)", 3), rep("NDVI (Canopy Greenness)", 3)),
  Category = factor(rep(c("Degradation", "Stable", "Recovery"), 2), 
                    levels = c("Degradation", "Stable", "Recovery")),
  Percentage = c(9.3, 36.5, 54.2, 6.2, 90.7, 3.1)
)

# Define a publication-ready color palette 
# (Burnt Orange for NBR/Fire, Forest Green for NDVI/Greenness)
index_colors <- c("NBR (Burn Severity & Structure)" = "#EE3B3B", 
                  "NDVI (Canopy Greenness)" = "#8EE5EE")

# Generate the plot
p_sens <- ggplot(df_comp, aes(x = Category, y = Percentage, fill = Index)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, 
           color = "black", linewidth = 0.6, alpha = 0.9) +
  
  # Add percentage labels directly above the bars
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_dodge(width = 0.8), 
            vjust = -0.8, size = 4.5, fontface = "bold", color = "black") +
  
  scale_fill_manual(values = index_colors) +
  # Extend the Y-axis slightly so the highest label (90.7%) doesn't get cut off
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
  
  labs(title = "Spectral Index Sensitivity Comparison", 
       subtitle = "NBR captures long-term structural changes, while NDVI saturates quickly due to opportunistic greenness.",
       x = "Ecological Status Classification", 
       y = "Proportion of Total Area (%)", 
       fill = "Spectral Index") +
  
  # Apply a clean, modern theme suitable for scientific publications
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 12)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 12)),
    axis.text.x = element_text(size = 13, color = "black"),
    panel.grid.major.x = element_blank(), # Remove vertical grid lines for cleaner look
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5)
  )

# Export at high resolution (600 DPI)
ggsave("Fig11_Sensitivity_Comparison.png", p_sens, width = 10, height = 6.5, dpi = 600)


### 
# ==============================================================================
# Script 04a: ADDITIONAL FIGURE SHOWING THE DIVERGENCE BETWEEN NDVI AND NBR TRENDS
# 1. Load required libraries
# install.packages(c("ggplot2", "dplyr", "tidyr")) # Uncomment if needed
library(ggplot2)
library(dplyr)
library(tidyr)

print("Libraries loaded successfully.")

# 2. Load the dataset
# Ensure the CSV file is in the same working directory
file_path <- "3_Zonal_Annual_Stats.csv"
df <- read.csv(file_path)

print("Data loaded. Transforming format for visualization...")

# 3. Data Wrangling: Convert from wide to long format for ggplot2
df_long <- df %>%
  select(Year, Zone, NBR_Mean, NDVI_Mean) %>%
  pivot_longer(cols = c(NBR_Mean, NDVI_Mean), 
               names_to = "Index", 
               values_to = "Mean_Value") %>%
  mutate(Index = recode(Index, 
                        "NBR_Mean" = "NBR (Structural Integrity)", 
                        "NDVI_Mean" = "NDVI (Canopy Greenness)"))

# 4. Generate the Comparative Plot
print("Generating plot...")

plot_indices <- ggplot(df_long, aes(x = Year, y = Mean_Value, color = Index, shape = Index, group = Index)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~ Zone, scales = "free_y") + # Create separate panels for each management zone
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("NBR (Structural Integrity)" = "#d95f02",  # Dark Orange
                                "NDVI (Canopy Greenness)" = "#1b9e77")) + # Strong Green
  labs(title = "Post-Fire Recovery Dynamics: NDVI vs. NBR (2017-2025)",
       subtitle = "Highlighting the 'greenness trap' where rapid NDVI recovery masks persistent NBR suppression.",
       x = "Year",
       y = "Mean Spectral Index Value",
       color = "Spectral Index",
       shape = "Spectral Index") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 5. Display and Save the Plot
print(plot_indices)

# Save as a high-resolution
output_filename <- "Fig5_Divergent_NDVI_NBR_Trajectories.png"
ggsave(output_filename, plot = plot_indices, width = 12, height = 8, dpi = 600)

print(paste("Process complete. Plot saved as:", output_filename))



# ==============================================================================
# Script 04b: Quantitative Statistics Extraction (Table Output)
# Description: Automatically finds the worst fire year (lowest NBR) for EACH zone,
# calculates the recovery gap, prints an ordered table, and exports it to CSV.
# ==============================================================================

library(dplyr)

# 1. Load the dataset
df <- read.csv("3_Zonal_Annual_Stats.csv")

# 2. Determine the final year of the study automatically
end_year <- max(df$Year, na.rm = TRUE) 
unique_zones <- unique(df$Zone)

# 3. Create an empty list to store the results
results_list <- list()

for (target_zone in unique_zones) {
  
  # Filter data for the specific zone
  zone_data <- df %>% filter(Zone == target_zone)
  
  if(nrow(zone_data) > 0) {
    
    # Find the fire year (Lowest NBR)
    stats_fire <- zone_data %>% filter(NBR_Mean == min(NBR_Mean, na.rm = TRUE))
    fire_year_auto <- stats_fire$Year[1] 
    
    # Filter data for the final year
    stats_recovery <- zone_data %>% filter(Year == end_year)
    
    if(nrow(stats_recovery) > 0) {
      years_elapsed <- end_year - fire_year_auto
      
      ndvi_post_fire <- round(stats_fire$NDVI_Mean[1], 2)
      nbr_post_fire  <- round(stats_fire$NBR_Mean[1], 2)
      
      ndvi_current   <- round(stats_recovery$NDVI_Mean[1], 2)
      nbr_current    <- round(stats_recovery$NBR_Mean[1], 2)
      
      # If there are recovery years, calculate metrics; otherwise, NA
      if(years_elapsed > 0) {
        ndvi_percent_increase <- round(((ndvi_current - ndvi_post_fire) / abs(ndvi_post_fire)) * 100, 1)
        nbr_diff <- round(nbr_current - nbr_post_fire, 2)
      } else {
        ndvi_percent_increase <- NA
        nbr_diff <- NA
      }
      
      # Save data into a row (temporary Data Frame)
      res_row <- data.frame(
        Zone = target_zone,
        Fire_Year = fire_year_auto,
        End_Year = end_year,
        Years_Elapsed = years_elapsed,
        NDVI_PostFire = ndvi_post_fire,
        NDVI_Current = ndvi_current,
        NDVI_Increase_Pct = ndvi_percent_increase,
        NBR_PostFire = nbr_post_fire,
        NBR_Current = nbr_current,
        NBR_Difference = nbr_diff
      )
      
      # Add to the general list
      results_list[[target_zone]] <- res_row
    }
  }
}

# 4. Combine all results into a single main dataframe
results_df <- bind_rows(results_list)

# (Optional) Sort the table: e.g., by NBR Difference
results_df <- results_df %>% arrange(NBR_Difference)

# 5. Print to Console
cat("\n========================================================\n")
cat("          ORDERED RESULTS (POST-FIRE RECOVERY)          \n")
cat("========================================================\n\n")
print(results_df)
cat("\n========================================================\n")

# 6. Export results to CSV
write.csv(results_df, "4_Recovery_Statistics_Table.csv", row.names = FALSE)
cat("\nDone! Table successfully exported as '4_Recovery_Statistics_Table.csv' in your working directory.\n")




