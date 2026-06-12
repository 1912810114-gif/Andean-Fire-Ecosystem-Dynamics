


# ==============================================================================
# SCRIPT 03: HYDROCLIMATIC DRIVERS & TOPOGRAPHICAL CONTROL
# Project: Andean Fire Ecosystem Dynamics (2017-2025)
# ==============================================================================

library(tidyverse)
library(ggpubr)
library(viridis)

# Publication theme
theme_set(theme_bw(base_size = 12, base_family = "sans") + 
            theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14, color = "#2c3e50"),
                  plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#7f8c8d"),
                  strip.background = element_rect(fill = "#f0f0f0", color="black"),
                  strip.text = element_text(face = "bold"),
                  plot.background = element_rect(fill = "white", color = NA)))

# ==============================================================================
# 1. LOAD DATA
# ==============================================================================
df_ts <- read_csv("1_Global_Monthly_Stats.csv")
df_rain <- read_csv("4_Precipitation_Monthly.csv")
df_topo <- read_csv("5_Topographic_Samples.csv") # Unburned control vs Burned

# Prepare Climate Data with Lag
df_clim <- left_join(df_ts, df_rain, by = "Date") %>%
  mutate(Date = as.Date(Date), Rain_Lag1 = lag(Precipitation_mm, 1)) %>% 
  filter(!is.na(Rain_Lag1) & !is.na(NBR) & !is.na(NDVI))

# ==============================================================================
# 2. KDE CONTOUR PLOT (HYDROLOGICAL DRIVERS)
# ==============================================================================
df_long_indices <- df_clim %>% 
  select(Rain_Lag1, NDVI, NBR) %>% 
  pivot_longer(cols = c("NDVI", "NBR"), names_to = "Index_Type", values_to = "Index_Value")

p_density <- ggplot(df_long_indices, aes(x = Rain_Lag1, y = Index_Value)) +
  geom_density_2d_filled(contour_var = "ndensity", alpha = 0.9) +
  geom_point(alpha = 0.4, size = 1.2, color = "white") + 
  
  # STATISTICAL ALIGNMENT: LOESS for non-parametric visual trend matching Spearman
  geom_smooth(method = "loess", color = "cyan", linetype = "dashed", linewidth = 0.8, fill="cyan", alpha=0.2) +
  
  # Spearman correlation label
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x.npc = "left", label.y.npc = 0.95, 
           size = 4.5, color = "black", geom = "label", fill = alpha("white", 0.8), label.size = 0) + 
  
  scale_fill_viridis_d(option = "magma", name = "Relative Density") +
  facet_wrap(~Index_Type, scales = "free_y", ncol = 2) +
  labs(title = "Hydrological Drivers of Vegetation Indices", 
       subtitle = "Density relationship between 1-month lagged precipitation and index response",
       x = "Monthly Precipitation (mm) - Lag 1 Month", y = "Index Value") +
  theme(legend.position = "right", 
        panel.background = element_rect(fill = "#000004", color = NA), 
        panel.grid = element_blank())

ggsave("Fig7_Correlation_Density.png", p_density, width = 11, height = 6, dpi = 600)


# ==============================================================================
# 3. TOPOGRAPHICAL DRIVERS (BURNED VS UNBURNED)
# ==============================================================================
# Format Topographic Data (Responding to Reviewer request for unburned comparison)
df_topo_long <- df_topo %>% 
  select(Burned_Class, Elevation_m, Slope_deg, Aspect_deg) %>%
  rename(Elevation = Elevation_m, Slope = Slope_deg, Aspect = Aspect_deg) %>%
  pivot_longer(cols = c("Elevation", "Slope", "Aspect"), names_to = "Variable", values_to = "Value") %>%
  mutate(Condition = ifelse(Burned_Class == "Burned", "Burned Areas", "Unburned (Control)"))

p_topography <- ggplot(df_topo_long, aes(x = Value, fill = Condition, color = Condition)) +
  geom_density(alpha = 0.5, linewidth = 0.8) + 
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("Burned Areas" = "#c0392b", "Unburned (Control)" = "#7f8c8d")) +
  scale_color_manual(values = c("Burned Areas" = "#922b21", "Unburned (Control)" = "#34495e")) +
  labs(title = "Topographical Niche of Fire Occurrence", 
       subtitle = "Kernel density comparison between burned footprints and unburned control areas",
       x = "Metric Value (m.a.s.l. / Degrees)", y = "Relative Density") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("Fig8_Topography_Drivers.png", p_topography, width = 12, height = 5, dpi = 600)

