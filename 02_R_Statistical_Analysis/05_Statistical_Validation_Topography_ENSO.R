
# ==============================================================================
# SCRIPT 05: STATISTICAL VALIDATION (TOPOGRAPHY & ENSO)
# Project: Andean Fire Ecosystem Dynamics (2017-2025)
# ==============================================================================

library(tidyverse)
library(ggpubr)

# Set publication-grade theme
theme_set(theme_bw(base_size = 12, base_family = "sans") +
            theme(plot.title = element_text(face = "bold", size = 14, color = "#2c3e50", hjust = 0.5),
                  plot.subtitle = element_text(size = 11, color = "#7f8c8d", hjust = 0.5),
                  axis.title = element_text(face = "bold"),
                  plot.background = element_rect(fill = "white", color = NA)))

# ==============================================================================
# 1. TOPOGRAPHY KOLMOGOROV-SMIRNOV TEST
# ==============================================================================
df_topo <- read_csv("5_Topographic_Samples.csv") %>% 
  rename(Burned = Burned_Class)

burned <- df_topo %>% filter(Burned == "Burned")
unburned <- df_topo %>% filter(Burned == "Unburned")

print("📊 Running Kolmogorov-Smirnov Tests...")
ks_elev <- ks.test(burned$Elevation_m, unburned$Elevation_m)
ks_slope <- ks.test(burned$Slope_deg, unburned$Slope_deg)
ks_aspect <- ks.test(burned$Aspect_deg, unburned$Aspect_deg)

cat("\nStatistical Results (K-S Test - Two Sample):\n",
    "Elevation: D =", round(ks_elev$statistic, 4), "| p-value =", ks_elev$p.value, "\n",
    "Slope:     D =", round(ks_slope$statistic, 4), "| p-value =", ks_slope$p.value, "\n",
    "Aspect:    D =", round(ks_aspect$statistic, 4), "| p-value =", ks_aspect$p.value, "\n\n")

# ==============================================================================
# 2. ENSO CORRELATION (MACRO-CLIMATIC DRIVER)
# ==============================================================================
# Sum total burned area across all zones per year
df_burn_total <- read_csv("3_Zonal_Annual_Stats.csv") %>% 
  group_by(Year) %>% 
  summarise(Burned_Ha = sum(Burned_Ha, na.rm = TRUE), .groups = "drop")

# Historical ONI Data (2017-2025)
df_oni <- tibble(
  Year = c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
  ONI = c(-0.5, -0.2, 0.3, -0.5, -1.0, -1.0, 1.0, 0.5, 0.0)
)

df_enso <- left_join(df_burn_total, df_oni, by = "Year")

# Plot with non-parametric trend and Spearman annotation
p_enso_corr <- ggplot(df_enso, aes(x = ONI, y = Burned_Ha)) +
  geom_point(size = 4, color = "#d35400", alpha = 0.8) +
  geom_smooth(method = "loess", color = "#2c3e50", se = FALSE, linetype = "dashed", linewidth = 1) +
  
  # Professional statistical annotation inside the plot
  stat_cor(method = "spearman", cor.coef.name = "rho", 
           label.x.npc = "left", label.y.npc = "top", size = 5, color = "black") +
  
  labs(title = "Macro-Climatic Drivers: ENSO Influence",
       subtitle = "Relationship between Oceanic Niño Index (ONI) and Total Burned Area",
       x = "Oceanic Niño Index (ONI)", y = "Total Burned Area (Hectares)")

ggsave("Fig_ENSO_Correlation.png", p_enso_corr, width = 7, height = 5, dpi = 600)
print(p_enso_corr)


