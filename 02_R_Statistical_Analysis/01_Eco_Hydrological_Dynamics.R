



# ==============================================================================
# SCRIPT 01: ECO-HYDROLOGICAL DYNAMICS & CLIMATE LAG ANALYSIS
# Project: Andean Fire Ecosystem Dynamics (2017-2025)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(ggpubr)
library(viridis)
library(hexbin)

# Tema base Q1
theme_set(theme_bw(base_size = 12, base_family = "sans") +
            theme(plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
                  plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
                  axis.title = element_text(face = "bold"),
                  plot.background = element_rect(fill = "white", color = NA)))

# 1. LOAD & PREPARE DATA 
df_ts <- read_csv("1_Global_Monthly_Stats.csv") %>% mutate(Date = as.Date(Date))
df_rain <- read_csv("4_Precipitation_Monthly.csv") %>% mutate(Date = as.Date(Date))

df_clim <- left_join(df_ts, df_rain, by = "Date") %>%
  mutate(Rain_Lag1 = lag(Precipitation_mm, 1),
         Month = month(Date),
         Season = ifelse(Month >= 5 & Month <= 11, "Dry Season (May-Nov)", "Wet Season (Dec-Apr)")) %>%
  filter(!is.na(NBR) & !is.na(NDVI))

# 2. CLIMOGRAPH
coeff <- max(df_clim$Precipitation_mm, na.rm = TRUE) / 0.45
df_clim_seg <- df_clim %>% arrange(Date) %>% 
  mutate(next_Date = lead(Date), next_NBR = lead(NBR), avg_NBR = (NBR + lead(NBR)) / 2) %>% filter(!is.na(next_NBR))

p_climograph <- ggplot() +
  geom_col(data = df_clim, aes(x = Date, y = Precipitation_mm / coeff), fill = "#3498db", alpha = 0.3, width = 25) +
  geom_smooth(data = df_clim, aes(x = Date, y = NBR), method = "loess", span = 0.2, color = "black", linetype = "dashed", linewidth = 0.7, se = FALSE) +
  geom_segment(data = df_clim_seg, aes(x = Date, xend = next_Date, y = NBR, yend = next_NBR, color = avg_NBR), linewidth = 1.2) +
  scale_color_gradientn(colors = c("red", "orange", "darkgreen")) +
  scale_y_continuous(name = "NBR Index", sec.axis = sec_axis(~.*coeff, name = "Precipitation (mm)")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Temporal Eco-Hydrological Dynamics", subtitle = "Precipitation (Bars) vs. Vegetation Health (Gradient Line)") +
  theme(legend.position = "none", axis.title.y.left = element_text(color = "#c0392b"), axis.title.y.right = element_text(color = "#3498db"))

ggsave("Fig1_Climograph.png", p_climograph, width = 10, height = 6, dpi = 600)


# --- Trend Analysis ---
# ==============================================================================
# STATISTICS FOR FIGURE 2c: LONG-TERM TREND ANALYSIS (MANN-KENDALL)
# ==============================================================================
# Install the 'trend' package if you haven't already: install.packages("trend")
library(trend)

# 1. Load the global monthly data (ensure the file path is correct)
# Note: Adjust the path to "1_Global_Monthly_Stats.csv" as needed.
df_global <- read.csv("1_Global_Monthly_Stats.csv")

# 2. Mann-Kendall test for NBR long-term trend
mk_result <- mk.test(df_global$NBR)

# 3. Calculate Sen's Slope to quantify the magnitude of the trend
sen_slope <- sens.slope(df_global$NBR)

# 4. Print results to the console (Extract p-value and estimates for the manuscript)
cat("--- MANN-KENDALL TEST RESULTS FOR NBR ---\n")
print(mk_result)

cat("\n--- SEN'S SLOPE RESULTS ---\n")
print(sen_slope)
# ==============================================================================



# 3. LOLLIPOP CHART (CORRECTED TO SPEARMAN WITH P-VALUES)
lags <- 0:6
cor_data <- data.frame()

# Loop to calculate correlation and extract the p-value
for(l in lags) {
  # Filter NAs to ensure cor.test works correctly
  temp <- df_clim %>% 
    mutate(Rain_Lag = lag(Precipitation_mm, l)) %>% 
    filter(!is.na(Rain_Lag) & !is.na(NBR) & !is.na(NDVI))
  
  # Run Spearman correlation tests
  test_nbr <- cor.test(temp$Rain_Lag, temp$NBR, method = "spearman", exact = FALSE)
  test_ndvi <- cor.test(temp$Rain_Lag, temp$NDVI, method = "spearman", exact = FALSE)
  
  # Save the estimates and p-values
  cor_data <- rbind(cor_data, 
                    data.frame(Lag = l, Index = "NBR", 
                               Correlation = test_nbr$estimate, 
                               P_value = test_nbr$p.value),
                    data.frame(Lag = l, Index = "NDVI", 
                               Correlation = test_ndvi$estimate, 
                               P_value = test_ndvi$p.value))
}

# Create a column to format the p-value labels
cor_data <- cor_data %>%
  mutate(
    p_label = case_when(
      P_value < 0.001 ~ "p < 0.001",
      P_value < 0.01  ~ "p < 0.01",
      P_value < 0.05  ~ "p < 0.05",
      TRUE            ~ paste0("p = ", round(P_value, 2))
    )
  )

# Create the improved lollipop chart
p_lollipop <- ggplot(cor_data, aes(x = factor(Lag), y = Correlation, color = Index)) +
  # Segments and points
  geom_segment(aes(x = factor(Lag), xend = factor(Lag), y = 0, yend = Correlation), linewidth = 1.2, alpha = 0.6) +
  geom_point(size = 5) +
  
  # Dynamic p-value labels (adjust up or down based on correlation sign)
  geom_text(aes(label = p_label, y = Correlation + sign(Correlation) * 0.08), 
            size = 3.5, color = "black", fontface = "italic") +
  
  # Baseline at zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  facet_wrap(~Index, ncol = 2) +
  scale_color_manual(values = c("NBR" = "#e74c3c", "NDVI" = "#2ecc71")) +
  
  # Expand Y-axis slightly so text labels aren't cut off at the edges
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  
  labs(title = "Ecological Memory: Lag Correlation Analysis",
       subtitle = "Spearman rank correlation (\u03c1) and significance at lags 0-6 months",
       x = "Lag (Months)", y = "Spearman Correlation (\u03c1)") +
  
  theme(legend.position = "none", 
        strip.background = element_rect(fill = "#f0f0f0", color = NA), 
        strip.text = element_text(face="bold", size=12),
        panel.grid.minor = element_blank()) 

ggsave("Fig2_Lollipop_Lags.png", p_lollipop, width = 10, height = 5, dpi = 600)



# 4. SEASONAL BOXPLOT & HEXBIN DENSITY
p_seasonal <- ggplot(df_clim, aes(x = Season, y = NBR, fill = Season)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "#2c3e50") +
  scale_fill_manual(values = c("Dry Season (May-Nov)" = "#e67e22", "Wet Season (Dec-Apr)" = "#27ae60")) +
  stat_compare_means(method = "wilcox.test", label.y = max(df_clim$NBR, na.rm=T) * 1.05) + 
  labs(title = "Seasonal Variability of Ecosystem Integrity", y = "NBR Index", x = NULL) +
  theme(legend.position = "none")

p_hexbin <- ggplot(df_clim, aes(x = Rain_Lag1, y = NBR)) +
  geom_hex(bins = 20, color = "white") +
  scale_fill_viridis(option = "magma", name = "Months Count", direction = -1) +
  geom_smooth(method = "loess", color = "cyan", linetype = "dashed", linewidth = 1, fill="cyan", alpha=0.1) +
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x.npc = "right", label.y.npc = "bottom", color = "black", size = 4.5,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")), hjust = 1, vjust = -1) + 
  labs(title = "Eco-Hydrological Response Density", subtitle = "Concentration of NBR response to 1-month lagged precipitation",
       x = "Monthly Precipitation (mm) - Lag 1", y = "NBR Index")

ggsave("Fig3_Seasonal_Boxplot.png", p_seasonal, width = 8, height = 6, dpi = 600)
ggsave("Fig4_Hexbin_Density.png", p_hexbin, width = 8, height = 6, dpi = 600)



