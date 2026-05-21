# STAT312 Complete R Analysis Script

# ============================================================
# STAT312 PROJECT ANALYSIS
# Rainfall and River Flow Relationship Analysis
# ============================================================

# ----------------------------
# 1. LOAD LIBRARIES
# ----------------------------

library(tidyverse)
library(lubridate)
library(zoo)
library(gridExtra)

# ----------------------------
# 2. IMPORT DATA
# ----------------------------

library(readxl)

raw_data <- read_excel("STAT312_ProjectData.xlsx")

# ----------------------------
# 3. CHECK DATA STRUCTURE
# ----------------------------

str(raw_data)
head(raw_data)
colnames(raw_data)

data <- raw_data

# Verify structure
str(data)

# ----------------------------
# 4. CHECK MISSING VALUES
# ----------------------------

colSums(is.na(data))

# Actual output:
# Date = 0
# Flow_Rate = 0
# Precipitation = 0

# Therefore:
# - no missing values exist,
# - no rows need removing.

# ----------------------------
# 5. CHECK DUPLICATES
# ----------------------------

sum(duplicated(data$Date))

# Actual output:
# 0 duplicated timestamps

# ----------------------------
# 6. CHECK TIME INTERVALS
# ----------------------------

summary(diff(data$Date))

# ============================================================
# CREATE LAG VARIABLES
# ============================================================

# ----------------------------
# 7. CREATE LAG VARIABLES
# ----------------------------

# Create rainfall lags from 1 to 24 hours

for(i in 1:24){
  data[[paste0("rain_lag", i)]] <- lag(data$Precipitation, i)
}

# ----------------------------
# 8. CREATE ROLLING RAINFALL TOTALS
# ----------------------------

library(zoo)

# 6-hour rolling rainfall

data$rain_6hr <- rollsum(
  data$Precipitation,
  6,
  fill = NA,
  align = "right"
)

# 12-hour rolling rainfall

data$rain_12hr <- rollsum(
  data$Precipitation,
  12,
  fill = NA,
  align = "right"
)

# 24-hour rolling rainfall

data$rain_24hr <- rollsum(
  data$Precipitation,
  24,
  fill = NA,
  align = "right"
)

# Remove rows with NA values introduced by lagging

data <- na.omit(data)

# ============================================================
# EXPLORATORY DATA ANALYSIS
# ============================================================

# ----------------------------
# 9. SUMMARY STATISTICS
# ----------------------------

summary(data$Precipitation)
summary(data$Flow_Rate)

sd(data$Precipitation)
sd(data$Flow_Rate)

# ----------------------------
# 10. TIME SERIES PLOTS
# ----------------------------

# Rainfall plot

p1 <- ggplot(data, aes(Date, Precipitation)) +
  geom_line() +
  labs(
    title = "Rainfall Over Time",
    x = "Date",
    y = "Rainfall"
  ) +
  theme_minimal()

# Flow rate plot

p2 <- ggplot(data, aes(Date, Flow_Rate)) +
  geom_line() +
  labs(
    title = "River Flow Rate Over Time",
    x = "Date",
    y = "Flow Rate"
  ) +
  theme_minimal()

# Display together

grid.arrange(p1, p2, ncol = 1)

# ----------------------------
# 11. STANDARDISED COMPARISON PLOT
# ----------------------------

scaled_data <- data %>%
  mutate(
    rain_scaled = as.numeric(scale(Precipitation)),
    flow_scaled = as.numeric(scale(Flow_Rate))
  )

comparison_plot <- ggplot(scaled_data, aes(Date)) +
  geom_line(aes(y = rain_scaled, colour = "Rainfall")) +
  geom_line(aes(y = flow_scaled, colour = "Flow Rate")) +
  labs(
    title = "Standardised Rainfall and Flow Rate",
    y = "Standardised Value",
    colour = "Variable"
  ) +
  theme_minimal()

print(comparison_plot)

# ----------------------------
# 12. HISTOGRAMS
# ----------------------------

# Rainfall distribution

ggplot(data, aes(log1p(Precipitation))) +
  geom_histogram(bins = 50) +
  labs(
    title = "Log-Scaled Distribution of Rainfall",
    x = "Log(Rainfall + 1)",
    y = "Frequency"
  ) +
  theme_minimal()

# Rainfall observations were highly zero-inflated, with most hourly observations recording no rainfall. Therefore, a separate histogram of non-zero rainfall observations was used to better visualise the distribution of rainfall events

ggplot(filter(data, Precipitation > 0),
       aes(Precipitation)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribution of Non-Zero Rainfall",
    x = "Rainfall (mm)",
    y = "Frequency"
  ) +
  theme_minimal()

# Flow rate distribution

ggplot(data, aes(log1p(Flow_Rate))) +
  geom_histogram(bins = 50) +
  labs(
    title = "Log-Scaled Distribution of Flow Rate",
    x = "Flow Rate",
    y = "Frequency"
  ) +
  theme_minimal()

# ----------------------------
# 13. SCATTERPLOT
# ----------------------------

scatter_plot <- ggplot(data, aes(Precipitation, Flow_Rate)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Rainfall vs Flow Rate",
    x = "Rainfall",
    y = "Flow Rate"
  ) +
  theme_minimal()

print(scatter_plot)

# ============================================================
# CORRELATION ANALYSIS
# ============================================================

# ----------------------------
# 14. SIMPLE CORRELATION
# ----------------------------

correlation <- cor(
  data$Precipitation,
  data$Flow_Rate,
  use = "complete.obs"
)

print(correlation)

# ----------------------------
# 15. LAG CORRELATIONS
# ----------------------------

lag_results <- data.frame(
  Lag = integer(),
  Correlation = numeric()
)

for(i in 0:24){
  
  lag_cor <- cor(
    data$Flow_Rate,
    lag(data$Precipitation, i),
    use = "complete.obs"
  )
  
  lag_results <- rbind(
    lag_results,
    data.frame(
      Lag = i,
      Correlation = lag_cor
    )
  )
}

print(lag_results)

# Plot lag correlations

ggplot(lag_results, aes(Lag, Correlation)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Correlation by Rainfall Lag",
    x = "Lag (Hours)",
    y = "Correlation"
  ) +
  theme_minimal()

# ============================================================
# CROSS-CORRELATION FUNCTION
# ============================================================

# ----------------------------
# 16. CROSS-CORRELATION
# ----------------------------

ccf(
  data$Precipitation,
  data$Flow_Rate,
  lag.max = 72,
  main = "Cross-Correlation Between Rainfall and Flow Rate"
)

# ============================================================
# REGRESSION MODELS
# ============================================================

# ----------------------------
# 17. SIMPLE LINEAR MODEL
# ----------------------------

model_simple <- lm(
  Flow_Rate ~ Precipitation,
  data = data
)

summary(model_simple)

# ----------------------------
# 18. FIND BEST LAG
# ----------------------------

best_lag <- lag_results$Lag[
  which.max(abs(lag_results$Correlation))
]

print(best_lag)

# ----------------------------
# 19. MODEL USING BEST LAG
# ----------------------------

best_lag_name <- paste0("rain_lag", best_lag)

formula_best <- as.formula(
  paste("Flow_Rate ~", best_lag_name)
)

model_best_lag <- lm(
  formula_best,
  data = data
)

summary(model_best_lag)

# ----------------------------
# 20. MULTIPLE LAG MODEL
# ----------------------------

model_multiple <- lm(
  Flow_Rate ~ rain_lag4 + rain_lag8 + rain_lag12 + rain_lag24,
  data = data
)

summary(model_multiple)

# ----------------------------
# 21. ROLLING RAINFALL MODEL
# ----------------------------

model_rolling <- lm(
  Flow_Rate ~ rain_24hr,
  data = data
)

summary(model_rolling)

# ----------------------------
# 21A. ROLLING RAINFALL SCATTERPLOT
# ----------------------------

rolling_scatter <- ggplot(data, aes(rain_24hr, Flow_Rate)) +
  geom_point(alpha = 0.15, size = 0.5) +
  geom_smooth(method = "lm") +
  labs(
    title = "24-Hour Rainfall Total vs River Flow Rate",
    x = "24-Hour Rainfall Total (mm)",
    y = "River Flow Rate"
  ) +
  theme_minimal()

print(rolling_scatter)

ggplot(data, aes(log1p(rain_24hr), log1p(Flow_Rate))) +
  geom_point(alpha = 0.08, size = 0.4) +
  geom_smooth(method = "lm") +
  labs(
    title = "Log-Scaled 24-Hour Rainfall vs River Flow Rate",
    x = "Log 24-Hour Rainfall Total",
    y = "Log River Flow Rate"
  ) +
  theme_minimal()

# ============================================================
# MODEL DIAGNOSTICS
# ============================================================

# ----------------------------
# 22. DIAGNOSTIC PLOTS
# ----------------------------

par(mfrow = c(2,2))
plot(model_best_lag)

# Reset plotting
par(mfrow = c(1,1))

# ============================================================
# EXTREME EVENT ANALYSIS
# ============================================================

# ----------------------------
# 23. IDENTIFY EXTREME RAINFALL EVENTS
# ----------------------------

threshold <- quantile(
  data$Precipitation,
  0.95,
  na.rm = TRUE
)

threshold

# Create event categories

data <- data %>%
  mutate(
    Event_Type = ifelse(
      Precipitation >= threshold,
      "Extreme",
      "Normal"
    )
  )

# ----------------------------
# 24. BOXPLOT OF FLOW RESPONSE
# ----------------------------

boxplot_event <- ggplot(data, aes(Event_Type, Flow_Rate)) +
  geom_boxplot() +
  labs(
    title = "River Flow During Extreme vs Normal Rainfall",
    x = "Rainfall Event Type",
    y = "Flow Rate"
  ) +
  theme_minimal()

print(boxplot_event)

# ----------------------------
# 25. T-TEST
# ----------------------------

extreme_test <- t.test(
  Flow_Rate ~ Event_Type,
  data = data
)

print(extreme_test)

# ============================================================
# EVENT VISUALISATION
# ============================================================

# ----------------------------
# 26. SELECT MAJOR EVENT WINDOW
# ----------------------------

# Find timestamp of largest rainfall event

peak_rain_time <- data$Date[
  which.max(data$Precipitation)
]

print(peak_rain_time)

# Create 3-day window before and after event

major_event <- data %>%
  filter(
    Date >= peak_rain_time - days(3) &
      Date <= peak_rain_time + days(3)
  )

# ----------------------------
# 27. PLOT MAJOR EVENT
# ----------------------------

major_event_plot <- ggplot(major_event, aes(Date)) +
  geom_line(aes(
    y = as.numeric(scale(Precipitation)),
    colour = "Rainfall"
  )) +
  geom_line(aes(
    y = as.numeric(scale(Flow_Rate)),
    colour = "Flow Rate"
  )) +
  labs(
    title = "Major Rainfall Event and River Response",
    y = "Standardised Value",
    colour = "Variable"
  ) +
  theme_minimal()

print(major_event_plot)

# ============================================================
# SAVE RESULTS
# ============================================================

# ----------------------------
# 28. EXPORT LAG RESULTS
# ----------------------------

write.csv(
  lag_results,
  "lag_correlation_results.csv",
  row.names = FALSE
)

# ----------------------------
# 29. SAVE CLEAN DATA
# ----------------------------

write.csv(
  data,
  "cleaned_project_data.csv",
  row.names = FALSE
)

# ----------------------------
# KEY FINDINGS
# ----------------------------

# 1. Simple rainfall-flow relationship

# Correlation result:
# r = 0.168

# Interpretation:
# There is a weak positive same-hour relationship between rainfall
# and river flow.
# This is expected because rivers do not respond immediately.

# ----------------------------
# 2. Lag analysis
# ----------------------------

# Strongest correlation occurred at:
# 16-hour lag

# Correlation at 16-hour lag:
# r = 0.269

# Interpretation:
# Rainfall appears to influence river flow most strongly
# approximately 16 hours later.

# This is likely due to:
# - runoff travel time,
# - infiltration,
# - catchment drainage processes.

# The steadily increasing correlation from lag 0 to lag 16
# strongly supports the existence of a delayed river response.

# ----------------------------
# 3. Simple regression model
# ----------------------------

# Model:
# Flow_Rate ~ Precipitation

# Results:
# R-squared = 0.028

# Interpretation:
# Current-hour rainfall explains only 2.8% of variation in flow.
# This indicates same-hour rainfall is not a strong predictor.

# ----------------------------
# 4. Best lag regression model
# ----------------------------

# Model:
# Flow_Rate ~ rain_lag16

# Results:
# R-squared = 0.073

# Interpretation:
# 16-hour lagged rainfall explains 7.3% of variation in flow.
# This is a major improvement over same-hour rainfall.

# This strongly supports the lag-response hypothesis.

# ----------------------------
# 5. Multiple lag model
# ----------------------------

# Model:
# Flow_Rate ~ rain_lag4 + rain_lag8 + rain_lag12 + rain_lag24

# Results:
# R-squared = 0.179

# Interpretation:
# Combining multiple lagged rainfall periods improves predictive
# performance substantially.

# This suggests river response depends on accumulated rainfall
# over multiple hours rather than a single rainfall event.

# ----------------------------
# 6. Rolling rainfall model
# ----------------------------

# Model:
# Flow_Rate ~ rain_24hr

# Results:
# R-squared = 0.327

# Interpretation:
# 24-hour accumulated rainfall explains 32.7% of variation
# in river flow.

# This indicates cumulative rainfall is much more important
# than individual hourly rainfall measurements.

# Hydrologically this makes strong sense because:
# - soil saturation accumulates over time,
# - groundwater contribution increases,
# - catchment storage effects occur.

# ----------------------------
# 7. Extreme rainfall analysis
# ----------------------------

# Extreme rainfall threshold:
# 95th percentile = 1.90 mm/hour

# T-test results:
# p < 2.2e-16

# Mean flow during extreme rainfall:
# 351.1

# Mean flow during normal rainfall:
# 213.3

# Interpretation:
# River flow is significantly higher during extreme rainfall events.

# The average increase is approximately 138 units.

# This strongly supports the hypothesis that high rainfall events
# generate disproportionately large river responses.

# ============================================================
# IMPORTANT LIMITATIONS TO DISCUSS
# ============================================================

# - Rainfall station may not represent the full catchment
# - River systems respond non-linearly
# - Time-series data are autocorrelated
# - Other hydrological factors are omitted
# - Missing data may influence results
# - Seasonal variation not explicitly modelled

# ============================================================
# END OF ANALYSIS
# ============================================================
