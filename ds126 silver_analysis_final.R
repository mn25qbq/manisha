
# 7 Silver Price Dataset
# R script for visualisation and correlation analysis
#
#
# This script:
#  1. Loads the silver price dataset (DS410)
#  2. Cleans and prepares the data
#  3. Produces:
#       - Time series of closing price
#       - Scatterplot of Volume vs Close with regression line
#       - Histogram of Close
#  4. Runs a Pearson correlation test and simple linear regression



# 0. Load required package


# install.packages("ggplot2")   # run once if not installed
library(ggplot2)

# 1. Import data

silver <- read.csv("silver prices.csv", stringsAsFactors = FALSE)

# Rename columns to simpler names
# Expected original names: Date, Close/Last, Volume, Open, High, Low
colnames(silver) <- c("Date", "Close", "Volume", "Open", "High", "Low")

# Convert Date to Date type
silver$Date <- as.Date(silver$Date, format = "%m/%d/%Y")

# Ensure Volume is numeric (it may be read as character)
silver$Volume <- as.numeric(silver$Volume)

# Remove rows with missing values in key variables
silver_clean <- na.omit(silver[, c("Date", "Close", "Volume")])


# 2. Visualisations


# 2.1 Time series of daily closing price (background plot)
p_timeseries <- ggplot(silver_clean, aes(x = Date, y = Close)) +
  geom_line() +
  labs(
    title = "Figure 1. Daily Closing Price of Silver",
    x = "Date",
    y = "Closing price (USD/oz)"
  ) +
  theme_bw()

print(p_timeseries)

# 2.2 Scatter plot of Volume vs Closing Price (main plot for RQ)
p_scatter <- ggplot(silver_clean, aes(x = Volume, y = Close)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Figure 2. Relationship between Trading Volume and Closing Price",
    x = "Trading volume (contracts per day)",
    y = "Closing price (USD/oz)"
  ) +
  theme_bw()

print(p_scatter)

# 2.3 Histogram of Closing Price (supplementary plot)
p_hist <- ggplot(silver_clean, aes(x = Close)) +
  geom_histogram(
    bins = 40,
    colour = "black",
    fill = "lightblue"
  ) +
  labs(
    title = "Figure 3. Distribution of Daily Closing Prices of Silver",
    x = "Closing price (USD/oz)",
    y = "Frequency (number of trading days)"
  ) +
  theme_bw()

print(p_hist)

# Optionally save plots as PNGs for the report
ggsave("Figure1_timeseries_close.png", plot = p_timeseries,
       width = 7, height = 5, dpi = 300)
ggsave("Figure2_scatter_volume_close.png", plot = p_scatter,
       width = 7, height = 5, dpi = 300)
ggsave("Figure3_hist_close.png", plot = p_hist,
       width = 7, height = 5, dpi = 300)


# 3. Correlation analysis


# Pearson correlation coefficient between Volume and Close
cor_coeff <- cor(
  silver_clean$Volume,
  silver_clean$Close,
  use = "complete.obs",
  method = "pearson"
)

# Hypothesis test for correlation
cor_test <- cor.test(
  silver_clean$Volume,
  silver_clean$Close,
  method = "pearson"
)

# Print numerical results
print(cor_coeff)
print(cor_test)


# 4. Simple linear regression


model <- lm(Close ~ Volume, data = silver_clean)
summary(model)

# Printing key coefficients nicely
cat("\n============================================\n")
cat("Correlation between trading volume and closing price\n")
cat("Pearson r:", round(cor_coeff, 3), "\n")
cat("p-value :", signif(cor_test$p.value, 3), "\n\n")
cat("Linear model: Close = a + b * Volume\n")
cat("Intercept (a):", round(coef(model)[1], 4), "\n")
cat("Slope (b)    :", signif(coef(model)[2], 3), "\n")
cat("============================================\n")


