# Load necessary libraries
library(tidyverse)    # For data manipulation and ggplot2
library(RColorBrewer) # For colour palettes
library(lubridate)    # For date handling
library(scales)       # For better axis formatting

# Read the RDS file
steam_data <- readRDS("steam_games_data_updated.rds")

# -----------------------------
# 1. Number of games published categorised by years
# -----------------------------

# Note: Extract the year from the release date to categorise games by publication year
steam_data$release_year <- year(as.Date(steam_data$release_date))

# Count games by year
games_by_year <- steam_data %>%
  count(release_year) %>%
  arrange(release_year)

# Display the table of games by year
print(games_by_year)

# Visualise games by year
ggplot(games_by_year, aes(x = release_year, y = n)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, "Blues")[6]) +
  labs(
    title = "Number of Games Published on Steam by Year",
    x = "Year",
    y = "Number of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(games_by_year$release_year), max(games_by_year$release_year), by = 1))


# -----------------------------
# 2. Distribution of games by price range
# -----------------------------

# For numerical calculations only (finding max price)
steam_data <- steam_data %>%
  mutate(
    price_usd_numeric = as.numeric(ifelse(price_usd %in% c("Free", "Unknown"), NA, price_usd)),
    price_cny_numeric = as.numeric(ifelse(price_cny %in% c("Free", "Unknown"), NA, price_cny))
  )

# Find maximum prices to understand the range
max_usd <- max(steam_data$price_usd_numeric, na.rm = TRUE)
max_cny <- max(steam_data$price_cny_numeric, na.rm = TRUE)

# Print maximum prices
cat("Maximum price in USD:", max_usd, "\n")
cat("Maximum price in CNY:", max_cny, "\n")

# Create price categories for USD with combined categories
steam_data <- steam_data %>%
  mutate(
    price_usd_category = case_when(
      is_free == TRUE ~ "Free",
      price_usd == "Unknown" ~ "Unknown",
      as.numeric(price_usd) < 10 ~ "< $10",
      as.numeric(price_usd) < 20 ~ "$10 - $20",
      as.numeric(price_usd) < 30 ~ "$20 - $30",
      as.numeric(price_usd) < 40 ~ "$30 - $40",
      as.numeric(price_usd) < 50 ~ "$40 - $50",
      as.numeric(price_usd) < 60 ~ "$50 - $60",
      TRUE ~ "$60+"  # Combined categories for $60+
    )
  )

# Create price categories for CNY with combined categories
steam_data <- steam_data %>%
  mutate(
    price_cny_category = case_when(
      is_free == TRUE ~ "Free",
      price_cny == "Unknown" ~ "Unknown",
      as.numeric(price_cny) < 50 ~ "< ¥50",
      as.numeric(price_cny) < 100 ~ "¥50 - ¥100",
      as.numeric(price_cny) < 150 ~ "¥100 - ¥150",
      as.numeric(price_cny) < 200 ~ "¥150 - ¥200",
      as.numeric(price_cny) < 250 ~ "¥200 - ¥250",
      as.numeric(price_cny) < 300 ~ "¥250 - ¥300",
      as.numeric(price_cny) < 350 ~ "¥300 - ¥350",
      as.numeric(price_cny) < 400 ~ "¥350 - ¥400",
      TRUE ~ "¥400+"  # Combined categories for ¥400+
    )
  )

# Create ordered factors for the price categories with new order
usd_levels <- c("Unknown", "Free", "< $10", "$10 - $20", "$20 - $30", "$30 - $40", "$40 - $50", 
                "$50 - $60", "$60+")
cny_levels <- c("Unknown", "Free", "< ¥50", "¥50 - ¥100", "¥100 - ¥150", "¥150 - ¥200", "¥200 - ¥250", 
                "¥250 - ¥300", "¥300 - ¥350", "¥350 - ¥400", "¥400+")

# Count games by USD price category
usd_price_distribution <- steam_data %>%
  count(price_usd_category) %>%
  mutate(
    percentage = n / sum(n) * 100,  # Calculate percentage
    price_usd_category = factor(price_usd_category, levels = usd_levels)  # Order categories
  ) %>%
  arrange(price_usd_category)  # Sort by the ordered factor

# Count games by CNY price category
cny_price_distribution <- steam_data %>%
  count(price_cny_category) %>%
  mutate(
    percentage = n / sum(n) * 100,  # Calculate percentage
    price_cny_category = factor(price_cny_category, levels = cny_levels)  # Order categories
  ) %>%
  arrange(price_cny_category)  # Sort by the ordered factor

# Add total games row to USD price distribution for the table output
usd_price_distribution_with_total <- rbind(
  usd_price_distribution,
  data.frame(
    price_usd_category = "Total games",
    n = sum(usd_price_distribution$n),
    percentage = 100
  )
)

# Add total games row to CNY price distribution for the table output
cny_price_distribution_with_total <- rbind(
  cny_price_distribution,
  data.frame(
    price_cny_category = "Total games",
    n = sum(cny_price_distribution$n),
    percentage = 100
  )
)

# Display the tables with totals
print(usd_price_distribution_with_total)
print(cny_price_distribution_with_total)

# Visualize USD price distribution (now including Unknown)
ggplot(usd_price_distribution, 
       aes(x = price_usd_category, y = n)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, "Blues")[5]) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(
    title = "Distribution of Steam Games by USD Price Range",
    x = "Price Range (USD)",
    y = "Number of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Visualize CNY price distribution (now including Unknown)
ggplot(cny_price_distribution, 
       aes(x = price_cny_category, y = n)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, "Reds")[5]) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(
    title = "Distribution of Steam Games by CNY Price Range",
    x = "Price Range (CNY)",
    y = "Number of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# -----------------------------
# 3. Proportion of free-to-play vs. paid games over time
# -----------------------------

# Calculate corrected game type counts:
# - Free: is_free == TRUE
# - Paid: not free AND has a price in either USD or CNY
# - Unknown: not free AND unknown price in both USD and CNY
free_paid_summary <- steam_data %>%
  summarise(
    Free = sum(is_free == TRUE, na.rm = TRUE),
    Paid = sum(is_free == FALSE & (price_usd != "Unknown" | price_cny != "Unknown"), na.rm = TRUE),
    Unknown = sum(is_free == FALSE & price_usd == "Unknown" & price_cny == "Unknown", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Count") %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = paste0(round(Percentage, 1), "%")  # Simplified label for the chart
  )

# Display the summary table
print(free_paid_summary)

# Bar chart for visulisation
ggplot(free_paid_summary, aes(x = reorder(Category, -Count), y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count, " (", Label, ")")), 
            vjust = -0.5, color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Free vs. Paid Games on Steam",
    x = "Category",
    y = "Number of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Now for the time series analysis with corrected categories
free_paid_by_year <- steam_data %>%
  filter(!is.na(release_year)) %>%
  group_by(release_year) %>%
  summarise(
    Total = n(),
    Free = sum(is_free == TRUE, na.rm = TRUE),
    Paid = sum(is_free == FALSE & (price_usd != "Unknown" | price_cny != "Unknown"), na.rm = TRUE),
    Unknown = sum(is_free == FALSE & price_usd == "Unknown" & price_cny == "Unknown", na.rm = TRUE)
  ) %>%
  mutate(
    Free_Percentage = Free / Total * 100,
    Paid_Percentage = Paid / Total * 100,
    Unknown_Percentage = Unknown / Total * 100
  )

# Display the free vs. paid games by year table
print(free_paid_by_year)

# Convert to long format for ggplot
# Include all three categories now
free_paid_long <- free_paid_by_year %>%
  select(release_year, Free_Percentage, Paid_Percentage, Unknown_Percentage) %>%
  pivot_longer(
    cols = c(Free_Percentage, Paid_Percentage, Unknown_Percentage),
    names_to = "Type",
    values_to = "Percentage"
  ) %>%
  mutate(Type = str_remove(Type, "_Percentage"))

# Visualise proportion of free vs paid games over time (stacked bar chart)
ggplot(free_paid_long, aes(x = release_year, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Proportion of Free, Paid and Unknown-Price Games on Steam Over Time",
    x = "Year",
    y = "Percentage of Games",
    fill = "Game Type",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(free_paid_by_year$release_year), max(free_paid_by_year$release_year), by = 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# Visualise proportion of free vs paid games over time (line chart)
ggplot(free_paid_by_year, aes(x = release_year)) +
  geom_line(aes(y = Free_Percentage, colour = "Free"), size = 1) +
  geom_line(aes(y = Paid_Percentage, colour = "Paid"), size = 1) +
  geom_line(aes(y = Unknown_Percentage, colour = "Unknown"), size = 1, linetype = "dashed") +
  geom_point(aes(y = Free_Percentage, colour = "Free"), size = 3) +
  geom_point(aes(y = Paid_Percentage, colour = "Paid"), size = 3) +
  geom_point(aes(y = Unknown_Percentage, colour = "Unknown"), size = 3) +
  scale_colour_brewer(palette = "Set2", name = "Game Type") +
  labs(
    title = "Trend of Free-to-Play vs. Paid Games on Steam Over Time",
    x = "Year",
    y = "Percentage of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(free_paid_by_year$release_year), max(free_paid_by_year$release_year), by = 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))
