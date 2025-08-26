# Load necessary libraries
library(tidyverse)
library(lubridate)
library(scales)
library(RColorBrewer) 
library(ggplot2)
library(stringr)
library(psych)      
library(tidyr)      
library(purrr)      
library(betareg)
library(sandwich)
library(lmtest)
library(car)
library(gridExtra)
library(sjPlot)
library(sjmisc)
library(sjlabelled)


# Read the dataset
steam_data <- readRDS("D:/Steam data collector/output/steam_data.rds")

# --- Initial Data Processing and Mutations ---
steam_data$release_year <- year(as.Date(steam_data$release_date))
steam_data <- steam_data %>%
  mutate(
    # ... (all your existing mutations) ...
    price_usd_numeric = case_when(
      price_usd == "Free" | is_free == TRUE ~ 0,
      price_usd == "Unknown" ~ NA_real_,
      TRUE ~ as.numeric(price_usd)
    ),
    price_cny_numeric = case_when(
      price_cny == "Free" | is_free == TRUE ~ 0,
      price_cny == "Unknown" ~ NA_real_,
      TRUE ~ as.numeric(price_cny)
    ),
    has_chinese_loc = chinese_simplified_interface_subtitles == TRUE | 
      chinese_simplified_audio == TRUE |
      chinese_traditional_interface_subtitles == TRUE |
      chinese_traditional_audio == TRUE,
    has_sc_loc = chinese_simplified_interface_subtitles == TRUE | 
      chinese_simplified_audio == TRUE,
    
    chinese_support_category = case_when(
      (chinese_simplified_interface_subtitles == TRUE | chinese_simplified_audio == TRUE) &
        (chinese_traditional_interface_subtitles == TRUE | chinese_traditional_audio == TRUE) ~ "Both SC and TC",
      (chinese_simplified_interface_subtitles == TRUE | chinese_simplified_audio == TRUE) ~ "SC Only",
      (chinese_traditional_interface_subtitles == TRUE | chinese_traditional_audio == TRUE) ~ "TC Only",
      TRUE ~ "No Chinese Support"
    ),
    has_indie_genre = ifelse(is.na(game_tags), NA, grepl("\\bIndie\\b", game_tags, ignore.case = TRUE))
  )

# --- Unified Color Scheme (Shades of Orange/Red) ---
primary_light_accent <- "#FEE391" 
primary_medium_accent <- "#FEC44F" 
primary_strong_accent <- "#FE9929" 
primary_dark_accent <- "#EC7014" 
primary_very_dark_accent <- "#CC4C02" 

main_focus_color <- primary_strong_accent      # For primary elements of interest
secondary_focus_color <- primary_medium_accent # For secondary elements or related categories
comparison_color <- primary_light_accent       # For comparison groups (e.g., "Without Chinese Loc", "English")
neutral_fill_color_boxplot <- "#FEF0D9"       # Very light orange for boxplot fills
reference_line_color_plot <- primary_very_dark_accent # Darkest shade for hlines etc.

# For plots with multiple (3-4) ordered categories:
multi_cat_color_1 <- primary_light_accent
multi_cat_color_2 <- primary_medium_accent
multi_cat_color_3 <- primary_strong_accent
multi_cat_color_4 <- primary_dark_accent


# --- Custom Theme  ---
my_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.3), hjust = 0.5),
    plot.subtitle = element_text(size = rel(1.1), hjust = 0.5),
    axis.title = element_text(size = rel(1.0)),
    axis.text = element_text(size = rel(0.9)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = rel(1.0)),
    legend.text = element_text(size = rel(0.9)),
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.major = element_line(color = "lightgray"),
    plot.margin = margin(10, 10, 10, 10, "pt")
  )

# --- Data Summaries  ---
games_by_year_summary <- steam_data %>%
  filter(!is.na(release_year)) %>%
  group_by(release_year) %>%
  summarise(
    Total_Games = n(),
    Games_With_Chinese_Loc = sum(has_chinese_loc == TRUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Games_Without_Chinese_Loc = Total_Games - Games_With_Chinese_Loc
  )

# --- Chart Generation  ---

# 1. Games Published by Year with Chinese Language Support
data_for_p1 <- games_by_year_summary %>%
  select(release_year, Games_With_Chinese_Loc, Games_Without_Chinese_Loc) %>%
  pivot_longer(
    cols = c(Games_With_Chinese_Loc, Games_Without_Chinese_Loc),
    names_to = "Localisation_Status_Segment",
    values_to = "Count"
  ) %>%
  mutate(
    Localisation_Status_Segment = factor(Localisation_Status_Segment,
                                         levels = c("Games_Without_Chinese_Loc", "Games_With_Chinese_Loc"),
                                         labels = c("Without Chinese Localisation", "With Chinese Localisation"))
  ) %>%
  filter(!is.na(release_year))

totals_for_p1_labels <- games_by_year_summary %>% select(release_year, Total_Games)

p1 <- ggplot(data_for_p1, aes(x = release_year, y = Count, fill = Localisation_Status_Segment)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    data = dplyr::filter(data_for_p1, Localisation_Status_Segment == "With Chinese Localisation" & Count > 0),
    aes(label = Count), position = position_stack(vjust = 0.5), size = 3, check_overlap = TRUE
  ) +
  geom_text(
    data = totals_for_p1_labels,
    aes(x = release_year, y = Total_Games, label = Total_Games, fill = NULL),
    vjust = -0.5, size = 3.5
  ) +
  scale_fill_manual(
    name = "Language Support",
    values = c("With Chinese Localisation" = main_focus_color, # Main focus
               "Without Chinese Localisation" = comparison_color) # Comparison
  ) +
  labs(
    title = "Games Published on Steam by Year and Chinese Language Support",
    subtitle = "Total games released, with breakdown of Chinese language support",
    x = "Release Year", y = "Number of Games"
  ) + my_theme +
  scale_x_continuous(
    breaks = seq(min(data_for_p1$release_year, na.rm = TRUE), max(data_for_p1$release_year, na.rm = TRUE), by = 1)
  )
ggsave("chart1_games_and_chinese_localisation.png", p1, width = 12, height = 8, dpi = 300)

# 2. Chinese Localisation Adoption Ratio Trends
chinese_support_by_year_for_ratio <- steam_data %>%
  filter(!is.na(release_year)) %>%
  group_by(release_year) %>%
  summarise(
    Total = n(), SC_Only = sum(chinese_support_category == "SC Only", na.rm = TRUE),
    TC_Only = sum(chinese_support_category == "TC Only", na.rm = TRUE),
    Both = sum(chinese_support_category == "Both SC and TC", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Any_Chinese = SC_Only + TC_Only + Both,
    SC_Only_Ratio = ifelse(Total > 0, SC_Only / Total * 100, 0),
    TC_Only_Ratio = ifelse(Total > 0, TC_Only / Total * 100, 0),
    Both_Ratio = ifelse(Total > 0, Both / Total * 100, 0),
    Any_Chinese_Ratio = ifelse(Total > 0, Any_Chinese / Total * 100, 0)
  )

ratio_long_p2 <- chinese_support_by_year_for_ratio %>%
  select(release_year, SC_Only_Ratio, TC_Only_Ratio, Both_Ratio, Any_Chinese_Ratio) %>%
  pivot_longer(
    cols = c(SC_Only_Ratio, TC_Only_Ratio, Both_Ratio, Any_Chinese_Ratio),
    names_to = "Type", values_to = "Ratio"
  ) %>%
  mutate(Type = factor(Type, 
                       levels = c("Any_Chinese_Ratio", "Both_Ratio", "SC_Only_Ratio", "TC_Only_Ratio"),
                       labels = c("Any Chinese", "Both SC and TC", "SC Only", "TC Only")))

p2 <- ggplot(ratio_long_p2, aes(x = release_year, y = Ratio, color = Type, group = Type)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
  scale_color_manual(values = c("Any Chinese" = multi_cat_color_4, # Darkest for "Any"
                                "Both SC and TC" = multi_cat_color_3,
                                "SC Only" = multi_cat_color_2,
                                "TC Only" = multi_cat_color_1)) + # Lightest
  labs(
    title = "Chinese Localisation Adoption Ratio Trends",
    subtitle = "Percentage of games with different types of Chinese language support",
    x = "Release Year", y = "Percentage of Games", color = "Chinese Language Support"
  ) + my_theme +
  scale_x_continuous(breaks = seq(min(ratio_long_p2$release_year, na.rm = TRUE), 
                                  max(ratio_long_p2$release_year, na.rm = TRUE), by = 1)) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%"))
ggsave("chart2_chinese_localisation_trends.png", p2, width = 12, height = 8, dpi = 300)


# 3. Review Percentage by Localisation Status (Weighted) - UPDATED
review_percentage_by_loc_p8 <- steam_data %>%
  mutate(
    localisation_depth = case_when(
      chinese_simplified_audio == TRUE | chinese_traditional_audio == TRUE ~ "Full Localisation",
      chinese_simplified_interface_subtitles == TRUE | chinese_traditional_interface_subtitles == TRUE ~ "Text Localisation",
      TRUE ~ "No Chinese Localisation"
    ),
    localisation_depth = factor(localisation_depth, 
                                levels = c("Full Localisation", "Text Localisation", "No Chinese Localisation"))
  ) %>%
  group_by(localisation_depth) %>%
  summarise(
    total_all_reviews = sum(total_reviews, na.rm = TRUE),
    total_sc_reviews = sum(schinese_reviews, na.rm = TRUE),
    total_english_reviews = sum(english_reviews, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sc_percentage = ifelse(total_all_reviews > 0, (total_sc_reviews / total_all_reviews) * 100, 0),
    english_percentage = ifelse(total_all_reviews > 0, (total_english_reviews / total_all_reviews) * 100, 0),
    total_other_reviews = total_all_reviews - total_sc_reviews - total_english_reviews,
    other_percentage = ifelse(total_all_reviews > 0, (total_other_reviews / total_all_reviews) * 100, 0)
  )

review_percentage_long_p8 <- review_percentage_by_loc_p8 %>%
  select(localisation_depth, sc_percentage, english_percentage, other_percentage) %>%
  pivot_longer(
    cols = c(sc_percentage, english_percentage, other_percentage),
    names_to = "language", 
    values_to = "percentage"
  ) %>%
  mutate(
    language = case_when(
      language == "sc_percentage" ~ "Simplified Chinese Reviews",
      language == "english_percentage" ~ "English Reviews",
      TRUE ~ "Other Languages"
    ),
    language = factor(language, levels = c("Simplified Chinese Reviews", "English Reviews", "Other Languages"))
  )

p8 <- ggplot(review_percentage_long_p8, aes(x = localisation_depth, y = percentage, fill = language)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(
    values = c(
      "Simplified Chinese Reviews" = main_focus_color,
      "English Reviews" = comparison_color,
      "Other Languages" = primary_medium_accent
    )
  ) +
  labs(
    title = "Review Language Distribution by Chinese Localisation Depth",
    subtitle = "Weighted percentage of reviews (games with more reviews have higher weight)",
    x = "Localisation Status", y = "Percentage of Reviews", fill = "Review Language"
  ) + my_theme +
  scale_y_continuous(labels = function(x) paste0(x, "%"))
ggsave("chart3_review_language_protion.png", p8, width = 12, height = 8, dpi = 300)

# Chart 4: Predicted vs Actual for Beta Regression Model (Model 3 - Chinese Review Ratio)
# Get predicted values from model 3 (betareg model)
model3_predictions <- predict(model3, type = "response")
model3_actual <- regression_data$sc_ratio_adj

# Create data frame for plotting
model3_plot_data <- data.frame(
  Predicted = model3_predictions,
  Actual = model3_actual,
  Residuals = model3_actual - model3_predictions
)

# Remove any rows with missing values
model3_plot_data <- model3_plot_data[complete.cases(model3_plot_data), ]

# Calculate R-squared for the plot subtitle
model3_r_squared <- summary(model3)$pseudo.r.squared

# Calculate axis limits to make the plot more balanced
x_range <- range(model3_plot_data$Predicted, na.rm = TRUE)
y_range <- range(model3_plot_data$Actual, na.rm = TRUE)
overall_range <- c(min(x_range[1], y_range[1]), max(x_range[2], y_range[2]))
axis_padding <- diff(overall_range) * 0.05

p4 <- ggplot(model3_plot_data, aes(x = Predicted, y = Actual)) +
  geom_point(color = main_focus_color, alpha = 0.6, size = 1.5) +
  geom_abline(aes(intercept = 0, slope = 1, linetype = "Perfect Prediction"), 
              color = reference_line_color_plot, linewidth = 1.2) +
  geom_smooth(aes(linetype = "Model Trend"), method = "lm", se = TRUE, 
              color = primary_dark_accent, fill = primary_light_accent, 
              alpha = 0.3, linewidth = 1) +
  scale_linetype_manual(
    name = "Reference Lines",
    values = c("Perfect Prediction" = "dashed", "Model Trend" = "solid"),
    guide = guide_legend(override.aes = list(color = c(reference_line_color_plot, primary_dark_accent)))
  ) +
  labs(
    title = "Beta Regression Model: Predicted vs Actual Chinese Review Ratio",
    subtitle = paste0("Model 3 with Polynomial Time Trends (Pseudo R² = ", 
                      round(model3_r_squared, 3), ")"),
    x = "Predicted Chinese Review Ratio",
    y = "Actual Chinese Review Ratio"
  ) +
  my_theme +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(overall_range[1] - axis_padding, overall_range[2] + axis_padding)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(overall_range[1] - axis_padding, overall_range[2] + axis_padding)
  ) +
  coord_fixed(ratio = 1) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(size = rel(1.2)),
    plot.subtitle = element_text(size = rel(1.0)),
    legend.position = "bottom"
  )

# Add correlation coefficient annotation
correlation_coeff <- cor(model3_plot_data$Predicted, model3_plot_data$Actual, use = "complete.obs")
p4 <- p4 + annotate("text", 
                    x = overall_range[1] + 0.05 * diff(overall_range),
                    y = overall_range[2] - 0.05 * diff(overall_range),
                    label = paste0("Correlation: ", round(correlation_coeff, 3)),
                    color = primary_very_dark_accent, fontface = "bold", size = 4)

ggsave("chart4_betareg_predicted_vs_actual.png", p4, width = 10, height = 8, dpi = 300)

# Chart 5: Predicted vs Actual for Linear Model (Model 4 - Sentiment Gap)
# Get predicted values from model 4 (linear model)
model4_predictions <- predict(model4)
model4_actual <- regression_data$sentiment_gap[!is.na(regression_data$sentiment_gap)]

# Ensure predictions and actuals have the same length
model4_complete_cases <- complete.cases(
  regression_data$sentiment_gap,
  regression_data$full_localisation_combo,
  regression_data$release_year_centered,
  regression_data$release_year_centered_squared,
  regression_data$release_year_centered_cubed,
  regression_data$log_total_reviews
)

model4_predictions <- model4_predictions[model4_complete_cases]
model4_actual <- regression_data$sentiment_gap[model4_complete_cases]

# Create data frame for plotting
model4_plot_data <- data.frame(
  Predicted = model4_predictions,
  Actual = model4_actual,
  Residuals = model4_actual - model4_predictions
)

# Remove any rows with missing values
model4_plot_data <- model4_plot_data[complete.cases(model4_plot_data), ]

# Calculate R-squared for the plot subtitle
model4_r_squared <- summary(model4)$adj.r.squared

# Calculate axis limits to make the plot more balanced
x_range_m4 <- range(model4_plot_data$Predicted, na.rm = TRUE)
y_range_m4 <- range(model4_plot_data$Actual, na.rm = TRUE)
overall_range_m4 <- c(min(x_range_m4[1], y_range_m4[1]), max(x_range_m4[2], y_range_m4[2]))
axis_padding_m4 <- diff(overall_range_m4) * 0.05

p5 <- ggplot(model4_plot_data, aes(x = Predicted, y = Actual)) +
  geom_point(color = main_focus_color, alpha = 0.6, size = 1.5) +
  geom_abline(aes(intercept = 0, slope = 1, linetype = "Perfect Prediction"), 
              color = reference_line_color_plot, linewidth = 1.2) +
  geom_smooth(aes(linetype = "Model Trend"), method = "lm", se = TRUE, 
              color = primary_dark_accent, fill = primary_light_accent, 
              alpha = 0.3, linewidth = 1) +
  scale_linetype_manual(
    name = "Reference Lines",
    values = c("Perfect Prediction" = "dashed", "Model Trend" = "solid"),
    guide = guide_legend(override.aes = list(color = c(reference_line_color_plot, primary_dark_accent)))
  ) +
  labs(
    title = "Linear Regression Model: Predicted vs Actual Sentiment Gap",
    subtitle = paste0("Model 4 with Polynomial Time Trends (Adj. R² = ", 
                      round(model4_r_squared, 3), ")"),
    x = "Predicted Sentiment Gap",
    y = "Actual Sentiment Gap"
  ) +
  my_theme +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    limits = c(overall_range_m4[1] - axis_padding_m4, overall_range_m4[2] + axis_padding_m4)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    limits = c(overall_range_m4[1] - axis_padding_m4, overall_range_m4[2] + axis_padding_m4)
  ) +
  coord_fixed(ratio = 1) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(size = rel(1.2)),
    plot.subtitle = element_text(size = rel(1.0)),
    legend.position = "bottom"
  )

# Add correlation coefficient annotation
correlation_coeff_model4 <- cor(model4_plot_data$Predicted, model4_plot_data$Actual, use = "complete.obs")
p5 <- p5 + annotate("text", 
                    x = overall_range_m4[1] + 0.05 * diff(overall_range_m4),
                    y = overall_range_m4[2] - 0.05 * diff(overall_range_m4),
                    label = paste0("Correlation: ", round(correlation_coeff_model4, 3)),
                    color = primary_very_dark_accent, fontface = "bold", size = 4)

ggsave("chart5_linear_predicted_vs_actual.png", p5, width = 10, height = 8, dpi = 300)

# --- Print Summary Statistics ---
cat("\n=== PREDICTED VS ACTUAL PLOTS SUMMARY ===\n")
cat("Chart 4 - Beta Regression Model 3 (Chinese Review Ratio):\n")
cat("  - Pseudo R-squared:", round(model3_r_squared, 4), "\n")
cat("  - Correlation (Predicted vs Actual):", round(correlation_coeff, 4), "\n")
cat("  - RMSE:", round(sqrt(mean(model3_plot_data$Residuals^2, na.rm = TRUE)), 4), "\n")
cat("  - Number of observations:", nrow(model3_plot_data), "\n\n")

cat("Chart 5 - Linear Model 4 (Sentiment Gap):\n")
cat("  - Adjusted R-squared:", round(model4_r_squared, 4), "\n")
cat("  - Correlation (Predicted vs Actual):", round(correlation_coeff_model4, 4), "\n")
cat("  - RMSE:", round(sqrt(mean(model4_plot_data$Residuals^2, na.rm = TRUE)), 4), "\n")
cat("  - Number of observations:", nrow(model4_plot_data), "\n\n")

# --- Print Summary Statistics ---
cat("\n=== PREDICTED VS ACTUAL PLOTS SUMMARY ===\n")
cat("Chart 4 - Beta Regression Model 3 (Chinese Review Ratio):\n")
cat("  - Pseudo R-squared:", round(model3_r_squared, 4), "\n")
cat("  - Correlation (Predicted vs Actual):", round(correlation_coeff, 4), "\n")
cat("  - RMSE:", round(sqrt(mean(model3_plot_data$Residuals^2, na.rm = TRUE)), 4), "\n")
cat("  - Number of observations:", nrow(model3_plot_data), "\n\n")

cat("Chart 5 - Linear Model 4 (Sentiment Gap):\n")
cat("  - Adjusted R-squared:", round(model4_r_squared, 4), "\n")
cat("  - Correlation (Predicted vs Actual):", round(correlation_coeff_model4, 4), "\n")
cat("  - RMSE:", round(sqrt(mean(model4_plot_data$Residuals^2, na.rm = TRUE)), 4), "\n")
cat("  - Number of observations:", nrow(model4_plot_data), "\n\n")

print("All charts have been generated successfully with the new unified orange/red color scheme!")



# --- Create Sub-dataset for Factor Analysis ---
# Only include games with at least 20 SC reviews AND 20 English reviews
steam_data_fa_subset <- steam_data %>%
  filter(schinese_reviews >= 20 & english_reviews >= 20)

cat("Original dataset size:", nrow(steam_data), "\n")
cat("Factor analysis subset size:", nrow(steam_data_fa_subset), "\n")
cat("Percentage retained:", round(100 * nrow(steam_data_fa_subset) / nrow(steam_data), 2), "%\n\n")

# --- Extract Tags for Factor Analysis ---
# Function to safely extract tags from the nested list structure
extract_tags_safe <- function(tag_list) {
  if (is.null(tag_list) || length(tag_list) == 0) {
    return(character(0))
  }
  # Access the inner character vector
  if (is.list(tag_list) && length(tag_list) > 0 && !is.null(tag_list[[1]])) {
    return(tag_list[[1]])
  }
  return(character(0))
}

# Create a binary tag matrix
tag_matrix_fa <- steam_data_fa_subset %>%
  mutate(row_id = row_number()) %>%
  mutate(extracted_tags = map(game_tags, extract_tags_safe)) %>%
  unnest(extracted_tags) %>%
  mutate(value = 1) %>%
  distinct(row_id, extracted_tags, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = c(row_id, app_id), 
    names_from = extracted_tags, 
    values_from = value,
    values_fill = 0
  ) %>%
  arrange(row_id) %>%
  select(-row_id)

# --- Filter Tags for Factor Analysis ---
# List of platform/technical tags to exclude
platform_tags <- c(
  "Family Sharing", "Steam Achievements", "Steam Trading Cards", "Steam Cloud", 
  "Steam Workshop", "Full controller support", "Tracked Controller Support", 
  "Partial Controller Support", "Controller", "Steam Leaderboards", 
  "Remote Play Together", "Remote Play on TV", "Remote Play on Phone", 
  "Remote Play on Tablet", "Captions available", "In-App Purchases",
  "Includes level editor", "Commentary available", "Cross-Platform Multiplayer",
  "Steam Turn Notifications", "SteamVR Collectibles", "Remote Play",
  "Valve Anti-Cheat enabled", "Stats", "Includes Source SDK", "Custom Volume Controls",
  "Playable without Timed Input", "Adjustable Difficulty", "Camera Comfort",
  "Stereo Sound", "Save Anytime", "Color Alternatives","Mouse Only Option"
)

# Count tag frequencies (excluding app_id column)
tag_freq <- colSums(tag_matrix_fa[, -1])
tags_to_keep <- names(tag_freq[tag_freq >= 100])
cat("Number of tags that appear in at least 100 games:", length(tags_to_keep), "\n")

# Filter tags: keep only frequent tags AND exclude platform tags
relevant_tags <- setdiff(tags_to_keep, platform_tags)
tag_matrix_filtered <- tag_matrix_fa[, c("app_id", relevant_tags)]

cat("Number of tags after filtering:", length(relevant_tags), "\n")
cat("Dimensions of filtered matrix:", dim(tag_matrix_filtered), "\n\n")

# --- Compute Tetrachoric Correlation Matrix ---
set.seed(123)  # For reproducibility
cat("Computing tetrachoric correlation matrix...\n")
tetra_cor <- tetrachoric(tag_matrix_filtered[, -1])$rho

# Check for NA values
na_count <- sum(is.na(tetra_cor))
cat("NA values in correlation matrix:", na_count, "\n")

# If there are NAs, replace with 0 (conservative approach)
if (na_count > 0) {
  tetra_cor[is.na(tetra_cor)] <- 0
  cat("Replaced NA values with 0\n")
}

# --- Sampling Adequacy Tests ---
# Kaiser-Meyer-Olkin (KMO) test
kmo_result <- KMO(tetra_cor)
cat("\nKMO Measure of Sampling Adequacy:", round(kmo_result$MSA, 3), "\n")
# KMO Measure of Sampling Adequacy: 0.797, highly suitable for factor analysis

# Bartlett's test of sphericity
bartlett_result <- cortest.bartlett(tetra_cor, n = nrow(tag_matrix_filtered))
cat("Bartlett's test p-value:", format.pval(bartlett_result$p.value), "\n\n")
# Bartlett's test p-value: < 2.22e-16 reject the null hypothesis

# --- Determine Number of Factors ---
cat("Running parallel analysis to determine number of factors...\n")
parallel_result <- fa.parallel(
  tetra_cor, 
  n.obs = nrow(tag_matrix_filtered),
  fa = "fa",
  cor = "cor",
  n.iter = 20,
  plot = FALSE
)

# Suggested number of factors
n_factors_suggested <- parallel_result$nfact
cat("Parallel analysis suggests", n_factors_suggested, "factors\n")

# Use suggested number or set manually (adjust based on interpretability)
n_factors <- min(n_factors_suggested, 20)  # Cap at 20 for interpretability
cat("Using", n_factors, "factors for analysis\n\n")

# --- Perform Factor Analysis ---
cat("Performing factor analysis with varimax rotation...\n")
fa_result <- fa(
  tetra_cor,
  nfactors = n_factors,
  rotate = "varimax",
  cor = "cor",
  n.obs = nrow(tag_matrix_filtered),
  fm = "minres"  # Minimum residual method
)

# --- Extract and Display Factor Loadings ---
# Function to extract top loading tags for each factor
get_top_loadings <- function(loadings_matrix, factor_num, n_tags = 20, cutoff = 0.25) {
  factor_loadings <- loadings_matrix[, factor_num]
  
  # Find loadings above cutoff
  significant_loadings <- abs(factor_loadings) >= cutoff
  significant_tags <- names(factor_loadings)[significant_loadings]
  significant_values <- factor_loadings[significant_loadings]
  
  # Create data frame and sort by absolute loading
  result <- data.frame(
    Tag = significant_tags,
    Loading = round(significant_values, 3),
    stringsAsFactors = FALSE
  )
  result <- result[order(-abs(result$Loading)), ]
  
  return(head(result, n_tags))
}

# Create factor summaries
factor_summaries <- list()
for (i in 1:n_factors) {
  factor_summaries[[i]] <- get_top_loadings(fa_result$loadings, i)
}

# Display factor summaries
cat("\n=== FACTOR ANALYSIS RESULTS ===\n\n")
for (i in 1:n_factors) {
  cat(sprintf("Factor %d (%.1f%% of variance)\n", i, round(fa_result$Vaccounted[2, i] * 100, 1)))
  print(factor_summaries[[i]], row.names = FALSE)
  cat("\n")
}


# --- Assign Factor Names Based on Loadings ---
# Manually assign the final, interpreted names to each factor.
factor_names <- c(
  "Management_City_Builder",       # Factor 1
  "Narrative_Driven_Visual_Novel", # Factor 2
  "Adult_Hentai",                  # Factor 3
  "Roguelite_Deckbuilder",         # Factor 4
  "Local_Multiplayer",             # Factor 5
  "Action_Roguelike_Bullet_Hell",  # Factor 6
  "Racing_Driving_Sim",            # Factor 7
  "Horror_Games",                  # Factor 8
  "Grand_Strategy_Wargame",        # Factor 9
  "Cozy_Wholesome",                # Factor 10
  "MMORPG",                        # Factor 11
  "Puzzle_Games",                  # Factor 12
  "ARPG_3D",                       # Factor 13
  "Beat_em_up",                    # Factor 14
  "Sci_Fi",                        # Factor 15
  "Metroidvania_2D_Platformer",    # Factor 16
  "Retro_Old_School",              # Factor 17
  "Comedy_Humor",                  # Factor 18
  "Investigation_3rd_Person",      # Factor 19
  "Cyberpunk_RPG"                  # Factor 20
)

cat("\n=== ASSIGNED FACTOR NAMES ===\n")
for (i in 1:n_factors) {
  cat(sprintf("Factor %d: %s\n", i, factor_names[i]))
}

# --- Calculate Factor Scores ---
cat("\n\nCalculating factor scores...\n")
fa_scores <- factor.scores(
  tag_matrix_filtered[, -1],
  fa_result,
  method = "Thurstone"
)

# Create data frame with app_id and factor scores
fa_scores_df <- as.data.frame(fa_scores$scores)
# Use the new meaningful names for the columns
colnames(fa_scores_df) <- factor_names
fa_scores_df$app_id <- tag_matrix_filtered$app_id

# --- Merge Factor Scores Back to Subset Data ---
steam_data_fa_subset <- steam_data_fa_subset %>%
  left_join(fa_scores_df, by = "app_id")

cat("Factor scores successfully calculated and merged\n")
cat("Dimensions of data with factor scores:", dim(steam_data_fa_subset), "\n")

# --- Save Factor Analysis Results ---
# Save the factor analysis object and related data for later use
fa_results_list <- list(
  fa_result = fa_result,
  factor_summaries = factor_summaries,
  factor_names = factor_names,
  tetra_cor = tetra_cor,
  n_factors = n_factors,
  tag_matrix_filtered = tag_matrix_filtered,
  platform_tags = platform_tags,
  relevant_tags = relevant_tags
)

# Save to RDS for later use
saveRDS(fa_results_list, "factor_analysis_results.rds")
saveRDS(steam_data_fa_subset, "steam_data_with_factors.rds")

cat("\nFactor analysis results saved to 'factor_analysis_results.rds'\n")
cat("Dataset with factor scores saved to 'steam_data_with_factors.rds'\n")

# --- Quick Validation ---
# Check variance explained
total_variance_explained <- sum(fa_result$Vaccounted[2, ])
cat("\nTotal variance explained by", n_factors, "factors:", 
    round(total_variance_explained * 100, 1), "%\n")

# Check communalities
communalities <- fa_result$communalities
low_communality_tags <- names(communalities[communalities < 0.2])
cat("\nNumber of tags with low communality (<0.2):", length(low_communality_tags), "\n")

cat("\n=== FACTOR ANALYSIS COMPLETE ===\n")


# --- Create Dedicated Regression Dataset ---

# Create regression dataset with the same games as factor analysis
regression_data <- steam_data %>%
  # Filter for the exact same set of games used in the factor analysis
  filter(app_id %in% steam_data_fa_subset$app_id) %>%
  # Join the factor scores from the factor analysis
  left_join(
    steam_data_fa_subset %>% 
      select(app_id, Management_City_Builder:Cyberpunk_RPG),
    by = "app_id"
  )

cat("Regression dataset created with", nrow(regression_data), "observations\n")
cat("Number of factors included:", sum(grepl("_", names(regression_data))), "\n\n")

# --- Prepare Variables for Regression Models ---

regression_data <- regression_data %>%
  mutate(
    # Create ratio and adjusted ratio for beta regression
    sc_ratio = schinese_reviews / total_reviews,
    sc_ratio_adj = case_when(
      sc_ratio == 0 ~ 0.00001,
      sc_ratio == 1 ~ 0.99999,
      TRUE ~ sc_ratio
    ),
    
    # Create sentiment gap variable
    sc_positive_ratio_calc = schinese_positive / schinese_reviews,
    english_positive_ratio_calc = english_positive / english_reviews,
    sentiment_gap = sc_positive_ratio_calc - english_positive_ratio_calc,
    
    # Create localisation categories for SC only
    sc_localisation_combo = case_when(
      chinese_simplified_audio & chinese_simplified_interface_subtitles ~ "Audio + Text",
      chinese_simplified_audio & !chinese_simplified_interface_subtitles ~ "Audio Only",
      !chinese_simplified_audio & chinese_simplified_interface_subtitles ~ "Text Only",
      TRUE ~ "None"
    ),
    
    # Create full localisation combo (SC or TC) - FIXED VERSION
    full_localisation_combo = case_when(
      (chinese_simplified_audio | chinese_traditional_audio) & 
        (chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Audio + Text",
      (chinese_simplified_audio | chinese_traditional_audio) & 
        !(chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Audio Only",
      !(chinese_simplified_audio | chinese_traditional_audio) & 
        (chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Text Only",
      TRUE ~ "None"
    ),
    
    # Set "None" (no localisation) as the reference level
    full_localisation_combo = relevel(factor(full_localisation_combo), ref = "None"),
    
    # Log transformation for total reviews
    log_total_reviews = log(total_reviews),
    
    # Create text-heavy game indicator
    has_story_rich = as.numeric(grepl("Story Rich", game_tags, ignore.case = TRUE)),
    has_visual_novel = as.numeric(grepl("Visual Novel", game_tags, ignore.case = TRUE)),
    text_heavy_game = as.numeric(has_story_rich == 1 | has_visual_novel == 1),
    
    # Create release year as factor for fixed effects
    release_year = factor(release_year),
    
    # Create centered release year for interaction models
    release_year_numeric = as.numeric(as.character(release_year)),
    release_year_centered = release_year_numeric - mean(release_year_numeric, na.rm = TRUE),
    release_year_centered_squared = release_year_centered^2,
    release_year_centered_cubed = release_year_centered^3,
    
    # Price category variables (USD)
    price_usd_category = case_when(
      price_usd_numeric == 0 ~ "Free",
      price_usd_numeric <= 10 ~ "Budget/Indie",
      price_usd_numeric <= 30 ~ "Mid-tier",
      price_usd_numeric <= 50 ~ "Major Release",
      price_usd_numeric > 50 ~ "Premium AAA",
      TRUE ~ NA_character_
    ),
    price_usd_category = relevel(factor(price_usd_category, 
                                        levels = c("Free", "Budget/Indie", "Mid-tier", 
                                                   "Major Release", "Premium AAA")), 
                                 ref = "Budget/Indie"),
    
    # Price category variables (CNY)
    price_cny_category = case_when(
      price_cny_numeric == 0 ~ "Free",
      price_cny_numeric <= 50 ~ "Budget/Indie",
      price_cny_numeric <= 120 ~ "Mid-tier",
      price_cny_numeric <= 200 ~ "Major Release",
      price_cny_numeric > 200 ~ "Premium AAA",
      TRUE ~ NA_character_
    ),
    price_cny_category = relevel(factor(price_cny_category, 
                                        levels = c("Free", "Budget/Indie", "Mid-tier", 
                                                   "Major Release", "Premium AAA")), 
                                 ref = "Budget/Indie")
  )

# Create weights for WLS models
sentiment_weights <- regression_data$total_reviews
sentiment_weights <- sentiment_weights / mean(sentiment_weights, na.rm = TRUE)

# Function to extract odds ratios from betareg models
betareg_or <- function(model, conf.level = 0.95, digits = 3) {
  # Extract coefficients and standard errors
  coef_summary <- summary(model)$coefficients$mean
  
  # Get coefficient estimates and standard errors
  log_odds <- coef_summary[, "Estimate"]
  se <- coef_summary[, "Std. Error"]
  
  # Calculate odds ratios
  odds_ratios <- exp(log_odds)
  
  # Calculate confidence intervals
  z_score <- qnorm((1 + conf.level) / 2)
  ci_lower <- exp(log_odds - z_score * se)
  ci_upper <- exp(log_odds + z_score * se)
  
  # Create results data frame
  results <- data.frame(
    Log_Odds = round(log_odds, digits),
    Odds_Ratio = round(odds_ratios, digits),
    CI_Lower = round(ci_lower, digits),
    CI_Upper = round(ci_upper, digits),
    p_value = round(coef_summary[, "Pr(>|z|)"], digits)
  )
  
  # Add significance stars
  results$Significance <- ifelse(results$p_value < 0.001, "***",
                                 ifelse(results$p_value < 0.01, "**",
                                        ifelse(results$p_value < 0.05, "*",
                                               ifelse(results$p_value < 0.1, ".", ""))))
  
  return(results)
}

# Enhanced function with percentage change interpretation
betareg_or_enhanced <- function(model, conf.level = 0.95, digits = 3) {
  results <- betareg_or(model, conf.level, digits)
  
  # Add percentage change interpretation
  results$Pct_Change <- round((results$Odds_Ratio - 1) * 100, 1)
  
  # Reorder columns for clarity
  results <- results[, c("Odds_Ratio", "CI_Lower", "CI_Upper", 
                         "Pct_Change", "p_value", "Significance", "Log_Odds")]
  
  return(results)
}

# --- Basic Models ---

# Model 1: Base model for Chinese review ratio (formerly model13)
model1 <- betareg(
  sc_ratio_adj ~ full_localisation_combo + release_year + log_total_reviews +
    Management_City_Builder +
    Narrative_Driven_Visual_Novel +
    Adult_Hentai +
    Roguelite_Deckbuilder +
    Local_Multiplayer +
    Action_Roguelike_Bullet_Hell +
    Racing_Driving_Sim +
    Horror_Games +
    Grand_Strategy_Wargame +
    Cozy_Wholesome +
    MMORPG +
    Puzzle_Games +
    ARPG_3D +
    Beat_em_up +
    Sci_Fi +
    Metroidvania_2D_Platformer +
    Retro_Old_School +
    Comedy_Humor +
    Investigation_3rd_Person +
    Cyberpunk_RPG,
  data = regression_data
)

# Transform model1
or_model1 <- betareg_or_enhanced(model1)
print(or_model1)

# For a more readable output, filter significant variables only
significant_or_model1 <- or_model1[or_model1$p_value < 0.05, ]
print(significant_or_model1)


# Model 2: Base model for sentiment gap (formerly model9_sg_wls)
model2 <- lm(
  sentiment_gap ~ full_localisation_combo + release_year + log_total_reviews +
    Management_City_Builder +
    Narrative_Driven_Visual_Novel +
    Adult_Hentai +
    Roguelite_Deckbuilder +
    Local_Multiplayer +
    Action_Roguelike_Bullet_Hell +
    Racing_Driving_Sim +
    Horror_Games +
    Grand_Strategy_Wargame +
    Cozy_Wholesome +
    MMORPG +
    Puzzle_Games +
    ARPG_3D +
    Beat_em_up +
    Sci_Fi +
    Metroidvania_2D_Platformer +
    Retro_Old_School +
    Comedy_Humor +
    Investigation_3rd_Person + +
    Cyberpunk_RPG,
  data = regression_data,
  weights = sentiment_weights
)

# --- Hypothesis 1: Temporal Models ---

# Model 3: Polynomial time trends for Chinese review ratio (formerly model20)
model3 <- betareg(
  sc_ratio_adj ~ full_localisation_combo*(release_year_centered + 
                                            release_year_centered_squared + 
                                            release_year_centered_cubed) + 
    log_total_reviews +
    Management_City_Builder +
    Narrative_Driven_Visual_Novel +
    Adult_Hentai +
    Roguelite_Deckbuilder +
    Local_Multiplayer +
    Action_Roguelike_Bullet_Hell +
    Racing_Driving_Sim +
    Horror_Games +
    Grand_Strategy_Wargame +
    Cozy_Wholesome +
    MMORPG +
    Puzzle_Games +
    ARPG_3D +
    Beat_em_up +
    Sci_Fi +
    Metroidvania_2D_Platformer +
    Retro_Old_School +
    Comedy_Humor +
    Investigation_3rd_Person + +
    Cyberpunk_RPG,
  data = regression_data
)

# Transform model3
or_model3 <- betareg_or_enhanced(model3)
print(or_model3)

# For a more readable output, filter significant variables only
significant_or_model3 <- or_model3[or_model3$p_value < 0.05, ]
print(significant_or_model3)

# Model 4: Polynomial time trends for sentiment gap (formerly model10_sg_poly_wls)
model4 <- lm(
  sentiment_gap ~ full_localisation_combo*(release_year_centered + 
                                             release_year_centered_squared + 
                                             release_year_centered_cubed) + 
    log_total_reviews +
    Management_City_Builder +
    Narrative_Driven_Visual_Novel +
    Adult_Hentai +
    Roguelite_Deckbuilder +
    Local_Multiplayer +
    Action_Roguelike_Bullet_Hell +
    Racing_Driving_Sim +
    Horror_Games +
    Grand_Strategy_Wargame +
    Cozy_Wholesome +
    MMORPG +
    Puzzle_Games +
    ARPG_3D +
    Beat_em_up +
    Sci_Fi +
    Metroidvania_2D_Platformer +
    Retro_Old_School +
    Comedy_Humor +
    Investigation_3rd_Person + +
    Cyberpunk_RPG,
  data = regression_data,
  weights = sentiment_weights
)

# --- Hypothesis 2: Text-Heavy Game Models ---

# Model 5: Text-heavy interaction for Chinese review ratio (formerly model16_simple)
model5 <- betareg(
  sc_ratio_adj ~ full_localisation_combo*text_heavy_game + 
    release_year + log_total_reviews,
  data = regression_data
)

# Transform model5
or_model5 <- betareg_or_enhanced(model5)
print(or_model5)

# For a more readable output, filter significant variables only
significant_or_model5 <- or_model5[or_model5$p_value < 0.05, ]
print(significant_or_model5)

# Model 6: Text-heavy interaction for sentiment gap (formerly model11_sg_wls)
model6 <- lm(
  sentiment_gap ~ full_localisation_combo*text_heavy_game + 
    release_year + log_total_reviews,
  data = regression_data,
  weights = sentiment_weights
)

# --- Hypothesis 3: Indie Game Models ---

# Model 7: Indie interaction for Chinese review ratio (formerly model17_simple)
model7 <- betareg(
  sc_ratio_adj ~ full_localisation_combo*has_indie_genre + 
    release_year + log_total_reviews,
  data = regression_data
)

# Transform model7
or_model7 <- betareg_or_enhanced(model7)
print(or_model7)

# For a more readable output, filter significant variables only
significant_or_model7 <- or_model7[or_model7$p_value < 0.05, ]
print(significant_or_model7)

# Model 8: Indie interaction for sentiment gap (formerly model13_sg_wls_simple)
model8 <- lm(
  sentiment_gap ~ full_localisation_combo*has_indie_genre + 
    release_year + log_total_reviews,
  data = regression_data,
  weights = sentiment_weights
)

# --- Hypothesis 4: Price Category Models ---

# Model 9: USD price interaction for Chinese review ratio (formerly model18)
model9 <- betareg(
  sc_ratio_adj ~ full_localisation_combo*price_usd_category + 
    release_year + log_total_reviews +
    Management_City_Builder +
    Narrative_Driven_Visual_Novel +
    Adult_Hentai +
    Roguelite_Deckbuilder +
    Local_Multiplayer +
    Action_Roguelike_Bullet_Hell +
    Racing_Driving_Sim +
    Horror_Games +
    Grand_Strategy_Wargame +
    Cozy_Wholesome +
    MMORPG +
    Puzzle_Games +
    ARPG_3D +
    Beat_em_up +
    Sci_Fi +
    Metroidvania_2D_Platformer +
    Retro_Old_School +
    Comedy_Humor +
    Investigation_3rd_Person + +
    Cyberpunk_RPG,
  data = filter(regression_data, !is.na(price_usd_category))
)

# Transform model9
or_model9 <- betareg_or_enhanced(model9)
print(or_model9)

# For a more readable output, filter significant variables only
significant_or_model9 <- or_model9[or_model9$p_value < 0.05, ]
print(significant_or_model9)

# Model 10: CNY price interaction for Chinese review ratio (formerly model19)
model10 <- betareg(
  sc_ratio_adj ~ full_localisation_combo*price_cny_category + 
    release_year + log_total_reviews +
    Management_City_Builder +
    Narrative_Driven_Visual_Novel +
    Adult_Hentai +
    Roguelite_Deckbuilder +
    Local_Multiplayer +
    Action_Roguelike_Bullet_Hell +
    Racing_Driving_Sim +
    Horror_Games +
    Grand_Strategy_Wargame +
    Cozy_Wholesome +
    MMORPG +
    Puzzle_Games +
    ARPG_3D +
    Beat_em_up +
    Sci_Fi +
    Metroidvania_2D_Platformer +
    Retro_Old_School +
    Comedy_Humor +
    Investigation_3rd_Person + +
    Cyberpunk_RPG,
  data = filter(regression_data, !is.na(price_cny_category))
)

# Transform model10
or_model10 <- betareg_or_enhanced(model10)
print(or_model10)

# For a more readable output, filter significant variables only
significant_or_model10 <- or_model10[or_model10$p_value < 0.05, ]
print(significant_or_model10)

# Model 11: USD price interaction for sentiment gap (formerly model12_usd_sg_wls)
model11 <- lm(
  sentiment_gap ~ full_localisation_combo*price_usd_category + 
    release_year + log_total_reviews,
  data = filter(regression_data, !is.na(price_usd_category)),
  weights = sentiment_weights[!is.na(regression_data$price_usd_category)]
)

# Model 12: CNY price interaction for sentiment gap (formerly model12_cny_sg_wls)
model12 <- lm(
  sentiment_gap ~ full_localisation_combo*price_cny_category + 
    release_year + log_total_reviews +
    Management_City_Builder +
    Narrative_Driven_Visual_Novel +
    Adult_Hentai +
    Roguelite_Deckbuilder +
    Local_Multiplayer +
    Action_Roguelike_Bullet_Hell +
    Racing_Driving_Sim +
    Horror_Games +
    Grand_Strategy_Wargame +
    Cozy_Wholesome +
    MMORPG +
    Puzzle_Games +
    ARPG_3D +
    Beat_em_up +
    Sci_Fi +
    Metroidvania_2D_Platformer +
    Retro_Old_School +
    Comedy_Humor +
    Investigation_3rd_Person +
    Cyberpunk_RPG,
  data = filter(regression_data, !is.na(price_cny_category)),
  weights = sentiment_weights[!is.na(regression_data$price_cny_category)]
)
summary(model12)

# --- Save All Models and Data ---
# Save all models and the regression dataset to an RData file
save(
  model1, model2, model3, model4, model5, model6,
  model7, model8, model9, model10, model11, model12,
  regression_data, sentiment_weights,
  file = "regression_models.RData"
)

# Also save the regression dataset separately for easy access
saveRDS(regression_data, "regression_data.rds")

cat("\n=== ALL REGRESSION MODELS CORRECTED ===\n")
cat("Models saved to 'regression_models_corrected.RData'\n")
cat("Regression dataset saved to 'regression_data_corrected.rds'\n")
cat("Total number of observations in regression dataset:", nrow(regression_data), "\n")
cat("Reference level for localisation is: Text Only\n")
cat("Factor scores successfully integrated from factor analysis\n")

# --- Model Comparison and Interpretation ---

# 1. Generate R-Squared Figures for Model Comparison
# Note: Beta regression models produce a "Pseudo R-squared," which is not directly
# comparable to the "Adjusted R-squared" from linear models (lm). However,
# comparing models of the same type (e.g., all betareg models) using this
# metric is a valid way to assess relative fit. We will collect the most
# appropriate metric for each model type into a summary table.

# Get all model objects from the environment
model_list <- mget(ls(pattern = "model\\d+"))
model_names <- names(model_list)

# Initialize a list to store the results
r_squared_results <- list()

for (i in 1:length(model_list)) {
  model_name <- model_names[i]
  model <- model_list[[i]]
  
  if (inherits(model, "betareg")) {
    # For beta regression, extract Pseudo R-squared
    r_squared_val <- summary(model)$pseudo.r.squared
    metric_name <- "Pseudo R-squared"
    model_type <- "Beta Regression (betareg)"
  } else if (inherits(model, "lm")) {
    # For linear models, extract Adjusted R-squared
    r_squared_val <- summary(model)$adj.r.squared
    metric_name <- "Adjusted R-squared"
    model_type <- "Linear Model (lm)"
  } else {
    next # Skip if it's not a model type we're handling
  }
  
  r_squared_results[[model_name]] <- data.frame(
    Model = model_name,
    Model_Type = model_type,
    Metric = metric_name,
    Value = round(r_squared_val, 4)
  )
}

# Combine the list into a single data frame
r_squared_summary_df <- do.call(rbind, r_squared_results)
rownames(r_squared_summary_df) <- NULL

cat("\n\n=== R-Squared Summary for All Models ===\n\n")
print(r_squared_summary_df)

#Descriptive Statistics for Dissertation

# 1. Total number of games in the original dataset
total_games <- nrow(steam_data)
cat("1. TOTAL GAMES IN ORIGINAL DATASET\n")
cat("   Total games:", format(total_games, big.mark = ","), "\n\n")

# 2. Number of games after factor analysis filtering
fa_games <- nrow(steam_data_fa_subset)
fa_percentage <- round((fa_games / total_games) * 100, 2)
cat("2. GAMES AFTER FACTOR ANALYSIS FILTERING\n")
cat("   Games with ≥20 SC reviews AND ≥20 English reviews:", format(fa_games, big.mark = ","), 
    " (", fa_percentage, "% of total)\n")
cat("   Games excluded:", format(total_games - fa_games, big.mark = ","), 
    " (", round(100 - fa_percentage, 2), "% of total)\n\n")

# 3. Number of samples available for CNY price analysis (model9/model10)
cny_available <- regression_data %>% filter(!is.na(price_cny_category)) %>% nrow()
cny_percentage <- round((cny_available / fa_games) * 100, 2)
cat("3. GAMES WITH CNY PRICE DATA (for model9/model10)\n")
cat("   Games with CNY price information:", format(cny_available, big.mark = ","), 
    " (", cny_percentage, "% of FA subset)\n")
cat("   Games without CNY price data:", format(fa_games - cny_available, big.mark = ","), 
    " (", round(100 - cny_percentage, 2), "% of FA subset)\n\n")

# 4. Games with indie tags
indie_games <- sum(regression_data$has_indie_genre == 1, na.rm = TRUE)
indie_percentage <- round((indie_games / fa_games) * 100, 2)
cat("4. GAMES WITH INDIE TAGS\n")
cat("   Games tagged as Indie:", format(indie_games, big.mark = ","), 
    " (", indie_percentage, "% of FA subset)\n")
cat("   Non-indie games:", format(fa_games - indie_games, big.mark = ","), 
    " (", round(100 - indie_percentage, 2), "% of FA subset)\n\n")

# 5. Text-heavy games
text_heavy_games <- sum(regression_data$text_heavy_game == 1, na.rm = TRUE)
text_heavy_percentage <- round((text_heavy_games / fa_games) * 100, 2)
cat("5. TEXT-HEAVY GAMES (Story Rich or Visual Novel)\n")
cat("   Text-heavy games:", format(text_heavy_games, big.mark = ","), 
    " (", text_heavy_percentage, "% of FA subset)\n")
cat("   Non text-heavy games:", format(fa_games - text_heavy_games, big.mark = ","), 
    " (", round(100 - text_heavy_percentage, 2), "% of FA subset)\n\n")

# 6. CNY Price Categories
cat("6. GAMES BY CNY PRICE CATEGORY\n")
cny_price_stats <- regression_data %>%
  filter(!is.na(price_cny_category)) %>%
  group_by(price_cny_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    percentage = round((count / sum(count)) * 100, 2),
    percentage_of_total = round((count / fa_games) * 100, 2)
  ) %>%
  arrange(match(price_cny_category, c("Free", "Budget/Indie", "Mid-tier", "Major Release", "Premium AAA")))

for(i in 1:nrow(cny_price_stats)) {
  cat("   ", cny_price_stats$price_cny_category[i], ": ", 
      format(cny_price_stats$count[i], big.mark = ","), 
      " (", cny_price_stats$percentage[i], "% of CNY-available games, ",
      cny_price_stats$percentage_of_total[i], "% of FA subset)\n")
}
cat("\n")

# 7. Games published by year
cat("7. GAMES PUBLISHED BY YEAR (FA Subset)\n")
yearly_stats <- regression_data %>%
  group_by(release_year_numeric) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = round((count / sum(count)) * 100, 2)) %>%
  arrange(release_year_numeric)

# Display recent years (2015 onwards) for readability
recent_years <- yearly_stats %>% filter(release_year_numeric >= 2015)
cat("   Recent years (2015+):\n")
for(i in 1:nrow(recent_years)) {
  cat("   ", recent_years$release_year_numeric[i], ": ", 
      format(recent_years$count[i], big.mark = ","), 
      " (", recent_years$percentage[i], "% of FA subset)\n")
}

# Summary for all years
total_recent <- sum(recent_years$count)
total_older <- sum(yearly_stats$count) - total_recent
cat("   \n   2015 and later: ", format(total_recent, big.mark = ","), 
    " (", round((total_recent / fa_games) * 100, 2), "% of FA subset)\n")
cat("   Before 2015: ", format(total_older, big.mark = ","), 
    " (", round((total_older / fa_games) * 100, 2), "% of FA subset)\n\n")

# 8. Additional breakdown: Localisation status
cat("8. CHINESE LOCALISATION STATUS (FA Subset)\n")
loc_stats <- regression_data %>%
  group_by(full_localisation_combo) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = round((count / sum(count)) * 100, 2)) %>%
  arrange(desc(count))

for(i in 1:nrow(loc_stats)) {
  cat("   ", loc_stats$full_localisation_combo[i], ": ", 
      format(loc_stats$count[i], big.mark = ","), 
      " (", loc_stats$percentage[i], "% of FA subset)\n")
}

cat("\n=== END OF DESCRIPTIVE STATISTICS ===\n")

# Create comprehensive summary data frames for CSV export

# 1. Main statistics summary
main_summary <- data.frame(
  Statistic = c(
    "Total Games (Original Dataset)",
    "Games in Factor Analysis Subset",
    "Games with CNY Price Data",
    "Games with Indie Tags",
    "Text-Heavy Games",
    "Games with Any Chinese Localisation"
  ),
  Count = c(
    total_games,
    fa_games,
    cny_available,
    indie_games,
    text_heavy_games,
    sum(regression_data$full_localisation_combo != "None", na.rm = TRUE)
  ),
  Percentage_of_Relevant_Base = c(
    100,
    fa_percentage,
    cny_percentage,
    indie_percentage,
    text_heavy_percentage,
    round((sum(regression_data$full_localisation_combo != "None", na.rm = TRUE) / fa_games) * 100, 2)
  )
)

# 2. Games by release year (FA subset)
yearly_summary <- regression_data %>%
  group_by(release_year_numeric) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    percentage_of_fa_subset = round((count / fa_games) * 100, 2),
    category = "Games by Release Year"
  ) %>%
  rename(
    release_year = release_year_numeric,
    games_count = count
  ) %>%
  select(category, release_year, games_count, percentage_of_fa_subset) %>%
  arrange(release_year)

# 3. Games by CNY price category
price_summary <- regression_data %>%
  filter(!is.na(price_cny_category)) %>%
  group_by(price_cny_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    percentage_of_cny_available = round((count / sum(count)) * 100, 2),
    percentage_of_fa_subset = round((count / fa_games) * 100, 2),
    category = "Games by CNY Price Category"
  ) %>%
  rename(
    price_category = price_cny_category,
    games_count = count
  ) %>%
  select(category, price_category, games_count, percentage_of_cny_available, percentage_of_fa_subset) %>%
  arrange(match(price_category, c("Free", "Budget/Indie", "Mid-tier", "Major Release", "Premium AAA")))

# 4. Games by localisation status
localisation_summary <- regression_data %>%
  group_by(full_localisation_combo) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    percentage_of_fa_subset = round((count / fa_games) * 100, 2),
    category = "Games by Localisation Status"
  ) %>%
  rename(
    localisation_status = full_localisation_combo,
    games_count = count
  ) %>%
  select(category, localisation_status, games_count, percentage_of_fa_subset) %>%
  arrange(desc(games_count))

# Display the summary tables
cat("\nMAIN STATISTICS SUMMARY:\n")
print(main_summary, row.names = FALSE)

cat("\nGAMES BY RELEASE YEAR SUMMARY:\n")
print(yearly_summary, row.names = FALSE)

cat("\nGAMES BY CNY PRICE CATEGORY SUMMARY:\n")
print(price_summary, row.names = FALSE)

cat("\nGAMES BY LOCALISATION STATUS SUMMARY:\n")
print(localisation_summary, row.names = FALSE)

# Save all summaries to separate CSV files
write.csv(main_summary, "01_main_statistics_summary.csv", row.names = FALSE)
write.csv(yearly_summary, "02_games_by_year_summary.csv", row.names = FALSE)
write.csv(price_summary, "03_games_by_cny_price_summary.csv", row.names = FALSE)
write.csv(localisation_summary, "04_games_by_localisation_summary.csv", row.names = FALSE)

# Create a comprehensive combined file
cat("\nCreating comprehensive combined CSV file...\n")

# Create a master summary with all breakdowns
combined_data <- list()

# Add main statistics
main_for_combined <- main_summary %>%
  mutate(
    Category = "Main Statistics",
    Subcategory = Statistic,
    Value_1 = Count,
    Value_2 = Percentage_of_Relevant_Base,
    Label_1 = "Count",
    Label_2 = "Percentage"
  ) %>%
  select(Category, Subcategory, Value_1, Value_2, Label_1, Label_2)

# Add yearly data
yearly_for_combined <- yearly_summary %>%
  mutate(
    Category = "Games by Release Year",
    Subcategory = paste("Year", release_year),
    Value_1 = games_count,
    Value_2 = percentage_of_fa_subset,
    Label_1 = "Count",
    Label_2 = "Percentage of FA Subset"
  ) %>%
  select(Category, Subcategory, Value_1, Value_2, Label_1, Label_2)

# Add price category data
price_for_combined <- price_summary %>%
  mutate(
    Category = "Games by CNY Price Category",
    Subcategory = price_category,
    Value_1 = games_count,
    Value_2 = percentage_of_fa_subset,
    Label_1 = "Count",
    Label_2 = "Percentage of FA Subset"
  ) %>%
  select(Category, Subcategory, Value_1, Value_2, Label_1, Label_2)

# Add localisation data
loc_for_combined <- localisation_summary %>%
  mutate(
    Category = "Games by Localisation Status",
    Subcategory = localisation_status,
    Value_1 = games_count,
    Value_2 = percentage_of_fa_subset,
    Label_1 = "Count",
    Label_2 = "Percentage of FA Subset"
  ) %>%
  select(Category, Subcategory, Value_1, Value_2, Label_1, Label_2)

# Combine all data
comprehensive_summary <- rbind(
  main_for_combined,
  yearly_for_combined,
  price_for_combined,
  loc_for_combined
)

# Save the comprehensive file
write.csv(comprehensive_summary, "00_comprehensive_descriptive_statistics.csv", row.names = FALSE)

cat("\nAll summary files saved:\n")
cat("- 00_comprehensive_descriptive_statistics.csv (all data combined)\n")
cat("- 01_main_statistics_summary.csv\n")
cat("- 02_games_by_year_summary.csv\n")
cat("- 03_games_by_cny_price_summary.csv\n")
cat("- 04_games_by_localisation_summary.csv\n")