library(tidyverse)  # For data manipulation
library(tidytext)
library(car)        # For diagnostic tests
library(sandwich)
library(lmtest)     # For model diagnostics
library(betareg)    # For beta regression
library(ggplot2)    # For visualisations
library(ggeffects)
library(psych)      # For factor analysis
library(lmtest)     # For model comparison tests
library(tidyr)      # For data manipulation
library(purrr)      # For functional programming
library(sjPlot)

# Read the data
setwd("C:/Users/Gairui/OneDrive - University College London/Dissertation/Data Analysis")
steam_data <- readRDS("Steam_data_gametags.rds")

# Data preparation
steam_data <- steam_data %>%
  # Create the dependent variable 'sc_ratio'
  mutate(
    sc_ratio = schinese_reviews / total_reviews,
    # Convert boolean variable to numeric dummy (1 = TRUE, 0 = FALSE)
    chinese_simplified_interface_subtitles_dummy = as.numeric(chinese_simplified_interface_subtitles)
  )

# Check for missing values in key variables
sum(is.na(steam_data$sc_ratio))
sum(is.na(steam_data$chinese_simplified_interface_subtitles_dummy))

# Explore the dependent variable
summary(steam_data$sc_ratio)
hist(steam_data$sc_ratio, main = "Distribution of Chinese Reviews Ratio", 
     xlab = "Ratio of Chinese Reviews to Total Reviews")

# Count exact zeros and ones
sum(steam_data$sc_ratio == 0)
sum(steam_data$sc_ratio == 1)

# Log transformation for dependent variable
steam_data <- steam_data %>%
  mutate(
    sc_ratio_adj = case_when(
      sc_ratio == 0 ~ 0.00001,
      sc_ratio == 1 ~ 0.99999,
      TRUE ~ sc_ratio
    ),
    sc_ratio_logit = log(sc_ratio_adj / (1 - sc_ratio_adj))
  )

# Log transformations for total review
steam_data <- steam_data %>%
  mutate(
    log_total_reviews = log(total_reviews)
  )

# Tag Analysis
# Function to extract tags from each game
extract_tags <- function(tag_string) {
  # Remove brackets and split by commas followed by spaces
  tags <- gsub("\\[|\\]", "", tag_string) %>%
    strsplit("', '") %>%
    unlist()
  
  # Clean up any remaining quotes
  tags <- gsub("^'|'$", "", tags)
  return(tags)
}

# Extract and count individual tags
all_individual_tags <- steam_data %>%
  mutate(game_tags = as.character(game_tags)) %>%
  rowwise() %>%
  mutate(extracted_tags = list(extract_tags(game_tags))) %>%
  # Unnest the extracted tags
  unnest(extracted_tags) %>%
  # Count frequency of each tag
  count(extracted_tags, sort = TRUE) %>%
  rename(Tag = extracted_tags, Frequency = n)

# Create a dataframe for better viewing
tag_df <- data.frame(
  Tag = all_individual_tags$Tag,
  Frequency = all_individual_tags$Frequency,
  Percentage = round(all_individual_tags$Frequency / nrow(steam_data) * 100, 2)
)

# Display top 200 tags
top_200_tags <- head(all_individual_tags, 200)
print(top_200_tags)

# Save the top 100 tag frequency table
write.csv(top_200_tags, "top_200_game_tags.csv", row.names = FALSE)

# Plot top 20 tags
ggplot(head(tag_df, 20), aes(x = reorder(Tag, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Game Tags on Steam",
       x = "Tag",
       y = "Frequency") +
  theme_minimal()

# Create dummy variables for major genres
steam_data <- steam_data %>%
  mutate(
    has_action = as.numeric(grepl("Action", game_tags, ignore.case = TRUE)),
    has_adventure = as.numeric(grepl("Adventure", game_tags, ignore.case = TRUE)),
    has_rpg = as.numeric(grepl("RPG", game_tags, ignore.case = TRUE)),
    has_story_rich = as.numeric(grepl("Story Rich", game_tags, ignore.case = TRUE)),
    has_strategy = as.numeric(grepl("Strategy", game_tags, ignore.case = TRUE)),
    has_casual = as.numeric(grepl("Casual", game_tags, ignore.case = TRUE)),
    has_simulation = as.numeric(grepl("Simulation", game_tags, ignore.case = TRUE)),
    has_anime = as.numeric(grepl("Anime", game_tags, ignore.case = TRUE)),
    has_puzzle = as.numeric(grepl("Puzzle", game_tags, ignore.case = TRUE))
  )

# Initial Models
# Simple OLS model
model0 <- lm(sc_ratio ~ chinese_simplified_interface_subtitles_dummy, data = steam_data)
summary(model0)

# Dependent variable log model
model1 <- lm(sc_ratio_logit ~ chinese_simplified_interface_subtitles_dummy, data = steam_data)
summary(model1)

# Checking OLS assumptions
# 1. Linearity - Not applicable for binary predictors
# 2. Independence - Assumed based on data collection methodology
# 3. Homoscedasticity
par(mfrow = c(2, 2))
plot(model0, main = "OLS Model")
par(mfrow = c(2, 2))
plot(model1, main = "Logit-transformed OLS")
par(mfrow = c(1, 1))

# Formal test for heteroscedasticity using Breusch-Pagan test
bp_test_model0 <- bptest(model0)
print(bp_test_model0)

bp_test_model1 <- bptest(model1)
print(bp_test_model1)

# Beta regression model
model2 <- betareg(sc_ratio_adj ~ chinese_simplified_interface_subtitles_dummy, data = steam_data)
summary(model2)
plot(model2)

# Enhanced Models with Control Variables
# Adding released year as a control variable
# Extract year from release_date
steam_data <- steam_data %>%
  mutate(
    release_year = as.factor(substr(release_date, 1, 4))
  )

# Check the distribution of years
table(steam_data$release_year)

# Beta regression with year fixed effects
model3 <- betareg(sc_ratio_adj ~ chinese_simplified_interface_subtitles_dummy + release_year, 
                  data = steam_data)
summary(model3)
exp(coef(model3)[1:2])

# Create 'has_indie' dummy
steam_data <- steam_data %>%
  mutate(
    has_indie = as.numeric(grepl("Indie", game_tags, ignore.case = TRUE))
  )

# Create categorical localisation variable for SC
steam_data <- steam_data %>%
  mutate(
    sc_localisation_combo = case_when(
      chinese_simplified_audio & chinese_simplified_interface_subtitles ~ "Audio + Text",
      chinese_simplified_audio & !chinese_simplified_interface_subtitles ~ "Audio Only",
      !chinese_simplified_audio & chinese_simplified_interface_subtitles ~ "Text Only",
      TRUE ~ "None"
    )
  )

# Create categorical localisation variable for SC or TC
steam_data <- steam_data %>%
  mutate(
    full_localisation_combo = case_when(
      # Audio + Text: Either SC or TC audio AND either SC or TC text/subtitles
      (chinese_simplified_audio | chinese_traditional_audio) & 
        (chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Audio + Text",
      # Audio Only: Either SC or TC audio but no Chinese text/subtitles  
      (chinese_simplified_audio | chinese_traditional_audio) & 
        !(chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Audio Only",
      # Text Only: Either SC or TC text/subtitles but no Chinese audio  
      !(chinese_simplified_audio | chinese_traditional_audio) & 
        (chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Text Only",
      # None: No Chinese localisation at all
      TRUE ~ "None"
    )
  )

# Beta regression with categorical localisation
model4 <- betareg(sc_ratio_adj ~ factor(full_localisation_combo) + release_year, data = steam_data)
summary(model4)
exp(coef(model4)[1:3])

# Models with Price Variables
# Convert price_cny to numeric (handle "Free" or "Unknown")
steam_data <- steam_data %>%
  mutate(
    price_cny_numeric = case_when(
      price_cny == "Free" ~ 0,
      price_cny == "Unknown" ~ NA_real_,
      TRUE ~ as.numeric(price_cny)
    )
  )

# Convert price_usd to numeric (handle "Free" or "Unknown")
steam_data <- steam_data %>%
  mutate(
    price_usd_numeric = case_when(
      price_usd == "Free" ~ 0,
      price_usd == "Unknown" ~ NA_real_,
      TRUE ~ as.numeric(price_usd)
    )
  )

# Check for missing values
sum(is.na(steam_data$price_cny_numeric))
sum(is.na(steam_data$price_usd_numeric))

# Beta regression with categorical localisation + price_cny/usd
model5 <- betareg(
  sc_ratio_adj ~ factor(sc_localisation_combo) + release_year + price_cny_numeric, 
  data = steam_data
)
summary(model5)

model6 <- betareg(
  sc_ratio_adj ~ factor(sc_localisation_combo) + release_year + price_usd_numeric, 
  data = steam_data
)
summary(model6)

# Models with Indie Game Variable
# Verify distribution
table(steam_data$has_indie)  # Check counts of 0 vs. 1

# Beta regression with indie dummy
model7 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo) + release_year + has_indie, 
  data = steam_data
)
summary(model7)

#AIC and BIC test for model 4 and 7
AIC(model4, model7)
BIC(model4, model7)

# Adding interaction for indie and SC localisation level
model8 <- betareg(sc_ratio_adj ~ factor(full_localisation_combo)*has_indie + release_year, data = steam_data)
summary(model8)

#Visualize interaction effects
ggplot(steam_data, aes(x = sc_localisation_combo, 
                       y = sc_ratio_adj, 
                       color = factor(has_indie),
                       group = interaction(sc_localisation_combo, has_indie))) +
  geom_boxplot() +
  labs(title = "Effect of Localisation on Chinese Reviews (Indie vs. Non-Indie)",
       x = "Simplified Chinese Localization",
       y = "Adjusted Chinese Review Ratio",
       color = "Indie Game") +
  scale_color_discrete(labels = c("Non-Indie", "Indie")) +
  theme_minimal()

#AIC and BIC test for model 4 and 8
AIC(model4, model8)
BIC(model4, model8)

# LRT test for model 4 and 8
lrt_result_48 <- lrtest(model4, model8)
print(lrt_result_48)

# Models with Combined Chinese Localisation
# Model with combined Chinese localisation (SC or TC)
model9 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo) + release_year, 
  data = steam_data
)
summary(model9)

# Model with combined Chinese localisation + indie interaction
model10 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo)*has_indie + release_year, 
  data = steam_data
)
summary(model10)

# Compare new models to previous ones
AIC(model4, model7, model8, model9, model10)
BIC(model4, model7, model8, model9, model10)

# LRT test for comparing nested models
lrt_result_910 <- lrtest(model9, model10)
print(lrt_result_910)

# Calculate odds ratios for model10
exp(coef(model10)[c(1:4, 20:21)])

# Models with Genre Controls
# Added genres dummy
model11 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo) + release_year +
    has_action + has_adventure + has_rpg + has_story_rich + has_strategy + 
    has_casual + has_simulation + has_anime + has_puzzle,
  data = steam_data
)
summary(model11)

# Added log total review
model12 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo) + release_year + log_total_reviews +
    has_action + has_adventure + has_rpg + has_story_rich + has_strategy + 
    has_casual + has_simulation + has_anime + has_puzzle,
  data = steam_data
)
summary(model12)

# Transfer coefficient to odds ratio
odds_model12 <- exp(coef(model12))
print(odds_model12)

# Compare new models to previous ones
AIC(model10, model11, model12)
BIC(model10, model11, model12)

# LRT test for comparing nested models
lrt_result_1112 <- lrtest(model11, model12)
print(lrt_result_1112)

# Factor Analysis for Genre Dimensionality Reduction
# Create a binary tag matrix
tag_matrix <- steam_data %>%
  mutate(game_tags = as.character(game_tags)) %>%
  rowwise() %>%
  mutate(extracted_tags = list(extract_tags(game_tags))) %>%
  unnest(extracted_tags) %>%
  mutate(value = 1) %>%
  distinct(app_id, extracted_tags, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = app_id, 
    names_from = extracted_tags, 
    values_from = value,
    values_fill = 0
  )

# List of tags to exclude (platform features rather than genre characteristics)
platform_tags <- c(
  "Family Sharing", "Steam Achievements", "Steam Trading Cards", "Steam Cloud", 
  "Steam Workshop", "Full controller support", "Tracked Controller Support", 
  "Partial Controller Support", "Controller", "Steam Leaderboards", 
  "Remote Play Together", "Remote Play on TV", "Remote Play on Phone", 
  "Remote Play on Tablet", "Captions available"
)

# Count tag frequencies to identify prevalent tags (≥100 games)
tag_freq <- colSums(tag_matrix[, -1])
tags_to_keep <- names(tag_freq[tag_freq >= 100])
cat("Number of tags that appear in at least 100 games:", length(tags_to_keep), "\n")

# Filter the tag matrix to include only prevalent tags AND exclude platform tags
relevant_tags <- setdiff(tags_to_keep, platform_tags)
tag_matrix_filtered <- tag_matrix[, c("app_id", relevant_tags)]

# Check dimensions of filtered matrix
dim(tag_matrix_filtered)

# Compute tetrachoric correlation matrix for the refined set
set.seed(123)  # For reproducibility
tetra_cor <- tetrachoric(tag_matrix_filtered[, -1])$rho

# Check for NA values in the correlation matrix (can happen with tetrachoric correlations)
sum(is.na(tetra_cor))

# Kaiser-Meyer-Olkin (KMO) test for sampling adequacy
kmo_result <- KMO(tetra_cor)
print(kmo_result)

# Bartlett's test of sphericity
bartlett_result <- cortest.bartlett(tetra_cor, n = nrow(tag_matrix_filtered))
print(bartlett_result)

# Determine number of factors using parallel analysis
set.seed(123)  # For reproducibility
parallel_result <- fa.parallel(
  tetra_cor, 
  n.obs = nrow(tag_matrix_filtered),
  fa = "fa",
  cor = "cor",  # We're passing a correlation matrix
  n.iter = 20   # Reduce iterations for faster computation
)

# Extract factors based on parallel analysis results
n_factors <- 15  # Adjust based on your parallel analysis results
fa_result <- fa(
  tetra_cor,
  nfactors = n_factors,
  rotate = "varimax",  # Orthogonal rotation for better interpretability
  cor = "cor",         # We're using a correlation matrix
  n.obs = nrow(tag_matrix_filtered)
)

# Print factor loadings with a reasonable cutoff
print(fa_result$loadings, cutoff = 0.3)

# Factor Interpretation and Naming
# Function to extract top loading tags for each factor
get_top_loadings <- function(loadings_matrix, factor_num, n_tags = 20, cutoff = 0.3) {
  # Extract loadings for the specified factor
  factor_loadings <- loadings_matrix[, factor_num]
  
  # Find absolute loadings above cutoff
  significant_loadings <- abs(factor_loadings) >= cutoff
  
  # Get the tags with significant loadings
  significant_tags <- names(factor_loadings)[significant_loadings]
  significant_values <- factor_loadings[significant_loadings]
  
  # Create a data frame with tags and loadings
  result <- data.frame(
    Tag = significant_tags,
    Loading = round(significant_values, 3)
  )
  
  # Sort by absolute loading in descending order
  result <- result[order(-abs(result$Loading)), ]
  
  # Return top n tags
  return(head(result, n_tags))
}

# Create a consolidated view of the factors
factor_summary <- list()
for (i in 1:n_factors) {
  factor_summary[[i]] <- get_top_loadings(fa_result$loadings, i)
}

# Print summaries for each factor
for (i in 1:n_factors) {
  cat(paste0("Factor ", i, " (", round(fa_result$Vaccounted[2, i] * 100, 2), "% of variance)\n"))
  print(factor_summary[[i]])
  cat("\n")
}

# Assign factor names based on explicit matching to factor indices
factor_names <- rep(NA, n_factors)
factor_names[1] <- "City_Builders_Economic_Management_Sims"
factor_names[2] <- "Detective_Interactive_Narrative_Games"
factor_names[3] <- "Atmospheric_Retro_RPGs"
factor_names[4] <- "Turn_Based_Tactical_Party_RPGs"
factor_names[5] <- "Couch_Co_Op_Local_Multiplayer_Games"
factor_names[6] <- "Free_to_Play_MMOs_Competitive_Online_Games"
factor_names[7] <- "Military_Strategy_War_Simulations"
factor_names[8] <- "Casual_Family_Friendly_Games"
factor_names[9] <- "Skill_Based_Action_RPGs_Souls_likes"
factor_names[10] <- "Immersive_3D_Simulations_Driving_Games"
factor_names[11] <- "Survival_Horror_Dark_Adventure"
factor_names[12] <- "Chaotic_Top_Down_Shooters_Roguelites"
factor_names[13] <- "Roguelike_Deckbuilders_Procedural_Indie_Games"
factor_names[14] <- "Sci_Fi_Space_Futuristic_Simulation"
factor_names[15] <- "Comedy_Dark_Humor"

# Creating Factor Scores
# Get app_ids from the tag matrix
app_ids <- tag_matrix_filtered$app_id

# Calculate factor scores using Thurstone method as recommended
fa_scores <- factor.scores(
  tag_matrix_filtered[, -1],  # Exclude app_id column
  fa_result,
  method = "Thurstone"
)

# Create a data frame with app_id and factor scores
fa_scores_df <- as.data.frame(fa_scores$scores)
colnames(fa_scores_df) <- factor_names  # Rename columns to factor names
fa_scores_df$app_id <- app_ids  # Add back the app_id column

# View the structure of the factor scores dataframe
str(fa_scores_df)

# Merge factor scores with original steam_data
steam_data_fa <- steam_data %>%
  left_join(fa_scores_df, by = "app_id")

if(!"full_localisation_combo" %in% names(steam_data_fa)) {
  steam_data_fa <- steam_data_fa %>%
    mutate(
      full_localisation_combo = case_when(
        (chinese_simplified_audio | chinese_traditional_audio) & 
          (chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Audio + Text",
        
        (chinese_simplified_audio | chinese_traditional_audio) & 
          !(chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Audio Only",
        
        !(chinese_simplified_audio | chinese_traditional_audio) & 
          (chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Text Only",
        
        TRUE ~ "None"
      )
    )
}

# Check if key variables exist in the merged dataset
variables_check <- c("sc_localisation_combo", "full_localisation_combo", "release_year", "log_total_reviews") %in% 
  names(steam_data_fa)
print(paste("Variables present:", paste(c("sc_localisation_combo", "full_localisation_combo", 
                                          "release_year", "log_total_reviews"), 
                                        variables_check, sep = ": ", collapse = ", ")))

# Check for missing values in factor columns
factor_names <- colnames(fa_scores_df)[1:15]  # Get just the factor names
missing_values <- colSums(is.na(steam_data_fa[, factor_names]))
print(missing_values)
# No missing value

# Models with Factor Scores
# MODEL13 with factor scores instead of genre dummies
model13 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo) + release_year + log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa
)

summary(model13)

# Transfer coefficient to odds ratio
odds_model13 <- exp(coef(model13))
print(odds_model13)

#Perform LR Test for model12 and model13
lrtest_1213 <- lrtest(model12, model13)
print(lrtest_1213)

# Compare models using AIC and BIC
AIC(model12, model13)
BIC(model12, model13)

# Compare pseudo R-squared
pseudo_r2_model12 <- summary(model12)$pseudo.r.squared
pseudo_r2_model13 <- summary(model13)$pseudo.r.squared

comparison <- data.frame(
  Model = c("Original (Model12)", "Factor Analysis (Model13)"),
  Pseudo_R_Squared = c(pseudo_r2_model12, pseudo_r2_model13),
  AIC = c(AIC(model12), AIC(model13)),
  BIC = c(BIC(model12), BIC(model13)),
  Parameters = c(length(coef(model12)), length(coef(model13)))
)
print(comparison)

# Check for multicollinearity among factors
if (requireNamespace("car", quietly = TRUE)) {
  vif_values <- car::vif(model13)
  print("Variance Inflation Factors (VIF):")
  print(vif_values)
}

# Test for significant factors
wald_test13 <- waldtest(model13)
print("Wald test for factor significance:")
print(wald_test13)

# MODEL14 with only significant factors from MODEL13
model14 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo) + release_year + log_total_reviews +
    Atmospheric_Retro_RPGs +
    Immersive_3D_Simulations_Driving_Games +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Survival_Horror_Dark_Adventure +
    Couch_Co_Op_Local_Multiplayer_Games +
    Detective_Interactive_Narrative_Games +
    Turn_Based_Tactical_Party_RPGs +
    Comedy_Dark_Humor +
    Casual_Family_Friendly_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Chaotic_Top_Down_Shooters_Roguelites +
    Military_Strategy_War_Simulations +
    City_Builders_Economic_Management_Sims,
  data = steam_data_fa
)
summary(model14)

# Compare all three models
AIC(model12, model13, model14)
BIC(model12, model13, model14)

########################################
# Sentiment Gap Analysis
########################################
# Data Preparation for Sentiment Gap Analysis
steam_data <- steam_data %>%
  # Create the dependent variable 'sc_ratio'
  mutate(
    sc_ratio = schinese_reviews / total_reviews,
    # Convert boolean variable to numeric dummy (1 = TRUE, 0 = FALSE)
    chinese_simplified_interface_subtitles_dummy = as.numeric(chinese_simplified_interface_subtitles)
  ) %>%
  # Create the sentiment gap variable (difference between Chinese and English positive ratios)
  mutate(
    # Create sentiment gap (Chinese - English)
    sentiment_gap = schinese_positive_ratio - english_positive_ratio
  )

# Check for missing values in key variables
sum(is.na(steam_data$sentiment_gap))
sum(is.na(steam_data$chinese_simplified_interface_subtitles_dummy))

# Explore the sentiment gap variable
summary(steam_data$sentiment_gap)
hist(steam_data$sentiment_gap, breaks = 50,
     main = "Distribution of Chinese vs English Sentiment Gap", 
     xlab = "Sentiment Gap (Chinese - English)")

# Filter out cases where we don't have enough reviews for comparison
# Only include games with at least 20 Chinese reviews and 20 English reviews
steam_data_filtered <- steam_data %>%
  filter(schinese_reviews >= 20, english_reviews >= 20)

# Re-check the distribution after filtering
hist(steam_data_filtered$sentiment_gap, breaks = 50, 
     main = "Distribution of Sentiment Gap", 
     xlab = "Sentiment Gap (Chinese Positive - English Positive)")

# Log transformation for total reviews (as a control variable)
steam_data <- steam_data %>%
  mutate(
    log_total_reviews = log(total_reviews)
  )

# Apply the same transformation to the filtered dataset
steam_data_filtered <- steam_data_filtered %>%
  mutate(
    log_total_reviews = log(total_reviews)
  )

# Create weights for all sentiment gap models
# Weights are proportional to total reviews (inverse of variance)
sentiment_weights <- steam_data_filtered$total_reviews
# Normalize weights to prevent numerical issues
sentiment_weights <- sentiment_weights / mean(sentiment_weights, na.rm = TRUE)

# Simple OLS model
model0_sg <- lm(sentiment_gap ~ chinese_simplified_interface_subtitles_dummy, 
                data = steam_data_filtered)
summary(model0_sg)

# Check OLS assumptions
par(mfrow = c(2, 2))
plot(model0_sg, main = "OLS Model for Sentiment Gap")
par(mfrow = c(1, 1))

# Test for heteroscedasticity using Breusch-Pagan test
bp_test_model0_sg <- bptest(model0_sg)
print(bp_test_model0_sg)

# Extract release year (reusing from previous section)
steam_data_filtered <- steam_data_filtered %>%
  mutate(
    release_year = as.factor(substr(release_date, 1, 4))
  )

# Model with release year as a control
model1_sg <- lm(sentiment_gap ~ chinese_simplified_interface_subtitles_dummy + release_year, 
                data = steam_data_filtered)
summary(model1_sg)

bp_test_model1_sg <- bptest(model1_sg)
print(bp_test_model1_sg)

# Apply robust standard errors to base model
robust_model0_sg <- coeftest(model0_sg, vcov = vcovHC(model0_sg, type = "HC1"))
print(robust_model0_sg)

# Apply robust standard errors to model1_sg
robust_model1_sg <- coeftest(model1_sg, vcov = vcovHC(model1_sg, type = "HC1"))
print(robust_model1_sg)

# Create categorical localisation variable for SC (if not already created)
if(!"sc_localisation_combo" %in% names(steam_data_filtered)) {
  steam_data_filtered <- steam_data_filtered %>%
    mutate(
      sc_localisation_combo = case_when(
        chinese_simplified_audio & chinese_simplified_interface_subtitles ~ "Audio + Text",
        chinese_simplified_audio & !chinese_simplified_interface_subtitles ~ "Audio Only",
        !chinese_simplified_audio & chinese_simplified_interface_subtitles ~ "Text Only",
        TRUE ~ "None"
      )
    )
}

# Create combined Chinese localisation variable (SC or TC) (if not already created)
if(!"full_localisation_combo" %in% names(steam_data_filtered)) {
  steam_data_filtered <- steam_data_filtered %>%
    mutate(
      full_localisation_combo = case_when(
        (chinese_simplified_audio | chinese_traditional_audio) & 
          (chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Audio + Text",
        
        (chinese_simplified_audio | chinese_traditional_audio) & 
          !(chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Audio Only",
        
        !(chinese_simplified_audio | chinese_traditional_audio) & 
          (chinese_simplified_interface_subtitles | chinese_traditional_interface_subtitles) ~ "Text Only",
        
        TRUE ~ "None"
      )
    )
}

# Model with categorical localisation
model2_sg <- lm(sentiment_gap ~ factor(full_localisation_combo) + release_year, 
                data = steam_data_filtered)
summary(model2_sg)
bp_test_model2_sg <- bptest(model2_sg)
print(bp_test_model2_sg)

# Apply robust standard errors
robust_model2_sg <- coeftest(model2_sg, vcov = vcovHC(model2_sg, type = "HC1"))
print(robust_model2_sg)

# Models with Price Variables and Robust Standard Errors
# Check for missing values
sum(is.na(steam_data_filtered$price_cny_numeric))
sum(is.na(steam_data_filtered$price_usd_numeric))

# Model with price variables
model3_sg <- lm(
  sentiment_gap ~ factor(full_localisation_combo) + release_year + price_cny_numeric, 
  data = steam_data_filtered
)

# Apply robust standard errors
robust_model3_sg <- coeftest(model3_sg, vcov = vcovHC(model3_sg, type = "HC1"))
print(robust_model3_sg)

model4_sg <- lm(
  sentiment_gap ~ factor(full_localisation_combo) + release_year + price_usd_numeric, 
  data = steam_data_filtered
)

# Apply robust standard errors
robust_model4_sg <- coeftest(model4_sg, vcov = vcovHC(model4_sg, type = "HC1"))
print(robust_model4_sg)

# Models with Indie Game Variable and Robust Standard Errors
table(steam_data_filtered$has_indie)
# Model with indie dummy
model5_sg <- lm(
  sentiment_gap ~ factor(full_localisation_combo) + release_year + has_indie, 
  data = steam_data_filtered
)

# Apply robust standard errors
robust_model5_sg <- coeftest(model5_sg, vcov = vcovHC(model5_sg, type = "HC1"))
print(robust_model5_sg)

# Adding interaction for indie and SC localisation level
model6_sg <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*has_indie + release_year, 
  data = steam_data_filtered
)

# Apply robust standard errors
robust_model6_sg <- coeftest(model6_sg, vcov = vcovHC(model6_sg, type = "HC1"))
print(robust_model6_sg)

# Visualize interaction effects
ggplot(steam_data_filtered, aes(x = full_localisation_combo, 
                                y = sentiment_gap, 
                                color = factor(has_indie),
                                group = interaction(full_localisation_combo, has_indie))) +
  geom_boxplot() +
  labs(title = "Effect of Localisation on Sentiment Gap (Indie vs. Non-Indie)",
       x = "Chinese Localization",
       y = "Sentiment Gap (Chinese - Non-Chinese)",
       color = "Indie Game") +
  scale_color_discrete(labels = c("Non-Indie", "Indie")) +
  theme_minimal()

# Compare models using AIC/BIC
AIC(model2_sg, model5_sg, model6_sg)
BIC(model2_sg, model5_sg, model6_sg)

# Robust Wald test for comparing nested models
waldtest(model5_sg, model6_sg, vcov = vcovHC)


# Create dummy variables for major genres if not already present
required_genres <- c("has_action", "has_adventure", "has_rpg", "has_story_rich", 
                     "has_strategy", "has_casual", "has_simulation", "has_anime", "has_puzzle")

missing_genres <- required_genres[!required_genres %in% names(steam_data_filtered)]

if(length(missing_genres) > 0) {
  # Create the missing genre dummies
  genre_formula <- paste0("steam_data_filtered <- steam_data_filtered %>%",
                          "mutate(",
                          paste0(missing_genres, " = as.numeric(grepl('", 
                                 gsub("has_", "", missing_genres), 
                                 "', game_tags, ignore.case = TRUE))", 
                                 collapse = ", "),
                          ")")
  eval(parse(text = genre_formula))
}

# Model with genre dummies
model7_sg <- lm(
  sentiment_gap ~ factor(full_localisation_combo) + release_year +
    has_action + has_adventure + has_rpg + has_story_rich + has_strategy + 
    has_casual + has_simulation + has_anime + has_puzzle,
  data = steam_data_filtered
)

# Apply robust standard errors
robust_model7_sg <- coeftest(model7_sg, vcov = vcovHC(model7_sg, type = "HC1"))
print(robust_model7_sg)

# Model with genre dummies and log total reviews
model8_sg <- lm(
  sentiment_gap ~ factor(full_localisation_combo) + release_year + log_total_reviews +
    has_action + has_adventure + has_rpg + has_story_rich + has_strategy + 
    has_casual + has_simulation + has_anime + has_puzzle,
  data = steam_data_filtered
)

# Apply robust standard errors
robust_model8_sg <- coeftest(model8_sg, vcov = vcovHC(model8_sg, type = "HC1"))
print(robust_model8_sg)

# Compare models using AIC/BIC
AIC(model7_sg, model8_sg)
BIC(model7_sg, model8_sg)

# Robust Wald test for comparing nested models
waldtest(model7_sg, model8_sg, vcov = vcovHC)

# Using Factor Analysis Results with Robust Standard Errors
# Filter the factor dataset to match the filtered dataset
steam_data_fa_filtered <- steam_data_fa %>%
  left_join(select(steam_data_filtered, app_id, sentiment_gap), by = "app_id") %>%
  filter(!is.na(sentiment_gap))
  
# Model with factor scores
model9_sg <- lm(
  sentiment_gap ~ factor(full_localisation_combo) + release_year + log_total_reviews +
      City_Builders_Economic_Management_Sims +
      Detective_Interactive_Narrative_Games +
      Atmospheric_Retro_RPGs +
      Turn_Based_Tactical_Party_RPGs +
      Couch_Co_Op_Local_Multiplayer_Games +
      Free_to_Play_MMOs_Competitive_Online_Games +
      Military_Strategy_War_Simulations +
      Casual_Family_Friendly_Games +
      Skill_Based_Action_RPGs_Souls_likes +
      Immersive_3D_Simulations_Driving_Games +
      Survival_Horror_Dark_Adventure +
      Chaotic_Top_Down_Shooters_Roguelites +
      Roguelike_Deckbuilders_Procedural_Indie_Games +
      Sci_Fi_Space_Futuristic_Simulation +
      Comedy_Dark_Humor,
    data = steam_data_fa_filtered
)
  
# Apply robust standard errors
robust_model9_sg <- coeftest(model9_sg, vcov = vcovHC(model9_sg, type = "HC1"))
print(robust_model9_sg)
  
# Compare with genre dummy model using AIC/BIC
AIC(model8_sg, model9_sg)
BIC(model8_sg, model9_sg)

# Setting up weights for WLS
fa_sentiment_weights <- steam_data_fa_filtered$total_reviews
fa_sentiment_weights <- fa_sentiment_weights / mean(fa_sentiment_weights, na.rm = TRUE)

# Applied WLS for model9_sg
model9_sg_wls <- lm(
  sentiment_gap ~ factor(full_localisation_combo) + release_year + log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa_filtered, weights = fa_sentiment_weights
)

# Apply robust standard errors
robust_model9_sg_wls <- coeftest(model9_sg_wls, vcov = vcovHC(model9_sg_wls, type = "HC1"))

# Comparing output
print(robust_model9_sg_wls)
print(robust_model9_sg)
summary(model9_sg_wls)
summary(model9_sg)

bp_test_model9_sg_wls <- bptest(model9_sg_wls)
print("Breusch-Pagan test for WLS model:")
print(bp_test_model9_sg_wls)

# Create a numeric year variable for interaction
steam_data_filtered <- steam_data_filtered %>%
  mutate(
    release_year_numeric = as.numeric(substr(release_date, 1, 4)),
    release_year_centered = release_year_numeric - mean(release_year_numeric, na.rm = TRUE)
  )

# Check actual localisation levels in the dataset
table(steam_data_filtered$full_localisation_combo)

# -------------------------------------------------------------------------
# ADDRESSING SKEWED DISTRIBUTION OF GAMES ACROSS YEARS
# -------------------------------------------------------------------------

# Ensure release_year_centered is available in the fa dataset
steam_data_fa_filtered <- steam_data_fa_filtered %>%
  mutate(
    release_year_numeric = as.numeric(substr(release_date, 1, 4)),
    release_year_centered = release_year_numeric - mean(release_year_numeric, na.rm = TRUE)
  )

# 1. FACTOR SCORES LINEAR MODEL
model10_sg_fa <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*release_year_centered + 
    log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa_filtered
)

# Apply robust standard errors
robust_model10_sg_fa <- coeftest(model10_sg_fa, vcov = vcovHC(model10_sg_fa, type = "HC1"))
print(robust_model10_sg_fa)

# Linear time trend model (WLS)
model10_sg_fa_wls <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*release_year_centered + 
    log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa_filtered, weights = fa_sentiment_weights
)
# Apply robust standard errors
robust_model10_sg_fa_wls <- coeftest(model10_sg_fa_wls, vcov = vcovHC(model10_sg_fa_wls, type = "HC1"))
print(robust_model10_sg_fa_wls)

# 2. WEIGHTED REGRESSION APPROACH
# Compute weights inversely proportional to number of games per year
year_counts <- table(steam_data_fa_filtered$release_year_numeric)
year_weights <- 1 / as.vector(year_counts[as.character(steam_data_fa_filtered$release_year_numeric)])
year_weights <- year_weights / mean(year_weights)  # Normalize weights

model10_sg_weighted <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*release_year_centered + 
    log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa_filtered,
  weights = year_weights
)

# Apply robust standard errors
robust_model10_sg_weighted <- coeftest(model10_sg_weighted, vcov = vcovHC(model10_sg_weighted, type = "HC1"))
print(robust_model10_sg_weighted)

# 3. POLYNOMIAL TIME TRENDS APPROACH (prefered)
# Create polynomial terms for release year
steam_data_fa_filtered <- steam_data_fa_filtered %>%
  mutate(
    release_year_centered_squared = release_year_centered^2,
    release_year_centered_cubed = release_year_centered^3
  )

model10_sg_poly <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*(release_year_centered + 
                                                   release_year_centered_squared + 
                                                   release_year_centered_cubed) + 
    log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa_filtered
)

# Apply robust standard errors
robust_model10_sg_poly <- coeftest(model10_sg_poly, vcov = vcovHC(model10_sg_poly, type = "HC1"))
print(robust_model10_sg_poly)

# Polynomial time trend model (WLS)
model10_sg_poly_wls <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*(release_year_centered + 
                                                     release_year_centered_squared + 
                                                     release_year_centered_cubed) + 
    log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa_filtered, weights = fa_sentiment_weights
)
# Apply robust standard errors
robust_model10_sg_poly_wls <- coeftest(model10_sg_poly_wls, vcov = vcovHC(model10_sg_poly_wls, type = "HC1"))
print(robust_model10_sg_poly_wls)

# 4. TIME PERIOD BUCKETING APPROACH
# Create time period buckets
steam_data_fa_filtered <- steam_data_fa_filtered %>%
  mutate(
    time_period = case_when(
      release_year_numeric <= 2013 ~ "2010-2013",
      release_year_numeric <= 2016 ~ "2014-2016",
      release_year_numeric <= 2019 ~ "2017-2019",
      release_year_numeric <= 2022 ~ "2020-2022",
      TRUE ~ "2023-2025"
    ),
    time_period = factor(time_period, levels = c("2010-2013", "2014-2016", "2017-2019", "2020-2022", "2023-2025"))
  )

# Check distribution across buckets
print("Distribution of games across time period buckets:")
print(table(steam_data_fa_filtered$time_period))

model10_sg_bucket <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*time_period + 
    log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa_filtered
)

# Apply robust standard errors
robust_model10_sg_bucket <- coeftest(model10_sg_bucket, vcov = vcovHC(model10_sg_bucket, type = "HC1"))
print(robust_model10_sg_bucket)

# Time period bucketing approach (WLS)
model10_sg_bucket_wls <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*time_period + 
    log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa_filtered, weights = fa_sentiment_weights
)
# Apply robust standard errors
robust_model10_sg_bucket_wls <- coeftest(model10_sg_bucket_wls, vcov = vcovHC(model10_sg_bucket_wls, type = "HC1"))
print(robust_model10_sg_bucket_wls)

# MODEL COMPARISON for Hypothesis 1 (WLS)
model_h1_comparison <- data.frame(
  Model = c("Linear", "Polynomial", "Time Buckets"),
  Adj.R.squared = c(
    summary(model10_sg_fa_wls)$adj.r.squared,
    summary(model10_sg_poly_wls)$adj.r.squared,
    summary(model10_sg_bucket_wls)$adj.r.squared
  )
)
print(model_h1_comparison)

# Test for model improvement with polynomial terms vs linear
anova_test_poly_wls <- anova(model10_sg_fa_wls, model10_sg_poly_wls)
print("ANOVA test comparing Linear vs Polynomial WLS models:")
print(anova_test_poly_wls)

# MODEL COMPARISON
model_comparison <- AIC(model10_sg_fa, model10_sg_weighted, model10_sg_poly, model10_sg_bucket)
model_comparison$Model <- c("Linear", "Weighted", "Polynomial", "Time Buckets")
model_comparison$BIC <- BIC(model10_sg_fa, model10_sg_weighted, model10_sg_poly, model10_sg_bucket)
model_comparison$df <- c(length(coef(model10_sg_fa)), 
                         length(coef(model10_sg_weighted)), 
                         length(coef(model10_sg_poly)), 
                         length(coef(model10_sg_bucket)))

# Add adjusted R-squared for comparison
model_comparison$Adj.R.squared <- c(
  summary(model10_sg_fa)$adj.r.squared,
  summary(model10_sg_weighted)$adj.r.squared,
  summary(model10_sg_poly)$adj.r.squared,
  summary(model10_sg_bucket)$adj.r.squared
)

# Print comparison table
print("Model Comparison:")
print(model_comparison[, c("Model", "df", "AIC", "BIC", "Adj.R.squared")])

# Test for model improvement with polynomial terms vs linear
anova_test_poly <- anova(model10_sg_fa, model10_sg_poly)
print("ANOVA test comparing Linear vs Polynomial models:")
print(anova_test_poly)

# VISUALISATIONS
# Define the year range for prediction
year_range <- seq(min(steam_data_fa_filtered$release_year_numeric, na.rm = TRUE), 
                  max(steam_data_fa_filtered$release_year_numeric, na.rm = TRUE), by = 1)
year_centered <- year_range - mean(steam_data_fa_filtered$release_year_numeric, na.rm = TRUE)
loc_levels <- unique(steam_data_fa_filtered$sc_localisation_combo)

# 1. Linear Model Visualisation
pred_data <- expand.grid(
  release_year_centered = year_centered,
  full_localisation_combo = loc_levels
)

# Add mean values for factor scores
for(var in c("log_total_reviews", 
             "City_Builders_Economic_Management_Sims",
             "Detective_Interactive_Narrative_Games",
             "Atmospheric_Retro_RPGs",
             "Turn_Based_Tactical_Party_RPGs",
             "Couch_Co_Op_Local_Multiplayer_Games",
             "Free_to_Play_MMOs_Competitive_Online_Games",
             "Military_Strategy_War_Simulations",
             "Casual_Family_Friendly_Games",
             "Skill_Based_Action_RPGs_Souls_likes",
             "Immersive_3D_Simulations_Driving_Games",
             "Survival_Horror_Dark_Adventure",
             "Chaotic_Top_Down_Shooters_Roguelites",
             "Roguelike_Deckbuilders_Procedural_Indie_Games",
             "Sci_Fi_Space_Futuristic_Simulation",
             "Comedy_Dark_Humor")) {
  pred_data[[var]] <- mean(steam_data_fa_filtered[[var]], na.rm = TRUE)
}

pred_data$predicted_gap <- predict(model10_sg_fa, newdata = pred_data)
pred_data$release_year <- year_range

ggplot(pred_data, aes(x = release_year, y = predicted_gap, color = full_localisation_combo)) +
  geom_line() +
  labs(title = "Change in Sentiment Gap Over Time by Localisation Level (Linear)",
       x = "Release Year",
       y = "Predicted Sentiment Gap",
       color = "Localisation Level") +
  theme_minimal()

# 2. Polynomial Model Visualisation
poly_pred_data <- expand.grid(
  release_year_centered = year_centered,
  full_localisation_combo = loc_levels
)

poly_pred_data$release_year_centered_squared <- poly_pred_data$release_year_centered^2
poly_pred_data$release_year_centered_cubed <- poly_pred_data$release_year_centered^3

for(var in c("log_total_reviews", 
             "City_Builders_Economic_Management_Sims",
             "Detective_Interactive_Narrative_Games",
             "Atmospheric_Retro_RPGs",
             "Turn_Based_Tactical_Party_RPGs",
             "Couch_Co_Op_Local_Multiplayer_Games",
             "Free_to_Play_MMOs_Competitive_Online_Games",
             "Military_Strategy_War_Simulations",
             "Casual_Family_Friendly_Games",
             "Skill_Based_Action_RPGs_Souls_likes",
             "Immersive_3D_Simulations_Driving_Games",
             "Survival_Horror_Dark_Adventure",
             "Chaotic_Top_Down_Shooters_Roguelites",
             "Roguelike_Deckbuilders_Procedural_Indie_Games",
             "Sci_Fi_Space_Futuristic_Simulation",
             "Comedy_Dark_Humor")) {
  poly_pred_data[[var]] <- mean(steam_data_fa_filtered[[var]], na.rm = TRUE)
}

poly_pred_data$predicted_gap <- predict(model10_sg_poly, newdata = poly_pred_data)
poly_pred_data$release_year <- year_range

ggplot(poly_pred_data, aes(x = release_year, y = predicted_gap, color = full_localisation_combo)) +
  geom_line() +
  labs(title = "Non-Linear Change in Sentiment Gap Over Time by Localisation Level",
       x = "Release Year",
       y = "Predicted Sentiment Gap",
       color = "Localisation Level") +
  theme_minimal()

# 3. Time Period Bucketing Visualisation
ggplot(steam_data_fa_filtered, aes(x = time_period, y = sentiment_gap, 
                                   color = full_localisation_combo)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = full_localisation_combo)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Mean Sentiment Gap by Time Period and Localisation Level",
       x = "Time Period",
       y = "Mean Sentiment Gap",
       color = "Localisation Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------------------------------------------------------
# Testing Genre-Based Hypothesis with Robust Standard Errors
# -------------------------------------------------------------------------
# Create Visual Novel tag if it doesn't exist
if(!"has_visual_novel" %in% names(steam_data_filtered)) {
  steam_data_filtered <- steam_data_filtered %>%
    mutate(
      has_visual_novel = as.numeric(grepl("Visual Novel", game_tags, ignore.case = TRUE))
    )
}

# Identify text-heavy games (Story Rich or Visual Novel)
steam_data_filtered <- steam_data_filtered %>%
  mutate(
    text_heavy_game = as.numeric(has_story_rich == 1 | has_visual_novel == 1)
  )

# Check distribution of the visual novel tag
table(steam_data_filtered$has_visual_novel)

# Check distribution of text-heavy games
table(steam_data_filtered$text_heavy_game)

# Model with text-heavy game interaction
model11_sg <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*text_heavy_game + 
    release_year + log_total_reviews,
  data = steam_data_filtered
)

# Apply robust standard errors
robust_model11_sg <- coeftest(model11_sg, vcov = vcovHC(model11_sg, type = "HC1"))
print(robust_model11_sg)

# Model with text-heavy game interaction (WLS)
model11_sg_wls <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*text_heavy_game + 
    release_year + log_total_reviews,
  data = steam_data_filtered, weights = sentiment_weights
)
# Apply robust standard errors
robust_model11_sg_wls <- coeftest(model11_sg_wls, vcov = vcovHC(model11_sg_wls, type = "HC1"))
print(robust_model11_sg_wls)

# Base model without interaction for comparison
model11_base_wls <- lm(
  sentiment_gap ~ factor(full_localisation_combo) + text_heavy_game + 
    release_year + log_total_reviews,
  data = steam_data_filtered, weights = sentiment_weights
)

# Compare models using Adjusted R-squared
model11_base_r2 <- summary(model11_base_wls)$adj.r.squared
model11_int_r2 <- summary(model11_sg_wls)$adj.r.squared
cat("Adjusted R-squared comparison (text-heavy models):", model11_base_r2, "vs", model11_int_r2, "\n")

# Robust Wald test for comparing nested models
waldtest_text_wls <- waldtest(model11_base_wls, model11_sg_wls, vcov = vcovHC)
print(waldtest_text_wls)

# Visualize the interaction
ggplot(steam_data_filtered, aes(x = full_localisation_combo, 
                                y = sentiment_gap, 
                                color = factor(text_heavy_game),
                                group = interaction(sc_localisation_combo, text_heavy_game))) +
  geom_boxplot() +
  labs(title = "Effect of Localisation on Sentiment Gap (Text-Heavy vs. Other Games)",
       x = "Simplified Chinese Localization",
       y = "Sentiment Gap (Chinese - Non-Chinese)",
       color = "Text-Heavy Game") +
  scale_color_discrete(labels = c("Not Text-Heavy", "Text-Heavy")) +
  theme_minimal()

# -------------------------------------------------------------------------
# Price-Based Hypothesis Testing with Statistical Price Categories
# -------------------------------------------------------------------------
# Model with indie game interaction (OLS)
model13_sg <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*has_indie + 
    release_year + log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa_filtered
)

# Model with indie game interaction (WLS)
model13_sg_wls <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*has_indie + 
    release_year + log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa_filtered,
  weights = fa_sentiment_weights
)
summary(model13_sg_wls)

# Apply robust standard errors
robust_model13_sg_wls <- coeftest(model13_sg_wls, vcov = vcovHC(model13_sg_wls, type = "HC1"))
print(robust_model13_sg_wls)

# Create statistically-driven price categories for USD
steam_data_filtered <- steam_data_filtered %>%
  mutate(
    # First handle free games separately
    is_free_usd = price_usd_numeric == 0,
    # For non-free games, get non-zero prices
    non_zero_usd = ifelse(price_usd_numeric > 0, price_usd_numeric, NA)
  )

# Calculate quartiles for non-zero USD prices
usd_quartiles <- quantile(steam_data_filtered$non_zero_usd, 
                          probs = c(0.25, 0.5, 0.75), 
                          na.rm = TRUE)

# Create price categories based on quartiles
steam_data_filtered <- steam_data_filtered %>%
  mutate(
    price_usd_category = case_when(
      is_free_usd ~ "Free",
      non_zero_usd <= usd_quartiles[1] ~ "Low Price",
      non_zero_usd <= usd_quartiles[2] ~ "Medium-Low Price",
      non_zero_usd <= usd_quartiles[3] ~ "Medium-High Price",
      non_zero_usd > usd_quartiles[3] ~ "High Price",
      TRUE ~ NA_character_
    ),
    price_usd_category = factor(price_usd_category, 
                                levels = c("Free", "Low Price", "Medium-Low Price", 
                                           "Medium-High Price", "High Price"))
  )

# Create market-based price categories for USD
steam_data_filtered <- steam_data_filtered %>%
  mutate(
    price_usd_category = case_when(
      price_usd_numeric == 0 ~ "Free",
      price_usd_numeric <= 10 ~ "Budget/Indie",
      price_usd_numeric <= 30 ~ "Mid-tier",
      price_usd_numeric <= 50 ~ "Major Release",
      price_usd_numeric > 50 ~ "Premium AAA",
      TRUE ~ NA_character_
    ),
    price_usd_category = factor(price_usd_category, 
                                levels = c("Free", "Budget/Indie", "Mid-tier", 
                                           "Major Release", "Premium AAA"))
  )

# Print USD price category distribution
table(steam_data_filtered$price_usd_category, useNA = "ifany")

# Create statistically-driven price categories for CNY
steam_data_filtered <- steam_data_filtered %>%
  mutate(
    # First handle free games separately
    is_free_cny = price_cny_numeric == 0,
    # For non-free games, get non-zero prices
    non_zero_cny = ifelse(price_cny_numeric > 0, price_cny_numeric, NA)
  )

# Calculate quartiles for non-zero CNY prices
cny_quartiles <- quantile(steam_data_filtered$non_zero_cny, 
                          probs = c(0.25, 0.5, 0.75), 
                          na.rm = TRUE)

# Create price categories based on quartiles
steam_data_filtered <- steam_data_filtered %>%
  mutate(
    price_cny_category = case_when(
      is_free_cny ~ "Free",
      non_zero_cny <= cny_quartiles[1] ~ "Low Price",
      non_zero_cny <= cny_quartiles[2] ~ "Medium-Low Price",
      non_zero_cny <= cny_quartiles[3] ~ "Medium-High Price",
      non_zero_cny > cny_quartiles[3] ~ "High Price",
      TRUE ~ NA_character_
    ),
    price_cny_category = factor(price_cny_category, 
                                levels = c("Free", "Low Price", "Medium-Low Price", 
                                           "Medium-High Price", "High Price"))
  )

# Create market-based price categories for CNY
steam_data_filtered <- steam_data_filtered %>%
  mutate(
    price_cny_category = case_when(
      price_cny_numeric == 0 ~ "Free",
      price_cny_numeric <= 50 ~ "Budget/Indie",
      price_cny_numeric <= 120 ~ "Mid-tier",
      price_cny_numeric <= 200 ~ "Major Release",
      price_cny_numeric > 200 ~ "Premium AAA",
      TRUE ~ NA_character_
    ),
    price_cny_category = factor(price_cny_category, 
                                levels = c("Free", "Budget/Indie", "Mid-tier", 
                                           "Major Release", "Premium AAA"))
  )

# Print CNY price category distribution
table(steam_data_filtered$price_cny_category, useNA = "ifany")

# Count NAs in price variables
cat("Number of NA values in USD price category:", sum(is.na(steam_data_filtered$price_usd_category)), "\n")
cat("Number of NA values in CNY price category:", sum(is.na(steam_data_filtered$price_cny_category)), "\n")

# Model with USD price category interaction
model12_usd_sg <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*price_usd_category + 
    release_year + log_total_reviews,
  data = filter(steam_data_filtered, !is.na(price_usd_category))
)

# Apply robust standard errors
robust_model12_usd_sg <- coeftest(model12_usd_sg, vcov = vcovHC(model12_usd_sg, type = "HC3"))
print(robust_model12_usd_sg)

# Model with CNY price category interaction
model12_cny_sg <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*price_cny_category + 
    release_year + log_total_reviews,
  data = filter(steam_data_filtered, !is.na(price_cny_category))
)

# Apply robust standard errors
robust_model12_cny_sg <- coeftest(model12_cny_sg, vcov = vcovHC(model12_cny_sg, type = "HC3"))
print(robust_model12_cny_sg)

# ANOVA to test significance of interactions
cat("ANOVA for USD price interaction model:\n")
print(Anova(model12_usd_sg, type = "II", white.adjust = TRUE))

cat("ANOVA for CNY price interaction model:\n")
print(Anova(model12_cny_sg, type = "II", white.adjust = TRUE))

# USD Price Category Interaction (WLS)
model12_usd_sg_wls <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*price_usd_category + 
    release_year + log_total_reviews,
  data = filter(steam_data_filtered, !is.na(price_usd_category)), 
  weights = sentiment_weights[!is.na(steam_data_filtered$price_usd_category)]
)
# Apply robust standard errors
robust_model12_usd_sg_wls <- coeftest(model12_usd_sg_wls, vcov = vcovHC(model12_usd_sg_wls, type = "HC1"))
print(robust_model12_usd_sg_wls)

# CNY Price Category Interaction (WLS)
model12_cny_sg_wls <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*price_cny_category + 
    release_year + log_total_reviews,
  data = filter(steam_data_filtered, !is.na(price_cny_category)), 
  weights = sentiment_weights[!is.na(steam_data_filtered$price_cny_category)]
)
# Apply robust standard errors
robust_model12_cny_sg_wls <- coeftest(model12_cny_sg_wls, vcov = vcovHC(model12_cny_sg_wls, type = "HC1"))
print(robust_model12_cny_sg_wls)

# Base models without interaction for comparison
model12_usd_base_wls <- lm(
  sentiment_gap ~ factor(full_localisation_combo) + price_usd_category + 
    release_year + log_total_reviews,
  data = filter(steam_data_filtered, !is.na(price_usd_category)), 
  weights = sentiment_weights[!is.na(steam_data_filtered$price_usd_category)]
)

model12_cny_base_wls <- lm(
  sentiment_gap ~ factor(full_localisation_combo) + price_cny_category + 
    release_year + log_total_reviews,
  data = filter(steam_data_filtered, !is.na(price_cny_category)), 
  weights = sentiment_weights[!is.na(steam_data_filtered$price_cny_category)]
)

# Compare models using Adjusted R-squared
model12_usd_base_r2 <- summary(model12_usd_base_wls)$adj.r.squared
model12_usd_int_r2 <- summary(model12_usd_sg_wls)$adj.r.squared
model12_cny_base_r2 <- summary(model12_cny_base_wls)$adj.r.squared
model12_cny_int_r2 <- summary(model12_cny_sg_wls)$adj.r.squared

cat("USD Price: Adjusted R-squared comparison:", model12_usd_base_r2, "vs", model12_usd_int_r2, "\n")
cat("CNY Price: Adjusted R-squared comparison:", model12_cny_base_r2, "vs", model12_cny_int_r2, "\n")

# ANOVA tests for significant interaction effects
cat("ANOVA for USD price interaction model (WLS):\n")
print(Anova(model12_usd_sg_wls, type = "II", white.adjust = TRUE))

cat("ANOVA for CNY price interaction model (WLS):\n")
print(Anova(model12_cny_sg_wls, type = "II", white.adjust = TRUE))

# Robust Wald tests for comparing nested models
waldtest_usd_price_wls <- waldtest(model12_usd_base_wls, model12_usd_sg_wls, vcov = vcovHC)
print("Wald test for USD price interaction:")
print(waldtest_usd_price_wls)

waldtest_cny_price_wls <- waldtest(model12_cny_base_wls, model12_cny_sg_wls, vcov = vcovHC)
print("Wald test for CNY price interaction:")
print(waldtest_cny_price_wls)

# Visualize the USD price interaction
ggplot(filter(steam_data_filtered, !is.na(price_usd_category)), 
       aes(x = full_localisation_combo, 
           y = sentiment_gap, 
           color = price_usd_category,
           group = interaction(full_localisation_combo, price_usd_category))) +
  geom_boxplot() +
  labs(title = "Effect of Localisation on Sentiment Gap by USD Price Category",
       x = "Chinese Localisation",
       y = "Sentiment Gap (Chinese - Non-Chinese)",
       color = "USD Price Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Visualize the CNY price interaction
ggplot(filter(steam_data_filtered, !is.na(price_cny_category)), 
       aes(x = full_localisation_combo, 
           y = sentiment_gap, 
           color = price_cny_category,
           group = interaction(full_localisation_combo, price_cny_category))) +
  geom_boxplot() +
  labs(title = "Effect of Localisation on Sentiment Gap by CNY Price Category",
       x = "Chinese Localization",
       y = "Sentiment Gap (Chinese - Non-Chinese)",
       color = "CNY Price Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


# -------------------------------------------------------------------------
# TESTING ADDITIONAL HYPOTHESES WHEN SC REVIEW RATIO IS DEPENDENT VARIABLE
# -------------------------------------------------------------------------

# HYPOTHESIS 1: A shrinking localisation effect over time
# Create a numeric year variable for interaction
steam_data_fa <- steam_data_fa %>%
  mutate(
    release_year_numeric = as.numeric(substr(release_date, 1, 4)),
    release_year_centered = release_year_numeric - mean(release_year_numeric, na.rm = TRUE)
  )

# Model with year interaction (using factor scores from model13)
model15 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo)*release_year_centered + 
    log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa
)

summary(model15)

# Transfer coefficient to odds ratio
odds_model15 <- exp(coef(model15))
print(odds_model15)

# Compare models using AIC and BIC
AIC(model13, model15)
BIC(model13, model15)

# Test significance of the interaction
lrtest_1315 <- lrtest(model13, model15)
print(lrtest_1315)

# Create polynomial terms for release year if they don't already exist
steam_data_fa <- steam_data_fa %>%
  mutate(
    release_year_centered_squared = release_year_centered^2,
    release_year_centered_cubed = release_year_centered^3
  )

# Beta regression model with polynomial time trends
model20 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo)*(release_year_centered + 
                                                    release_year_centered_squared + 
                                                    release_year_centered_cubed) + 
    log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa
)

summary(model20)

# Transfer coefficient to odds ratio
odds_model20 <- exp(coef(model20))
print(odds_model20)

# Compare with linear time trend model (model15)
AIC(model15, model20)
BIC(model15, model20)

# Test significance of the polynomial terms
lrtest_1520 <- lrtest(model15, model20)
print(lrtest_1520)

# Visualize the interaction effect
# Create prediction data
year_range <- seq(min(steam_data_fa$release_year_numeric, na.rm = TRUE), 
                  max(steam_data_fa$release_year_numeric, na.rm = TRUE), by = 1)
year_centered <- year_range - mean(steam_data_fa$release_year_numeric, na.rm = TRUE)

# Use only localisation levels that exist in the dataset
loc_levels <- unique(steam_data_fa$full_localisation_combo)

# Create prediction grid
pred_data_time <- expand.grid(
  release_year_centered = year_centered,
  full_localisation_combo = loc_levels
)

# Add mean values for other predictors
for(var in c("log_total_reviews", 
             "City_Builders_Economic_Management_Sims",
             "Detective_Interactive_Narrative_Games",
             "Atmospheric_Retro_RPGs",
             "Turn_Based_Tactical_Party_RPGs",
             "Couch_Co_Op_Local_Multiplayer_Games",
             "Free_to_Play_MMOs_Competitive_Online_Games",
             "Military_Strategy_War_Simulations",
             "Casual_Family_Friendly_Games",
             "Skill_Based_Action_RPGs_Souls_likes",
             "Immersive_3D_Simulations_Driving_Games",
             "Survival_Horror_Dark_Adventure",
             "Chaotic_Top_Down_Shooters_Roguelites",
             "Roguelike_Deckbuilders_Procedural_Indie_Games",
             "Sci_Fi_Space_Futuristic_Simulation",
             "Comedy_Dark_Humor")) {
  pred_data_time[[var]] <- mean(steam_data_fa[[var]], na.rm = TRUE)
}

# Get predictions (on the scale of the linear predictor)
pred_data_time$predicted_ratio <- predict(model15, newdata = pred_data_time, type = "link")
# Transform to response scale (probability scale)
pred_data_time$predicted_ratio <- plogis(pred_data_time$predicted_ratio)

# Add actual year for plotting
pred_data_time$release_year <- year_range

# Plot the interaction
ggplot(pred_data_time, aes(x = release_year, y = predicted_ratio, color = full_localisation_combo)) +
  geom_line() +
  labs(title = "Change in Chinese Review Ratio Over Time by Localisation Level",
       x = "Release Year",
       y = "Predicted Chinese Review Ratio",
       color = "Localisation Level") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Visualize the polynomial interaction
# Create prediction data for polynomial model
year_range <- seq(min(steam_data_fa$release_year_numeric, na.rm = TRUE), 
                  max(steam_data_fa$release_year_numeric, na.rm = TRUE), by = 1)
year_centered <- year_range - mean(steam_data_fa$release_year_numeric, na.rm = TRUE)

# Use only localisation levels that exist in the dataset
loc_levels <- unique(steam_data_fa$full_localisation_combo)

# Create prediction grid
pred_data_poly <- expand.grid(
  release_year_centered = year_centered,
  full_localisation_combo = loc_levels
)

# Add polynomial terms
pred_data_poly$release_year_centered_squared <- pred_data_poly$release_year_centered^2
pred_data_poly$release_year_centered_cubed <- pred_data_poly$release_year_centered^3

# Add mean values for other predictors
for(var in c("log_total_reviews", 
             "City_Builders_Economic_Management_Sims",
             "Detective_Interactive_Narrative_Games",
             "Atmospheric_Retro_RPGs",
             "Turn_Based_Tactical_Party_RPGs",
             "Couch_Co_Op_Local_Multiplayer_Games",
             "Free_to_Play_MMOs_Competitive_Online_Games",
             "Military_Strategy_War_Simulations",
             "Casual_Family_Friendly_Games",
             "Skill_Based_Action_RPGs_Souls_likes",
             "Immersive_3D_Simulations_Driving_Games",
             "Survival_Horror_Dark_Adventure",
             "Chaotic_Top_Down_Shooters_Roguelites",
             "Roguelike_Deckbuilders_Procedural_Indie_Games",
             "Sci_Fi_Space_Futuristic_Simulation",
             "Comedy_Dark_Humor")) {
  pred_data_poly[[var]] <- mean(steam_data_fa[[var]], na.rm = TRUE)
}

# Get predictions (on the scale of the linear predictor)
pred_data_poly$predicted_ratio <- predict(model20, newdata = pred_data_poly, type = "link")
# Transform to response scale (probability scale)
pred_data_poly$predicted_ratio <- plogis(pred_data_poly$predicted_ratio)

# Add actual year for plotting
pred_data_poly$release_year <- year_range

# Plot the interaction with polynomial trends
ggplot(pred_data_poly, aes(x = release_year, y = predicted_ratio, color = full_localisation_combo)) +
  geom_line() +
  labs(title = "Non-Linear Change in Chinese Review Ratio Over Time by Localisation Level",
       x = "Release Year",
       y = "Predicted Chinese Review Ratio",
       color = "Localisation Level") +
  theme_minimal() +
  theme(legend.position = "bottom")


# HYPOTHESIS 2: Localisation matters more for text-intensive games
# Create a text-heavy game indicator (Story Rich or Visual Novel)
if(!"has_visual_novel" %in% names(steam_data_fa)) {
  steam_data_fa <- steam_data_fa %>%
    mutate(
      has_visual_novel = as.numeric(grepl("Visual Novel", game_tags, ignore.case = TRUE))
    )
}

# Create has_story_rich if it doesn't exist
if(!"has_story_rich" %in% names(steam_data_fa)) {
  steam_data_fa <- steam_data_fa %>%
    mutate(
      has_story_rich = as.numeric(grepl("Story Rich", game_tags, ignore.case = TRUE))
    )
}

# Create text-heavy game indicator
steam_data_fa <- steam_data_fa %>%
  mutate(
    text_heavy_game = as.numeric(has_story_rich == 1 | has_visual_novel == 1)
  )


# Check distribution
table(steam_data_fa$text_heavy_game, useNA = "ifany")

# Model with text-heavy game interaction
model16 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo)*text_heavy_game + 
    release_year + log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa
)

summary(model16)

# Transfer coefficient to odds ratio
odds_model16 <- exp(coef(model16))
print(odds_model16)

# Compare models
AIC(model13, model16)
BIC(model13, model16)

# Test significance of the interaction
lrtest_1316 <- lrtest(model13, model16)
print(lrtest_1316)

# Visualise the interaction
# Create boxplot to visualize effect
ggplot(steam_data_fa, aes(x = full_localisation_combo, 
                          y = sc_ratio_adj, 
                          color = factor(text_heavy_game),
                          group = interaction(full_localisation_combo, text_heavy_game))) +
  geom_boxplot() +
  labs(title = "Effect of Localisation on Chinese Review Ratio (Text-Heavy vs. Other Games)",
       x = "Chinese Localization",
       y = "Adjusted Chinese Review Ratio",
       color = "Text-Heavy Game") +
  scale_color_discrete(labels = c("Not Text-Heavy", "Text-Heavy")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# HYPOTHESIS 3: People care less about localisation for cheaper / indie games
# Part A: Testing with indie games

# Model with indie game interaction
model17 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo)*has_indie + 
    release_year + log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = steam_data_fa
)

summary(model17)

# Transfer coefficient to odds ratio
odds_model17 <- exp(coef(model17))
print(odds_model17)

# Compare models
AIC(model13, model17)
BIC(model13, model17)

# Test significance of the interaction
lrtest_1317 <- lrtest(model13, model17)
print(lrtest_1317)

# Visualise the indie game interaction
ggplot(steam_data_fa, aes(x = full_localisation_combo, 
                          y = sc_ratio_adj, 
                          color = factor(has_indie),
                          group = interaction(full_localisation_combo, has_indie))) +
  geom_boxplot() +
  labs(title = "Effect of Localisation on Chinese Review Ratio (Indie vs. Non-Indie)",
       x = "Chinese Localization",
       y = "Adjusted Chinese Review Ratio",
       color = "Indie Game") +
  scale_color_discrete(labels = c("Non-Indie", "Indie")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Part B: Testing with price categories
# --------------------------------
# 1. USD Price Categories
# --------------------------------
# Create statistically-driven price categories for USD
steam_data_fa <- steam_data_fa %>%
  mutate(
    # First handle free games separately
    is_free_usd = price_usd_numeric == 0,
    # For non-free games, get non-zero prices
    non_zero_usd = ifelse(price_usd_numeric > 0, price_usd_numeric, NA)
  )

# Calculate quartiles for non-zero USD prices
usd_quartiles <- quantile(steam_data_fa$non_zero_usd, 
                          probs = c(0.25, 0.5, 0.75), 
                          na.rm = TRUE)

# Create price categories based on quartiles
steam_data_fa <- steam_data_fa %>%
  mutate(
    price_usd_category = case_when(
      is_free_usd ~ "Free",
      non_zero_usd <= usd_quartiles[1] ~ "Low Price",
      non_zero_usd <= usd_quartiles[2] ~ "Medium-Low Price",
      non_zero_usd <= usd_quartiles[3] ~ "Medium-High Price",
      non_zero_usd > usd_quartiles[3] ~ "High Price",
      TRUE ~ NA_character_
    ),
    price_usd_category = factor(price_usd_category, 
                                levels = c("Free", "Low Price", "Medium-Low Price", 
                                           "Medium-High Price", "High Price"))
  )

# Create market-based price categories for USD in steam_data_fa
steam_data_fa <- steam_data_fa %>%
  mutate(
    price_usd_category = case_when(
      price_usd_numeric == 0 ~ "Free",
      price_usd_numeric <= 10 ~ "Budget/Indie",
      price_usd_numeric <= 30 ~ "Mid-tier",
      price_usd_numeric <= 50 ~ "Major Release",
      price_usd_numeric > 50 ~ "Premium AAA",
      TRUE ~ NA_character_
    ),
    price_usd_category = factor(price_usd_category, 
                                levels = c("Free", "Budget/Indie", "Mid-tier", 
                                           "Major Release", "Premium AAA"))
  )

# Print USD price category distribution
print("Distribution of USD price categories:")
table(steam_data_fa$price_usd_category, useNA = "ifany")

# Model with USD price category interaction
model18 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo)*price_usd_category + 
    release_year + log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = filter(steam_data_fa, !is.na(price_usd_category))
)

summary(model18)

# Transfer coefficient to odds ratio
odds_model18 <- exp(coef(model18))
print(odds_model18)

# Compare models (restricting model13 to same data for fair comparison)
model13_price_usd_subset <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo) + 
    release_year + log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = filter(steam_data_fa, !is.na(price_usd_category))
)

AIC(model13_price_usd_subset, model18)
BIC(model13_price_usd_subset, model18)

# Test significance of the interaction
lrtest_price_usd <- lrtest(model13_price_usd_subset, model18)
print(lrtest_price_usd)

# Visualize the USD price interaction
ggplot(filter(steam_data_fa, !is.na(price_usd_category)), 
       aes(x = full_localisation_combo, 
           y = sc_ratio_adj, 
           color = price_usd_category,
           group = interaction(full_localisation_combo, price_usd_category))) +
  geom_boxplot() +
  labs(title = "Effect of Localisation on Chinese Review Ratio by USD Price Category",
       x = "Chinese Localization",
       y = "Adjusted Chinese Review Ratio",
       color = "USD Price Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# --------------------------------
# 2. CNY Price Categories
# --------------------------------
# Create statistically-driven price categories for CNY
steam_data_fa <- steam_data_fa %>%
  mutate(
    # First handle free games separately
    is_free_cny = price_cny_numeric == 0,
    # For non-free games, get non-zero prices
    non_zero_cny = ifelse(price_cny_numeric > 0, price_cny_numeric, NA)
  )

# Calculate quartiles for non-zero CNY prices
cny_quartiles <- quantile(steam_data_fa$non_zero_cny, 
                          probs = c(0.25, 0.5, 0.75), 
                          na.rm = TRUE)

# Create price categories based on quartiles
steam_data_fa <- steam_data_fa %>%
  mutate(
    price_cny_category = case_when(
      is_free_cny ~ "Free",
      non_zero_cny <= cny_quartiles[1] ~ "Low Price",
      non_zero_cny <= cny_quartiles[2] ~ "Medium-Low Price",
      non_zero_cny <= cny_quartiles[3] ~ "Medium-High Price",
      non_zero_cny > cny_quartiles[3] ~ "High Price",
      TRUE ~ NA_character_
    ),
    price_cny_category = factor(price_cny_category, 
                                levels = c("Free", "Low Price", "Medium-Low Price", 
                                           "Medium-High Price", "High Price"))
  )

# Create market-based price categories for CNY in steam_data_fa
steam_data_fa <- steam_data_fa %>%
  mutate(
    price_cny_category = case_when(
      price_cny_numeric == 0 ~ "Free",
      price_cny_numeric <= 50 ~ "Budget/Indie",
      price_cny_numeric <= 120 ~ "Mid-tier",
      price_cny_numeric <= 200 ~ "Major Release",
      price_cny_numeric > 200 ~ "Premium AAA",
      TRUE ~ NA_character_
    ),
    price_cny_category = factor(price_cny_category, 
                                levels = c("Free", "Budget/Indie", "Mid-tier", 
                                           "Major Release", "Premium AAA"))
  )


# Print CNY price category distribution
print("Distribution of CNY price categories:")
table(steam_data_fa$price_cny_category, useNA = "ifany")

# Model with CNY price category interaction
model19 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo)*price_cny_category + 
    release_year + log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = filter(steam_data_fa, !is.na(price_cny_category))
)

summary(model19)

# Transfer coefficient to odds ratio
odds_model19 <- exp(coef(model19))
print(odds_model19)

# Compare models (restricting model13 to same data for fair comparison)
model13_price_cny_subset <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo) + 
    release_year + log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Casual_Family_Friendly_Games +
    Skill_Based_Action_RPGs_Souls_likes +
    Immersive_3D_Simulations_Driving_Games +
    Survival_Horror_Dark_Adventure +
    Chaotic_Top_Down_Shooters_Roguelites +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Sci_Fi_Space_Futuristic_Simulation +
    Comedy_Dark_Humor,
  data = filter(steam_data_fa, !is.na(price_cny_category))
)

AIC(model13_price_cny_subset, model19)
BIC(model13_price_cny_subset, model19)

# Test significance of the interaction
lrtest_price_cny <- lrtest(model13_price_cny_subset, model19)
print(lrtest_price_cny)

# Visualize the CNY price interaction
ggplot(filter(steam_data_fa, !is.na(price_cny_category)), 
       aes(x = full_localisation_combo, 
           y = sc_ratio_adj, 
           color = price_cny_category,
           group = interaction(full_localisation_combo, price_cny_category))) +
  geom_boxplot() +
  labs(title = "Effect of Localisation on Chinese Review Ratio by CNY Price Category",
       x = "Chinese Localization",
       y = "Adjusted Chinese Review Ratio",
       color = "CNY Price Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# --------------------------------
# 3. Compare USD and CNY Price Models
# --------------------------------
# Create a comparison table for USD and CNY price models
price_model_comparison <- data.frame(
  Model = c("USD Price Interaction (18)", "CNY Price Interaction (19)"),
  Pseudo_R_Squared = c(summary(model18)$pseudo.r.squared,
                       summary(model19)$pseudo.r.squared),
  AIC = c(AIC(model18), AIC(model19)),
  BIC = c(BIC(model18), BIC(model19)),
  Parameters = c(length(coef(model18)), length(coef(model19))),
  LRT_p_value = c(lrtest_price_usd$"Pr(>Chisq)"[2], 
                  lrtest_price_cny$"Pr(>Chisq)"[2])
)

print(price_model_comparison)


# -------------------------------------------------------------------------
# TRANSFORMATIONS FOR ADDRESSING HETEROSCEDASTICITY IN SENTIMENT GAP
# -------------------------------------------------------------------------

# Create a copy of the filtered dataset for transformations
steam_data_transformed <- steam_data_filtered

# 1. Yeo-Johnson Transformation
# Find optimal lambda for transformation
yj_transform <- powerTransform(steam_data_filtered$sentiment_gap, family="yjPower")
lambda <- yj_transform$lambda
print(paste("Optimal lambda for Yeo-Johnson transformation:", round(lambda, 4)))

# Apply transformation with the optimal lambda
steam_data_transformed$sentiment_gap_yj <- yjPower(steam_data_filtered$sentiment_gap, lambda)

# Visualize the transformed variable
hist(steam_data_transformed$sentiment_gap_yj, 
     main = "Distribution of Transformed Sentiment Gap (Yeo-Johnson)", 
     xlab = "Transformed Sentiment Gap")

# Run model with transformed variable
model_yj <- lm(sentiment_gap_yj ~ factor(sc_localisation_combo) + release_year, 
               data = steam_data_transformed)
summary(model_yj)

# Test for heteroscedasticity on the new model
bp_test_model_yj <- bptest(model_yj)
print("Breusch-Pagan test for Yeo-Johnson transformed model:")
print(bp_test_model_yj)

# 2. Signed Square Root Transformation
# Apply signed square root transformation
steam_data_transformed$sentiment_gap_sqrt <- sign(steam_data_filtered$sentiment_gap) * 
  sqrt(abs(steam_data_filtered$sentiment_gap))

# Visualize 
hist(steam_data_transformed$sentiment_gap_sqrt, 
     main = "Distribution of Transformed Sentiment Gap (Signed Square Root)", 
     xlab = "Transformed Sentiment Gap")

# Run model with transformed variable
model_sqrt <- lm(sentiment_gap_sqrt ~ factor(sc_localisation_combo) + release_year, 
                 data = steam_data_transformed)
summary(model_sqrt)

# Test for heteroscedasticity
bp_test_model_sqrt <- bptest(model_sqrt)
print("Breusch-Pagan test for Square Root transformed model:")
print(bp_test_model_sqrt)

# 3. Inverse Hyperbolic Sine Transformation
# Apply inverse hyperbolic sine transformation
steam_data_transformed$sentiment_gap_ihs <- asinh(steam_data_filtered$sentiment_gap)

# Visualize
hist(steam_data_transformed$sentiment_gap_ihs, 
     main = "Distribution of Transformed Sentiment Gap (IHS)", 
     xlab = "Transformed Sentiment Gap")

# Run model with transformed variable
model_ihs <- lm(sentiment_gap_ihs ~ factor(sc_localisation_combo) + release_year, 
                data = steam_data_transformed)
summary(model_ihs)

# Test for heteroscedasticity
bp_test_model_ihs <- bptest(model_ihs)
print("Breusch-Pagan test for IHS transformed model:")
print(bp_test_model_ihs)

# 4. Quantile Transformation (install bestNormalize package if needed)
if (!requireNamespace("bestNormalize", quietly = TRUE)) {
  install.packages("bestNormalize")
}
library(bestNormalize)

# Apply quantile transformation
qt_obj <- bestNormalize::orderNorm(steam_data_filtered$sentiment_gap)
steam_data_transformed$sentiment_gap_qt <- predict(qt_obj)

# Visualize
hist(steam_data_transformed$sentiment_gap_qt, 
     main = "Distribution of Transformed Sentiment Gap (Quantile)", 
     xlab = "Transformed Sentiment Gap")

# Run model with transformed variable
model_qt <- lm(sentiment_gap_qt ~ factor(sc_localisation_combo) + release_year, 
               data = steam_data_transformed)
summary(model_qt)

# Test for heteroscedasticity
bp_test_model_qt <- bptest(model_qt)
print("Breusch-Pagan test for Quantile transformed model:")
print(bp_test_model_qt)

# 5. Compare all transformations
transformations <- c("Original", "Yeo-Johnson", "Signed Square Root", "IHS", "Quantile")
bp_p_values <- c(
  bp_test_model1_sg$p.value,
  bp_test_model_yj$p.value,
  bp_test_model_sqrt$p.value,
  bp_test_model_ihs$p.value,
  bp_test_model_qt$p.value
)

comparison_df <- data.frame(
  Transformation = transformations,
  BP_Test_p_value = round(bp_p_values, 4),
  Heteroscedasticity = ifelse(bp_p_values < 0.05, "Present", "Resolved")
)

print("Comparison of transformations for addressing heteroscedasticity:")
print(comparison_df)

# Select the best transformation based on BP test results
best_transform <- comparison_df[which.max(comparison_df$BP_Test_p_value),]
cat("\nBest transformation method:", best_transform$Transformation, 
    "\nBP p-value:", best_transform$BP_Test_p_value,
    "\nHeteroscedasticity status:", best_transform$Heteroscedasticity)

# If a transformation successfully resolves heteroscedasticity, use it for further models
# Example: Assuming the quantile transformation is the best
if (any(comparison_df$Heteroscedasticity == "Resolved")) {
  best_transform_name <- comparison_df$Transformation[comparison_df$Heteroscedasticity == "Resolved"][1]
  cat("\n\nUsing", best_transform_name, "transformation for further models\n")
  
  # Example model with the best transformation
  if (best_transform_name == "Yeo-Johnson") {
    model_best <- lm(sentiment_gap_yj ~ factor(sc_localisation_combo) + release_year + log_total_reviews, 
                     data = steam_data_transformed)
  } else if (best_transform_name == "Signed Square Root") {
    model_best <- lm(sentiment_gap_sqrt ~ factor(sc_localisation_combo) + release_year + log_total_reviews, 
                     data = steam_data_transformed)
  } else if (best_transform_name == "IHS") {
    model_best <- lm(sentiment_gap_ihs ~ factor(sc_localisation_combo) + release_year + log_total_reviews, 
                     data = steam_data_transformed)
  } else if (best_transform_name == "Quantile") {
    model_best <- lm(sentiment_gap_qt ~ factor(sc_localisation_combo) + release_year + log_total_reviews, 
                     data = steam_data_transformed)
  }
  
  # Print model summary
  summary(model_best)
  
  # Check residual plots
  par(mfrow = c(2, 2))
  plot(model_best)
  par(mfrow = c(1, 1))
} else {
  cat("\n\nNone of the transformations fully resolved heteroscedasticity.",
      "Continuing with robust standard errors approach as already implemented.\n")
}

# Plot residuals against log_total_reviews
model0_residuals <- residuals(model0_sg)
plot(steam_data_filtered$log_total_reviews, model0_residuals,
     main = "Residuals vs Log Total Reviews",
     xlab = "Log of Total Reviews", ylab = "Residuals")
abline(h = 0, col = "red")

# Calculate variance by review count quantiles
steam_data_filtered$review_quantile <- cut(steam_data_filtered$total_reviews, 
                                           quantile(steam_data_filtered$total_reviews, 
                                                    probs = seq(0, 1, 0.25), 
                                                    na.rm = TRUE),
                                           include.lowest = TRUE, labels = 1:4)

variance_by_quantile <- aggregate(sentiment_gap ~ review_quantile, 
                                  data = steam_data_filtered, 
                                  FUN = var)
print(variance_by_quantile)

# Simplified model for Chinese Review Ratio (without game tag factors)
model17_simple <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo)*has_indie + 
    release_year + log_total_reviews,
  data = steam_data_fa
)
summary(model17_simple)

# Simplified model for Sentiment Gap (without game tag factors)
model13_sg_wls_simple <- lm(
  sentiment_gap ~ factor(full_localisation_combo)*has_indie + 
    release_year + log_total_reviews,
  data = steam_data_fa_filtered,
  weights = fa_sentiment_weights
)

# Get robust standard errors for the sentiment gap model
robust_model13_sg_wls_simple <- coeftest(model13_sg_wls_simple, vcov = vcovHC(model13_sg_wls_simple, type = "HC1"))

# Simplified model for text-heavy game interaction (without game tag factors)
model16_simple <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo)*text_heavy_game + 
    release_year + log_total_reviews,
  data = steam_data_fa
)
summary(model16_simple)
