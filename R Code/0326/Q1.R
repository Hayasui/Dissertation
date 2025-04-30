# Load necessary libraries
library(tidyverse)  # For data manipulation
library(tidytext)
library(car)        # For diagnostic tests
library(lmtest)     # For model diagnostics
library(betareg)    # For beta regression
library(ggplot2)    # For visualisations
library(ggeffects)
library(psych)    # For factor analysis
library(lmtest)   # For model comparison tests
library(tidyr)    # For data manipulation
library(purrr)    # For functional programming


# Read the data
setwd("C:/Users/Gairui/OneDrive - University College London/Dissertation/Data Analysis")
# If using RDS format (as mentioned in your documentation)
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

# Display top 100 tags
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

# Compare model fit
# For OLS models, extract R-squared
r2_model0 <- summary(model0)$r.squared
r2_model1 <- summary(model1)$r.squared

# For beta regression, extract pseudo R-squared
r2_model2 <- summary(model2)$pseudo.r.squared

# Create comparison table
model_comparison <- data.frame(
  Model = c("Standard OLS", "Logit-transformed OLS", "Beta Regression"),
  R_squared = c(r2_model0, r2_model1, r2_model2),
  AIC = c(AIC(model0), AIC(model1), AIC(model2))
)
print(model_comparison)

# Compare coefficient estimates
coef_model0 <- coef(model0)[2]  # Extract coefficient for localisation dummy
coef_model1 <- coef(model1)[2]  # For logit model
coef_model2 <- coef(model2)[2]  # For beta model (mean submodel)

# Create coefficient comparison
coef_comparison <- data.frame(
  Model = c("Standard OLS", "Logit-transformed OLS", "Beta Regression"),
  Coefficient = c(coef_model0, coef_model1, coef_model2)
)
print(coef_comparison)


# Compare predicted vs actual values
steam_data <- steam_data %>%
  mutate(
    pred_model0 = predict(model0),
    pred_model1 = predict(model1),
    pred_model2 = predict(model2, type = "response")
  )

# Correlation between predicted and actual values
cor_model0 <- cor(steam_data$sc_ratio, steam_data$pred_model0)
cor_model1 <- cor(steam_data$sc_ratio, exp(steam_data$pred_model1)/(1+exp(steam_data$pred_model1)))
cor_model2 <- cor(steam_data$sc_ratio, steam_data$pred_model2)

pred_comparison <- data.frame(
  Model = c("Standard OLS", "Logit-transformed OLS", "Beta Regression"),
  Correlation = c(cor_model0, cor_model1, cor_model2)
)
print(pred_comparison)

# Adding released year as a control variable to model2
# Extract year from release_date
steam_data <- steam_data %>%
  mutate(
    # Extract year from release_date (first 4 characters)
    release_year = as.factor(substr(release_date, 1, 4))
  )
# Check the distribution of years
table(steam_data$release_year)

# Beta regression with year fixed effects
model3 <- betareg(sc_ratio_adj ~ chinese_simplified_interface_subtitles_dummy + release_year, 
                  data = steam_data)
summary(model3) # plogis(-2.107) = 0.108
exp(coef(model3)[1:2])

# Create 'has_indie' dummy (1 if "Indie" is in game_tags, 0 otherwise)
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

# Create combined Chinese localisation variable (SC or TC)
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

# MODLE4 Beta regression with categorical localisation
model4 <- betareg(sc_ratio_adj ~ factor(sc_localisation_combo) + release_year, data = steam_data)
summary(model4) #plogis(-0.807) = 0.3085
exp(coef(model4)[1:3])

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

# MODEL5/6 Beta regression with categorical localisation + price_cny/usd
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

# Check AIC/BIC for model comparison
AIC(model4, model5, model6)
BIC(model4, model5, model6)

# Verify distribution
table(steam_data$has_indie)  # Check counts of 0 vs. 1

# MODEL7 Beta regression with indie dummy
model7 <- betareg(
  sc_ratio_adj ~ factor(sc_localisation_combo) + release_year + has_indie, 
  data = steam_data
)
summary(model7)

#AIC and BIC test for model 4 and 7
AIC(model4, model7)
BIC(model4, model7)

# MODEL8 Adding interaction for indie and SC localisation level
model8 <- betareg(sc_ratio_adj ~ factor(sc_localisation_combo)*has_indie + release_year, data = steam_data)
summary(model8)

#Visualize interaction effects
ggplot(steam_data, aes(x = sc_localisation_combo, y = sc_ratio_adj, color = has_indie)) +
  geom_boxplot() +
  labs(title = "Effect of Localisation on Chinese Reviews (Indie vs. Non-Indie)")

#AIC and BIC test for model 4 and 8
AIC(model4, model8)
BIC(model4, model8)

# LRT test for model 4 and 8
lrt_result_48 <- lrtest(model4, model8)
print(lrt_result_48)

# MODEL 9 Model with combined Chinese localisation (SC or TC)
model9 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo) + release_year, 
  data = steam_data
)
summary(model9)

# MODEL 10 Model with combined Chinese localisation + indie interaction
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

# MODEL11 added genres dummy
model11 <- betareg(
  sc_ratio_adj ~ factor(sc_localisation_combo) + release_year +
    has_action + has_adventure + has_rpg + has_story_rich + has_strategy + 
    has_casual + has_simulation + has_anime + has_puzzle,
  data = steam_data
)
summary(model11)

# MODEL12 added log total review
model12 <- betareg(
  sc_ratio_adj ~ factor(sc_localisation_combo) + release_year + log_total_reviews +
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


#Testing
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

# Count tag frequencies to identify prevalent tags (≥150 games)
tag_freq <- colSums(tag_matrix[, -1])
tags_to_keep <- names(tag_freq[tag_freq >= 150])
cat("Number of tags that appear in at least 150 games:", length(tags_to_keep), "\n")

# Filter the tag matrix to include only prevalent tags
tag_matrix_filtered <- tag_matrix[, c("app_id", tags_to_keep)]

# Check dimensions of filtered matrix
dim(tag_matrix_filtered)

set.seed(123)  # For reproducibility
tetra_cor <- tetrachoric(tag_matrix_filtered[, -1])$rho

# Check for NA values in the correlation matrix (can happen with tetrachoric correlations)
sum(is.na(tetra_cor))
# If NAs exist, we might need to handle them
if(sum(is.na(tetra_cor)) > 0) {
  # Replace NAs with 0s for subsequent analyses
  tetra_cor[is.na(tetra_cor)] <- 0
}

# Kaiser-Meyer-Olkin (KMO) test for sampling adequacy
kmo_result <- KMO(tetra_cor)
print(kmo_result) # The KMO test yielded an overall MSA of 0.8, which is considered good (values >0.7 are typically deemed adequate)

# Bartlett's test of sphericity
bartlett_result <- cortest.bartlett(tetra_cor, n = nrow(tag_matrix_filtered))
print(bartlett_result) # Bartlett's test is significant (p-value = 0), confirming that the correlation matrix is not an identity matrix

# Determine number of factors using parallel analysis
set.seed(123)  # For reproducibility
parallel_result <- fa.parallel(
  tetra_cor, 
  n.obs = nrow(tag_matrix_filtered),
  fa = "fa",
  cor = "cor",  # We're passing a correlation matrix
  n.iter = 20   # Reduce iterations for faster computation
)

# Record the suggested number of factors
n_factors <- parallel_result$nfact
cat("Suggested number of factors:", n_factors, "\n")

# Extract 20 factors
set.seed(123)  # For reproducibility
n_factors <- 20

fa_result <- fa(
  tetra_cor,
  nfactors = n_factors,
  rotate = "varimax",  # Orthogonal rotation for better interpretability
  cor = "cor",  # We're using a correlation matrix
  n.obs = nrow(tag_matrix_filtered)
)

# Print factor loadings with a reasonable cutoff
print(fa_result$loadings, cutoff = 0.3)

# List of tags to exclude (platform features rather than genre characteristics)
platform_tags <- c(
  "Family Sharing", "Steam Achievements", "Steam Trading Cards", "Steam Cloud", 
  "Steam Workshop", "Full controller support", "Tracked Controller Support", 
  "Partial Controller Support", "Controller", "Steam Leaderboards", 
  "Remote Play Together", "Remote Play on TV", "Remote Play on Phone", 
  "Remote Play on Tablet", "Captions available"
)

# Filter out platform tags from tag matrix
platform_cols <- which(names(tag_matrix_filtered) %in% platform_tags)
tag_matrix_filtered_noplat <- tag_matrix_filtered[, -platform_cols]

# Check dimensions after removing platform tags
dim(tag_matrix_filtered_noplat)

# Compute tetrachoric correlation matrix for the refined set
set.seed(123)
tetra_cor_filtered <- tetrachoric(tag_matrix_filtered_noplat[, -1])$rho

# Replace any NAs with 0s
if(sum(is.na(tetra_cor_filtered)) > 0) {
  tetra_cor_filtered[is.na(tetra_cor_filtered)] <- 0
}

# Extract factors with a more reasonable number
n_factors <- 15  # Capturing around 50% of variance based on previous analysis
fa_result_refined <- fa(
  tetra_cor_filtered,
  nfactors = n_factors,
  rotate = "varimax",
  cor = "cor",
  n.obs = nrow(tag_matrix_filtered_noplat)
)

# Print factor loadings with a reasonable cutoff
print(fa_result_refined$loadings, cutoff = 0.3)

# Calculate proportion of variance explained
print(fa_result_refined$Vaccounted)

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
  factor_summary[[i]] <- get_top_loadings(fa_result_refined$loadings, i)
}

# Print summaries for each factor
for (i in 1:n_factors) {
  cat(paste0("Factor ", i, " (", round(fa_result_refined$Vaccounted[2, i] * 100, 2), "% of variance)\n"))
  print(factor_summary[[i]])
  cat("\n")
}

# Assign factor names based on explicit matching to factor indices
factor_names <- rep(NA, n_factors)
factor_names[1] <- "City_Builders_Economic_Management_Sims"
factor_names[2] <- "Detective_Interactive_Narrative_Games"
factor_names[3] <- "Atmospheric_Retro_RPGs_Non_Mature"
factor_names[4] <- "Turn_Based_Tactical_Party_RPGs"
factor_names[5] <- "Couch_Co_Op_Local_Multiplayer_Games"
factor_names[6] <- "Free_to_Play_MMOs_Competitive_Online_Games"
factor_names[7] <- "Military_Strategy_War_Simulations"
factor_names[8] <- "Wholesome_Casual_Family_Friendly_Games"
factor_names[9] <- "Skill_Based_Action_RPGs_Souls_likes"
factor_names[10] <- "Immersive_3D_Simulations_Driving_Games"
factor_names[11] <- "Survival_Horror_Dark_Adventure"
factor_names[12] <- "Chaotic_Top_Down_Shooters_Roguelites"
factor_names[13] <- "Roguelike_Deckbuilders_Procedural_Indie_Games"
factor_names[14] <- "Sci_Fi_Space_Futuristic_Simulation"
factor_names[15] <- "Comedy_Dark_Humor"

# Get app_ids from the tag matrix
app_ids <- tag_matrix_filtered_noplat$app_id

# Calculate factor scores using Thurstone method as recommended
fa_scores <- factor.scores(
  tag_matrix_filtered_noplat[, -1],  # Exclude app_id column
  fa_result_refined,
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

# MODEL13 with factor scores instead of genre dummies
model13 <- betareg(
  sc_ratio_adj ~ factor(full_localisation_combo) + release_year + log_total_reviews +
    City_Builders_Economic_Management_Sims +
    Detective_Interactive_Narrative_Games +
    Atmospheric_Retro_RPGs_Non_Mature +
    Turn_Based_Tactical_Party_RPGs +
    Couch_Co_Op_Local_Multiplayer_Games +
    Free_to_Play_MMOs_Competitive_Online_Games +
    Military_Strategy_War_Simulations +
    Wholesome_Casual_Family_Friendly_Games +
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
  sc_ratio_adj ~ factor(sc_localisation_combo) + release_year + log_total_reviews +
    Atmospheric_Retro_RPGs_Non_Mature +
    Immersive_3D_Simulations_Driving_Games +
    Roguelike_Deckbuilders_Procedural_Indie_Games +
    Survival_Horror_Dark_Adventure +
    Couch_Co_Op_Local_Multiplayer_Games +
    Detective_Interactive_Narrative_Games +
    Turn_Based_Tactical_Party_RPGs +
    Comedy_Dark_Humor +
    Wholesome_Casual_Family_Friendly_Games +
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
