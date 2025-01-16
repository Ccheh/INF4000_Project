# Load necessary libraries
library(tidyverse)
library(corrplot)
library(factoextra)
library(psych)
library(car)
library(reshape2)


# 1. Data Loading and Initial Cleaning
# =====================================
# Load data
df <- read_csv("dataset.csv", 
               locale = locale(encoding = "iso-8859-1"),
               show_col_types = FALSE)

# Clean data: Remove missing and outlier values
df_cleaned <- df %>%
  filter(!is.na(artists) & !is.na(album_name) & !is.na(track_name)) %>%
  filter(tempo != 0)  # Remove records with tempo equal to 0

# Select features for analysis
features <- c("danceability", "energy", "loudness", "speechiness", 
              "acousticness", "instrumentalness", "liveness", 
              "valence", "tempo", "popularity")

# Extract feature matrix
feature_matrix <- df_cleaned[features]

# 2. Correlation Analysis
# ========================
# Calculate correlation matrix
cor_matrix <- cor(feature_matrix)
cor_melt <- melt(cor_matrix)

# Visualize correlation matrix
ggplot(data = cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Add gridlines to separate cells
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",  # Color gradient
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"  # Legend title
  ) +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +  # Display correlation values rounded to 2 decimals
  theme_minimal() +  # Use minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),  # X-axis labels
    axis.text.y = element_text(size = 10, face = "bold"),  # Y-axis labels
    plot.title = element_text(size = 14, face = "bold"),  # Title styling
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    legend.position = "right"  # Place legend on the right
  ) +
  labs(
    title = "Correlation Matrix",  # Chart title
    x = NULL,  # Remove X-axis title
    y = NULL   # Remove Y-axis title
  )

# Extract correlations with popularity
popularity_correlations <- cor_matrix["popularity", -which(features == "popularity")]
sorted_correlations <- sort(abs(popularity_correlations), decreasing = TRUE)

# Display correlations (sorted by absolute value)
print(sorted_correlations)

# 3. Multicollinearity Analysis
# =============================
# Create linear model
features_no_pop <- features[features != "popularity"]
formula_str <- paste("popularity ~", paste(features_no_pop, collapse = " + "))
lm_model <- lm(formula_str, data = feature_matrix)

# Calculate VIF
vif_values <- vif(lm_model)
print(vif_values)

# 4. PCA Analysis
# ===============
# Prepare data for PCA (exclude popularity)
pca_data <- scale(feature_matrix[features_no_pop])
pca_result <- prcomp(pca_data)

# Calculate explained variance ratio
explained_variance <- pca_result$sdev^2
explained_variance_ratio <- explained_variance/sum(explained_variance)
cumulative_variance_ratio <- cumsum(explained_variance_ratio)

# Create dataframe for variance explanation
variance_df <- data.frame(
  PC = 1:length(explained_variance_ratio),
  Individual = explained_variance_ratio,
  Cumulative = cumulative_variance_ratio
)

# Display variance explanation
print("\nVariance Explained by Principal Components:")
print(variance_df)

# Plot scree plot
fviz_eig(pca_result, 
         addlabels = TRUE, 
         ylim = c(0, 50),
         main = "Scree Plot with Cumulative Variance Explained")

# Display PCA loadings
print("\nPCA Loadings:")
print(pca_result$rotation)

# 5. Feature Selection Analysis
# =============================
# Calculate feature importance scores
feature_importance <- data.frame(
  Feature = features_no_pop,
  Correlation = abs(popularity_correlations),
  VIF = vif_values,
  PCA_Loading_PC1 = abs(pca_result$rotation[,1])
)

# Standardize indicators
feature_importance <- feature_importance %>%
  mutate(
    Correlation_Scaled = scale(Correlation),
    VIF_Scaled = scale(-log(VIF)),  # Negative because smaller VIF is better
    PCA_Loading_Scaled = scale(PCA_Loading_PC1),
    Total_Score = Correlation_Scaled + VIF_Scaled + PCA_Loading_Scaled
  ) %>%
  arrange(desc(Total_Score))

print("\nFeature Importance Ranking:")
print(feature_importance)

# 6. Visualization of Final Results
# ==================================
# Plot feature importance

ggplot(feature_importance, aes(x = reorder(Feature, Total_Score), y = Total_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance Scores",
       x = "Features",
       y = "Importance Score") +
  theme_minimal()

# 7. Output Key Statistics
# =========================
cat("\nNumber of samples after data preprocessing:", nrow(df_cleaned), "\n")
cat("\nKaiser criterion (Eigenvalue > 1) suggests number of principal components:", sum(explained_variance > 1), "\n")
cat("\nNumber of principal components needed to explain 80% of variance:", which(cumulative_variance_ratio >= 0.8)[1], "\n")

# ==================
# Chart 1: Feature Combination Patterns

# Prepare data
# 1. Extract top 10% popular songs
popularity_threshold <- quantile(df_cleaned$popularity, 0.9)
top_songs <- df_cleaned %>% 
  filter(popularity >= popularity_threshold)

# 2. Categorize features
pattern_data <- top_songs %>%
  mutate(
    danceability_level = case_when(
      danceability < 0.33 ~ "Low",
      danceability < 0.66 ~ "Medium",
      TRUE ~ "High"
    ),
    instrumentalness_level = case_when(
      instrumentalness < 0.01 ~ "Low",
      instrumentalness < 0.3 ~ "Medium",
      TRUE ~ "High"
    ),
    loudness_level = case_when(
      loudness < -10 ~ "Low",
      loudness < -5 ~ "Medium",
      TRUE ~ "High"
    ),
    speechiness_level = case_when(
      speechiness < 0.1 ~ "Low",
      speechiness < 0.3 ~ "Medium",
      TRUE ~ "High"
    ),
    valence_level = case_when(
      valence < 0.33 ~ "Low",
      valence < 0.66 ~ "Medium",
      TRUE ~ "High"
    )
  )

# 3. Extract top 5 combinations
total_top_songs <- nrow(top_songs)  # Get total number of top 10% songs

top_patterns <- pattern_data %>%
  count(danceability_level, instrumentalness_level, loudness_level,
        speechiness_level, valence_level) %>%
  arrange(desc(n)) %>%
  head(5) %>%
  mutate(
    rank = row_number(),
    percentage = n/total_top_songs*100,
    # Simplify labels for a single line
    pattern_label = sprintf("#%d %d (%.1f%%)", rank, n, percentage)
  )

# 4. Convert to long format
patterns_long <- top_patterns %>%
  pivot_longer(
    cols = ends_with("_level"),
    names_to = "feature",
    values_to = "level"
  ) %>%
  mutate(
    feature = str_remove(feature, "_level"),
    feature = str_to_title(feature),
    rank = factor(rank)
  )

# 5. Create visualization
chart1 <- ggplot(patterns_long) +
  geom_tile(aes(x = feature, y = rank, fill = level),
            width = 0.95, height = 0.7, color = "white") +
  geom_text(aes(x = feature, y = rank, 
                label = level,
                color = level == "Low"),
            size = 3.5) +
  # Adjust left labels position
  geom_text(data = unique(patterns_long[c("rank", "pattern_label")]),
            aes(x = 0.5, y = rank, label = pattern_label),
            hjust = 1,
            size = 3.5,
            fontface = "bold",
            nudge_x = -0.05) +  # Slight offset adjustment
  scale_fill_manual(values = c(
    "High" = "#4169E1",    
    "Medium" = "#63B3ED",  
    "Low" = "#EBF8FF"      
  )) +
  scale_color_manual(values = c(
    "TRUE" = "#2D3748",  
    "FALSE" = "white"    
  )) +
  labs(title = "Most Successful Feature Combinations",
       subtitle = "Analysis of feature patterns in top 10% most popular songs") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 10)),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(
      size = 12, 
      face = "bold",
      color = "black",
      angle = 0,
      vjust = -0.5,
      margin = margin(b = 5)
    ),
    axis.text.y = element_blank(),
    plot.margin = margin(l = 90, r = 20, t = 30, b = 20),  # Adjust left margin
    axis.ticks.x = element_line(color = "grey70"),
    axis.line.x = element_line(color = "grey70")
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_discrete(limits = rev(levels(patterns_long$rank))) +
  scale_x_discrete(position = "top")


# ==================
# ==================
# chart2 Genre Feature Patterns
# 1. Data Preparation
popularity_threshold <- quantile(df_cleaned$popularity, 0.9)
top_songs <- df_cleaned %>% 
  filter(popularity >= popularity_threshold)

# 2. Calculate the main genre distribution
genre_distribution <- top_songs %>%
  count(track_genre) %>%
  mutate(percentage = n / nrow(top_songs) * 100) %>%
  arrange(desc(percentage)) %>%
  head(5)  # Select the top 5 genres

# 3. Calculate mean feature values for each genre and normalize loudness
loudness_min <- min(top_songs$loudness)
loudness_max <- max(top_songs$loudness)

genre_features <- top_songs %>%
  filter(track_genre %in% genre_distribution$track_genre) %>%
  group_by(track_genre) %>%
  summarise(
    Dan = mean(danceability),
    Val = mean(valence),
    Loud = mean(loudness), # Raw loudness mean
    Loud_normalized = (loudness_max - mean(loudness)) / (loudness_max - loudness_min), # Standardized Loudness
    Speech = mean(speechiness),
    Ins = mean(instrumentalness),
    count = n(),
    percentage = n() / nrow(top_songs) * 100
  )

# Print the results to validate calculations
print(genre_features)

# Transform data to long format and adjust factor levels
features_long <- genre_features %>%
  pivot_longer(
    cols = c(Dan, Val, Loud, Speech, Ins), # Including raw Loud
    names_to = "feature",
    values_to = "value"
  ) %>%
  mutate(
    feature = factor(feature, levels = c("Dan", "Val", "Loud", "Speech", "Ins")),
    track_genre = factor(track_genre, levels = genre_features$track_genre)
  )

genre_features <- genre_features %>%
  arrange(desc(percentage)) %>% # Sort by percentage in descending order
  mutate(label = sprintf("%s %.1f%%", track_genre, percentage))

features_long <- features_long %>%
  mutate(track_genre = factor(track_genre, levels = genre_features$track_genre)) # Ensure factor levels match the ranking

# Update visualization
chart2 <- ggplot(features_long, aes(x = feature, y = track_genre)) +
  # Add genre labels and percentages on the left
  geom_text(data = genre_features,
            aes(x = 0, y = track_genre, 
                label = label),
            hjust = 0.95, # Align with the bubble points
            size = 4,
            fontface = "bold",
            color = "#2D3748",
            nudge_x = -0.2) + # Reduce offset distance
  # Add bubbles
  geom_point(aes(size = ifelse(feature == "Loud", 
                               genre_features$Loud_normalized[match(track_genre, genre_features$track_genre)], 
                               value), 
                 fill = ifelse(feature == "Loud", 
                               genre_features$Loud_normalized[match(track_genre, genre_features$track_genre)], 
                               value)),
             shape = 21, color = "white", stroke = 0.5) +
  # Add values inside the bubbles
  geom_text(aes(label = ifelse(feature == "Loud", 
                               sprintf("%.2f", genre_features$Loud[match(track_genre, genre_features$track_genre)]), # Display raw Loud
                               sprintf("%.2f", value))),
            size = 3.2, 
            color = ifelse(features_long$value > 0.4, "white", "black")) +
  # Dynamic color mapping
  scale_fill_gradientn(
    colors = c("#ECECEC", "#FED8B1", "#FD9F44", "#B44B16"), # Unique color scheme for Loud
    values = c(0, 0.5, 0.75, 1)
  ) +
  scale_size_continuous(range = c(5, 12)) +  # Adjust the bubble size range
  scale_x_discrete(
    position = "bottom", # Move feature names to the bottom
    expand = c(0.02, 0.02),  # Reduce x-axis white space
    labels = c("Dan", "Val", "Loud", "Speech", "Ins")  # Use abbreviations
  ) +
  scale_y_discrete(
    expand = c(0.02, 0.02),  # Reduce y-axis white space
    limits = rev,
    labels = NULL
  ) +
  labs(title = "Genre Success Patterns in Top 10% Songs",
       subtitle = "bubble size indicates feature value") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 10)),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(
      size = 12,
      face = "bold",
      angle = 0,
      vjust = 1.5, # Adjust position of feature names
      hjust = 0.5,
      margin = margin(t = 10) # Increase spacing below
    ),
    axis.text.y = element_blank(),
    plot.margin = margin(l = 100, r = 10, t = 40, b = 40)  # Adjust margins, increase bottom space
  ) +
  coord_fixed(ratio = 0.5, clip = "off")  # Adjust aspect ratio


# ==================
# chart3 Feature Synergy Analysis
# 1. Data Preparation
popularity_threshold <- quantile(df_cleaned$popularity, 0.9)
top_songs <- df_cleaned %>% 
  filter(popularity >= popularity_threshold)

# 2. Add feature level classifications
labeled_songs <- top_songs %>%
  mutate(
    danceability_level = case_when(
      danceability < 0.33 ~ "Low",
      danceability < 0.66 ~ "Medium",
      TRUE ~ "High"
    ),
    energy_level = case_when(
      energy < 0.33 ~ "Low",
      energy < 0.66 ~ "Medium",
      TRUE ~ "High"
    ),
    valence_level = case_when(
      valence < 0.33 ~ "Low",
      valence < 0.66 ~ "Medium",
      TRUE ~ "High"
    ),
    loudness_level = case_when(
      loudness < -10 ~ "Low",
      loudness < -5 ~ "Medium",
      TRUE ~ "High"
    ),
    acousticness_level = case_when(
      acousticness < 0.33 ~ "Low",
      acousticness < 0.66 ~ "Medium",
      TRUE ~ "High"
    )
  )

# 3. Function to analyze feature combinations
analyze_feature_combination <- function(data, feature1, feature2) {
  level1_col <- paste0(feature1, "_level")
  level2_col <- paste0(feature2, "_level")
  
  data %>%
    group_by(!!sym(level1_col), !!sym(level2_col)) %>%
    summarise(
      avg_popularity = mean(popularity),
      count = n(),
      percentage = n() / nrow(.) * 100,
      .groups = 'drop'
    ) %>%
    arrange(desc(avg_popularity)) %>%
    head(3)
}

# 4. Function to create a single feature combination plot
create_combination_plot <- function(data, feature1, feature2, insight) {
  results <- analyze_feature_combination(data, feature1, feature2)
  
  # Prepare data and generate the plot
  plot_data <- results %>%
    mutate(
      x_pos = 1:3,
      combination_label = sprintf(
        "%s-%s", 
        !!sym(paste0(feature1, "_level")),
        !!sym(paste0(feature2, "_level"))
      ),
      percentage_label = sprintf("%.1f%% of top songs", percentage),
      # Calculate text color: High popularity = white, Low popularity = dark blue
      text_color = ifelse(avg_popularity >= mean(avg_popularity), 
                          "white", "#2C3E50")
    )
  
  ggplot(plot_data, aes(x = x_pos, y = 1)) +
    # Create card-style display
    geom_tile(
      aes(fill = avg_popularity),
      color = "white",
      width = 0.9,
      height = 0.7
    ) +
    # Combination label (pattern)
    geom_text(
      aes(label = combination_label,
          color = text_color),  # Use dynamic color
      size = 5,
      vjust = -1,
      fontface = "bold"
    ) +
    # Popularity label
    geom_text(
      aes(label = sprintf("Popularity: %.1f", avg_popularity),
          color = text_color),  # Use dynamic color
      size = 4,
      vjust = 0.5
    ) +
    # Percentage label
    geom_text(
      aes(label = percentage_label,
          color = text_color),  # Use dynamic color
      size = 4,
      vjust = 2
    ) +
    # Titles
    labs(
      title = sprintf("%s × %s", 
                      str_to_title(feature1), 
                      str_to_title(feature2)),
      subtitle = insight
    ) +
    # Blue gradient for fill
    scale_fill_gradientn(
      colors = c("#D4E6F1", "#5DADE2", "#21618C"),
      values = scales::rescale(c(min(results$avg_popularity),
                                 mean(range(results$avg_popularity)),
                                 max(results$avg_popularity)))
    ) +
    # Set text colors
    scale_color_identity() +
    # Adjust coordinate ranges
    scale_x_continuous(
      limits = c(0.5, 3.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0.5, 1.5),
      expand = c(0, 0)
    ) +
    # Theme settings
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(
        size = 14, 
        face = "bold",
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = 12,
        color = "grey40",
        face = "italic",
        margin = margin(b = 20)
      ),
      legend.position = "none",
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

# 5. Define feature pairs
feature_pairs <- list(
  list(
    feature1 = "energy",
    feature2 = "danceability",
    insight = "High energy + high danceability creates the most popular tracks"
  ),
  list(
    feature1 = "danceability",
    feature2 = "acousticness",
    insight = "High danceability works best with low acousticness"
  ),
  list(
    feature1 = "energy",
    feature2 = "valence",
    insight = "High energy combines well with positive valence"
  )
)

# 6. Create plots
library(patchwork)

plots <- map(feature_pairs, ~{
  create_combination_plot(
    labeled_songs,
    .x$feature1,
    .x$feature2,
    .x$insight
  )
})

# 7. Combine final visualization
final_plot <- plots[[1]] / plots[[2]] / plots[[3]] +
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(
    title = "Feature Synergy Effects",
    subtitle = "Analysis of feature combinations and their impact on popularity in top 10% songs",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
  )
# Display final plot
chart3 <- final_plot
final_plot


# ==================
# chart4 Feature Balance Radar Chart
# 1. Data Preprocessing
selected_features <- c("energy", "loudness", "danceability", "valence", "acousticness", "instrumentalness")

percentiles <- df_cleaned %>%
  summarise(
    p90 = quantile(popularity, 0.9),
    p70 = quantile(popularity, 0.7),
    p50 = quantile(popularity, 0.5)
  )
df_features <- df_cleaned %>%
  mutate(
    success_level = case_when(
      popularity >= percentiles$p90 ~ "Outstanding",
      popularity >= percentiles$p70 ~ "Successful",
      popularity >= percentiles$p50 ~ "Average",
      TRUE ~ NA_character_
    ),
    success_level = factor(success_level, levels = c("Outstanding", "Successful", "Average"))
  )

# Calculate mean values and normalize loudness
feature_means <- df_features %>%
  filter(!is.na(success_level)) %>%
  group_by(success_level) %>%
  summarise(across(all_of(selected_features), mean)) %>%
  mutate(
    loudness = (loudness - min(df_cleaned$loudness)) / (max(df_cleaned$loudness) - min(df_cleaned$loudness))
  )

print(feature_means)

# Convert data to long format
angles <- seq(0, 2 * pi, length.out = length(selected_features) + 1)[-length(selected_features) - 1]
plot_data <- feature_means %>%
  pivot_longer(cols = all_of(selected_features), names_to = "feature", values_to = "value") %>%
  mutate(
    feature = factor(feature, levels = selected_features),
    angle = angles[match(feature, selected_features)]
  )

# Label positions
label_data <- data.frame(
  feature = str_to_title(selected_features),
  angle = angles,
  x = 1.3 * cos(angles),
  y = 1.3 * sin(angles)
)

# 2. Create radar chart
chart4 <- ggplot() +
  # Grid lines
  geom_polygon(data = expand.grid(radius = seq(0.2, 1, by = 0.2), angle = seq(0, 2*pi, length.out = 100)),
               aes(x = radius * cos(angle), y = radius * sin(angle), group = radius),
               color = "gray80", size = 0.3, fill = NA) +
  # Radial lines
  geom_segment(data = data.frame(x = 0, y = 0, xend = cos(angles), yend = sin(angles)),
               aes(x = x, y = y, xend = xend, yend = yend), color = "gray70", size = 0.4) +
  # Polygons
  geom_polygon(data = plot_data, aes(x = value * cos(angle), y = value * sin(angle), group = success_level, fill = success_level),
               alpha = 0.4) +
  # Outline paths
  geom_path(data = plot_data, aes(x = value * cos(angle), y = value * sin(angle), group = success_level, color = success_level),
            size = 1.2) +
  # Feature labels (abbreviate Valence and Energy)
  geom_text(data = label_data %>% mutate(feature = c("Eng", "Loudness", "Danceability", "Val", "Acousticness", "Instrumentalness")), 
            aes(x = x, y = y, label = feature), size = 4, fontface = "bold") +
  # Adjust color scheme
  scale_fill_manual(
    values = c("Outstanding" = "#1f78b4", "Successful" = "#ff7f00", "Average" = "#6a3d9a"),
    labels = c("Top 10%", "Top 10-30%", "Top 30-50%")  # Custom legend labels
  ) +
  scale_color_manual(
    values = c("Outstanding" = "#1f78b4", "Successful" = "#ff7f00", "Average" = "#6a3d9a"),
    labels = c("Top 10%", "Top 10-30%", "Top 30-50%")  # Custom legend labels
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),  # Hide legend title
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Center-align title
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0.5)  # Center-align subtitle
  ) +
  labs(
    title = "Musical Feature Balance Across Success Levels",
    subtitle = "Key features in songs with different popularity levels"
  )

# Combine four charts
library(patchwork)
final_composite <- wrap_plots(chart1, chart2, chart3, chart4) +
  plot_annotation(
    title = "Composite Visualisation: Audio Features and Popularity Analysis",
    theme = theme(
      plot.title = element_text(
        size = 24,         # Font size
        face = "bold",     # Bold font
        hjust = 0.5,       # Center alignment
        color = "#FF5733", # Change title color to orange (or customize)
        family = "serif",  # Use serif font
        margin = margin(t = 10, b = 20) # Adjust title spacing
      )
    )
  )

# Save as an image
ggsave("composite_visualisation_grid.png", final_composite, width = 16, height = 12)

