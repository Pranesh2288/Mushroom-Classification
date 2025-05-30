# Suppress global variable binding warnings for ggplot2
utils::globalVariables(c(
  "Var1", "Freq", "pred", "obs", "Resample",
  "Value", "n", "Class", "habitat", "population",
  "Category", "Count", "Feature", "Importance"
))

# Import required libraries
library(openxlsx)
library(caret)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(corrplot)
library(igraph)
library(RWeka)
library(pROC)
library(e1071)
library(rpart)
library(rpart.plot)
library(FSelector)
library(gridExtra)  # For arranging multiple plots

# Create directory for plots if it doesn't exist
dir.create("images", showWarnings = FALSE)

#----------------------------------------
# 1. Data Loading and Initial Exploration
#----------------------------------------

# Load the data
data <- read.csv("archive/mushrooms.csv")

# Display dataset information
cat("\n=== DATASET DIMENSIONS ===\n")
cat("Rows:   ", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")

# Display the structure and first few rows
cat("\n=== DATA STRUCTURE ===\n")
str(data)
cat("\n=== FIRST 5 ROWS ===\n")
print(head(data, 5))

# Data quality checks
cat("\n=== DATA QUALITY METRICS ===\n")
cat("Null values:     ", is.null(data), "\n")
cat("Missing values:  ", sum(is.na(data)), "\n")
cat("Duplicate rows:  ", nrow(data[duplicated(data), ]), "\n")

# Check unique values in each column
cat("\n=== UNIQUE VALUES BY COLUMN ===\n")
for (i in 1:23) {
  col_name <- names(data)[i]
  unique_values <- unique(data[[i]])
  cat("\nColumn", i, "(", col_name, "):\n")
  print(unique_values)
}

#----------------------------------------
# 2. Data Preprocessing and Visualization
#----------------------------------------

# Identify low/high cardinality features
object_columns <- sapply(data, is.character)
cardinality <- as.list(sapply(data[object_columns], function(x) length(unique(x))))
cat("\n=== FEATURE CARDINALITY ===\n")
for (col in names(cardinality)) {
  cat(sprintf("%-25s: %d\n", col, cardinality[[col]]))
}

# Identify columns to drop based on cardinality
columns_to_drop <- c(
  "bruises",
  "gill.attachment",
  "gill.spacing",
  "gill.size",
  "stalk.shape",
  "veil.type"
)

# Visualize class distribution
class_dist <- data.frame(
  Class = c("Edible", "Poisonous"),
  Count = c(sum(data$class == "e"), sum(data$class == "p"))
)

p_class <- ggplot(class_dist, aes(x = Class, y = Count, fill = Class)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Distribution of Edible vs Poisonous Mushrooms",
    x = "Mushroom Type",
    y = "Count"
  ) +
  scale_fill_manual(values = c("green", "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  geom_text(aes(label = Count), vjust = -0.5, size = 4)

# Save and display the plot
ggsave(
  filename = file.path("images", "class_distribution.png"),
  plot = p_class,
  width = 8,
  height = 6,
  dpi = 300
)
print(p_class)

# Function to create feature distribution plots
create_feature_plot <- function(data, feature_name) {
  # Calculate frequencies
  freq_table <- as.data.frame(table(data[[feature_name]]))
  names(freq_table) <- c("Category", "Count")
  
  # Create plot
  p <- ggplot(freq_table, aes_string(x = "Category", y = "Count")) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(
      title = paste("Distribution of", feature_name),
      x = feature_name,
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# Generate and save feature distribution plots
feature_plots <- list()
categorical_cols <- names(data)[sapply(data, function(x) 
  is.character(x) || is.factor(x))]

for (col in categorical_cols) {
  p <- create_feature_plot(data, col)
  
  # Save individual plot
  ggsave(
    filename = file.path("images", paste0("distribution_", col, ".png")),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  feature_plots[[col]] <- p
}

# Create combined plot
if (length(feature_plots) > 0) {
  n_plots <- length(feature_plots)
  n_cols <- min(3, n_plots)
  n_rows <- ceiling(n_plots / n_cols)
  
  combined_plot <- do.call(gridExtra::grid.arrange,
    c(feature_plots,
      ncol = n_cols,
      nrow = n_rows,
      top = "Feature Distributions")
  )
  
  # Save combined plot
  ggsave(
    filename = file.path("images", "all_distributions.png"),
    plot = combined_plot,
    width = 18,
    height = 4 * n_rows,
    dpi = 300
  )
}

#----------------------------------------
# 3. Feature Analysis
#----------------------------------------

# Analyze habitat distribution
habitat_distribution <- data %>%
  group_by(habitat) %>%
  summarise(
    poisonous = sum(class == "p"),
    edible = sum(class == "e"),
    .groups = "drop"
  )

# Convert to long format for plotting
habitat_long <- tidyr::pivot_longer(habitat_distribution,
  cols = c(poisonous, edible),
  names_to = "Class",
  values_to = "Frequency"
)

# Plot habitat distribution
p_habitat <- ggplot(habitat_long, aes(x = habitat, y = Frequency, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Habitat Distribution by Mushroom Class",
    x = "Habitat Type",
    y = "Frequency",
    fill = "Class"
  ) +
  scale_fill_manual(values = c("green", "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Save and display the plot
ggsave(
  filename = file.path("images", "habitat_distribution.png"),
  plot = p_habitat,
  width = 10,
  height = 6,
  dpi = 300
)
print(p_habitat)

# Analyze population distribution
population_distribution <- data %>%
  group_by(population) %>%
  summarise(
    poisonous = sum(class == "p"),
    edible = sum(class == "e"),
    .groups = "drop"
  )

# Convert to long format for plotting
population_long <- tidyr::pivot_longer(population_distribution,
  cols = c(poisonous, edible),
  names_to = "Class",
  values_to = "Frequency"
)

# Plot population distribution
p_population <- ggplot(population_long, aes(x = population, y = Frequency, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Population Distribution by Mushroom Class",
    x = "Population Type",
    y = "Frequency",
    fill = "Class"
  ) +
  scale_fill_manual(
    values = c("green", "red"),
    labels = c("Edible", "Poisonous")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Save and display the plot
ggsave(
  filename = file.path("images", "population_distribution.png"),
  plot = p_population,
  width = 10,
  height = 6,
  dpi = 300
)
print(p_population)

# Feature correlation analysis
data_numeric <- data
class_col <- data_numeric$class
data_numeric$class <- NULL
data_numeric[] <- lapply(data_numeric, function(x) as.numeric(factor(x)))

# Remove columns with zero variance
var_cols <- apply(data_numeric, 2, var, na.rm = TRUE)
data_numeric <- data_numeric[, var_cols > 0]

# Calculate correlation matrix with handling for NA values
cor_matrix <- cor(data_numeric, use = "pairwise.complete.obs")

# Replace any remaining NA/Inf values with 0
cor_matrix[is.na(cor_matrix)] <- 0
cor_matrix[!is.finite(cor_matrix)] <- 0

# Create correlation plot
png(
  filename = file.path("images", "correlation_matrix.png"),
  width = 12,
  height = 10,
  units = "in",
  res = 300
)
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE,
  title = "Feature Correlation Matrix",
  mar = c(0,0,2,0)
)
dev.off()

# Display correlation plot in R environment
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE,
  title = "Feature Correlation Matrix"
)

data_numeric$class <- class_col

#----------------------------------------
# 4. Model Building and Training
#----------------------------------------

# Data preparation
data <- read.csv("archive/mushrooms.csv", stringsAsFactors = TRUE)
data$veil.type <- NULL  # Remove column with single level
data[] <- lapply(data, factor)

# Create train-test split
set.seed(123)
trainIndex <- createDataPartition(data$class, p = 0.7, list = FALSE)
train_data <- data[trainIndex,]
test_data <- data[-trainIndex,]

# Define cross-validation parameters
ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  savePredictions = TRUE,
  summaryFunction = twoClassSummary
)

# Define model parameters grid
j48_grid <- expand.grid(
  C = c(0.01, 0.1, 0.25),
  M = c(2, 5, 10)
)

# Train model with cross-validation
c4_5_model <- train(
  class ~ .,
  data = train_data,
  method = "J48",
  trControl = ctrl,
  tuneGrid = j48_grid,
  metric = "ROC"
)

# Calculate and visualize feature importance
importance <- information.gain(class ~ ., data = train_data)
importance <- data.frame(
  Feature = rownames(importance),
  Importance = importance$attr_importance
)
importance <- importance[order(-importance$Importance), ]

# Create feature importance plot
p_importance <- ggplot(head(importance, 10), aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Most Important Features",
    x = "Feature",
    y = "Information Gain"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  # Add percentage labels
  geom_text(aes(label = sprintf("%.1f%%", Importance * 100)), 
            hjust = -0.1,
            size = 3.5)

# Save feature importance plot
ggsave(
  filename = file.path("images", "feature_importance.png"),
  plot = p_importance,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\n=== TOP 10 MOST IMPORTANT FEATURES ===\n")
print(head(importance, 10))

#----------------------------------------
# 5. Model Evaluation and Analysis
#----------------------------------------

# Make predictions on test set
test_pred <- predict(c4_5_model, test_data)
test_pred_prob <- predict(c4_5_model, test_data, type = "prob")

# Print debugging information
cat("\n=== ROC Curve Debugging Info ===\n")
cat("Test data class levels:", levels(test_data$class), "\n")
cat("Probability matrix dimensions:", dim(test_pred_prob), "\n")
cat("First few probability predictions:\n")
print(head(test_pred_prob))

# Ensure proper factor levels and convert probabilities to numeric
test_data$class <- factor(test_data$class, levels = c("e", "p"))
prob_edible <- as.numeric(test_pred_prob[, "e"])

# Print more debugging info
cat("\nProbability summary for edible class:\n")
print(summary(prob_edible))

# Calculate ROC curve
roc_obj <- roc(response = test_data$class,
               predictor = prob_edible,
               levels = c("p", "e"),
               direction = ">",
               quiet = TRUE)

cat("\nROC object summary:\n")
print(roc_obj)

auc_value <- auc(roc_obj)
cat("\nAUC value:", auc_value, "\n")

# Create ROC curve plot with base R first to verify
pdf(file.path("images", "roc_curve_base.pdf"))
plot(roc_obj, main = "ROC Curve Check")
dev.off()

# Create ggplot2 version
p_roc <- ggroc(roc_obj, legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(
    title = paste("ROC Curve (AUC =", round(auc_value, 3), ")"),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  coord_equal()

# Save ROC curve plot
ggsave(
  filename = file.path("images", "roc_curve.png"),
  plot = p_roc,
  width = 8,
  height = 8,
  dpi = 300
)
