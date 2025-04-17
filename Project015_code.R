# Import libraries
library(openxlsx) 
library(caret) # For splitting the data
library(dplyr)
library(data.table)
library(ggplot2)
library(corrplot)
library(igraph)

# Load the data
data <- read.csv("mushrooms.csv")

# Data Pre-processing
#-------------------------------------------------------------------------------
# ------------------ Understanding our data-set --------------------------------
#-------------------------------------------------------------------------------


# shows the first 5 rows of our data set
#---------------------------------------
head(data, 5)

# Finding the dimension of our data set
#--------------------------------------
print("Dimension of the dataset is :")
print(dim(data))

print(paste("Number of columns : ", ncol(data))) # from here we can conclude that there are in total 23 attributes.
print(paste("Number of rows : ", nrow(data))) # We have 8124 records

# Finding the structure of our data-set (column names, types, etc.):
#-----------------------------------------------------------------
str(data)
# From here we can conclude that all our attributes have character data .

#-------------------------------------------------------------------------------
#------------------------------ DATA CLEANING ----------------------------------
#-------------------------------------------------------------------------------
# Looking for null value
#-----------------------
# Check if data frame is NULL
print(paste("Null values in the data-set : ", is.null(data)))

# Since the result is false , we can say that there are no null row-values in the data-set.
# Check for missing values
#--------------------------
print(paste("Missing data : ", sum(is.na(data))))

# Since the count for NA's rows is 0 , we can conclude that our data-set has no row-missing values.

# Hence finally we can conclude here that our data set has no missing or null values.

# Looking for duplicate rows
#------------------------------

# count number of duplicate rows
print(paste("Count for duplicate rows : ", nrow(data[duplicated(data), ])))

# Since the outcome is zero we can conclude that there are no duplicate rows in our data-set

# checking for noise in our data-set and missing values for each column :
#------------------------------------------------------------------------
for (i in 1:23) {
  unique_values <- unique(data[[i]])
  print(paste("Unique values in column ", i, " are: "))
  print(unique_values)
}

# Looking at the result we found that there are unknown values in column 12 which is 'stalk-root' column of our data-set
# Which are identified by ? int the cells.
# Let us now try to find the count of such values.
count_question_mark <- sum(data$stalk.root == "?")
print(count_question_mark)

# We found that the count of ? in stalk-root column is 2480 which is pretty high compared to total number of rows in the data-set.
# This gives us a hint to investigate what all attributes are actually needed for our need for our model .
# Because attributes with only one kind of values and attribute with all rows having unique value do not contribute much towards our goal.
# Hence we try finding the cardiniality of each attribute.

#-------------------------------------------------------------------------------
#---------------------- Exploring Our Data-set ---------------------------------
#-------------------------------------------------------------------------------

# Finding number of unique values in each columnes
#------------------------------------------------
object_columns <- sapply(data, is.character)
result <- as.list(sapply(data[object_columns], function(x) length(unique(x))))
print(result)

# Dropping columns with very low/high cardinality
#----------------------------------------------------
dim(data)

columns_to_drop <- c(
  "bruises",
  "gill.attachment",
  "gill.spacing",
  "gill.size",
  "stalk.shape",
  "veil.type"
)

# Function to plot a pie chart for unique values of an attribute
# Function to plot a pie chart for unique values of an attribute
plot_pie_chart <- function(attribute_name) {
  unique_values <- table(data[[attribute_name]])
  labels <- names(unique_values)
  
  pie(unique_values, labels = paste(labels, "(", unique_values, ")", sep = ""), main = paste("Pie Chart for", attribute_name))
}

# Loop through each attribute in the dataset and plot a pie chart
for (attribute_name in columns_to_drop) {
  plot_pie_chart(attribute_name)
}

dim(data)

# Count for edible vs poisonous mushrooms in our data-set
A <- c(sum(data$class == "e"), sum(data$class == "p"))
B <- c("Edible", "Poisonous")

barplot(A,
        names.arg = B, xlab = "Type of Mushroom",
        ylab = "Count", main = "Count : Edible vs Poisonous Mushroom", col = "pink"
)

# Frequency of each attribute
#-----------------------------

plot_unique_frequency <- function(data) {
  for (col in names(data)) {
    if (
      class(data[[col]]) %in% c("character", "factor") 
      && length(unique(data[[col]])) > 0
    ) {
      unique_counts <- as.data.frame(table(data[[col]]))
      
      # Plotting bar plot for each attribute
      p <- ggplot(unique_counts, aes(x = Var1, y = Freq)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(
          title = paste("Frequency of Unique Values for", col),
          x = col,
          y = "Frequency"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      print(p)
    }
  }
}

plot_unique_frequency(data)

# Our data is moderately balanced

# Lets us try finding the major habitats of poisonous mushrooms and edible mushrooms
#-----------------------------------------------------------------------------------

df_grp_region <- data %>%
  group_by(habitat) %>%
  summarise(
    poisonous_frequency = sum(class == "p"),
    edible_frequency = sum(class == "e"),
    .groups = "drop"
  )

View(df_grp_region)
print(df_grp_region)

df_grp_region_long <- tidyr::pivot_longer(df_grp_region,
                                          cols = c(poisonous_frequency, edible_frequency),
                                          names_to = "Class", values_to = "Frequency"
)

df_grp_region_long


ggplot(df_grp_region_long, aes(x = habitat, y = Frequency, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Habitat", y = "Frequency", fill = "Class") +
  scale_fill_manual(values = c("green", "red"), labels = c("Edible", "Poisonous")) +
  ggtitle("Habitat Distribution: Edible Vs Poisonous") +
  theme_minimal()




# With the help of graph we can see that most of the poisonous mushrooms grow across paths




# Lets try finding out the populations these mushrooms usually fall in
#----------------------------------------------------------------------


df_grp_population <- data %>%
  group_by(population) %>%
  summarise(
    poisonous_frequency = sum(class == "p"),
    edible_frequency = sum(class == "e"),
    .groups = "drop"
  )

View(df_grp_population)
print(df_grp_population)

df_grp_population_long <- tidyr::pivot_longer(df_grp_population,
                                              cols = c(poisonous_frequency, edible_frequency),
                                              names_to = "Class", values_to = "Frequency"
)


ggplot(df_grp_population_long, aes(x = population, y = Frequency, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "population", y = "Frequency", fill = "Class") +
  scale_fill_manual(values = c("green", "red"), labels = c("Edible", "Poisonous")) +
  ggtitle("Population Distribution: Edible Vs Poisonous") +
  theme_minimal()

string_columns <- names((sapply(data, is.character)))
for (column in string_columns) {
  data[[column]] <- as.numeric(factor(data[[column]],
                                      levels = unique(data[[column]])
  ))
}

# Correlation
cor_matrix <- cor(data)
corrplot(cor_matrix)

print(ncol(data))

write.xlsx(
  as.table(cor_matrix),
  file = "cor.matrix.xlsx",
  rowNames = TRUE,
  colNames = TRUE
)

columns_to_remove <- integer(0)

for (i in seq(1, ncol(data))) {
  print(i)
  print(abs(cor_matrix[i]))
  if(!is.na(cor_matrix[i]) && abs(cor_matrix[i]) < .5) {
    columns_to_remove <- c(columns_to_remove, i)
  }
}

data <- data[, -columns_to_remove]
data <- data[, -which(names(data) == "veil.type")]

# b) Split the data randomly into 2:1 ratio
set.seed(123) # Set seed for reproducibility
train_indices <- createDataPartition(data$class, p = 2/3, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Actual labels of the test class
actual_labels <- as.factor(test_data$class)

# Function to calculate entropy
calculate_entropy <- function(labels) {
  probabilities <- table(labels) / length(labels)
  -sum(probabilities * log2(probabilities + 1e-10))
}

# Function to calculate information gain
calculate_information_gain <- function(data, feature, target) {
  total_entropy <- calculate_entropy(data[[target]])
  feature_levels <- unique(data[[feature]])
  
  # Calculate split entropy
  split_entropy <- sum(
    sapply(feature_levels, function(level) {
      subset_data <- data[data[[feature]] == level, ]
      weight <- nrow(subset_data) / nrow(data)
      weight * calculate_entropy(subset_data[[target]])
    })
  )
  
  # Calculate intrinsic value (IV)
  iv <- -sum(
    sapply(feature_levels, function(level) {
      subset_data <- data[data[[feature]] == level, ]
      weight <- nrow(subset_data) / nrow(data)
      weight * log2(weight + 1e-10)
    })
  )
  
  # Calculate gain ratio
  gain_ratio <- (total_entropy - split_entropy) / iv
  
  gain_ratio
}

# Function to find the best split
find_best_split <- function(data, features, target) {
  information_gains <- sapply(features, function(feature) {
    calculate_information_gain(data, feature, target)
  })
  best_feature <- features[which.max(information_gains)]
  best_feature
}

# Function to build the decision tree
build_decision_tree <- function(data, features, target) {
  if (length(unique(data[[target]])) == 1) {
    # If all examples have the same label, create a leaf node
    return(data[[target]][1])
  }
  
  if (length(features) == 0) {
    # If there are no features left, create a leaf node with the majority label
    majority_label <- names(sort(table(data[[target]]), decreasing = TRUE)[1])
    return(majority_label)
  }
  
  best_feature <- find_best_split(data, features, target)
  
  tree <- list()
  tree$feature <- best_feature
  tree$children <- list()
  
  feature_levels <- unique(data[[best_feature]])
  for (level in feature_levels) {
    subset_data <- data[data[[best_feature]] == level, ]
    subset_data <- subset_data[, !names(subset_data) %in% best_feature]
    subset_features <- features[features != best_feature]
    subtree <- build_decision_tree(subset_data, subset_features, target)
    tree$children[[as.character(level)]] <- subtree
  }
  
  return(tree)
}

# Example usage
features <- names(data)[-1]
target <- names(data)[1]

# Function to make predictions on the test set
predict_tree <- function(tree, test_data) {
  predictions <- apply(test_data, 1, function(row) predict_decision_tree(tree, data.frame(t(row))))
  return(predictions)
}

# Predict Decision Tree Function
predict_decision_tree <- function(tree, new_data) {
  while (!is.character(tree) && !is.null(names(tree))) {
    feature <- tree$feature
    feature_value <- as.character(new_data[[feature]])
    
    # Convert feature_value to the same format as in the training data
    feature_value <- as.numeric(factor(feature_value, levels = levels(train_data[[feature]])))
    
    if (!feature_value %in% names(tree$children)) {
      # If feature value is not in the tree, return the majority class
      majority_class <- names(sort(table(train_data$class), decreasing = TRUE))[1]
      return(majority_class)
    }
    
    # Access the child node
    tree <- tree$children[[feature_value]]
  }
  
  return(tree)
}

# Function to calculate confusion matrix
calculate_confusion_matrix <- function(predictions, actual, positive_class) {
  conf_matrix <- table(predictions, actual)
  return(conf_matrix)
}

# Function to calculate Precision
calculate_precision <- function(predictions, actual, positive_class) {
  true_positive <- sum(predictions == positive_class & actual == positive_class)
  false_positive <- sum(predictions == positive_class & actual != positive_class)
  
  if (true_positive + false_positive == 0) {
    return(0)
  }
  
  precision <- true_positive / (true_positive + false_positive)
  return(precision)
}

# Function to calculate Recall
calculate_recall <- function(predictions, actual, positive_class) {
  true_positive <- sum(predictions == positive_class & actual == positive_class)
  false_negative <- sum(predictions != positive_class & actual == positive_class)
  
  if (true_positive + false_negative == 0) {
    return(0)
  }
  
  recall <- true_positive / (true_positive + false_negative)
  return(recall)
}

# Function to calculate F1-score
calculate_f1_score <- function(predictions, actual, positive_class) {
  precision <- calculate_precision(predictions, actual, positive_class)
  recall <- calculate_recall(predictions, actual, positive_class)
  
  if (precision + recall == 0) {
    return(0)
  }
  
  f1_score <- 2 * precision * recall / (precision + recall)
  return(f1_score)
}

# Function to calculate accuracy
calculate_accuracy <- function(predictions, actual, positive_class) {
  true_positive <- sum(predictions == positive_class & actual == positive_class)
  true_negative <- sum(predictions != positive_class & actual != positive_class)
  false_positive <- sum(predictions == positive_class & actual != positive_class)
  false_negative <- sum(predictions != positive_class & actual == positive_class)
  accuracy <- (true_positive + true_negative) / (true_positive + true_negative + false_positive + false_negative)
  return(accuracy)
}

# Function to calculate ROC curve
calculate_roc_curve <- function(predictions, actual, positive_class) {
  thresholds <- seq(0, 1, 0.01)
  roc_data <- data.frame(TP = numeric(length(thresholds)), FP = numeric(length(thresholds)))
  
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    binary_predictions <- as.integer(predictions == positive_class & actual == positive_class)
    
    # Handle cases where there are no positive predictions
    if (sum(binary_predictions) == 0) {
      roc_data[i, "TP"] <- 0
      roc_data[i, "FP"] <- 0
    } else {
      conf_matrix <- table(binary_predictions, as.integer(actual == positive_class))
      
      # Check if the matrix has the required dimensions
      if (nrow(conf_matrix) >= 2 && ncol(conf_matrix) >= 2) {
        roc_data[i, "TP"] <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
        roc_data[i, "FP"] <- conf_matrix[1, 2] / sum(conf_matrix[1, ])
      } else {
        roc_data[i, "TP"] <- 0
        roc_data[i, "FP"] <- 0
      }
    }
  }
  
  return(roc_data)
}

# Build the decision tree
tree <- build_decision_tree(train_data, features, target)
# Verifying the classifier's performance on test data
predictions <- predict_tree(tree, test_data)

# Confusion matrix
conf_matrix <- calculate_confusion_matrix(predictions, actual_labels, positive_class = 2)
print("Confusion Matrix:")
print(conf_matrix)
# Evaluating the model performance based on accuracy, precision, recall, f1-score
calculate_precision(predictions, test_data$class, positive_class = 2)
calculate_recall(predictions, test_data$class, positive_class = 2)
calculate_accuracy(predictions, test_data$class, positive_class = 2)
calculate_f1_score(predictions, test_data$class, positive_class = 2)

# Function for k-fold cross-validation
k_fold_cross_validation <- function(data, features, target, k) {
  set.seed(123) # set seed for reproducibility
  folds <- sample(1:k, nrow(data), replace = TRUE) # k folds
  metrics <- data.frame(
    Precision = numeric(k),
    Recall = numeric(k),
    Accuracy = numeric(k),
    F1_Score = numeric(k)
  )
  # Evaluation
  for (i in 1:k) {
    train_data <- data[folds != i, ]
    test_data <- data[folds == i, ]
    
    tree <- build_decision_tree(train_data, features, target)
    
    predictions <- predict_tree(tree, test_data)
    
    metrics[i, "Precision"] <- calculate_precision(predictions, test_data$class, positive_class = 2)
    metrics[i, "Recall"] <- calculate_recall(predictions, test_data$class, positive_class = 2)
    metrics[i, "Accuracy"] <- calculate_accuracy(predictions, test_data$class, positive_class = 2)
    metrics[i, "F1_Score"] <- calculate_f1_score(predictions, test_data$class, positive_class = 2)
  }
  
  return(metrics)
}

# ROC Curve
roc_data <- calculate_roc_curve(predictions, actual_labels, positive_class = "p")
plot(roc_data$FP, roc_data$TP, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve")
abline(h = 1, v = 0, col = "red", lty = 2)

# k-fold cross validation
features <- names(data)[-1]
target <- names(data)[1]
k_fold_metrics <- k_fold_cross_validation(data, features, target = "class", k = 10)

# Print k-fold cross-validation metrics
print(k_fold_metrics)