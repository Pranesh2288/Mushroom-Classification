# Mushroom Classification using C4.5 Algorithm

## Overview

This repository demonstrates a machine learning approach to mushroom classification, focusing on distinguishing between edible and poisonous mushrooms. The project employs the C4.5 decision tree algorithm and includes comprehensive data analysis, visualization, and model development.

## Dataset

The dataset used in this project is the Mushroom Classification dataset, which includes:

### Data Composition

- Total samples: 8,124 mushroom instances
- Features: 22 categorical attributes
- Target variable: Binary (edible/poisonous)
- Class distribution: Well-balanced (48% poisonous, 52% edible)

### Features

1. Physical Characteristics:

   - Cap: shape, surface, color
   - Gill: attachment, spacing, color
   - Stalk: shape, root, surface, color
   - Veil: type, color
   - Ring: number, type
   - Spore print color

2. Environmental Attributes:

   - Habitat
   - Population

3. Other Properties:
   - Odor
   - Bruises

## Study Process

### a) Data Pre-processing

- Converted all categorical variables to factors
- Removed 'veil.type' due to having only one unique value
- No missing values in the dataset except in 'stalk-root' feature
- Well-balanced classes: 48% poisonous, 52% edible

### b) Model Development

- Used C4.5 algorithm (J48 implementation)
- Split ratio: 70% training, 30% testing
- Implemented 10-fold cross-validation
- Parameter tuning grid:
  - Confidence factor (C): 0.01, 0.1, 0.25
  - Minimum instances per leaf (M): 2, 5, 10

### c) Feature Importance Analysis

The feature importance was calculated using Information Gain Ratio, which measures how much information a feature provides for classifying mushrooms while accounting for the feature's intrinsic information. Scores range from 0 (no predictive value) to 1 (perfect predictor).

Most significant predictive features:

1. Odor (0.628) - Primary discriminator

   - Contains 9 distinct values (none, almond, anise, creosote, fishy, foul, musty, pungent, spicy)
   - Nearly perfect predictor alone with 98.52% accuracy
   - Key finding: Poisonous mushrooms never have pleasant odors (almond/anise)
   - All mushrooms with foul/fishy/musty/pungent odors are poisonous

2. Spore print color (0.339) - Secondary indicator

   - 9 distinct colors provide reliable classification patterns
   - Essential taxonomic characteristic
   - Strong correlation with mushroom species and edibility

3. Gill color (0.294) - Tertiary feature

   - 12 distinct values offering detailed classification
   - Works synergistically with spore print color
   - Critical morphological identifier

4. Ring type (0.223) - Supporting feature
   - 5 distinct types providing additional validation
   - Important when combined with primary features
   - Helps resolve ambiguous cases
   - Supporting features that help refine classification
   - Useful for edge cases and verification
   - Provide ecological context

## Results

### Model Performance

- Accuracy: 100% on test set
- ROC AUC: 0.9994
- Sensitivity (True Positive Rate): 100%
- Specificity (True Negative Rate): 100%
- Cross-validation accuracy: 99.90% ± 0.14%

### Best Model Parameters

- Confidence Factor (C) = 0.25
- Minimum instances per leaf (M) = 2

### Understanding the Perfect Accuracy

The model achieved 100% accuracy due to several factors:

1. Strong Feature Relationships:

   - Odor alone can predict mushroom edibility with 98.52% accuracy
   - Combination of odor and spore print color provides near-perfect separation
   - Multiple features have strong correlations with edibility

2. Clear Biological Patterns:

   - Certain odors (e.g., foul, pungent) are strong indicators of poisonous mushrooms
   - Specific combinations of gill color and spore print color are definitive markers
   - Ring type and stalk characteristics provide additional confirmation

3. Data Quality:
   - Well-balanced dataset
   - Clear categorical distinctions in features
   - Minimal noise in the data

### Practical Applications

For field identification, a simplified decision tree using just the top 3 features can provide highly accurate results:

1. Check the odor (most important)
2. Examine spore print color
3. Observe gill color

However, for safety reasons, mushroom identification should always be verified by experts and not rely solely on this model.

## References

- Original dataset: [Mushroom Classification](https://www.kaggle.com/datasets/uciml/mushroom-classification)
- UCI Machine Learning Repository: [Mushroom Data Set](https://archive.ics.uci.edu/ml/datasets/mushroom)

## Warning

⚠️ This model is for educational purposes only. Never use it as the sole method for determining if a mushroom is safe to eat. Always consult mycology experts for mushroom identification.

## Dependencies

- R version 4.0 or higher
- Required R packages:
  - caret (for model training)
  - dplyr (for data manipulation)
  - ggplot2 (for visualization)
  - RWeka (for J48/C4.5 implementation)
  - rpart & rpart.plot (for decision tree visualization)
  - corrplot (for correlation visualization)
  - pROC (for ROC curve analysis)
  - FSelector (for feature importance)
  - e1071 (for additional metrics)

## Usage

1. Installation

   ```R
   install.packages(c("caret", "dplyr", "ggplot2", "RWeka",
                     "rpart", "rpart.plot", "corrplot", "pROC",
                     "e1071", "FSelector"))
   ```

2. Execution
   ```R
   source("Project015_code.R")
   ```
