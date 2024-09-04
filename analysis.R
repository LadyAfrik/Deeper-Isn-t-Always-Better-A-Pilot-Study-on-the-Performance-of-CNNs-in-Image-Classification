# Load necessary libraries
library(readxl)
library(ggplot2)
library(reshape2)
library(corrplot)

# Load the data
df <- read_excel("path_to_your_file/model_results.xlsx")

# Perform linear regression for Valid accuracy [opt]
model_valid_accuracy <- lm(`Valid accuracy [opt]` ~ `No of CNN`, data = df)
summary(model_valid_accuracy)

# Perform linear regression for Number_of_Wrong Predictions
model_wrong_predictions <- lm(`Number_of_Wrong Predictions` ~ `No of CNN`, data = df)
summary(model_wrong_predictions)

# Perform linear regression for Number_of_Hard Predictions
model_hard_predictions <- lm(`Number_of_Hard Predictions` ~ `No of CNN`, data = df)
summary(model_hard_predictions)

# Correlation matrix
cor_matrix <- cor(df[, c("No of CNN", "Valid accuracy [opt]", "Number_of_Wrong Predictions", "Number_of_Hard Predictions")])

# Create a heatmap
heatmap_data <- melt(cor_matrix)
ggplot(data = heatmap_data, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

# Alternatively, using the corrplot package for a more refined heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", 
         addCoef.col = "black", tl.srt = 45)
