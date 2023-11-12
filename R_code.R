# Read the CSV file
data <- read.csv("proportional_species_richness_V3.csv")

# Get column names
column_names <- names(data)

# Print the column names
print(column_names)

#Data Exploration
# Extracting columns related to BD7
BD7 <- data[, c("Bees", "Bird", "Bryophytes", "Butterflies", "Hoverflies", "Isopods", "Ladybirds")]

# Univariate exploration
summary(BD7)


library(corrplot)
# Correlation matrix for BD7 variables
correlation_matrix <- cor(BD7)
print(correlation_matrix)
# Visualize the correlation matrix using corrplot
corrplot(correlation_matrix, method = "circle", type = "lower")

# If considering other variables
other_vars <- data[, c("period", "dominantLandClass", "Easting", "Northing")]
summary(other_vars)



#Hypothesis tests
t_test_func <- function(var1, var2) {
  test <- t.test(var1, var2)
  return(test$p.value)
}
chi_test_func <- function(var1, var2) {
  table <- table(var1, var2)
  test <- chisq.test(table)
  return(test$p.value)
}

vars <- c("Bees", "Bird", "Bryophytes", "Butterflies", "Hoverflies", "Isopods", "Ladybirds")
results_t_test <- matrix(NA, nrow=length(vars), ncol=length(vars))
results_chi_test <- matrix(NA, nrow=length(vars), ncol=length(vars))
rownames(results_t_test) <- vars
colnames(results_t_test) <- vars
rownames(results_chi_test) <- vars
colnames(results_chi_test) <- vars

for (i in 1:length(vars)) {
  for (j in i:length(vars)) {
    if (i != j) {
      results_t_test[i, j] <- t_test_func(data[[vars[i]]], data[[vars[j]]])
      results_chi_test[i, j] <- chi_test_func(data[[vars[i]]], data[[vars[j]]])
    }
  }
}
print("T-test Results:")
print(results_t_test)

print("Chi-square Test Results:")
print(results_chi_test)

library(ggplot2)
library(reshape2)

# Transform data into a format suitable for ggplot2
melted_t_test <- melt(results_t_test)
melted_chi_test <- melt(results_chi_test)

# Plot heatmap for t-test results
ggplot(melted_t_test, aes(x=Var1, y=Var2)) + 
  geom_tile(aes(fill=value), colour="white") +
  scale_fill_gradient(low="white", high="red", na.value = NA) +
  theme_minimal() +
  labs(title="T-test Results", fill="P-value")

# Plot heatmap for chi-square test results
ggplot(melted_chi_test, aes(x=Var1, y=Var2)) + 
  geom_tile(aes(fill=value), colour="white") +
  scale_fill_gradient(low="white", high="red", na.value = NA) +
  theme_minimal() +
  labs(title="Chi-square Test Results", fill="P-value")





#Simple linear regression
data$BD11 <- rowMeans(data[,c("Bees", "Bird", "Bryophytes", "Butterflies", "Carabids", "Hoverflies", "Isopods", "Ladybirds", "Macromoths", "Grasshoppers_._Crickets", "Vascular_plants")])
data$BD7 <- rowMeans(data[,c("Bees", "Bird", "Bryophytes", "Butterflies", "Hoverflies", "Isopods", "Ladybirds")])

regression_all <- lm(BD7 ~ BD11, data=data)
summary(regression_all)
unique_periods <- unique(data$period)
for(period in unique_periods) {
  period_data <- subset(data, period == period)
  regression_period <- lm(BD7 ~ BD11, data=period_data)
  print(paste("Regression for period:", period))
  print(summary(regression_period))
}



#Multiple linear regression
#the value of BD7 has already been calculated
remaining_taxa <- setdiff(c("Bees", "Bird", "Bryophytes", "Butterflies", "Carabids", "Hoverflies", "Isopods", "Ladybirds", "Macromoths", "Grasshoppers_._Crickets", "Vascular_plants"),
                          c("Bees", "Bird", "Bryophytes", "Butterflies", "Hoverflies", "Isopods", "Ladybirds"))
data$BD4 <- rowMeans(data[, remaining_taxa])


mlr_model <- lm(BD4 ~ Bees + Bird + Bryophytes + Butterflies + Hoverflies + Isopods + Ladybirds, data=data)
summary(mlr_model)

# Remove non-significant predictor variables based on p-values
# Assuming our initial model is mlr_model, we use the step() function progressively.
reduced_model <- step(mlr_model, direction="backward", test="F")


#Open analysis
#1. BD7 vs. Period:
lm_period <- lm(BD7 ~ period, data=data)
summary(lm_period)
#2. BD7 vs. Dominant Land Class:
lm_landClass <- lm(BD7 ~ dominantLandClass, data=data)
summary(lm_landClass)
#3. BD7 vs. Easting and Northing:
lm_easting <- lm(BD7 ~ Easting, data=data)
lm_northing <- lm(BD7 ~ Northing, data=data)
summary(lm_easting)
summary(lm_northing)
#Multiple Linear Regression:
mlr_all_vars <- lm(BD7 ~ period + dominantLandClass + Easting + Northing, data=data)
summary(mlr_all_vars)

















