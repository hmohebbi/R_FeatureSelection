# R_FeatureSelection
Forward and backward feature selection with adjusted R squared in R


# Usage
```
dataset_path <- "./insurance.csv"
data <- read.csv(dataset_path, header=TRUE, sep=',')
data$children <- as.factor(data$children)

# Backward Selection with Adjusted R Squared:
reg_backw <- backwardSelection_adjRsquared(data, response = "charges", verbose = TRUE)


# Forward Selection with Adjusted R Squared:
reg_forw <- forwardSelection_adjRsquared(data, response = "charges", verbose = TRUE)
```

