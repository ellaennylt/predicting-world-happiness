## Setting working directory ---------------------------------------------------
getwd()
setwd(
  "C:/Users/ellae/OneDrive - Asia Pacific University/2023 S1 AML/Assignment Documents-20230411"
)

## Importing libraries for preprocessing ---------------------------------------
library(DataExplorer)
library(ggplot2)
library(dplyr)
library(corrplot)
library(gridExtra)
library(reshape2)
library(psych)
library(missForest)

## Reading the data ------------------------------------------------------------
dataset <- read.csv("World Happiness Report.csv",
                    header = T,
                    stringsAsFactors = T)

# Renaming some columns for visualisation
df <- dataset %>%
  rename(
    "Country" = "Country.Name",
    "Region" = "Regional.Indicator",
    "Life Ladder" = "Life.Ladder",
    "Log GDP per Capita" = "Log.GDP.Per.Capita",
    "Social Support" = "Social.Support",
    "Healthy Life Expectancy" = "Healthy.Life.Expectancy.At.Birth",
    "Freedom to Make Life Choices" = "Freedom.To.Make.Life.Choices",
    "Perceptions of Corruption" = "Perceptions.Of.Corruption",
    "Positive Affect" = "Positive.Affect",
    "Negative Affect" = "Negative.Affect",
    "Confidence in National Government" = "Confidence.In.National.Government"
  )

## Viewing, understanding and exploring the data -------------------------------
head(df, n = 5) # view first 5 rows
tail(df, n = 5) # view last 5 rows
dim(df) # determine how many rows and columns data frame has
View(df) # view the dataset using the data viewer
str(df) # view summary of data frame and data type of variables
sum(is.na(df)) # sum of missing values
df <-
  replace(df, df == '', NA) # replace empty spaces with NA
sum(is.na(df)) # sum of missing values after replacing empty spaces
colSums(is.na(df)) # find which variable has missing values
plot_missing(df) # find the percentage of missing values for each variable

## Histogram to determine distribution of data
plot_histogram(df, geom_histogram_args = list(bins = 17))

# Summary statistics
describe(df, IQR = T, omit = T)

## For plot display, initials of Regional Indicators used instead of their full names
# Central and Eastern Europe -- CEE
# Commonwealth of Independent States -- CIS
# East Asia -- EA
# Latin America and Caribbean -- LAC
# Middle East and North Africa -- MNA
# North America and ANZ -- NAA
# South Asia -- SA
# Southeast Asia -- SEA
# Sub-Saharan Africa -- SSA
# Western Europe -- WE
sapply(df, levels) # display all factor levels
levels(df$Region) = c("",
                      "CEE",
                      "CIS",
                      "EA",
                      "LAC",
                      "MNA",
                      "NAA",
                      "SA",
                      "SEA",
                      "SSA",
                      "WE")

## Boxplot of regions with each feature
pbp <- ggplot(data = df)
pbp1 <- pbp + geom_boxplot(aes(Region, Year))
pbp2 <- pbp + geom_boxplot(aes(Region, `Life Ladder`))
pbp3 <- pbp + geom_boxplot(aes(Region, `Log GDP per Capita`))
pbp4 <- pbp + geom_boxplot(aes(Region, `Social Support`))
pbp5 <- pbp + geom_boxplot(aes(Region, `Healthy Life Expectancy`))
pbp6 <-
  pbp + geom_boxplot(aes(Region, `Freedom to Make Life Choices`))
pbp7 <- pbp + geom_boxplot(aes(Region, Generosity))
pbp8 <- pbp + geom_boxplot(aes(Region, `Perceptions of Corruption`))
pbp9 <- pbp + geom_boxplot(aes(Region, `Positive Affect`))
pbp10 <- pbp + geom_boxplot(aes(Region, `Negative Affect`))
pbp11 <-
  pbp + geom_boxplot(aes(Region, `Confidence in National Government`))
pbpfin <-
  grid.arrange(pbp1,
               pbp2,
               pbp3,
               pbp4,
               pbp5,
               pbp6,
               pbp7,
               pbp8,
               pbp9,
               pbp10,
               pbp11,
               nrow = 4)
# Warning messages of row removal due to NAs discovered previously

## Scatter plot to determine relationship between Life Ladder and features
# Relationship between Log GDP per Capita and Life Ladder
psp1 = df %>%
  group_by(Region) %>%
  ggplot(aes(y = `Life Ladder`, x = `Log GDP per Capita`)) +
  geom_point() +
  geom_smooth(method = lm,
              aes(y = `Life Ladder`, x = `Log GDP per Capita`,
                  color = Region)) +
  facet_wrap( ~ Region, scales = "free") +
  labs(x = "Region", y = "Life Ladder",
       title = "Log GDP per Capita versus Life Ladder") +
  theme_minimal() +
  theme(legend.position = "none")

# Relationship between Social Support and Life Ladder
psp2 <- df %>%
  group_by(Region) %>%
  ggplot(aes(y = `Life Ladder`, x = `Social Support`)) +
  geom_point() +
  geom_smooth(method = lm, aes(y = `Life Ladder`, x = `Social Support`,
                               color = Region)) +
  facet_wrap(~ Region, scales = "free") +
  labs(x = "Region", y = "Life Ladder",
       title = "Social Support versus Life Ladder") +
  theme_minimal() +
  theme(legend.position = "none")

# Relationship between Healthy Life Expectancy and Life Ladder
psp3 <- df %>%
  group_by(Region) %>%
  ggplot(aes(y = `Life Ladder`, x = `Healthy Life Expectancy`)) +
  geom_point() +
  geom_smooth(method = lm,
              aes(y = `Life Ladder`, x = `Healthy Life Expectancy`,
                  color = Region)) +
  facet_wrap( ~ Region, scales = "free") +
  labs(x = "Region", y = "Life Ladder",
       title = "Healthy Life Expectancy versus Life Ladder") +
  theme_minimal() +
  theme(legend.position = "none")

# Relationship between Freedom to Make Life Choices and Life Ladder
psp4 <- df %>%
  group_by(Region) %>%
  ggplot(aes(y = `Life Ladder`, x = `Freedom to Make Life Choices`)) +
  geom_point() +
  geom_smooth(method = lm,
              aes(y = `Life Ladder`, x = `Freedom to Make Life Choices`,
                  color = Region)) +
  facet_wrap( ~ Region, scales = "free") +
  labs(x = "Region", y = "Life Ladder",
       title = "Freedom to Make Life Choices versus Life Ladder") +
  theme_minimal() +
  theme(legend.position = "none")

# Relationship between Generosity and Life Ladder
psp5 <- df %>%
  group_by(Region) %>%
  ggplot(aes(y = `Life Ladder`, x = `Generosity`)) +
  geom_point() +
  geom_smooth(method = lm, aes(y = `Life Ladder`, x = `Generosity`,
                               color = Region)) +
  facet_wrap( ~ Region, scales = "free") +
  labs(x = "Region", y = "Life Ladder",
       title = "Generosity versus Life Ladder") +
  theme_minimal() +
  theme(legend.position = "none")

# Relationship between Perceptions of Corruption and Life Ladder
psp6 <- df %>%
  group_by(Region) %>%
  ggplot(aes(y = `Life Ladder`, x = `Perceptions of Corruption`)) +
  geom_point() +
  geom_smooth(method = lm,
              aes(y = `Life Ladder`, x = `Perceptions of Corruption`,
                  color = Region)) +
  facet_wrap( ~ Region, scales = "free") +
  labs(x = "Region", y = "Life Ladder",
       title = "Perceptions of Corruption versus Life Ladder") +
  theme_minimal() +
  theme(legend.position = "none")

# Relationship between Positive Affect and Life Ladder
psp7 <- df %>%
  group_by(Region) %>%
  ggplot(aes(y = `Life Ladder`, x = `Positive Affect`)) +
  geom_point() +
  geom_smooth(method = lm,
              aes(y = `Life Ladder`, x = `Positive Affect`,
                  color = Region)) +
  facet_wrap( ~ Region, scales = "free") +
  labs(x = "Region", y = "Life Ladder",
       title = "Positive Affect versus Life Ladder") +
  theme_minimal() +
  theme(legend.position = "none")

# Relationship between Negative Affect and Life Ladder
psp8 <- df %>%
  group_by(Region) %>%
  ggplot(aes(y = `Life Ladder`, x = `Negative Affect`)) +
  geom_point() +
  geom_smooth(method = lm,
              aes(y = `Life Ladder`, x = `Negative Affect`,
                  color = Region)) +
  facet_wrap( ~ Region, scales = "free") +
  labs(x = "Region", y = "Life Ladder",
       title = "Negative Affect versus Life Ladder") +
  theme_minimal() +
  theme(legend.position = "none")

# Relationship between Confidence in National Government and Life Ladder
psp9 <- df %>%
  group_by(Region) %>%
  ggplot(aes(y = `Life Ladder`, x = `Confidence in National Government`)) +
  geom_point() +
  geom_smooth(method = lm,
              aes(y = `Life Ladder`, x = `Confidence in National Government`,
                  color = Region)) +
  facet_wrap( ~ Region, scales = "free") +
  labs(x = "Region", y = "Life Ladder",
       title = "Confidence in National Government versus Life Ladder") +
  theme_minimal() +
  theme(legend.position = "none")

pspfin <-
  grid.arrange(psp1, psp2, psp3, psp4, psp5, psp6, psp7, psp8, psp9) # group scatter plots together

## Correlation heatmap
cormat <- round(cor(df[3:13], use = "pairwise.complete.obs"), 2)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

# Use correlation between variables as distance
reorder_cormat <- function(cormat) {
  dd <- as.dist((1 - cormat) / 2)
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
}

# Reorder and get the upper triangle of the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)

# Remove the upper triangle from correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Plot the correlation heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = "Pearson\nCorrelation"
  ) +
  theme_minimal(base_size = 12, base_family = "") +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  )) +
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black",
            size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.4, 0.7),
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_colorbar(
    barwidth = 7,
    barheight = 1,
    title.position = "top",
    title.hjust = 0.5
  ))

## Determine countries who have data from 2005 to 2022
# df_nonunique <- df %>%
#   group_by(Country) %>%
#   filter(n() == 18 ) %>%
#   ungroup()

## Data Cleaning ---------------------------------------------------------------
dffin <- df

# Drop non-related, non-highly correlated columns
dffin <-
  drop_columns(
    dffin,
    c(
      "Country",
      "Year",
      "Region",
      "Confidence in National Government",
      "Generosity",
      "Perceptions of Corruption",
      "Negative Affect"
    )
  )
colSums(is.na(dffin))

# Missing values imputation using MissForest
dimp <- missForest(dffin, verbose = TRUE)
dffin <- dimp$ximp
sum(is.na(dffin))

# Replace outliers with mean
dffin %>% mutate_if(is.numeric, ~replace(., . %in% boxplot.stats(.)$out, mean(.)))
describe(dffin, IQR = T, omit = T)

# Min-max normalisation for all continuous variables
normalise <- function(x) {
  x <- (x - min(x)) / (max(x) - min(x))
  return(x)
}

dffin$`Log GDP per Capita` <- normalise(dffin$`Log GDP per Capita`)
dffin$`Healthy Life Expectancy` <-
  normalise(dffin$`Healthy Life Expectancy`)
dffin$`Social Support` <- normalise(dffin$`Social Support`)
dffin$`Freedom to Make Life Choices` <-
  normalise(dffin$`Freedom to Make Life Choices`)
dffin$`Positive Affect` <- normalise(dffin$`Positive Affect`)
describe(dffin)

# Rename variables for model building
dfmb <- dffin %>%
  rename(
    "LifeLadder" = "Life Ladder",
    "LogGDP" = "Log GDP per Capita",
    "SocialSupport" = "Social Support",
    "HealthyLifeExpectancy" = "Healthy Life Expectancy",
    "Freedom" = "Freedom to Make Life Choices",
    "PositiveAffect" = "Positive Affect",
  )

## Split the data into test and train
# Importing library for splitting
library(caTools)
set.seed(111)
split = sample.split(dfmb$LifeLadder, SplitRatio = 0.7)
training_set = subset(dfmb, split == TRUE)
test_set = subset(dfmb, split == FALSE)
dim(training_set)
dim(test_set)

## Model Building: Multiple Linear Regression ----------------------------------
# Fitting Multiple Linear Regression to the Training set
library(caret)
mlr1 = lm(formula = LifeLadder ~ ., data = training_set)
summary(mlr1)

# Results
plot(mlr1)

# Predicting the Training set results
mlry1_pred_train = predict(mlr1, newdata = training_set)

# Predicting the Test set results
mlry1_pred_test = predict(mlr1, newdata = test_set)
mlry1_test_table <- data.frame(mlry1_pred_test, test_set$LifeLadder)

# Fit and error values
library(Metrics)
dfmlr1 <- data.frame(
  Set = c("Train", "Test"),
  R2 = c(R2(mlry1_pred_train, training_set$LifeLadder),
          R2(mlry1_pred_test, test_set$LifeLadder)),
  RMSE = c(RMSE(mlry1_pred_train, training_set$LifeLadder),
            RMSE(mlry1_pred_test, test_set$LifeLadder)),
  MSE = c(mse(mlry1_pred_train, training_set$LifeLadder),
          mse(mlry1_pred_test, test_set$LifeLadder)))
dfmlr1

# MLR Model Tuning: Repeated K-fold Cross Validation Grid Search
# Define training control
customctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search="grid", verboseIter = T)

# Fit a MLR Model with Cross Validation
set.seed(111) 
mlrRKCV1 <- train(LifeLadder ~ ., data = training_set, method = "lm",
                   trControl = customctrl)
summary(mlrRKCV)

# Performance Measures
print(mlrRKCV)
# mlrRKCV$resample
plot(mlrRKCV$finalModel)
plot(varImp(mlrRKCV, scale = F))

# Predicting the Training set results
mlryrkcv_pred_train = predict(mlrRKCV, newdata = training_set)

# Predicting the Test set results
mlryrkcv_pred_test = predict(mlrRKCV, newdata = test_set)
mlryrkcv_pred_test_table <- data.frame(mlryrkcv_pred_test, test_set$LifeLadder)

dfmlrRKCV <- data.frame(
  Set = c("Train", "Test"),
  R2 = c(R2(mlryrkcv_pred_train, training_set$LifeLadder),
         R2(mlryrkcv_pred_test, test_set$LifeLadder)),
  RMSE = c(RMSE(mlryrkcv_pred_train, training_set$LifeLadder),
           RMSE(mlryrkcv_pred_test, test_set$LifeLadder)),
  MSE = c(mse(mlryrkcv_pred_train, training_set$LifeLadder),
          mse(mlryrkcv_pred_test, test_set$LifeLadder)))
dfmlrRKCV 

## Model Building: LASSO Regression --------------------------------------------
library(glmnet)

# Define matrix of predictive features
xTrain = model.matrix(LifeLadder ~ ., data = training_set)[,-1]
xTest = model.matrix(LifeLadder ~ ., data = test_set)[,-1]

# Fitting LASSO Regression to the Training set
lasso <- glmnet(xTrain, 
                training_set$LifeLadder, family="gaussian",
                alpha = 1)

# Getting the best lambda
cv_out <- cv.glmnet(xTrain, training_set$LifeLadder, alpha = 1)
plot(cv_out) # draw plot of training MSE as a function of lambda

lambda_min <- cv_out$lambda.min
lambda_min
lambda_1se <- cv_out$lambda.1se
lambda_1se

plot(lasso, xvar = 'lambda', label = T) # draw plot of coefficients
abline(v = log (cv_out$lambda.1se), col = "red", lty = "dashed")
abline(v = log (cv_out$lambda.min), col = "blue", lty = "dashed")


# Building LASSO Regression model using best lambda
lasso1 <- glmnet(xTrain, training_set$LifeLadder, family="gaussian", lambda = lambda_1se, alpha=1)
coef(lasso1)
plot(coef(lasso1))

# Predicting the Training set results 
lasso1y_pred_train = predict(lasso1, xTrain)

# Predicting the Test set results
lasso1y_pred_test = predict(lasso1, xTest)
lasso1y_test_table <- data.frame(lasso1y_pred_test, test_set$LifeLadder)

# Fit and error values
dflasso1 <- data.frame(
  Set = c("Train", "Test"),
  R2 = c(R2(lasso1y_pred_train, training_set$LifeLadder),
         R2(lasso1y_pred_test, test_set$LifeLadder)),
  RMSE = c(RMSE(lasso1y_pred_train, training_set$LifeLadder),
           RMSE(lasso1y_pred_test, test_set$LifeLadder)),
  MSE = c(mse(lasso1y_pred_train, training_set$LifeLadder),
          mse(lasso1y_pred_test, test_set$LifeLadder)))
dflasso1

# LASSO Model Tuning: Repeated K-fold Cross Validation Grid Search
# Define training control
customctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search="grid", verboseIter = T)

# Fit a LASSO Model with Cross Validation
set.seed(111) 
lassoRKCV <- train(
    LifeLadder ~ .,
    data = training_set,
    method = "glmnet",
    tuneGrid = expand.grid(alpha = 1,
                           lambda = seq(0.001, 1, length = 5)),
    trControl = customctrl)

# Performance Measures
print(lassoRKCV)
# plot(lassoRKCV$finalModel, xvar = 'lambda', label = T)
# abline(v = log (lassoRKCV$bestTune), col = "red", lty = "dashed")
plot(varImp(lassoRKCV, scale=T))

# Building lasso with RKCV best lambda
lassoRKCVf = glmnet(xTrain, training_set$LifeLadder, family="gaussian", lambda = 0.0010, alpha=1)
coef(lassoRKCVf)
# plot(coef(lassoRKCVf))

# Predicting the Training set results
lassoRKCV_pred_train = predict(lassoRKCVf, xTrain)

# Predicting the Test set results
lassoRKCV_pred_test = predict(lassoRKCVf, xTest)
lassoRKCV_pred_test_table <- data.frame(lassoRKCV_pred_test, test_set$LifeLadder)

dflassoRKCV <- data.frame(
  Set = c("Train", "Test"),
  R2 = c(R2(lassoRKCV_pred_train, training_set$LifeLadder),
         R2(lassoRKCV_pred_test, test_set$LifeLadder)),
  RMSE = c(RMSE(lassoRKCV_pred_train, training_set$LifeLadder),
           RMSE(lassoRKCV_pred_test, test_set$LifeLadder)),
  MSE = c(mse(lassoRKCV_pred_train, training_set$LifeLadder),
          mse(lassoRKCV_pred_test, test_set$LifeLadder)))
dflassoRKCV 


## Model Building: Random Forest -----------------------------------------------
library(randomForest)
# Fitting Random Forest to the Training set
set.seed(111)
rf1 <- randomForest(LifeLadder ~ ., data = training_set)
print(rf1)

# Results
plot(rf1)

# Predicting the Training set results
rf1y_pred_train <- predict(rf1, training_set)

# Predicting the Test set results
rf1y_pred_test <- predict(rf1, test_set)
rf1y_test_table <- data.frame(rf1y_pred_test, test_set$LifeLadder)

# Fit and error values
dfrf1 <- data.frame(
  Set = c("Train", "Test"),
  R2 = c(R2(rf1y_pred_train, training_set$LifeLadder),
         R2(rf1y_pred_test, test_set$LifeLadder)),
  RMSE = c(RMSE(rf1y_pred_train, training_set$LifeLadder),
           RMSE(rf1y_pred_test, test_set$LifeLadder)),
  MSE = c(mse(rf1y_pred_train, training_set$LifeLadder),
          mse(rf1y_pred_test, test_set$LifeLadder)))
dfrf1

# Random Forest Model Tuning: Repeated K-fold Cross Validation Grid Search
# Define training control
customctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search="grid", verboseIter = T)

# Fit a Random Forest Model with Cross Validation
# Search for best mtry
set.seed(111)
tunegrid2 <- expand.grid(.mtry=c(1:5))
rf_grid <- train(LifeLadder ~ ., data = training_set, method="rf", metric="RMSE", tuneGrid= tunegrid2, trControl = customctrl)
print(rf_grid)
plot(rf_grid)
# best mtry = 3

# Search for best ntrees
store_maxtrees <- list()
for (ntree in c(500,1000,1500,2000)) {
  set.seed(111)
  rf_maxtrees <- train(LifeLadder ~ .,
                       data = training_set,
                       method = "rf", metric="RMSE",
                       tuneGrid = expand.grid(.mtry = 3),
                       trControl = customctrl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}

# Results
results_ntrees <- resamples(store_maxtrees)
summary(results_ntrees)
# library(lattice)
# dotplot(results_ntrees)

# Final Random Forest model 
rffinRKCV <- randomForest(LifeLadder ~ ., data = training_set,
                       mtry = 3, ntree = 1050,
                       importance = TRUE,
                       proximity = TRUE)

# Performance Measures
print(rffinRKCV)
plot(rffinRKCV)

# Predicting the Training set results
rffinRKCV_pred_train <- predict(rffinRKCV, training_set)

# Predicting the Test set results
rffinRKCV_pred_test <- predict(rffinRKCV, test_set)
rffinRKCV_pred_test_table <- data.frame(rffinRKCV_pred_test, test_set$LifeLadder)

dfrffinRKCV <- data.frame(
  Set = c("Train", "Test"),
  R2 = c(R2(rffinRKCV_pred_train, training_set$LifeLadder),
         R2(rffinRKCV_pred_test, test_set$LifeLadder)),
  RMSE = c(RMSE(rffinRKCV_pred_train, training_set$LifeLadder),
           RMSE(rffinRKCV_pred_test, test_set$LifeLadder)),
  MSE = c(mse(rffinRKCV_pred_train, training_set$LifeLadder),
          mse(rffinRKCV_pred_test, test_set$LifeLadder)))
dfrffinRKCV

# Tuning hyperparameters using custom algorithm: mtry and ntree 
x <- dfmb %>% 
  select(LogGDP, SocialSupport, HealthyLifeExpectancy, Freedom, PositiveAffect)
y <- dfmb$LifeLadder

X_train_ = training_set[1:800, -1]
y_train_ = training_set[1:800, 1]

customRF <- list(type = "Regression", library = "randomForest", loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree = param$ntree, ...)
}

# Predict label
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)

# Predict prob
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Set grid search parameters
customctrl <- trainControl(method = "repeatedcv", number=10, repeats=3, search = "grid", allowParallel = TRUE)

# Outline the grid of parameters
tunegrid <- expand.grid(.mtry=c(1:5), .ntree=c(500,1000,1500,2000))
set.seed(111)

# Train the model
rfcustom <- train(x=X_train_, y=y_train_, method = customRF, metric="RMSE", tuneGrid = tunegrid, trControl = customctrl)

print(rfcustom)
plot(rfcustom)
varImpPlot(rfcustom$finalModel, main ='Feature Importance')

# Build Random Forest model with best hyperparameters
rffincustom <- randomForest(LifeLadder ~ ., data = training_set,
                          mtry = 2, ntree = 2000,
                          importance = TRUE,
                          proximity = TRUE)

# Performance Measures
print(rffincustom)
plot(rffincustom)

# Predicting the Training set results
rffincustom_pred_train <- predict(rffincustom, training_set)

# Predicting the Test set results
rffincustom_pred_test <- predict(rffincustom, test_set)
rffincustom_pred_test_table <- data.frame(rffincustom_pred_test, test_set$LifeLadder)

dfrfcustomRKCV <- data.frame(
  Set = c("Train", "Test"),
  R2 = c(R2(rffincustom_pred_train, training_set$LifeLadder),
         R2(rffincustom_pred_test, test_set$LifeLadder)),
  RMSE = c(RMSE(rffincustom_pred_train, training_set$LifeLadder),
           RMSE(rffincustom_pred_test, test_set$LifeLadder)),
  MSE = c(mse(rffincustom_pred_train, training_set$LifeLadder),
          mse(rffincustom_pred_test, test_set$LifeLadder)))
dfrfcustomRKCV

## Compare models and results --------------------------------------------------
# Model performance comparison
Model <- c("MLR","LASSO","Tuned LASSO", "Random Forest", "Tuned Random Forest", "Custom Tuned Random Forest")

# R2
TrainR2 <- c(dfmlr1$R2[1], dflasso1$R2[1], dflassoRKCV$R2[1], dfrf1$R2[1], dfrffinRKCV$R2[1], dfrfcustomRKCV$R2[1])
TestR2 <- c(dfmlr1$R2[2], dflasso1$R2[2], dflassoRKCV$R2[2], dfrf1$R2[2], dfrffinRKCV$R2[2], dfrfcustomRKCV$R2[2])
dfR2 <- data.frame(Model, TrainR2, TestR2)
library(knitr)
kable(dfR2, longtable = TRUE, booktabs = TRUE, digits = 3, col.names =c("Models", "R2 for Train", "R2 for Test"))

library(tidyverse)
df.R2 <- gather(dfR2, Dataset, R2, -Model, factor_key =TRUE)
df.R2$R2 <-round(df.R2$R2,3)
order <- c(1,2,3,4,5,6,1,2,3,4,5,6)
R2plot <- ggplot(data = df.R2, aes(x = reorder(Model, +order), y = R2, fill = Dataset)) +
  geom_col(position = position_dodge()) +
  scale_fill_discrete(labels=c("Train", "Test")) + theme_minimal() +
  xlab("Model")
R2plot + theme(axis.text.x = element_text(angle = 45, hjust=1))

# RMSE
TrainRMSE <- c(dfmlr1$RMSE[1], dflasso1$RMSE[1], dflassoRKCV$RMSE[1], dfrf1$RMSE[1], dfrffinRKCV$RMSE[1], dfrfcustomRKCV$RMSE[1])
TestRMSE <- c(dfmlr1$RMSE[2], dflasso1$RMSE[2], dflassoRKCV$RMSE[2], dfrf1$RMSE[2], dfrffinRKCV$RMSE[2], dfrfcustomRKCV$RMSE[2])
dfRMSE <- data.frame(Model, TrainRMSE, TestRMSE)
kable(dfRMSE, longtable = TRUE, booktabs = TRUE, digits = 3, col.names =c("Models", "RMSE for Train", "RMSE for Test"))

df.RMSE <- gather(dfRMSE, Dataset, RMSE, -Model, factor_key =TRUE)
df.RMSE$RMSE <-round(df.RMSE$RMSE,3)
order <- c(1,2,3,4,5,6,1,2,3,4,5,6)
RMSEplot <- ggplot(data = df.RMSE, aes(x = reorder(Model, +order), y = RMSE, fill = Dataset)) +
  geom_col(position = position_dodge()) +
  scale_fill_discrete(labels=c("Train", "Test")) + theme_minimal() +
  xlab("Model")
RMSEplot + theme(axis.text.x = element_text(angle = 45, hjust=1))

# MSE
TrainMSE <- c(dfmlr1$MSE[1], dflasso1$MSE[1], dflassoRKCV$MSE[1], dfrf1$MSE[1], dfrffinRKCV$MSE[1], dfrfcustomRKCV$MSE[1])
TestMSE <- c(dfmlr1$MSE[2], dflasso1$MSE[2], dflassoRKCV$MSE[2], dfrf1$MSE[2], dfrffinRKCV$MSE[2], dfrfcustomRKCV$MSE[2])
dfMSE <- data.frame(Model, TrainMSE, TestMSE)
kable(dfMSE, longtable = TRUE, booktabs = TRUE, digits = 3, col.names =c("Models", "MSE for Train", "MSE for Test"))

df.MSE <- gather(dfMSE, Dataset, MSE, -Model, factor_key =TRUE)
df.MSE$MSE <-round(df.MSE$MSE,3)
order <- c(1,2,3,4,5,6,1,2,3,4,5,6)
MSEplot <- ggplot(data = df.MSE, aes(x = reorder(Model, +order), y = MSE, fill = Dataset)) +
  geom_col(position = position_dodge()) +
  scale_fill_discrete(labels=c("Train", "Test")) + theme_minimal() +
  xlab("Model")
MSEplot + theme(axis.text.x = element_text(angle = 45, hjust=1))

# RMSE, MSE, and R2
dfmlr1 # mlr not overffited
dfmlrRKCV

dflasso1
dflassoRKCV # better lasso

dfrf1
dfrffinRKCV 
dfrfcustomRKCV # better rf

# MAE(mlryrkcv_pred_train, training_set$LifeLadder)
# MAE(mlryrkcv_pred_test, test_set$LifeLadder)
# 
# MAE(lassoRKCV_pred_train, training_set$LifeLadder)
# MAE(lassoRKCV_pred_test, test_set$LifeLadder)
# 
# MAE(rffinRKCV_pred_train, training_set$LifeLadder)
# MAE(rffinRKCV_pred_test, test_set$LifeLadder)

# Visualisation of actual vs predicted
# MLR
figMLR <- ggplot(data = mlry1_test_table) +
  geom_point(
    aes(
      x = 1:length(test_set.LifeLadder),
      y = test_set.LifeLadder, colour = "red", alpha = 0.5)
  ) +
  geom_point(
    aes(x = 1:length(mlry1_pred_test), y = mlry1_pred_test, colour = "blueviolet", alpha = 0.5)
  ) +
  labs(x = "Observations",
       y = "Life Ladder",
       color = "") +
  theme_minimal() + 
  scale_color_manual(labels = c( "Actual", "MLR"), values = c("red", "blueviolet")) 

print(figMLR)

# LASSO
figLASSO <- ggplot(data = lassoRKCV_pred_test_table) +
  geom_point(
    aes(
      x = 1:length(test_set.LifeLadder),
      y = test_set.LifeLadder, colour = "red", alpha = 0.5)
  ) +
  geom_point(
    aes(x = 1:length(s0), y = s0, colour = "blue", alpha = 0.5)
  ) +
  labs(x = "Observations",
       y = "Life Ladder",
       color = "") +
  theme_minimal() + 
  scale_color_manual(labels = c( "Actual", "LASSO"), values = c("red", "blue")) 

print(figLASSO)

# RF
figRF <- ggplot(data = rffincustom_pred_test_table) +
  geom_point(
    aes(
      x = 1:length(test_set.LifeLadder),
      y = test_set.LifeLadder, colour = "red", alpha = 0.5)
  ) +
  geom_point(
    aes(x = 1:length(rffincustom_pred_test), y = rffincustom_pred_test, colour = "darkgreen", alpha = 0.5)
  ) +
  labs(x = "Observations",
       y = "Life Ladder",
       color = "") +
  theme_minimal() + 
  scale_color_manual(labels = c( "Actual", "RF"), values = c("red", "darkgreen")) 

print(figRF)
