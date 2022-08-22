# Importing Libraries
library(odbc)
library(dplyr)
library(corrplot)
library(leaps)

# Connecting to SQL Database
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "DESKTOP-GSAK43D",
                 Database = "HomePrediction",
                 Port = 1433)
# Listing tables & fields in Database
dbListTables(con)
dbListFields(con, 'train')

# Converting SQL tables as an R table and storing in containers 
traindata <- tbl(con, 'train')
testdata <- tbl(con, 'test')

# Collecting records and restoring
traindata <- collect(traindata)
testdata <- collect(testdata)

# Inspecting tables
View(traindata)
View(testdata)

# Disconnecting SQL from R
dbDisconnect(con)

summary(traindata)
summary(testdata)

# Correlation plot for traindata
num <- unlist(lapply(traindata, is.numeric), use.names = FALSE)
corrplot(cor(traindata[num]),method = 'number', tl.cex =0.5)

# Converting non-numeric column datatypes to numeric datatype
traindata <- traindata %>% mutate_at(c('LotFrontage', 'MasVnrArea', 'BsmtFinSF1',
                                       'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF',
                                       '2ndFlrSF', 'LowQualFinSF', 'BsmtFullBath',
                                       'BsmtHalfBath', 'FullBath', 'HalfBath',
                                       'BedroomAbvGr', 'KitchenAbvGr', 'Fireplaces',
                                       'GarageCars', 'GarageArea', 'WoodDeckSF',
                                       'OpenPorchSF', 'EnclosedPorch', '3SsnPorch',
                                       'ScreenPorch', 'PoolArea', 'MiscVal',
                                       'MoSold'), as.numeric)
testdata <- testdata %>% mutate_at(c('LotFrontage', 'OverallCond', 'MasVnrArea', 'BsmtFinSF1',
                                     'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF',
                                     '2ndFlrSF', 'LowQualFinSF', 'BsmtFullBath',
                                     'BsmtHalfBath', 'FullBath', 'HalfBath',
                                     'BedroomAbvGr', 'KitchenAbvGr', 'Fireplaces',
                                     'GarageCars', 'GarageArea', 'WoodDeckSF',
                                     'OpenPorchSF', 'EnclosedPorch', '3SsnPorch',
                                     'ScreenPorch', 'PoolArea', 'MiscVal',
                                     'MoSold'), as.numeric)
str(traindata)
str(testdata)

# Correlation plot for train & test data after datatype convertion
num <- unlist(lapply(traindata, is.numeric), use.names = FALSE)
corrplot(cor(traindata[num]),method = 'number', tl.cex =0.5)
num <- unlist(lapply(testdata, is.numeric), use.names = FALSE)
corrplot(cor(testdata[num]),method = 'number', tl.cex =0.5)

# Slicing categorical columns/variables on train & test data
df_train <- select(traindata, c(-1,-3:-4,-6:-17,-20:-26,-28:-34,-36,-40:-43,-54,
                                -56,-58:-61,-64:-66,-73:-75,-77:-80))
df_test <- select(testdata, c(-1,-3:-4,-6:-17,-20:-26,-28:-34,-36,-40:-43,-54,
                              -56,-58:-61,-64:-66,-73:-75,-77:-80))

lapply(df_train, class)
lapply(df_test, class)

View(df_train)
View(df_test)

# Dropping NAs from both train & test data 
cleaned_df_train <- na.omit(df_train)
cleaned_df_test <- na.omit(df_test)

View(cleaned_df_train)
View(cleaned_df_test)

summary(cleaned_df_train)
summary(cleaned_df_test)

str(cleaned_df_train)
str(cleaned_df_test)

# Running Principal Component Analysis on both train & test data
my_pca <- prcomp(cleaned_df_train, center = TRUE, scale = TRUE)
my_pca2 <- prcomp(cleaned_df_test, center = TRUE, scale = TRUE)

summary(my_pca)
summary(my_pca2)

print(my_pca)
print(my_pca2)

# Finding the variance of the train data for a scree plot
variance_pca = my_pca$sdev^2 / sum(my_pca$sdev^2)

# Plotting a scree plot to determining number of PCAs to select
library(ggplot2)
qplot(c(1:31), variance_pca) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot")

# Kaiser Criterion for determining number of PCAs to select
kaiser_criterion <- my_pca$sdev^2
kaiser_criterion

print(my_pca$rotation)

# Using the biplot to visualize groupings of each independent variables
biplot <- my_pca %>% biplot(cex = .5)
biplot

# Linear regression without PCA
linreg1 <- lm(cleaned_df_train$SalePrice~., data = cleaned_df_train)
summary(linreg1)

# Converting PCAs to a dataframe for linreg2
components <- cbind(cleaned_df_train[, "SalePrice"],data.frame(my_pca$x[, 1:6]))

# Linear regression with PCA
linreg2 <- lm(cleaned_df_train$SalePrice~., data = components)
summary(linreg2)

# Converting the first six(6) PCAs to a dataframe
components2 <- as.data.frame(my_pca2$x[, 1:6])

# Predicting sales price using the linreg2 model
Predicted_price <- round(predict(linreg2, components2), 0)

View(Predicted_price)

# Storing the Predicted price as a csv file
write.csv(Predicted_price, "C:\\Users\\hp\\Documents\\PwC Bootcamp\\Project 1\\PredictedSalesPrice.csv", row.names = TRUE)

