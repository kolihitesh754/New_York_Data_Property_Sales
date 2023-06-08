library(dplyr)
#Loading dataframe
nyc_data <- read.csv("/Users/cdmstudent/Downloads/nyc-rolling-sales.csv")
dim(nyc_data)
head(nyc_data)
#checking column names of dataframe
colnames(nyc_data)
#dimensions of dataframe
dim(nyc_data)
#replacing  '-' with " "
colnames(nyc_data) <- gsub("\\.", "_", colnames(nyc_data))
head(nyc_data)
# Dropping index and EASE-MENT
nyc_data <- subset(nyc_data, select = -c(EASE_MENT, X, APARTMENT_NUMBER, BUILDING_CLASS_AT_TIME_OF_SALE))
head(nyc_data)
colnames(nyc_data)
str(nyc_data)
# Count the occurrences of each unique value in the "BUILDING CLASS CATEGORY" column
category_counts <- table(nyc_data$'BUILDING_CLASS_CATEGORY')
category_counts
# Print the counts
print(category_counts)
#converting chr to float
floats <- c('SALE_PRICE', 'LAND_SQUARE_FEET', 'GROSS_SQUARE_FEET')
for (col in floats) {
  nyc_data[[col]] <- as.numeric(as.character(nyc_data[[col]]))
}
#removing white spaces
nyc_data$BUILDING_CLASS_CATEGORY <- trimws(nyc_data$BUILDING_CLASS_CATEGORY, "left")
nyc_data$BUILDING_CLASS_CATEGORY <- trimws(nyc_data$BUILDING_CLASS_CATEGORY, "right")
#distinct values
distinct_categories <- unique(nyc_data$BUILDING_CLASS_CATEGORY)
distinct_categories
head(nyc_data)
# Categories we would like to focus on
nyc_data1 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "01 ONE FAMILY DWELLINGS", ]
nyc_data2 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "02 TWO FAMILY DWELLINGS", ]
nyc_data3 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "13 CONDOS - ELEVATOR APARTMENTS", ]
nyc_data4 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "10 COOPS - ELEVATOR APARTMENTS", ]
nyc_data5 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "03 THREE FAMILY DWELLINGS", ]
nyc_data6 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "09 COOPS - WALKUP APARTMENTS", ]
nyc_data7 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "15 CONDOS - 2-10 UNIT RESIDENTIAL", ]
nyc_data8 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "07 RENTALS - WALKUP APARTMENTS", ]
nyc_data9 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "08 RENTALS - ELEVATOR APARTMENTS", ]
nyc_data10 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "06 TAX CLASS 1 - OTHER", ]
nyc_data11 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "41 TAX CLASS 4 - OTHER", ]
nyc_data12 <- nyc_data[nyc_data$BUILDING_CLASS_CATEGORY == "04 TAX CLASS 1 CONDOS", ]
new_list <- list(nyc_data1, nyc_data2, nyc_data3, nyc_data4, nyc_data5, nyc_data6, nyc_data7, nyc_data8, nyc_data9, nyc_data10, nyc_data11, nyc_data12)
nyc_data <- do.call(rbind, new_list)
unique(nyc_data$BUILDING_CLASS_CATEGORY)
#checking saleprice
colnames(nyc_data)
nyc_data <- nyc_data[nyc_data$SALE_PRICE != 0, ]
nyc_data
#In the above code, df$SALE PRICE > 5e5 specifies the condition for filtering rows where the sale price is greater than 500,000, and df$SALE PRICE < 5e6 specifies the condition for filtering rows where the sale price is less than 5,000,000.
nyc_data <- nyc_data[nyc_data$'SALE_PRICE' > 1000, ]
nyc_data <- nyc_data[nyc_data$'SALE_PRICE' < 5000000, ]
nyc_data
# Remove NaN
nyc_data <- na.omit(nyc_data)
nyc_data
unique(nyc_data$BUILDING_CLASS_CATEGORY)
# No commercial units
table(nyc_data$COMMERCIAL_UNITS)
nyc_data <- nyc_data[nyc_data['COMMERCIAL_UNITS'] == 0,]
head(nyc_data)
# # Residential units in range between 1 and 9 inclusive
table(nyc_data$RESIDENTIAL_UNITS)
nyc_data <- nyc_data[nyc_data$'RESIDENTIAL_UNITS' < 10, ]
nyc_data <- nyc_data[nyc_data$'RESIDENTIAL_UNITS' != 0, ]
unique(nyc_data$BUILDING_CLASS_CATEGORY)
#over here 10 coops  and 9 coopps building class category is removed
nyc_data$SALE_DATE <- as.POSIXct(nyc_data$SALE_DATE)
head(nyc_data$SALE_DATE)
# Convert 'SALE DATE' to date format
nyc_data$SALE_DATE <- as.Date(nyc_data$SALE_DATE)

# Extract year and month
nyc_data$YEAR_SOLD <- format(nyc_data$SALE_DATE, "%Y")
nyc_data$MONTH_SOLD <- format(nyc_data$SALE_DATE, "%m")

# Remove 'SALE DATE' column
nyc_data <- subset(nyc_data, select = -c(SALE_DATE))

unique(nyc_data$BUILDING_CLASS_CATEGORY)
library(dplyr)

nyc_data <- nyc_data[nyc_data$YEAR_BUILT > 0, ]
nyc_data <- nyc_data[nyc_data$GROSS_SQUARE_FEET > 0, ]

# Drop unnecessary columns
nyc_data <- nyc_data[, !(names(nyc_data) %in% c("COMMERCIAL_UNITS", "TOTAL_UNITS"))]
nyc_data

# Print the dimensions of the data frame
print(dim(nyc_data))
colnames(nyc_data)
# Logarithmic transformation of SALE PRICE column
library(dplyr)
nyc_data
# Remove duplicates
nyc_data <- unique(nyc_data)
nyc_data
library(dplyr)
nyc_data <- distinct(nyc_data)
nyc_data
dim(nyc_data)
# New copy of dataframe
nyc_copy <- nyc_data
dim(nyc_copy)
#Replacing Sale_price with log of saleprice.
#nyc_copy$SALE_PRICE <- log(nyc_copy$SALE_PRICE)
#nyc_copy
# Map different columns to numbers
label_encoder <- function(x) {
  levels <- unique(x)
  levels <- sort(levels)
  names(levels) <- 1:length(levels)
  return(levels)
}
nyc_copy$NEIGHBORHOOD <- as.numeric(factor(nyc_copy$NEIGHBORHOOD, levels = label_encoder(nyc_copy$NEIGHBORHOOD)))
nyc_copy$BUILDING_CLASS_CATEGORY <- as.numeric(factor(nyc_copy$BUILDING_CLASS_CATEGORY, levels = label_encoder(nyc_copy$BUILDING_CLASS_CATEGORY)))
nyc_copy$TAX_CLASS_AT_PRESENT <- as.numeric(factor(nyc_copy$TAX_CLASS_AT_PRESENT, levels = c('1', '1A', '1B', '1C', '2', '2C', '2B', '2A', '4')))
nyc_copy
library(ggplot2)
#Set style
theme_set(theme_minimal())
# Select numeric columns
numeric_cols <- sapply(nyc_copy, is.numeric)
# Compute the correlation matrix for numeric columns
corr <- cor(nyc_copy[, numeric_cols], method = "pearson")
# Generate a mask for the upper triangle
mask <- upper.tri(corr)
# Create a heatmap using ggplot2
p <- ggplot(data = reshape2::melt(corr), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0, 
                       limits = c(-1, 1), guide = "colorbar") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Display correlation values
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "", fill = "Correlation") +
  coord_fixed() +
  ggtitle("Pearson Correlation Heatmap")

# Display the heatmap
print(p)
nyc_copy$BOROUGH <- factor(nyc_copy$BOROUGH, levels = c(1, 2, 3, 4, 5),
                     labels = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"))
head(nyc_copy)
# Create the boxplot
library(ggplot2)

# Set the figure size
options(repr.plot.width = 10, repr.plot.height = 6)
# Create the boxplot with colorful aesthetics
ggplot(nyc_data
       , aes(x = RESIDENTIAL_UNITS, y = SALE_PRICE, fill = factor(RESIDENTIAL_UNITS))) +
  geom_boxplot() +
  labs(title = "Residential Units vs Sale Price") +
  scale_fill_hue() +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, max(nyc_data$RESIDENTIAL_UNITS), by = 1))
#removing ouliers
library(ggplot2)

ggplot(nyc_data, aes(x = BOROUGH, y = SALE_PRICE, fill = factor(BOROUGH))) +
  geom_boxplot() +
  labs(title = "Borough vs Sale Price") +
  scale_fill_hue() +
  theme_bw() +
  scale_x_discrete(labels = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"))
#Manhatten is priced higher than the others
#Building Class Category vs Price Per unit
#We made a new column called price per unit which is the SALE PRICE divided by the number of residential units in order to standardize the data for the next plots.
unique(nyc_data$BUILDING_CLASS_CATEGORY)
library(ggplot2)
ggplot(nyc_data, aes(x = BUILDING_CLASS_CATEGORY, y = SALE_PRICE, fill = BUILDING_CLASS_CATEGORY)) +
  geom_boxplot() +
  labs(title = "Building Class Category vs Sale Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_hue()

onefamily_dwelling <- subset(nyc_data, BUILDING_CLASS_CATEGORY == '01 ONE FAMILY DWELLINGS')
table(onefamily_dwelling$RESIDENTIAL_UNITS)
#Based on the table output, it reveals that there are 12,293 rows in the "onefamily_dwelling" subset 
#where the number of residential units is 1, 
#and there are 8 rows where the number of residential units is 2.
# Create the boxplot
ggplot(onefamily_dwelling, aes(x = BOROUGH, y = SALE_PRICE, fill = factor(BOROUGH))) +
  geom_boxplot() +
  labs(title = "Borough vs Sale Price") +
  scale_fill_hue() +
  theme_bw() +
  scale_x_discrete(labels = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"))
#Let’s examine the distribution of Residential units and building class category. We can find if there are other building classes that have one residential units and then divide the units so we can get the price per unit. 
#This will allow us to see if there is a preference type of Building class category to see if we want to keep it as a feature.
ggplot(nyc_data, aes(x=BUILDING_CLASS_CATEGORY, y=RESIDENTIAL_UNITS,fill = BUILDING_CLASS_CATEGORY)) +
  geom_boxplot() +
  labs(title = "Residential Units vs Building Class Category") +
  scale_fill_hue() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
library(dplyr)
# Calculate the price per unit
nyc_data <- nyc_data %>% 
  mutate(price_per_unit = SALE_PRICE / RESIDENTIAL_UNITS)
ggplot(nyc_data, aes(x=BUILDING_CLASS_CATEGORY, y=price_per_unit,fill = BUILDING_CLASS_CATEGORY)) +
  geom_boxplot() +
  labs(title = "Building Class Category vs Price Per Unit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(nyc_data, aes(x = BUILDING_CLASS_CATEGORY, y = price_per_unit, fill = factor(BOROUGH))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Price Per Unit vs Building Class Category in each Borough") +
  facet_wrap(~ BOROUGH) +
  scale_fill_brewer(palette = "Set1") +
  scale_fill_discrete(labels = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"))
library(ggplot2)
# Create the bar plot
ggplot(nyc_data, aes(x = YEAR_SOLD, y = SALE_PRICE, fill = factor(BOROUGH))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sales per Borough from 2016-2017") +
  facet_wrap(~ BOROUGH) +
  scale_fill_brewer(palette = "Set1") +
  scale_fill_discrete(labels = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"))

ggplot(nyc_data, aes(x = MONTH_SOLD, y = SALE_PRICE, fill = factor(BOROUGH))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sales per Borough from 2016-2017") +
  facet_wrap(~ BOROUGH) +
  scale_fill_brewer(palette = "Set1") +
  scale_fill_discrete(labels = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"))
colnames(nyc_data)
#calculating newfeature "Building Age"
nyc_data$YEAR_BUILT
nyc_data$YEAR_SOLD
nyc_data$YEAR_SOLD - nyc_data$YEAR_BUILT
nyc_data$BUILDING_AGE <- as.numeric(nyc_data$YEAR_SOLD) - as.numeric(nyc_data$YEAR_BUILT)
nyc_data$BUILDING_AGE
# Replace empty strings with NA
nyc_data[nyc_data == " "] <- NA
#Specifying categorical and numerical variables
# Drop duplicates in DataFrame
nyc_data <- nyc_data[!duplicated(nyc_data), ]
# Check for remaining duplicates
sum(duplicated(nyc_data))
dim(nyc_data)
nyc_data$YEAR_BUILT <- NULL
nyc_data
dim(nyc_data)
str(nyc_data)
str(nyc_data)
write.csv(nyc_data, file ="/Users/cdmstudent/Desktop/nyc_data.csv", row.names = FALSE)
nyc_data <- read.csv("/Users/cdmstudent/Desktop/nyc_data.csv")
dim(nyc_data)
colnames(nyc_data)
str(nyc_data)
###Using Normalization
library(caret)
str(nyc_data)
nyc_data$SALE_PRICE
boxplot(nyc_data$SALE_PRICE)
##Removing outliers from sale price
# Filter the data to remove values above 1e+06
nyc_data <- nyc_data[nyc_data$SALE_PRICE <= 1.3e+06, ]

# Create the boxplot using the filtered data
boxplot(nyc_data$SALE_PRICE)
nyc_data$SALE_PRICE <- log(nyc_data$SALE_PRICE)

# Log transform the "SALE PRICE" column
nyc_data$SALE_PRICE <- log(nyc_data$SALE_PRICE)
nyc_data
hist(nyc_data$SALE_PRICE)
# Select the chosen features for ML models
x_df <- nyc_data[, c("BOROUGH", "NEIGHBORHOOD", "BLOCK", "LOT", "BUILDING_CLASS_CATEGORY", "RESIDENTIAL_UNITS", "GROSS_SQUARE_FEET")]
# Create the target variable
y <- nyc_data$SALE_PRICE
library(caret)

# Set the seed for reproducibility
set.seed(42)

# Split the data into train and test sets
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
tr_x_train <- x_df[train_index, ]
tr_x_test <- x_df[-train_index, ]
tr_y_train <- y[train_index]
tr_y_test <- y[-train_index]
# Calculate price per unit and transform to log
nyc_data$price_per_unit <- log(nyc_data$SALE_PRICE / nyc_data$RESIDENTIAL_UNITS)

# Remove outliers
boxplot(nyc_data$price_per_unit)
nyc_data <- nyc_data[nyc_data$price_per_unit > -0.75, ]
boxplot(nyc_data$price_per_unit)
# Select the desired columns for the modified dataframe
lr_modified <- nyc_data[, c("BOROUGH", "NEIGHBORHOOD", "BLOCK", "LOT", "BUILDING_CLASS_CATEGORY", "RESIDENTIAL_UNITS", "GROSS_SQUARE_FEET", "price_per_unit", "SALE_PRICE")]
library(caret)






















