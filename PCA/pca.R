nycData = nyc.rolling.sales

# To know about the variables in the dataset
str(nycData)
head(nycData)
summary(nycData)
# To Remove the variables
nycData <- nycData[,-c(1,8,11)]

# To figure out the unknown item " -" present instead of numerical value in /SALE.PRICE. 
Result <- ((length(which(nycData$SALE.PRICE == " -  ")))/length(nycData$SALE.PRICE))*100
Result

# After removal of unknown item in SALE.PRICE
nycData <- subset(nycData, !grepl(" -  ", nycData$SALE.PRICE))

# Change of datatypes from Character to Numeric
nycData$SALE.PRICE <-as.numeric(nycData$SALE.PRICE)
nycData$LAND.SQUARE.FEET  <- as.numeric(nycData$LAND.SQUARE.FEET)
nycData$GROSS.SQUARE.FEET <- as.numeric(nycData$GROSS.SQUARE.FEET)

# Remove NA values from the dataset
nycData <- na.omit(nycData)

# NYC dataset - Sale price variable data distribution 
hist(nycData$SALE.PRICE,main="NYC House Sale Price",xlab="Sale Price",col="gray69",freq=FALSE)

hist(nycData$LAND.SQUARE.FEET,main="NYC House Land Square Feet",xlab="Land Square feet",col="gray69",freq=FALSE)
hist(nycData$GROSS.SQUARE.FEET,main="NYC House Gross Square Feet",xlab="Gross Square Feet",col="gray69",freq=FALSE)


# NYC dataset - SALE.PRICE, LAND.SQUARE.FEET and GROSS.SQUARE.FEET transformation
nycData$SALE.PRICE <- log(nycData$SALE.PRICE)
nycData$LAND.SQUARE.FEET <- log(nycData$LAND.SQUARE.FEET)
nycData$GROSS.SQUARE.FEET <- log(nycData$GROSS.SQUARE.FEET)

# Create a bar chart of the boroughs
barplot(table(nycData$BOROUGH), main = "Number of Properties by Borough", 
        xlab = "Borough", ylab = "Count")

# Create a scatter plot of sale price and land area
plot(nycData$LAND.SQUARE.FEET, nycData$SALE.PRICE, main = "Sale Price vs Land Area", 
     xlab = "Land Area", ylab = "Sale Price")

# Create a histogram of sale prices
hist(nycData$SALE.PRICE, main = "Histogram of Sale Prices", xlab = "Sale Price")

# Create a scatter plot of gross square feet vs sale price
plot(nycData$GROSS.SQUARE.FEET, nycData$SALE.PRICE, main = "Gross Square Feet vs Sale Price", 
     xlab = "Gross Square Feet", ylab = "Sale Price")

#Remove the sale price with zero for high value of arce
nycData <- nycData[nycData$SALE.PRICE > 7, ]

#Remove the address column as its not required
nycData <- nycData[, !(names(nycData) %in% "ADDRESS")]

# To know the frequency of the components 
component_freq <- table(nycData$BUILDING.CLASS.AT.TIME.OF.SALE)

print(component_freq)


library(dplyr)

# Define the mapping for renaming
component_mapping <- c("1" = "1",
                       "1A" = "1",
                       "1B" = "1",
                       "1C" = "1",
                       "2" = "2",
                       "2A" = "2",
                       "2B" = "2",
                       "2C" = "2",
                       "4" = "4")

# Rename the components in 'TAX.CLASS.AT.PRESENT'
nycData <- nycData %>%
  mutate(TAX.CLASS.AT.PRESENT = recode(TAX.CLASS.AT.PRESENT, !!!component_mapping))

# Print the updated dataset
print(nycData$TAX.CLASS.AT.PRESENT)

library(funModeling)
df_status(nycData)
nyc1= nycData
head(nyc1)
str(nyc1)
nyc1= nycData[,-c(2,3,7,16,18)] #2-neighborhood,3-building class category,7-BUILDING.CLASS.AT.PRESENT ,16-BUILDING.CLASS.AT.TIME.OF.SALE, 18 - Sale.price

nyc1$TAX.CLASS.AT.PRESENT = as.numeric(as.character(nyc1$TAX.CLASS.AT.PRESENT))
summary(nyc1)

nycData_cleaned <- na.omit(nyc1)

summary(nycData_cleaned)




# Specify the columns containing negative infinity values
columns_with_inf <- c("LAND.SQUARE.FEET", "GROSS.SQUARE.FEET")

# Loop through the columns and remove negative infinity values
for (column in columns_with_inf) {
  nycData_cleaned[[column]][is.infinite(nycData_cleaned[[column]])] <- NA
}


summary(nycData_cleaned)

nycData_cleaned <- na.omit(nycData_cleaned)


#==============================================================================
#==============================================================================
#==============================================================================

#-----------PCA----------------------
# Perform PCA
pca_result <- prcomp(nycData_cleaned, scale. = TRUE)
plot(pca_result)
abline(1,0, col="red")
print(pca_result, digits=4,format="fg",flag="#") 

library(psych)
library(ggplot2)

plot_pca <- function(pca_result) {
  # Create a data frame with the PC scores and labels
  pc_data <- data.frame(PC1 = pca_result$x[, 1],
                        PC2 = pca_result$x[, 2],
                        label = rownames(pca_result$x))
  
  # Plot the PC scores
  ggplot(pc_data, aes(x = PC1, y = PC2, label = label)) +
    geom_point() +
    geom_text(hjust = 0, vjust = 0) +
    xlab("Principal Component 1") +
    ylab("Principal Component 2") +
    ggtitle("PCA Plot")
}


# Perform PCA
pca_result <- prcomp(nycData_cleaned, scale. = TRUE)

# Plot PCA results
plot_pca(pca_result)


# factor rotation
p2 = principal(nycData_cleaned[, -1],nfactors = 4, rot="varimax")
print(p2$loadings, cutoff=0.5)


# Perform factor analysis
fa_result <- factanal(nycData_cleaned[, -1], factors = 5, rotation = "varimax")

# Print the factor analysis results
print(fa_result)


# Load the necessary package
library(psych)

# Perform parallel analysis
pa_result <- fa.parallel(nycData_cleaned[, -1], n.iter = 100, fm = "ml", fa = "pc")

# Print the parallel analysis results
print(pa_result)
 

########################################################
#============ for components of 5 =====================

library(psych)

# Perform factor analysis
fa_result <- fa(nycData_cleaned[, -1], nfactors = 5)

# Obtain the factor loadings
loadings <- fa_result$loadings

# Create a correlation plot
correlation_matrix <- cor(nycData_cleaned[, -1])
cor.plot(correlation_matrix, main = "Correlation Plot")

# Overlay the factor loadings on the correlation plot
text(correlation_matrix, labels = names(correlation_matrix), cex = 0.7)
arrows(0, 0, loadings[, 1], loadings[, 2], length = 0.1, col = "red")

#============ for components of 4 =====================

library(psych)

# Perform factor analysis
fa_result <- fa(nycData_cleaned[, -1], nfactors = 4)

# Obtain the factor loadings
loadings <- fa_result$loadings

# Create a correlation plot
correlation_matrix <- cor(nycData_cleaned[, -1])
cor.plot(correlation_matrix, main = "Correlation Plot")

# Overlay the factor loadings on the correlation plot
text(correlation_matrix, labels = names(correlation_matrix), cex = 0.7)
arrows(0, 0, loadings[, 1], loadings[, 2], length = 0.1, col = "red")



# Get the PCA results
summary(pca_result)

#scores
scores <- pca_result$x
scores

#Loading
loadings <- pca_result$rotation
loadings

#Explained Variance
# Extract the proportion of variance explained by each principal component
prop_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)
prop_var

# Cumulative proportion of variance explained
cum_prop_var <- cumsum(prop_var)
cum_prop_var

# Bar plot of the explained variance
barplot(prop_var, names.arg = paste0("PC", 1:length(prop_var)),
        xlab = "Principal Components", ylab = "Proportion of Variance Explained",
        main = "Explained Variance by Principal Components")

# Plot the cumulative explained variance
plot(cum_prop_var, type = "b", xlab = "Number of Principal Components",
     ylab = "Cumulative Proportion of Variance Explained",
     main = "Cumulative Explained Variance")



#Scree plot
# Scree plot of the eigenvalues
plot(pca_result$sdev^2, type = "b", xlab = "Principal Components",
     ylab = "Eigenvalues", main = "Scree Plot")

#Biplot
# Biplot of the first two principal components
biplot(pca_result, scale = 0)

#Contribution Plots:
# Contribution plot of variables to the first two principal components
contribution_plot <- function(pca_result, n = 2) {
  loadings <- pca_result$rotation[, 1:n]
  contributions <- scale(nycData_cleaned) %*% loadings
  
  plot(contributions, col = "blue", pch = 16,
       xlab = paste0("PC", 1), ylab = paste0("PC", 2),
       main = "Contribution Plot")
  text(contributions, labels = colnames(nycData_cleaned), pos = 3)
}

contribution_plot(pca_result, n = 2)

#heatmap
heatmap(pca_result$rotation)

#Factor Plot
library(ggplot2)

# Extract the variable contributions from the PCA result
var_contrib <- pca_result$sdev^2

# Create a data frame for plotting
plot_data <- data.frame(PC = paste0("PC", 1:length(var_contrib)),
                        Contribution = var_contrib / sum(var_contrib))

# Plot the variable contributions using a bar plot
ggplot(plot_data, aes(x = PC, y = Contribution)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Principal Component", y = "Contribution") +
  ggtitle("Variable Contributions to Principal Components")

#-----------CA----------------------
# Load required packages
library(ca)
library(psych)

# Perform Correspondence Analysis (CA)
ca_result <- ca(nycData_cleaned)  


# Print CA results
summary(ca_result) 

#Biplot
library(FactoMineR)

# Perform Correspondence Analysis (CA)
ca_result <- CA(nycData_cleaned)

# Visualize row coordinates in a biplot-like manner
plot(ca_result, choix = "row", col.row = "blue", col.col = "red")


#Factor/Circle Plot
plot(ca_result, type = "var")





#-----------PFA----------------------
# Perform Parallel Factor Analysis (PFA)
pfa_result <- principal(nycData_cleaned, nfactors = 3, rotate = "varimax")  


# Print PFA results
print(pfa_result) 

#Factor loading for PFA
# Bar plot of factor loadings for the first two factors
barplot(pfa_result$loadings[, 1:2], beside = TRUE, col = c("blue", "red"))

#Biplot for PFA
# Biplot of the first two factors
biplot(pfa_result, choices = c(1, 2))


summary(nycData_cleaned)

head(nycData_cleaned)




library(lavaan)

data <- nycData_cleaned


model <- "
  PropertySize =~ LAND.SQUARE.FEET + RESIDENTIAL.UNITS + COMMERCIAL.UNITS + TOTAL.UNITS
  PropertyValue =~ GROSS.SQUARE.FEET + SALE.PRICE
  PropertyAge =~ YEAR.BUILT
"


cfa_result <- cfa(model, data)

fit_indices <- fitMeasures(cfa_result)
print(fit_indices)

parameter_estimates <- lavaan::parameterEstimates(cfa_result)


summary(cfa_result)


