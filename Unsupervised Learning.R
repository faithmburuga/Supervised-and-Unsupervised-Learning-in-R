# 1. Load the Data
# Display the first 6 rows of the dataset
shoppers <- read.csv('http://bit.ly/EcommerceCustomersDataset')
head(shoppers)

# 2. Data Exploration
# Check the dimensions of the dataset
dim(shoppers)

# Display the structure of the R object
str(shoppers)

# Check descriptive statistics of the dataframe
summary(shoppers)

# Check the datatypes of all the variables in the dataframe
sapply(shoppers, class)

# Check the unique values in the variable Administrative
length(unique(shoppers$Administrative))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$Administrative_Duration))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$Informational))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$Informational_Duration))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$ProductRelated))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$ProductRelated_Duration))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$BounceRates))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$ExitRates))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$PageValues))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$SpecialDay))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$Month))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$OperatingSystems))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$Browser))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$Region))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$TrafficType))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$VisitorType))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$Weekend))

# Check the unique values in the variable Administrative Duration
length(unique(shoppers$Revenue))


# 3. Data Cleaning
# A. Check for missing values
length(which(is.na(shoppers)))

# Find total missing values in each column
colSums(is.na(shoppers))

# Find the number of non-missing values
length(which(!is.na(shoppers)))

# Since the number of entries that are not missing are a lot, I will omit the missing values.
# Create a new dataset without the missing values
new_shoppers <- na.omit(shoppers)

# Confirm that there are no missing values. Check the dimensions of the new dataset.

length(which(is.na(new_shoppers)))

dim(new_shoppers)

# B. Remove duplicates.
# Create a new dataset without the duplicates
new_dfshoppers <- new_shoppers[!duplicated(new_shoppers),]
head(new_dfshoppers)

dim(new_dfshoppers)

# C. Check for outliers. Plot boxplots for the numerical variables.

boxplot(new_dfshoppers$Administrative, 
        data = new_dfshoppers,
        main="Boxplot for Administrative",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$Administrative_Duration, 
        data = new_dfshoppers,
        main="Boxplot for Administrative_Duration",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$Informational, 
        data = new_dfshoppers,
        main="Boxplot for Informational",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$Informational_Duration, 
        data = new_dfshoppers,
        main="Boxplot for Informational_Duration",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$ProductRelated, 
        data = new_dfshoppers,
        main="Boxplot for ProductRelated",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$ProductRelated_Duration, 
        data = new_dfshoppers,
        main="Boxplot for ProductRelated_Duration",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$BounceRates, 
        data = new_dfshoppers,
        main="Boxplot for BounceRates",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$ExitRates, 
        data = new_dfshoppers,
        main="Boxplot for ExitRates",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$PageValues, 
        data = new_dfshoppers,
        main="Boxplot for PageValues",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$SpecialDay, 
        data = new_dfshoppers,
        main="Boxplot for SpecialDay",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$OperatingSystems, 
        data = new_dfshoppers,
        main="Boxplot for OperatingSystems",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$Browser, 
        data = new_dfshoppers,
        main="Boxplot for Browser",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$Region, 
        data = new_dfshoppers,
        main="Boxplot for Region",
        col="orange",
        border="brown"
)

boxplot(new_dfshoppers$TrafficType, 
        data = new_dfshoppers,
        main="Boxplot for TrafficType",
        col="orange",
        border="brown"
)

# From the boxplots, we can see that all the numerical variables have outliers.
# I won't remove them as it is normal to have values that are higher than average in all the categories

# 4. Univariate Analysis
# A. Numerical variables
# Histograms

Administrative <- new_dfshoppers$Administrative
hist(Administrative)

Administrative_Duration <- new_dfshoppers$Administrative_Duration
hist(Administrative_Duration)


Informational <- new_dfshoppers$Informational
hist(Informational)


Informational_Duration <- new_dfshoppers$Informational_Duration
hist(Informational_Duration)


ProductRelated <- new_dfshoppers$ProductRelated
hist(ProductRelated)


ProductRelated_Duration <- new_dfshoppers$ProductRelated_Duration
hist(ProductRelated_Duration)


BounceRates <- new_dfshoppers$BounceRates
hist(BounceRates)


ExitRates <- new_dfshoppers$ExitRates
hist(ExitRates)


PageValues <- new_dfshoppers$PageValues
hist(PageValues)


SpecialDay <- new_dfshoppers$SpecialDay
hist(SpecialDay)


OperatingSystems <- new_dfshoppers$OperatingSystems
hist(OperatingSystems)


Browser <- new_dfshoppers$Browser
hist(Browser)


Region <- new_dfshoppers$Region
hist(Region)


TrafficType <- new_dfshoppers$TrafficType
hist(TrafficType)

# B. Categorical Variables
# Bar charts

months <- new_dfshoppers$Month

months_frequency <- table(months)

barplot(months_frequency, col="blue",
        main="Months Chart",border="red")


visitors <- new_dfshoppers$VisitorType

visitors_frequency <- table(visitors)

barplot(visitors_frequency, col="blue",
        main="Type of Visitors Chart",border="red")


weekend <- new_dfshoppers$Weekend

weekend_frequency <- table(weekend)

barplot(weekend_frequency, col="blue",
        main="Weekend or Weekday Chart",border="red")


revenue <- new_dfshoppers$Revenue

revenue_frequency <- table(revenue)

barplot(revenue_frequency, col="blue",
        main="Revenue Chart",border="red")


# 5. Bivariate Analysis
# A. Categorical and categorical variables
# Double Bar Charts

counts <- table(new_dfshoppers$Revenue, new_dfshoppers$Month)
barplot(counts, main="Revenue Distribution by Month",
        xlab="Months", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

counts <- table(new_dfshoppers$Revenue, new_dfshoppers$VisitorType)
barplot(counts, main="Revenue Distribution by VisitorType",
        xlab="Visitor Type", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

counts <- table(new_dfshoppers$Revenue, new_dfshoppers$Weekend)
barplot(counts, main="Click Distribution by Weekend/Weekday",
        xlab="Weekend/Weekday", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


# Encode the categorical variables to be numerical so we can check for correlation

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order))
  x
}


table(new_dfshoppers[["Month"]], encode_ordinal(new_dfshoppers[["Month"]]), useNA = "ifany")

table(new_dfshoppers[["VisitorType"]], encode_ordinal(new_dfshoppers[["VisitorType"]]), useNA = "ifany")

table(new_dfshoppers[["Weekend"]], encode_ordinal(new_dfshoppers[["Weekend"]]), useNA = "ifany")

table(new_dfshoppers[["Revenue"]], encode_ordinal(new_dfshoppers[["Revenue"]]), useNA = "ifany")

df <- new_dfshoppers
df[["Month_encoded"]] <- encode_ordinal(new_dfshoppers[["Month"]])
df[["VisitorType_encoded"]] <- encode_ordinal(new_dfshoppers[["VisitorType"]])
df[["Weekend_encoded"]] <- encode_ordinal(new_dfshoppers[["Weekend"]])
df[["Revenue_encoded"]] <- encode_ordinal(new_dfshoppers[["Revenue"]])
head(df)

# Drop the categorical columns

new_df= subset(df, select = -c(Month, VisitorType, Weekend, Revenue) )
head(new_df)

# Confirm the datatypes of the encoded columns

sapply(new_df, class)


# B. Numerical and numerical variables

# Correlation matrix

matrix <- cor(new_df)
round(matrix, 2)


library(corrplot)

# Run the corrplot function

new_df.cor = cor(new_df, method = c("spearman"))
corrplot(new_df.cor)

# Drop a column each from the pairs of highly correlated variables.

new_df1= subset(new_df, select = -c(Administrative_Duration, Informational_Duration, ProductRelated_Duration, ExitRates) )
head(new_df1)

# 6. K-Means Clustering
# Preprocessing the dataset

features<- new_df1[(1:13)]
label<- new_df1[14]
head(features)
head(label)

# Use the scale function to scale the data

new_dfnorm <- as.data.frame(scale(features))
head(new_dfnorm)

# Compute k-means using the k-means functions

new_dfK2 <- kmeans(new_dfnorm, centers = 2, nstart = 25)
print(new_dfK2)

# Check the number of records in each cluster
new_dfK2$size

# Get the datapoints of THE 2 cluster centers

new_dfK2$centers 

# Get the cluster vector that shows the cluster where each record falls

new_dfK2$cluster


# Visualize the cluster created

library("factoextra")
fviz_cluster(new_dfK2, data = new_dfnorm)


# Use different values of k and examine results

new_dfK3 <- kmeans(new_dfnorm, centers = 3, nstart = 25)
new_dfK4 <- kmeans(new_dfnorm, centers = 4, nstart = 25)
new_dfK5 <- kmeans(new_dfnorm, centers = 5, nstart = 25)

# Plot the clusters for the different k values

p1 <- fviz_cluster(new_dfK2, geom = "point", data = new_dfnorm) + ggtitle(" K = 2")
p2 <- fviz_cluster(new_dfK3, geom = "point", data = new_dfnorm) + ggtitle(" K = 3")
p3 <- fviz_cluster(new_dfK4, geom = "point", data = new_dfnorm) + ggtitle(" K = 4")
p4 <- fviz_cluster(new_dfK5, geom = "point", data = new_dfnorm) + ggtitle(" K = 5")

library("gridExtra")
grid.arrange(p1, p2, p3, p4, nrow = 2)



# 7. Hierarchical Clustering
# Use the dist() function to compute the Euclidean distance between observations, 
# d will be the first argument in the hclust() function dissimilarity matrix

d <- dist(new_dfnorm, method = "euclidean")

# Do hierarchical clustering using the Ward's method

res.hc <- hclust(d, method = "ward.D2" )

# Plot the obtained dendrogram

plot(res.hc, cex = 0.6, hang = -1)


