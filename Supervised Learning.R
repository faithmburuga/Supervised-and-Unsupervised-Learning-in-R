# Load the Data
# Display the first 6 rows of the dataset
advert <- read.csv('http://bit.ly/IPAdvertisingData')
head(advert)

# Data Exploration
# Check the dimensions of the dataset
dim(advert)

# Display the structure of the R object
str(advert)

# Check descriptive statistics of the dataframe
summary(advert)

# Check the datatypes of all the variables in the dataframe
sapply(advert, class)

# Count the unique values in the variable Daily.Time.Spent.on.Site
length(unique(advert$Daily.Time.Spent.on.Site))

# Count the unique values in the variable Age
length(unique(advert$Age))

# Count the unique values in the variable Area.Income
length(unique(advert$Area.Income))

# Count the unique values in the variable Daily.Internet.Usage
length(unique(advert$Daily.Internet.Usage))

# Count the unique values in the variable Ad.Topic.Line
length(unique(advert$Ad.Topic.Line))

# Count the unique values in the variable City
length(unique(advert$City))

# Count the unique values in the variable Male
length(unique(advert$Male))

# Count the unique values in the variable Country
length(unique(advert$Country))

# Count the unique values in the variable Timestamp
length(unique(advert$Timestamp))

# Count the unique values in the variable Clicked.on.Ad 
length(unique(advert$Clicked.on.Ad ))
# 2 variables have discrete data. 4 variables are not numeric or integer


# Data Cleaning
# Check for missing values
length(which(is.na(advert)))
# There are no missing values


# Check for duplicates
duplicated_rows <- advert[duplicated(advert),]
duplicated_rows
# There are no duplicated rows

# Check for outliers. I'll do plots for 4 variables since the rest are either character or discrete data
boxplot(advert$Daily.Time.Spent.on.Site, 
        data = advert,
        main="Boxplot for Daily Time Spent on Site",
        ylab="Time in minutes",
        col="orange",
        border="brown"
)

boxplot(advert$Age, 
        data = advert,
        main="Boxplot for Age",
        col="red",
        border="black"
)

boxplot(advert$Area.Income, 
        data = advert,
        main="Boxpot for Area Income",
        col="purple",
        border="black"
)

boxplot(advert$Daily.Internet.Usage, 
        data = advert,
        main="Boxplot for Daily Internet Usage",
        col="pink",
        border="brown"
)
# From the 4 boxplots, one has outliers; the Area Income variable
# It is quite common to have incomes that are higher/lower than the average, so I wont remove the outliers

# Univariate Analysis: Numerical Variables
# Measures of central tendency
# Mean
dailytimespent.mean <- mean(advert$Daily.Time.Spent.on.Site)
dailytimespent.mean

age.mean <- mean(advert$Age)
age.mean

areaincome.mean <- mean(advert$Area.Income)
areaincome.mean

Daily.Internet.Usage.mean <- mean(advert$Daily.Internet.Usage)
Daily.Internet.Usage.mean

# Median
dailytimespent.median <- median(advert$Daily.Time.Spent.on.Site)
dailytimespent.median

age.median <- median(advert$Age)
age.median

areaincome.median <- median(advert$Area.Income)
areaincome.median

Daily.Internet.Usage.median <- median(advert$Daily.Internet.Usage)
Daily.Internet.Usage.median

# Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Calculate mode using getmode() function
dailytimespent.mode <- getmode(advert$Daily.Time.Spent.on.Site)
dailytimespent.mode


age.mode <- getmode(advert$Age)
age.mode


areaincome.mode <- getmode(advert$Area.Income)
areaincome.mode


Daily.Internet.Usage.mode <- getmode(advert$Daily.Internet.Usage)
Daily.Internet.Usage.mode

# Measures of dispersion
# Minimum
dailytimespent.min <- min(advert$Daily.Time.Spent.on.Site)
dailytimespent.min


age.min <- min(advert$Age)
age.min


areaincome.min <- min(advert$Area.Income)
areaincome.min


Daily.Internet.Usage.min <- min(advert$Daily.Internet.Usage)
Daily.Internet.Usage.min

# Maximum
dailytimespent.max <- max(advert$Daily.Time.Spent.on.Site)
dailytimespent.max


age.max <- max(advert$Age)
age.max


areaincome.max <- max(advert$Area.Income)
areaincome.max


Daily.Internet.Usage.max <- max(advert$Daily.Internet.Usage)
Daily.Internet.Usage.max

# Range
# This returns the interval between the highest and lowest value
dailytimespent.range <- range(advert$Daily.Time.Spent.on.Site)
dailytimespent.range


age.range <- range(advert$Age)
age.range


areaincome.range <- range(advert$Area.Income)
areaincome.range


Daily.Internet.Usage.range <- range(advert$Daily.Internet.Usage)
Daily.Internet.Usage.range

# Quantile
# This gives the first and the third quartile together with the range and the median (2nd quantile)
dailytimespent.quantile <- quantile(advert$Daily.Time.Spent.on.Site)
dailytimespent.quantile


age.quantile <- quantile(advert$Age)
age.quantile


areaincome.quantile <- quantile(advert$Area.Income)
areaincome.quantile


Daily.Internet.Usage.quantile <- quantile(advert$Daily.Internet.Usage)
Daily.Internet.Usage.quantile


# Variance
dailytimespent.variance <- var(advert$Daily.Time.Spent.on.Site)
dailytimespent.variance


age.variance <- var(advert$Age)
age.variance


areaincome.variance <- var(advert$Area.Income)
areaincome.variance


Daily.Internet.Usage.variance <- var(advert$Daily.Internet.Usage)
Daily.Internet.Usage.variance


# Standard Deviation
dailytimespent.sd <- sd(advert$Daily.Time.Spent.on.Site)
dailytimespent.sd


age.sd <- sd(advert$Age)
age.sd


areaincome.sd <- sd(advert$Area.Income)
areaincome.sd


Daily.Internet.Usage.sd <- sd(advert$Daily.Internet.Usage)
Daily.Internet.Usage.sd

# Visualizations
# Histograms

DailyTimeSpent <- advert$Daily.Time.Spent.on.Site
hist(DailyTimeSpent)

Age <- advert$Age
hist(Age)


AreaIncome <- advert$Area.Income
hist(AreaIncome)


DailyInternetUsage <- advert$Daily.Internet.Usage
hist(DailyInternetUsage)



# Univariate Analysis:Categorical variables
# Bar Charts for the 2 discrete variables
# Male variable
gender <- advert$Male

gender_frequency <- table(gender)

barplot(gender_frequency, col="blue",
        main="Gender Chart",border="red")

# Clicked on Ad variable
clicked <- advert$Clicked.on.Ad

clicked_frequency <- table(clicked)

barplot(clicked_frequency, col="black",
        main="Clicked on Ad Chart",border="red")
# Females are more than males in the dataset
# Those who clicked on the ad and those who didn't are the same number. The dataset is balanced.

# Frequency tables for the categorical variables have too many values. 
# Therefore,I didn't use that to check distribution



# Bivariate Analysis: Categorical and Categorical
counts <- table(advert$Clicked.on.Ad, advert$Male)
barplot(counts, main="Click Distribution by Gender",
        xlab="Gender", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

counts <- table(advert$Clicked.on.Ad, advert$Country)
barplot(counts, main="Click Distribution by Country",
        xlab="Country", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

# Encode the categorical variables to be numerical so we can check for correlation

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order))
  x
}


table(advert[["Ad.Topic.Line"]], encode_ordinal(advert[["Ad.Topic.Line"]]), useNA = "ifany")

table(advert[["City"]], encode_ordinal(advert[["City"]]), useNA = "ifany")

table(advert[["Country"]], encode_ordinal(advert[["Country"]]), useNA = "ifany")

table(advert[["Timestamp"]], encode_ordinal(advert[["Timestamp"]]), useNA = "ifany")

new_advert <- advert
new_advert[["Ad.Topic.Line_encoded"]] <- encode_ordinal(advert[["Ad.Topic.Line"]])
new_advert[["City_encoded"]] <- encode_ordinal(advert[["City"]])
new_advert[["Country_encoded"]] <- encode_ordinal(advert[["Country"]])
new_advert[["Timestamp_encoded"]] <- encode_ordinal(advert[["Timestamp"]])
head(new_advert)

# Drop the categorical columns

new_advert1 = subset(new_advert, select = -c(Ad.Topic.Line, City, Country, Timestamp) )
head(new_advert1)

# Confirm the datatypes of the encoded columns

sapply(new_advert1, class)

# Bivariate Analysis: Numerical and Numerical
# Scatter plot

attach(new_advert1)
plot(new_advert1[,0:10])

# Correlation matrix
# First install the corrplot package

install.packages("corrplot")
library(corrplot)

# Run the corrplot function

new_advert1.cor = cor(new_advert1, method = c("spearman"))
corrplot(new_advert1.cor)

# Supervised Learning Model
# 1. KNN Model

# Randomizing the rows, creates a uniform distribution of 150
random <- runif(150)
advert_random <- new_advert1[order(random),]

# Selecting the first 6 rows from iris_random
head(advert_random)

# Normalize the dataset

normal <- function(x) (
  return( ((x - min(x)) /(max(x)-min(x))) )
)
normal(1:10)
advert_new <- as.data.frame(lapply(advert_random[c(1,2,3,4,5,7,8,9,10)], normal))
summary(advert_new)

# Create test and train data sets

train <- advert_new[1:130,]
test <- advert_new[131:150,]
train_sp <- advert_random[1:130,6]
test_sp <- advert_random[131:150,6]


# Build the model

library(class)    
require(class)
model <- knn(train= train,test=test, ,cl= train_sp,k=13)

# Evaluate the model performance

library(gmodels)
CrossTable(x = test_sp, y = model, prop.chisq=FALSE)
# There are no incorrect classifications.


# 2. Decision Trees

# Normalize the dataset

normal <- function(x) (
  return( ((x - min(x)) /(max(x)-min(x))) )
)
normal(1:10)
advert_new1 <- as.data.frame(lapply(advert_random[c(1,2,3,4,5,6,7,8,9,10)], normal))
summary(advert_new1)

# Create test and train data sets

data_train <- advert_new1[1:130,]
data_test <- advert_new1[131:150,]

# Build the model

library(rpart)
library(rpart.plot)
fit <- rpart(Clicked.on.Ad~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)

# Do predictions 
pred <-predict(fit, data_test, type = 'class')

# Evaluate the model performance
table_mat <- table(data_test$Clicked.on.Ad, pred)
table_mat

# Only 2 zero values were misclassified.


# 3. Naive Bayes Model
# Splitting data into training and test data sets

library(caret)
indxTrain <- createDataPartition(y = new_advert1$Clicked.on.Ad,p = 0.75,list = FALSE)
training <- new_advert1[indxTrain,]
testing <- new_advert1[-indxTrain,]


# Comparing the outcome of the training and testing phase. Creating objects x 
# which holds the predictor variables and y which holds the response variables


# Convert the Clicked.on.Ad to a factor variable
training[["Clicked.on.Ad"]] = factor(training[["Clicked.on.Ad"]])

testing[["Clicked.on.Ad"]] = factor(testing[["Clicked.on.Ad"]])

x = training[c(1,2,3,4,5,7,8,9,10)]
y = training$Clicked.on.Ad

x_test = testing[c(1,2,3,4,5,7,8,9,10)]
y_test = testing$Clicked.on.Ad

# Loading our inbuilt e1071 package that holds the Naive Bayes function.
# ---
# 
library(e1071)

# Now building our model 

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))


# Do predictions

Predict <- predict(model,newdata = x_test )

# Getting the confusion matrix to see accuracy value and other parameter values

confusionMatrix(Predict,y_test)


# 4. SVM Model
# Create train and test data

library(caret)
intrain <- createDataPartition(y = new_advert1$Clicked.on.Ad, p= 0.7, list = FALSE)
training <- new_advert1[intrain,]
testing <- new_advert1[-intrain,]

# Convert the Clicked.on.Ad to a factor variable
training[["Clicked.on.Ad"]] = factor(training[["Clicked.on.Ad"]])

# Check the dimensions of the training dataframe and testing dataframe

dim(training) 
dim(testing)

# Train the model
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(Clicked.on.Ad ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

# Check the result of the training model
svm_Linear

# DO predictions

test_pred <- predict(svm_Linear, newdata = testing)
test_pred

# Check accuracy of the model using confusion matrix
confusionMatrix(table(test_pred, testing$Clicked.on.Ad))



