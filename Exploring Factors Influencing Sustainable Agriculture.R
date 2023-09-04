# Objective: In this research project, I aim to analyze the factors influencing the indicator of the Sustainable Development Goals related to "Zero Hunger," which focuses on ending hunger, achieving food security, improving nutrition, and promoting sustainable agriculture. I will consider countries and years as observations to understand what drives this indicator's variation.

## Research Questions:

# What are the key factors that influence progress towards achieving "Zero Hunger" in different countries over the years?
# Can we use machine learning methods to predict and understand the factors affecting "Zero Hunger" progress?
# 


## Methodology:

# Step 1: Data Preparation:
# 
# Import the dataset containing information about countries, years, and the "Zero Hunger" indicator.
# Clean the data by addressing missing values or inconsistencies.
# Step 2: Feature Selection:
# 
# Identify a set of observable variables (features) that I believe may have a significant impact on the "Zero Hunger" indicator based on my research questions.
# Step 3: Data Analysis:
# 
# Perform exploratory data analysis (EDA) to gain insights into the relationships between the selected features and the "Zero Hunger" indicator.
# Utilize data visualization techniques to visually represent these relationships.
# Step 4: Machine Learning Methods:
# 
# Apply at least three machine learning methods to predict and interpret the factors influencing the "Zero Hunger" indicator. I'll use R programming and common machine learning methods such as Linear Regression, Decision Trees, and Random Forests.

# Step 5: Model Evaluation:
#   
#   Split the dataset into training and testing subsets to evaluate the performance of each machine learning method.
# Utilize metrics like Mean Absolute Error (MAE), Mean Squared Error (MSE), and R-squared (R2) to assess model performance.
# Step 6: Identifying Important Variables:
#   
#   Analyze feature importance or coefficients for each machine learning method to identify the most influential observable variables that affect the "Zero Hunger" indicator.
# Step 7: Report Writing:
  
  
  
  
  
  
  # 1.0 Load the libraries:
library(ggplot2)  # for graphics
library(dplyr) # For piping function
library(rpart)        # For decision trees
library(class)        # For k-NN
library(neuralnet)    # For neural networks
#install.packages('caTools')
#install.packages('party')
#install.packages('magrittr')
#install.packages('rpart.plot')
library(caTools)
library(party)
library(magrittr)
library(rpart.plot)
library(caret)
#install.packages(c('neuralnet','keras','tensorflow'),dependencies = T)
```



# 2.0 Load dataset

df <- read.csv("AWU.csv",sep = "," ,header = TRUE)
df <- as.data.frame(df)
head(df)


# 3.0 Clean data by removing all empty cell
# remove all rows with (x	Not available	)
df <- subset(df, !apply(df == "x", 1, any))

df["Target"] <- as.factor(x=df$Target)
str(df)  # check the structure of the cleaned data

sum(is.na(df))



# 4.0 Statistical summaries
summary(df)



#### 4.1 Graphs  For Few Selected countries

##### 4.1.1 Visualize the trend of indicators over time for Austria

ggplot(df, aes(x=Years , y = Austria, color = Target)) + geom_line() +
  labs(title = "Sustainable Development Goal 2 For Austria Over The Years", x = "Years", y = "Selected Indicators")


##### 4.1.2 Visualize the trend of indicators over time for Belgium

ggplot(df, aes(x=Years , y =Belgium, color = Target)) + geom_line() +
  labs(title = "Sustainable Development Goal 2 For Belgium Over The Years", x = "Years", y = "Selected Indicators")


##### 4.1.3 Visualize the trend of indicators over time for United Kingdom

ggplot(df, aes(x=Years , y=UnitedKingdom, color = Target)) + geom_line() +
  labs(title = "Sustainable Development Goal 2 For United Kingdom Over The Years", x = "Years", y = "Selected Indicators")

##### 4.1.4 Visualize the trend of indicators over time for Spain

ggplot(df, aes(x=Years , y =Spain, color = Target)) + geom_line() +
  labs(title = "Sustainable Development Goal 2 For Spain Over The Years", x = "Years", y = "Selected Indicators")



##### 4.1.5 Visualize the trend of indicators over time for Hungary

ggplot(df, aes(x=Years , y =Hungary, color = Target)) + geom_line() +
  labs(title = "Sustainable Development Goal 2 For Hungary Over The Years", x = "Years", y = "Selected Indicators")

##### 4.1.6 Visualize the trend of indicators over time for Finland

ggplot(df, aes(x=Years , y =Finland, color = Target)) + geom_line() +
  labs(title = "Sustainable Development Goal 2 For Finland Over The Years", x = "Years", y = "Selected Indicators")

##### 4.1.7 Visualize the trend of indicators over time for France

ggplot(df, aes(x=Years , y =France, color = Target)) + geom_line() +
  labs(title = "Sustainable Development Goal 2 For France Over The Years", x = "Years", y = "Selected Indicators")

##### 4.1.8 Visualize the trend of indicators over time for Portugal

ggplot(df, aes(x=Years , y =Portugal, color = Target)) + geom_line() +
  labs(title = "Sustainable Development Goal 2 For Portugal Over The Years", x = "Years", y = "Selected Indicators")


##### 4.1.9 Visualize the trend of indicators over time for CzechRepublic

ggplot(df, aes(x=Years , y =CzechRepublic, color = Target)) + geom_line() +
  labs(title = "Sustainable Development Goal 2 For CzechRepublic Over The Years", x = "Years", y = "Selected Indicators")



##### 4.1.10 Visualize the trend of indicators over time for Poland
ggplot(df, aes(x=Years , y =Poland, color = Target)) + geom_line()+
  labs(title = "Sustainable Development Goal 2 For Poland Over The Years", x = "Years", y = "Selected Indicators")




# 5.0 Model Training
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(df))
df <- df[train_indices,2:30]  # shuffle the dataset excluding the years

# Split data into training and testing sets
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}



# feature scaling 
train_data <- create_train_test(df, 0.8, train = TRUE)
test_data <- create_train_test(df, 0.8, train = FALSE)


train_data[,2:29] <- as.data.frame(scale(train_data[,2:29]))
test_data[,2:29] <- as.data.frame(scale(test_data[,2:29]))


# Check the dimension of the data 
dim(train_data)
dim(test_data)
prop.table(table(train_data$Target))

# Check for possible NA's 
sum(is.na(train_data))
sum(is.na(test_data))


head(train_data)



#### 5.1 Decision Tree
# Decision Tree
tree_model <- rpart(Target~., data = train_data , control = rpart.control(maxdepth = 30,minsplit = 1))
rpart.plot(tree_model)
summary(tree_model)
tree_model$variable.importance


# Performance Evaluation
tree_predictions <- predict(tree_model, test_data, type = "class")
tree_accuracy <- (sum(tree_predictions == test_data$Target) / nrow(test_data)) * 100


# Print the metrics
cat("Decision Tree Accuracy:", tree_accuracy, "% \n")


#### 5.2. K nearest neighbors

# Choose the  K-Value for model accuracy.

trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

set.seed(7)
grid <- expand.grid(.k=seq(1,25,by=1))
fit.knn <- train(Target~., data=train_data, method="knn", 
                 metric=metric, tuneGrid=grid, trControl=trainControl)

knn.k2 <- fit.knn$bestTune

print(fit.knn)
plot(fit.knn)


# I found optimal k= 13, the number of closest instances to collect in order to make a prediction.

# Using the fit model to predict class for our test set, and print out the confusion matrix:
  

set.seed(7)
prediction <- predict(fit.knn, newdata = test_data)
cf <- confusionMatrix(prediction, test_data$Target)
print(cf)


# With k = 13, the accuracy of model is 100%




#### 5.3 Neural Networks Model
set.seed(1234)
nn_model = neuralnet(Target~., data=train_data, linear.output = FALSE)

plot(nn_model,rep = "best")


pred <- predict(nn_model, test_data)
labels <- c("Awu", "Auof", "Rd")
prediction_label <- data.frame(max.col(pred)) %>%     
  mutate(pred=labels[max.col.pred.]) %>%
  select(2) %>%unlist()

table(test_data$Target, prediction_label)

check = as.numeric(test_data$Target) == max.col(pred)
accuracy = (sum(check)/nrow(test_data))*100

# Print the metrics
cat("Neural Network Accuracy is :",accuracy, "% \n")
