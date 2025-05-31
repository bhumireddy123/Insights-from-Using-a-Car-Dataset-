


library(class)
library(e1071)
library(gmodels)
library(randomForest)
library(ggplot2)



car_data<-read.csv(file.choose(),stringsAsFactors=TRUE)
View(car_data)
str(car_data)
# Load the dataset


# 1. Linear Regression Load necessary libraries

linear_model <- lm(Price ~ Mileage + Year + Condition, data = car_data)

summary(linear_model)

ggplot(car_data, aes(x = Mileage, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Price vs Mileage", x = "Mileage", y = "Price")


#2. Logistic Regression Convert Condition to a factor
car_data$Condition <- as.factor(car_data$Condition)


logistic_model <- glm(Condition ~ Mileage + Year + Price, data = car_data, family = binomial)

summary(logistic_model)

car_data$predicted_prob <- predict(logistic_model, type = "response")

ggplot(car_data, aes(x = Mileage, y = predicted_prob)) +
  geom_point(aes(color = Condition)) +
  labs(title = "Predicted Probability of Condition", x = "Mileage", y = "Probability")



#3. Visualization: Distribution of Car Prices Plotting histogram of car prices


ggplot(car_data, aes(x = Price)) +
  
  geom_histogram(binwidth=5000, fill="blue", color="black") +
  labs(title="Distribution of Car Prices", x="Price", y="Frequency")



#4. Random Forest Load necessary library for random forest
library(randomForest)


set.seed(123)  # For reproducibility
rf_model <- randomForest(Price ~ Mileage + Year + Condition, data = car_data, ntree = 100)

ranprint(rf_model)
importance(rf_model)
varImpPlot(rf_model)



# 5. K-Means Clustering Scale the data for clustering
scaled_data <- scale(car_data[, c("Price", "Mileage")])


set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3)

car_data$cluster <- as.factor(kmeans_result$cluster)

ggplot(car_data, aes(x = Mileage, y = Price, color = cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering of Cars", x = "Mileage", y = "Price")





