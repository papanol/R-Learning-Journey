#Load Library
library(dplyr)
library(ggplot2)

#Dataset Review
MultiRegDataset
str(MultiRegDataset)
summary(MultiRegDataset)
hist(MultiRegDataset$expenses)
hist(MultiRegDataset$expenses, 
     main = "Histogram of Expenses",
     xlab = "Value",
     ylab = "Frequency",
     col = "blue",
     border = "black",
     breaks = 20) 
#T-Test
#One Sample t-test
t.test(MultiRegDataset$expenses, mu=15000)

#Linear regression with Smoker
#Create dummy variable for smoker
MultiRegDataset$smoker_dup <- as.factor(MultiRegDataset$smoker)
# Convert the factor to a numeric representation
MultiRegDataset$smoker_dum <- as.numeric(MultiRegDataset$smoker_dup)

# Original Plot with Line
ggplot(MultiRegDataset, aes(x =smoker_dum , y = expenses ))+
  geom_point(colour = "red") + geom_smooth(method = "lm", fill = NA) +
  labs(title = "Original Model",
       x = "Smoker", y = "Expenses")

#Correlation
cor(MultiRegDataset$smoker_dum, MultiRegDataset$expenses)

#Linear Model - No Transformation
lmodel <- lm(expenses ~ smoker_dum, data = MultiRegDataset)
summary(lmodel)

# Transformed Plot with Line SQRT
ggplot(MultiRegDataset, aes(x =sqrt(smoker_dum) , y = sqrt(expenses) ))+
  geom_point(colour = "red") + geom_smooth(method = "lm", fill = NA) +
  labs(title = "Transformed Model SQRT",
       x = "Smoker", y = "Expenses")
#Correlation Transformed SQRT
cor(sqrt(MultiRegDataset$smoker_dum), sqrt(MultiRegDataset$expenses))

# Transformed Plot with Line LOG
ggplot(MultiRegDataset, aes(x =log1p(smoker_dum) , y = log1p(expenses) ))+
  geom_point(colour = "red") + geom_smooth(method = "lm", fill = NA) +
  labs(title = "Transformed Model Log",
       x = "Smoker", y = "Expenses")
#Correlation Transformed SQRT
cor(log1p(MultiRegDataset$smoker_dum), log1p(MultiRegDataset$expenses))


#Linear Model - SQRT Transformation
lmodel <- lm(sqrt(expenses) ~ sqrt(smoker_dum), data = MultiRegDataset)
summary(lmodel)

#Linear Model - Log Transformation
lmodel <- lm(log1p(expenses) ~ log1p(smoker_dum), data = MultiRegDataset)
summary(lmodel)









#Multilinear Regression
#Create dummy variable for smoker
MultiRegDataset$smoker_dup <- as.factor(MultiRegDataset$smoker)
# Convert the factor to a numeric representation
MultiRegDataset$smoker_dum <- as.numeric(MultiRegDataset$smoker_dup)
#Create dummy variable for region
MultiRegDataset$region_dup <- as.factor(MultiRegDataset$region)
#Convert the factor to numeric representation
MultiRegDataset$region_dum <- as.numeric(MultiRegDataset$region_dup)
#Create dummy for sex
MultiRegDataset$sex_dup <- as.factor(MultiRegDataset$sex)
#Convert to numerical dummy
MultiRegDataset$sex_dum <- as.numeric(MultiRegDataset$sex_dup)

# Create the relationship model.
model <- lm(expenses~age+sex+smoker+children+region+bmi, data = MultiRegDataset)
print(model)
# Create the relationship model.
model_dum <- lm(expenses~age+sex_dum+smoker_dum+children+region_dum+bmi, data = MultiRegDataset)
print(model_dum)

# Create the relationship model.
model <- lm(expenses~age+sex+smoker+children+region+bmi, data = MultiRegDataset)
print(model)
#Model Summary 
summary(model)

#Model with Dummy Summary
summary(model_dum)

#Create Model2
model2 <- lm(expenses~., data = MultiRegDataset)
#Summary of Model2
print(model2)
summary(model2)