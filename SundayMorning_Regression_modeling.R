library(rpart)
library(rpart.plot)
library(tidyverse)
library(rsample)

cars2020 <- read_csv("data/cars2020.csv")

### splitting the data into an 80-20 split

set.seed(2001) ## to ensure that we all get the same data splits

cars_split <- initial_split(cars2020, prop = 0.8)
cars_training_data <- training(cars_split)
cars_testing_data <- testing(cars_split)

model1 <- rpart(mpg~transmission+disp, data = cars_training_data)

cars_testing_data$model1 <- predict(model1, newdata = cars_testing_data)


ggplot(data = cars_testing_data, 
       aes(x = mpg, y = model1)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(c(0,60)) + ylim(c(0,60))

model2 <- rpart(mpg~transmission+disp+fuel, data = cars_training_data)
rpart.plot(model2)

cars_testing_data$model2 <- predict(model2, newdata = cars_testing_data)
ggplot(data = cars_testing_data,
       aes(x = mpg, y = model2)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(c(0,60)) + ylim(c(0,60))

model3 <- rpart(mpg~disp+atvType, data = cars_training_data)
rpart.plot(model3)
cars_testing_data$model3 <- predict(model3, newdata = cars_testing_data)

ggplot(data = cars_testing_data,
       aes(x = mpg, y = model3)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(c(0,60)) + ylim(c(0,60))

model4 <- lm(mpg~transmission, data = cars_training_data)

cars_testing_data$model4 <- predict(model4, newdata = cars_testing_data)


model5 <- lm(mpg~disp, data = cars_training_data)
cars_testing_data$model5 <- predict(model5, newdata = cars_testing_data)

model6 <- lm(mpg~transmission+disp+atvType, data = cars_training_data)
cars_testing_data$model6 <- predict(model6, newdata = cars_testing_data)


cars_testing_long <- pivot_longer(cars_testing_data, model1:model6,
                                  names_to = "model_num", values_to = "pred_mpg")


ggplot(cars_testing_long, 
       aes(x = mpg, y = pred_mpg)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(c(0,60)) + ylim(c(0,60)) + 
  facet_wrap(~model_num)

