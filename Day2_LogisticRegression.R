library(rsample)

so <- read_csv("data/stackoverflow.csv") |>
  mutate(remote2 = if_else(remote == "Remote", 1, 0))


remote_colors <- c("Remote" = "blue", "Not remote" = "red")

ggplot(so, 
       aes(x = country, fill = remote)) + 
  geom_bar(position = "dodge") + 
  scale_fill_manual(values = remote_colors) + theme_minimal()

ggplot(so,
       aes(x = salary)) +
  geom_histogram(fill = "#1EA896", color = "black") +
  facet_wrap(~country)

set.seed(2001)
so_split <- initial_split(so, prop = 0.7)
so_train <- training(so_split)
so_test <- testing(so_split)

model1 <- glm(remote2~country+salary+years_coded_job, data = so_train,
    family = binomial(link = "logit"))

so_test$model1 <- predict(model1, newdata = so_test, type = "response")


ggplot(data = so_test,
       aes(x = remote, y = model1)) + 
  geom_point(alpha = 0.1)

ggplot(data = so_test,
       aes(x = remote, y = model1)) + 
  geom_boxplot()

ggplot(data = so_test,
       aes(x = model1)) + geom_histogram(color = "black",
                                         fill = "#904E55") +
  facet_wrap(~remote, ncol = 1,
             scales = "free_y")


### Going to start over

so_remote <- so |>
  filter(remote == "Remote")
so_not_remote <- so |>
  filter(remote == "Not remote")

## Now make a training set that is evenly-split between remote and not remote
so_remote_split <- initial_split(so_remote, prop = 0.8)
so_remote_train <- training(so_remote_split) ## this is part of my training data
so_remote_test <- testing(so_remote_split)

so_not_remote_split <- initial_split(so_not_remote, prop = 574/6273)
so_not_remote_train <- training(so_not_remote_split) ## this is also training
so_not_remote_test <- testing(so_not_remote_split)

so_new_training_data <- bind_rows(so_remote_train, so_not_remote_train)
so_new_testing_data <- bind_rows(so_remote_test, so_not_remote_test)

model2 <- glm(remote2~country+salary+years_coded_job, data = so_new_training_data)

so_new_testing_data$model2 <- predict(model2, newdata = so_new_testing_data,
                                      type = "response")
ggplot(data = so_new_testing_data,
       aes(x = remote, y = model2)) + 
  geom_boxplot()

ggplot(data = so_new_testing_data,
       aes(x = model2)) + 
  geom_histogram(color = "black", fill = "yellow") +
  facet_wrap(~remote, ncol = 1, scales = "free_y") + 
  geom_vline(xintercept = 0.5, #linetype = "dashed",
             color = "#7B287D")



























