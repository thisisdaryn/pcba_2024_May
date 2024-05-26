library(tidyverse)
library(rsample)

so <- read_csv("data/stackoverflow.csv") |>
  mutate(remote2 = if_else(remote == "Not remote", 0, 1))

so_remote <- so |>
  filter(remote == "Remote")
so_not_remote <- so |>
  filter(remote == "Not remote")

set.seed(2001)
so_remote_split <- initial_split(so_remote, prop = 0.8)
so_remote_training_data <- training(so_remote_split)
so_remote_testing_data <- testing(so_remote_split)

so_nr_split <- initial_split(so_not_remote, prop = 0.09)
so_nr_training_data <- training(so_nr_split)


new_training_data <- bind_rows(so_remote_training_data, 
                               so_nr_training_data)

so_nr_testing_data <- testing(so_nr_split)
new_testing_data <- bind_rows(so_remote_testing_data,
                              so_nr_testing_data)

balanced_model <- glm(remote2~country+salary+years_coded_job,
                      data = new_training_data, 
                      family = binomial(link = "logit"))
new_testing_data$balanced_model <- predict(balanced_model,
                                           newdata = new_testing_data,
                                           type = "response")

ggplot(data = new_testing_data,
       aes(y = remote, x = balanced_model)) + 
  geom_boxplot() +
  geom_vline(xintercept = 0.5)

results_df <- new_testing_data |>
  select(remote, balanced_model) |>
  mutate(prediction = if_else(balanced_model > 0.5, "Remote", "Not remote")) |>
  mutate(eval = case_when(remote == "Remote" & prediction == "Remote" ~ "True positive",
                          remote == "Remote" & prediction == "Not remote" ~ "False negative",
                          remote == "Not remote" & prediction == "Remote" ~ "False positive",
                          remote == "Not remote" & prediction == "Not remote" ~ "True negative"))

count(results_df, eval)
