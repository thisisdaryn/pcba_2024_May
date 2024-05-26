library(tidyverse) # contains dplyr, ggplot2, tidyr, readr, lubridate
library(ggplot2)
library(rsample)
library(ggbeeswarm)


so <- read_csv("data/stackoverflow.csv", show_col_types = FALSE)

count(so, country, remote)

remote_colours <- c("Not remote" = "blue", 
                    "Remote" = "orange")
ggplot(data = so,
       aes(x = country, fill = remote)) + 
  geom_bar() + 
  scale_fill_manual(values = remote_colours)

### splitting the data without rebalancing

set.seed(2001) ## to ensure that we all get the same data splits


so <- read_csv("data/stackoverflow.csv", show_col_types = FALSE) |>
  mutate(remote2 = if_else(remote == "Not remote", 0, 1)) |>
  relocate(remote2, .after = remote)

so_split <- initial_split(so, prop = 0.8)
so_training_data <- training(so_split)
so_testing_data <- testing(so_split)

model1 <- glm(remote2~country+salary+years_coded_job, 
              data = so_training_data, family = binomial(link = "logit"))

model1

so_testing_data$model1 <- predict(model1, newdata = so_testing_data,
                                  type =  "response") # type = response used to get p

plot1 <- ggplot(data = so_testing_data,
       aes(y = remote, x = model1)) +
  geom_boxplot()

plot2 <- ggplot(data = so_testing_data,
       aes(y = remote, x = model1)) +
  geom_beeswarm()

plot1



model1 <- glm(remote~country+salary+years_coded_job, 
              data = so_training_data, family = binomial(link = "logit"))


ggplot(data = so_testing_data,
       aes(x = model1)) + geom_density() + 
  facet_wrap(~remote, ncol = 1)







model1 <- glm(family = binomial(link = "logit"))