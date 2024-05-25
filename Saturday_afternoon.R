library(tidyverse)

cars2020 <- read_csv("data/cars2020.csv")

boxplot(mpg~transmission, data = cars2020)

# read in the penguins data set 

penguins <- read_csv("data/penguins.csv")

count(penguins, species, island)

## Make a scatter of plot of bill depth (y-variable) vs
## flipper length (x-variable)

ggplot(data = penguins,
       aes(x = flipper_length_mm, y = bill_depth_mm)) + 
  geom_point() + theme_bw()

### adding another aesthetic for colour mapping to species
ggplot(data = penguins,
       aes(x = flipper_length_mm, 
           y = bill_depth_mm, colour = species,
           shape = sex)) +
  geom_point() 

## making a histogram of bill lengths
hist(penguins$bill_length_mm)

ggplot(data = penguins,
       aes(x = bill_length_mm)) + 
  geom_histogram(fill = "blue", color = "black") + 
  theme_bw() +
  labs(title = "Distribution of penguin bill lengths",
       subtitle = "Some scientists recorded some penguins. This is what they found",
       x = "Bill length", y = "# of penguins")

## Using facets to show subgraphs
### Making separate sub-histograms for each of the species
## using the facet_wrap

ggplot(data = penguins,
       aes(x = body_mass_g, fill = species)) + 
  geom_histogram(color = "black") + 
  facet_wrap(~species, ncol = 1) + 
  theme(legend.position = "bottom",
        legend.title = element_blank())

## box plot using base R 
boxplot(body_mass_g~species, data = penguins)

ggplot(data = penguins,
       aes(x = species, y = body_mass_g)) +
  geom_boxplot() 

penguins2 <- penguins |> 
  filter(!is.na(sex))

ggplot(data = penguins2,
       aes(x = body_mass_g, fill = species)) + 
  geom_histogram(color = "black") + 
  facet_grid(species~sex) + 
  theme(legend.position = "bottom")






















