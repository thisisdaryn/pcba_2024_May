library(tidyverse)

cars2020 <- read_csv("data/cars2020.csv")

manual <- filter(cars2020, transmission == "Manual")

cyl_4 <- filter(cars2020, cyl >= 4)

manual_gt4cyl <- filter(cars2020, transmission == "Manual",
                        cyl > 4)
