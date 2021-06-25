library(tidyverse)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

unique(coffee_ratings$certification_body)

coffee_ratings[which(coffee_ratings$certification_body == "Blossom Valley International\n"), 
               which(colnames(coffee_ratings) == "certification_body")] <- "Blossom Valley International"

boxplot(coffee_ratings$altitude_mean_meters)

coffee_ratings[which(coffee_ratings$altitude_low_meters == 190164), 
               c("altitude_low_meters", "altitude_high_meters", "altitude_mean_meters")] <- 1901.64

coffee_ratings[which(coffee_ratings$altitude_low_meters == 110000.00), 
               c("altitude_low_meters", "altitude_high_meters", "altitude_mean_meters")] <- 1100.00

coffee_ratings[which(coffee_ratings$altitude_low_meters == 11000.00), 
               c("altitude_low_meters", "altitude_high_meters", "altitude_mean_meters")] <- 1100.00

features <- names(coffee_ratings)[which(colnames(coffee_ratings) == "aroma"):
                                         which(colnames(coffee_ratings) == "moisture")]

coffee_ratings %>% 
      select(species, features) %>%
      pivot_longer(cols = feature_names) %>%
      ggplot(aes(value)) +
          geom_density(aes(color = species)) +
          facet_wrap(~name, scales = "free") +
          labs(title = "Coffee Rating Features Density Plot by Species") +
          theme(axis.text.y = element_blank(), panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"),
                axis.ticks.y = element_blank()) 

