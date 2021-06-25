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
      pivot_longer(cols = features) %>%
      ggplot(aes(value)) +
          geom_density(aes(color = species)) +
          facet_wrap(~name, scales = "free") +
          labs(title = "Coffee Rating Features Density Plot by Species") +
          theme(axis.text.y = element_blank(), panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"),
                axis.ticks.y = element_blank(), axis.line.x.bottom = element_line(color = "black")) +
                xlim(0, 10)

coffee_ratings %>%
      ggplot(aes(species, total_cup_points, fill = species)) + 
          geom_violin() +
          stat_summary(fun.data = "mean_sdl", geom = "pointrange") +
          labs(title = "Coffee Rating Violin Plot by Species") +
          theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(), 
                panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey"))

coffee_ratings_wide <- coffee_ratings %>%
                            pivot_wider(names_from = species, values_from = total_cup_points)

arabica_out <- boxplot.stats(test$Arabica, coef = 3)$out
robusta_out <- boxplot.stats(test$Robusta, coef = 3)$out

coffee_ratings_no_outliers <- coffee_ratings_wide %>%
                                    filter(!Arabica %in% arabica_out, !Robusta %in% robusta_out) %>%
                                    pivot_longer(cols = c(Arabica, Robusta), names_to = "species", 
                                                 values_to = "total_cup_points",
                                                 values_drop_na = TRUE)

coffee_ratings_no_outliers %>% 
      ggplot(aes(species, total_cup_points, fill = species)) +
          geom_violin(trim = FALSE) +
          stat_summary(fun.data = "mean_sdl", geom = "pointrange") +
          labs(title = "Coffee Rating Violin Plot by Species") +
          theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(), 
                panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey"))