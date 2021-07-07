library(tidyverse)
library(gridExtra)
library(cowplot)
library(viridis)
library(lubridate)
library(corrplot)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

unique(coffee_ratings$certification_body)

coffee_ratings[which(coffee_ratings$certification_body == "Blossom Valley International\n"), 
               which(colnames(coffee_ratings) == "certification_body")] <- "Blossom Valley International"

coffee_ratings[which(coffee_ratings$in_country_partner == "Blossom Valley International\n"), 
               which(colnames(coffee_ratings) == "in_country_partner")] <- "Blossom Valley International"

coffee_ratings[which(coffee_ratings$in_country_partner == "Specialty Coffee Ass"), 
               which(colnames(coffee_ratings) == "in_country_partner")] <- "Specialty Coffee Association"

boxplot(coffee_ratings$altitude_mean_meters)

coffee_ratings[which(coffee_ratings$altitude_low_meters == 190164), 
               c("altitude_low_meters", "altitude_high_meters", "altitude_mean_meters")] <- 1901.64

coffee_ratings[which(coffee_ratings$altitude_low_meters == 110000.00), 
               c("altitude_low_meters", "altitude_high_meters", "altitude_mean_meters")] <- 1100.00

coffee_ratings[which(coffee_ratings$altitude_low_meters == 11000.00), 
               c("altitude_low_meters", "altitude_high_meters", "altitude_mean_meters")] <- 1100.00

#Mention the small sample size before going forward with more graphs. Focus on one later one

coffee_ratings %>%
      group_by(species) %>%
      count() %>%
      rmarkdown::paged_table()

features <- names(coffee_ratings)[which(colnames(coffee_ratings) == "aroma"):
                                         which(colnames(coffee_ratings) == "moisture")]

coffee_ratings %>% 
      select(c("species", features)) %>%
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

arabica_out <- boxplot.stats(coffee_ratings_wide$Arabica, coef = 3)$out
robusta_out <- boxplot.stats(coffee_ratings_wide$Robusta, coef = 3)$out

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

# After brief look at differences between species, focusing on Arabica entirely now due to sample size

coffee_ratings_arabica <- coffee_ratings_no_outliers %>%
                                 filter(species == "Arabica")

sum(is.na(coffee_ratings_arabica$country_of_origin))

coffee_ratings_arabica <- coffee_ratings_arabica %>%
                                 filter(!is.na(country_of_origin))

average <- mean(coffee_ratings_arabica$total_cup_points)

coffee_arabica_greater <- coffee_ratings_arabica %>% 
               mutate(greater = total_cup_points > average)

coffee_greater_sum <- coffee_arabica_greater %>% 
            group_by(country_of_origin) %>% 
            summarise(greater_sum = sum(greater))

order_names <- coffee_arabica_greater %>% 
                  group_by(country_of_origin) %>%
                  summarise(count = n()) %>% 
                  arrange(count)

order_names <- as.vector(order_names$country_of_origin)

coffee_arabica_greater$country_of_origin <- factor(coffee_arabica_greater$country_of_origin, levels = order_names)
coffee_greater_sum$country_of_origin <- factor(coffee_greater_sum$country_of_origin, levels = order_names)

coffee_arabica_greater %>% 
   group_by(country_of_origin) %>%
   summarise(count = n()) %>%
   left_join(., coffee_greater_sum, by = "country_of_origin") %>%
   ggplot(aes(reorder(country_of_origin, desc(-count)), count, col = country_of_origin)) + 
   geom_bar(fill = "white", stat = "identity", show.legend = FALSE, width = .75) + 
   geom_bar(mapping = aes(country_of_origin, greater_sum, fill = country_of_origin), 
            stat = "identity", position = "identity", show.legend = FALSE, width = .75) + 
   coord_flip() + 
   theme(panel.grid.major.y = element_blank(), panel.background = element_rect(color = "light grey")) +
   labs(title = "Count of Distinct Coffees Submitted for Scoring by Country of Origin",
        subtitle = "Filled by Count of Coffees Above Cross-Country Average Score",
        y = "Count of Coffees Submitted for Scoring",
        x = "Country of Origin") +
   scale_fill_viridis(option = "viridis", discrete = TRUE, direction = -1, end = .7) +
   scale_color_viridis(option = "viridis", discrete = TRUE, direction = -1, end = .7) +
   annotate(geom = "rect", xmin = 4.5, xmax = 12, ymin = 112.5, ymax = 235, fill = "light grey",
            color = "black") +
   annotate(geom = "rect", xmin = 8.5, xmax = 11, ymin = 119, ymax = 131,
            fill = "#440154", color = "#440154") +
   annotate(geom = "rect", xmin = 5.5, xmax = 8, ymin = 119, ymax = 131,
            fill = "white", color = "#440154") +
   annotate(geom = "text", label = "greater than the cross-country average", 
            x = 9.8, y = 183) +
   annotate(geom = "text", label = "less than the cross-country average", 
            x = 6.8, y = 180)

coffee_arabica_greater %>%
   group_by(country_of_origin) %>%
   summarise(count = n()) %>%
   filter(count > 10) %>%
   left_join(., coffee_greater_sum, by = "country_of_origin") %>%
   mutate(percent_total = count/sum(count), 
          greater_percent = greater_sum/count) %>%
   ggplot(aes(reorder(country_of_origin, desc(-greater_percent)), greater_percent, fill = country_of_origin)) + 
   geom_bar(aes(col = country_of_origin), stat = "identity", show.legend = FALSE, width = .75) + 
   geom_bar(mapping = aes(country_of_origin, percent_total), fill = "white", 
            stat = "identity", position = "identity", show.legend = FALSE, width = .75) + 
   geom_bar(mapping = aes(country_of_origin, percent_total, fill = country_of_origin), 
            stat = "identity", position = "identity", show.legend = FALSE, width = .5, col = 'white') +
   coord_flip() + 
   theme(panel.grid.major.y = element_blank(), panel.background = element_rect(color = "light grey")) +
   labs(title = paste("Percentage of Coffees Submitted for Scoring Above Cross-Country Average", "by Country of Origin",
                      sep = "\n"),
        subtitle = "Overlaid by Percentage of Total Coffees Submitted for Scoring",
        y = "Percentage",
        x = "Country of Origin") +
   scale_fill_viridis(option = "viridis", discrete = TRUE, direction = -1, end = .7) +
   scale_color_viridis(option = "viridis", discrete = TRUE, direction = -1, end = .7) +
   annotate(geom = "rect", xmin = 4, xmax = 6, ymin = .55, ymax = .96, fill = "light grey",
            color = "black") +
   annotate(geom = "rect", xmin = 2, xmax = 4, ymin = .55, ymax = .96, fill = "light grey",
            color = "black") +
   annotate(geom = "rect", xmin = 4.5, xmax = 5.5, ymin = .57, ymax = .62, fill = "#25848e",
            color = "#25848e") +
   annotate(geom = "rect", xmin = 2.5, xmax = 3.5, ymin = .57, ymax = .62, fill = "white",
            color = "white") +
   annotate(geom = "rect", xmin = 2.66, xmax = 3.34, ymin = .573, ymax = .617, fill = "#25848e",
            color = "#25848e") +
   annotate(geom = "text", label = "% above cross-country average", 
            x = 5, y = .79) +
   annotate(geom = "text", label = "% of total submitted coffees", 
            x = 3, y = .77)

coffee_arabica_greater %>% #Might have to mess around with this for Rmd because of paged_table
      group_by(certification_body) %>%
      summarise(cert_avg = mean(total_cup_points)) %>%
      filter(cert_avg == max(cert_avg) | cert_avg == min(cert_avg)) %>%
      rmarkdown::paged_table() %>%
      summarise(difference = max(cert_avg) - min(cert_avg))

coffee_arabica_greater %>% 
   ggplot(aes(altitude_mean_meters, total_cup_points, col = greater)) + 
      geom_point(alpha = .4) +
      geom_smooth(method = "lm") +
      theme(axis.line.x.bottom = element_line(color = "black"),
         axis.line.y.left = element_line(color = "black"), panel.background = element_blank())

coffee_arabica_greater$grading_date <- mdy(coffee_arabica_greater$grading_date)

coffee_arabica_greater %>%
   ggplot(aes(grading_date, total_cup_points, col = total_cup_points)) +
      geom_line() +
      geom_smooth(method = "lm", color = "yellow") +
      scale_color_gradient(low = "blue", high = "red") +
      theme(panel.background = element_blank(), 
            axis.line.x.bottom = element_line(color = "black"),
            axis.line.y.left = element_line(color = "black"))

coffee_arabica_greater %>% 
      filter(!is.na(processing_method)) %>%
      group_by(processing_method) %>%
      ggplot(aes(processing_method, total_cup_points, fill = processing_method)) +
         geom_boxplot(color = "white") +
         scale_fill_manual(values = c("#2426d1", "#6404b8", "#cf3e4d", "#1596d6", "#069658")) +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(color = "black"),
               panel.grid.minor.y = element_line(color = "black"),
               panel.background = element_rect(fill = "grey"),
               legend.position = "none")

coffee_arabica_greater %>% 
      filter(!is.na(processing_method)) %>%
      group_by(processing_method) %>%
      count()