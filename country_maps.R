library(tidyverse)
library(maps)
library(forcats)

df_coffee <- read_csv("Coffee-modified.csv")

df_coffee2 <- df_coffee %>% filter(!is.na(ID), !is.na(Country.of.Origin))

df_coffee2 <- df_coffee2 %>% mutate(Country.of.Origin = recode(Country.of.Origin, 
                                                               "Cote d?Ivoire" = "Ivory Coast",
                                                               "United States" = "USA", 
                                                               "Tanzania, United Republic Of" = "Tanzania",
                                                               "United States (Hawaii)" = "Hawaii",
                                                               "United States (Puerto Rico)" = "Puerto Rico"))

coffee_countries <- unique(df_coffee2$Country.of.Origin)

print(coffee_countries)

coffee_maps <- map_data("world", region = coffee_countries)
world_map <- map_data("world")
                          
region.lab.data <- coffee_maps %>% group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(coffee_maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = region)) +
  #geom_polygon(aes(group = world_map)) +
  #geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d() +
  theme(legend.position = "none")

ggplot(df_coffee2) + geom_bar(aes(fct_infreq(Country.of.Origin)), fill = 'maroon') +
  labs(x = "Country of Origin", y = "Country Count", title = "Count of Each Country in the Data Set") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
