library(tidyverse)
library(maps)

some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic"
)
# Retrievethe map data
#some.eu.maps <- map_data("world", region = some.eu.countries)
#some.eu.maps <- map_data("world")
some.eu.maps <- map_data("world", region = c("China", "Thailand", "South Korea", "North Korea", 
                                             "Vietnam", "Cambodia", "Laos", "Malaysia", "Myanmar", 
                                             "Nepal", "India", "Bangladesh", "Mongolia", "Japan", 
                                             "Brunei", "Pakistan", "Afghanistan", "Bhutan", "Indonesia"))

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(some.eu.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region)) +
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d() +
  theme(legend.position = "none")
