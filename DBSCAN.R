#Part1 of Exercise 05

library(tidyverse)
library(purrr)
library(maps)
library(dbscan)
library(stringr)
library(magrittr)
library(leaflet)
install.packages("fpc")
installed.packages()
#-Data Load---------------------------------------------------------------
world.cities<-read_csv("worldcitiespop.csv")
#---------------------------------------------------------------------------
world.cities <- world.cities %>% filter(Population >= 50000)
minPts <- 8   #min points 
eps <- 0.15   #radius

clusters <- dbscan(select(world.cities, Latitude, Longitude), minPts = minPts, eps = eps)
world.cities$cluster <- clusters$cluster

# Mark objects within a cluster with `1` and
# objects outside a cluster with `-1` (noise pt).
world.cities$type <- ifelse(clusters$cluster > 0, 1, -1)

# Mark border objects with `0` -> Thus all
# objects with `type == 1` are core objects.
world.cities$type[which(! clusters$cluster ==
                          dbscan(select(world.cities, Latitude, Longitude),
                                 minPts = minPts, eps = eps,
                                 borderPoints = F)$cluster)] <- 0

# Two alternative representations
groups <- world.cities %>% filter(cluster != 0)   #Corresponds to Core and border points
noise <- world.cities %>% filter(cluster == 0)    #Corresponds to Noise points

# (1)
ggplot(world.cities, aes(x = Longitude, y = Latitude, alpha = 0.5)) +
  geom_point(aes(fill = "blue"), noise) +
  geom_point(aes(colour = as.factor(cluster)), groups,
             size = 3) +
  coord_map() +      #for map projections
  theme(legend.position = "none")

max(world.cities$cluster)   #unique cities in that cluster 

tibble(
  num_core = sum(world.cities$type == 1),
  num_border = sum(world.cities$type == 0),
  num_noise = sum(world.cities$type == -1)
)

cluster_sizes <- world.cities %>% filter(cluster > 0) %>%
  count(cluster, sort = T) %>% ungroup()
cluster_sizes %>% slice(1)


world.cities %>% filter(cluster == cluster_sizes$cluster[5]) %$%
  table(.$Country)

world.cities %>% filter(cluster > 0) %>% count(Country, sort = T)
print("d")
world.cities %>% filter(AccentCity %in% c("Abu Dhabi", "Rajpur"))
print("e")
world.cities %>% filter(Country == "jp", cluster > 0)


cluster_sizes <- world.cities %>% filter(cluster > 0) %>%
  count(cluster, sort = T) %>% ungroup()
cluster_sizes %>% slice(1)

#---------TODO----------------------------------------------
world.cities %>% filter(cluster == cluster_sizes$cluster[1]) %$%
  table(.$Country)


world.cities %>% filter(cluster > 0) %>% count(Country, sort = T)

print("f")

bochum_coord <- world.cities %>% filter(AccentCity == "Bochum") %$% c(.$Latitude[1], .$Longitude[1])
bochum_cluster <- world.cities %>% filter(AccentCity == "Bochum") %$% .$cluster[1]
world.cities %>%
  mutate(dist_bochum = sqrt((Latitude-bochum_coord[1])^2 + (Longitude-bochum_coord[2])^2)) %>%
  filter(dist_bochum > eps, cluster == bochum_cluster)