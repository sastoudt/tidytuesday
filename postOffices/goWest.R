setwd("~/Desktop/tidytuesday/data/2021/2021-04-13")
library(dplyr)
library(ggplot2)
library(gridExtra)
post_offices <- read.csv("post_offices.csv")

table(post_offices$coordinates)

## each year what was the furthest west post office established?
farWest <- post_offices %>%
  filter(coordinates == T) %>%
  filter(established > 200) %>%
  group_by(established) %>%
  summarise(goWest = min(longitude))

# https://stackoverflow.com/questions/24070714/extract-row-corresponding-to-minimum-value-of-a-variable-by-group
furthestWestPerYear <- post_offices %>%
  filter(coordinates == T) %>%
  filter(established > 200) %>%
  group_by(established) %>%
  slice(which.min(longitude))

g3 <- ggplot(farWest, aes(established, goWest)) +
  geom_point() +
  theme_minimal(base_size = 14) +
  ylab("minimum longitude") +
  ggtitle("Westward bound!")
## weird u shape


firstPerState <- post_offices %>%
  filter(coordinates == T) %>%
  filter(established > 200) %>%
  group_by(state) %>%
  slice(which.min(established))

## where in the state do these things happen?
library(maps)
all_states <- map_data("state")
p <- ggplot() +
  geom_polygon(data = all_states, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + geom_point(data = firstPerState, aes(x = longitude, y = latitude), col = "red", cex = 2, pch = 8) + geom_point() + theme_minimal(base_size = 14) + xlim(-130, -60) + ylim(25, 55) + ggtitle("Location of first post office established in each state")
p

longevity <- post_offices %>%
  filter(coordinates == T) %>%
  filter(established > 200) %>%
  group_by(established) %>%
  summarise(mLifespan = mean(duration, na.rm = T), propNA = sum(is.na(duration)) / n(), count = n()) %>%
  filter(mLifespan > 0)

g4 <- ggplot(longevity, aes(established, mLifespan)) +
  geom_point() +
  theme_minimal(base_size = 14) +
  ylab("average duration") +
  ggtitle("Longevity of post offices") ## getting less stable over time? censored!

g5 <- ggplot(longevity, aes(established, propNA)) +
  geom_point() +
  theme_minimal(base_size = 14) +
  ylab("proportion NA \n (still open)") +
  ggtitle("Watch out for that censoring!") ## getting less stable over time? censored!

g1 <- ggplot(longevity, aes(established, count)) +
  geom_point() +
  theme_minimal(base_size = 14) +
  ylab("number opened") +
  ggtitle("The rise of post offices...") +
  xlim(1700, 2000) ## getting less stable over time? censored!
g1


closings <- post_offices %>%
  filter(coordinates == T) %>%
  filter(established > 200) %>%
  filter(discontinued > 1000) %>%
  filter(discontinued < 2030) %>%
  group_by(discontinued) %>%
  summarise(count = n())

g2 <- ggplot(closings, aes(discontinued, count)) +
  geom_point() +
  theme_minimal(base_size = 14) +
  ylab("number closed") +
  ggtitle("The fall of post offices...") +
  xlim(1700, 2000)
g2

## storyline 
grid.arrange(g1, g2, ncol = 1)

p

g3

grid.arrange(g4, g5, ncol = 1)
