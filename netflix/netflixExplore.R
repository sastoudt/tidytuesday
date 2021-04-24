setwd("~/Desktop/tidytuesday/data/2021/2021-04-20")

library(dplyr)
library(sentimentr)
library(reactable)

data <- read.csv("netflix_titles.csv")


data$description <- as.character(data$description)
allSentiment <- sentiment(data$description)
allSentiment <- allSentiment %>%
  group_by(element_id) %>%
  summarise(avgSentiment = mean(sentiment))

data$id <- 1:nrow(data)

data <- merge(allSentiment, data, by.x = "element_id", by.y = "id")


data$listed_in <- as.character(data$listed_in)
test <- strsplit(data$listed_in, ",")

## just taking first in genre for simplicity
data$first_genre <- lapply(test, function(x) {
  x[1]
}) %>% unlist()

#unique(data$first_genre)

mostNegPos <- data %>%
  group_by(first_genre) %>%
  slice(which.min(avgSentiment), which.max(avgSentiment))

mostNegPos <- mostNegPos %>% mutate(avgSentiment = round(avgSentiment, 2))

#View(mostNegPos[, c("first_genre", "title", "description", "avgSentiment")])

reactable(mostNegPos[, c("first_genre", "title", "description", "avgSentiment")], striped = T)

# adapt code from https://glin.github.io/reactable/articles/examples.html#conditional-styling
reactable(mostNegPos[, c("first_genre", "title", "description", "avgSentiment")], columns = list(
  avgSentiment = colDef(
    style = function(value) {
      if (value > 0) {
        color <- "#FFA500	"
      } else if (value < 0) {
        color <- "#005A9C"
      } else {
        color <- "#777"
      }
      list(color = color, fontWeight = "bold")
    }
  )
))
