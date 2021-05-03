setwd("~/Desktop/tidytuesday/data/2021/2021-04-27")

library(dplyr)
library(ggplot2)
library(infer)

data <- read.csv("departures.csv")

## involuntarily removed?
which(data$ceo_dismissal == 1) %>% length() / nrow(data)

## uses middle name?
get_names <- lapply(data$exec_fullname, function(x) {
  str_split(x, " ")
})

num_names <- lapply(get_names, function(x) {
  length(unlist(x))
}) %>% unlist()

summary(num_names)

data[which.max(num_names), ] # lol

## no middle name or initial
which(num_names == 2) %>% length() / nrow(data)

data$numNames <- num_names

data$hasMiddle <- ifelse(data$numNames > 2, "Has Middle Name", "No Middle Name")
data$involuntaryRemoved <- ifelse(data$ceo_dismissal == 1, "Involuntarily Removed", "Not Involuntarily Removed")

## are there enough in each category to be ok?
table(data$hasMiddle, data$involuntaryRemoved)


## Null Hypothesis: there is no difference in the proportion of CEOs who are involuntarily removed between those who use a middle name or initial  v. those who don't

## Alternative Hypothesis: there is a difference in the proportion of CEOs who are involuntarily removed between those who use a middle name or initial  v. those who don't

obs_stat <- data %>%
  specify(involuntaryRemoved ~ hasMiddle, success = "Involuntarily Removed") %>%
  calculate(stat = "diff in props", order = c("Has Middle Name", "No Middle Name"))


null_distribution_2_sample_permute <- data %>%
  specify(involuntaryRemoved ~ hasMiddle, success = "Involuntarily Removed") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("Has Middle Name", "No Middle Name"))

null_distribution_2_sample_permute %>%
  visualize() +
  shade_p_value(obs_stat,
    direction = "two-sided"
  )

null_distribution_2_sample_permute %>%
  get_p_value(obs_stat,
    direction = "two-sided"
  )
## ah, fail to reject the null
