#### Set-up ####

setwd("~/Desktop/tidytuesday/data/2021/2021-07-13")

data <- read.csv("scoobydoo.csv")
names(data)

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggalluvial)

#### Wrangling ####

str(data)

for (i in 1:ncol(data)) {
  if (class(data[, i]) == "factor") {
    data[, i] <- as.character(data[, i])
  }
}

str(data)

for (i in 1:ncol(data)) {
  if (data[1, i] %in% c("TRUE", "FALSE")) {
    data[, i] <- as.logical(data[, i])
  }
}

str(data)

for (i in 1:ncol(data)) {
  if (data[1, i] %in% c("0", "1", "2")) {
    data[, i] <- as.numeric(data[, i])
  }
}




data$date_aired <- as.Date(as.character(data$date_aired))
data$year <- year(data$date_aired)


byYear <- data %>%
  group_by(year) %>%
  summarise(
    caught_velma = sum(caught_velma, na.rm = T),
    caught_shaggy = sum(caught_shaggy, na.rm = T),
    caught_fred = sum(caught_fred, na.rm = T),
    caught_daphnie = sum(caught_daphnie, na.rm = T),
    caught_scooby = sum(caught_scooby, na.rm = T),
    split_up = sum(split_up, na.rm = T),
    groovy = sum(groovy, na.rm = T),
    jinkies = sum(jinkies, na.rm = T),
    rooby_rooby_roo = sum(rooby_rooby_roo, na.rm = T),

    zoinks = sum(zoinks, na.rm = T),
    scooby_doo_where_are_you = sum(scooby_doo_where_are_you, na.rm = T),
    jeepers = sum(jeepers, na.rm = T),
    just_about_wrapped_up = sum(just_about_wrapped_up, na.rm = T),
    my_glasses = sum(my_glasses, na.rm = T),
    unmask_velma = sum(unmask_velma, na.rm = T),
    unmask_shaggy = sum(unmask_shaggy, na.rm = T),
    unmask_fred = sum(unmask_fred, na.rm = T),
    unmask_daphnie = sum(unmask_daphnie, na.rm = T),
    unmask_scooby = sum(unmask_scooby, na.rm = T),
    captured_velma = sum(captured_velma, na.rm = T),
    captured_shaggy = sum(captured_shaggy, na.rm = T),
    captured_fred = sum(captured_fred, na.rm = T),
    captured_daphnie = sum(captured_daphnie, na.rm = T),
    captured_scooby = sum(captured_scooby, na.rm = T),
    total = n()
  )

catch_phrases <- byYear[, c(1, which(names(byYear) == "groovy"):which(names(byYear) == "my_glasses"), ncol(byYear))]
captured <- byYear[, c(1, which(names(byYear) == "captured_velma"):which(names(byYear) == "captured_scooby"), ncol(byYear))]
unmask <- byYear[, c(1, which(names(byYear) == "unmask_velma"):which(names(byYear) == "unmask_scooby"), ncol(byYear))]
caught <- byYear[, c(1, which(names(byYear) == "caught_velma"):which(names(byYear) == "caught_scooby"), ncol(byYear))]

catch_phrases <- catch_phrases %>% pivot_longer(names(catch_phrases)[2:(ncol(catch_phrases) - 1)])
captured <- captured %>% pivot_longer(names(captured)[2:(ncol(captured) - 1)])
unmask <- unmask %>% pivot_longer(names(unmask)[2:(ncol(unmask) - 1)])
caught <- caught %>% pivot_longer(names(caught)[2:(ncol(caught) - 1)])

#### EDA ####

ggplot(catch_phrases, aes(year, value / total, col = name)) +
  geom_point() +
  geom_line()



ggplot(captured, aes(year, value / total, col = name)) +
  geom_point() +
  geom_line()
ggplot(captured, aes(year, value / total)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name, ncol = 1)

ggplot(unmask, aes(year, value / total, col = name)) +
  geom_point() +
  geom_line()
ggplot(unmask, aes(year, value / total)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name, ncol = 1)


ggplot(caught, aes(year, value / total, col = name)) +
  geom_point() +
  geom_line()
ggplot(caught, aes(year, value / total)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name, ncol = 1)

ggplot(byYear, aes(year, split_up / total)) +
  geom_point() +
  geom_line()

#### Tidying for Alluvial Plot ####

capturedAll <- pivot_longer(data[, c(1, which(names(data) == "captured_fred"):which(names(data) == "captured_scooby"))], -index)
unmaskAll <- pivot_longer(data[, c(1, which(names(data) == "unmask_fred"):which(names(data) == "unmask_scooby"))], -index)
caughtAll <- pivot_longer(data[, c(1, which(names(data) == "caught_fred"):which(names(data) == "caught_scooby"))], -index)

capturedAll <- capturedAll %>% filter(value == T)
unmaskAll <- unmaskAll %>% filter(value == T)
caughtAll <- caughtAll %>% filter(value == T)

join1 <- merge(caughtAll, unmaskAll, by.x = "index", by.y = "index", all.x = T, all.y = T)
join2 <- merge(join1, capturedAll, by.x = "index", by.y = "index", all.x = T, all.y = T)

names(join2)
join3 <- join2[, c("index", "name.x", "name.y", "name")]

names(join3)[c(2, 3, 4)] <- c("caught", "unmask", "captured")

join3$caught <- gsub(pattern = "caught_", replacement = "", join3$caught)
join3$unmask <- gsub(pattern = "unmask_", replacement = "", join3$unmask)
join3$captured <- gsub(pattern = "captured_", replacement = "", join3$captured)

join4 <- join3[complete.cases(join3), ]

toPlot <- join4 %>%
  group_by(caught, unmask, captured) %>%
  count()



is_alluvia_form(toPlot,
  weight = "n"
)

ggplot(toPlot, aes(y = n, axis1 = captured, axis2 = caught, axis3 = unmask)) +
  geom_alluvium(aes(fill = unmask), widt = 0, kno.pos = 0, reverse = F) +
  guides(fill = F) +
  geom_stratum(width = 1 / 5, reverse = F) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), reverse = F) +
  scale_x_continuous(breaks = 1:3, labels = c("Captured", "Caught", "Unmask")) +
  theme_minimal()

#### Finding - The Real Teams ####

toPlot2 <- toPlot %>%
  group_by(caught, unmask) %>%
  summarise(n = sum(n))

ggplot(toPlot, aes(y = n, axis1 = caught, axis2 = unmask)) +
  geom_alluvium(aes(fill = caught), widt = 0, kno.pos = 0, reverse = F) +
  guides(fill = F) +
  geom_stratum(width = 1 / 5, reverse = F) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), reverse = F) +
  scale_x_continuous(breaks = 1:2, labels = c("Caught", "Unmask")) +
  theme_minimal()


#### Finding - Groovy is a Fake Throwback ####

ggplot(catch_phrases, aes(year, value / total)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name, ncol = 1) +
  theme_minimal() ## throwback to what?

#### Finding - Unique Motive ####

unique(data$motive)
data[which(data$motive == "Loneliness"), ]
## outlier is the story
## https://scoobydoo.fandom.com/wiki/Adolf
