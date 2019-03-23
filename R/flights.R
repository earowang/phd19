## ---- load-flights
library(lubridate)
library(tidyverse)
library(tsibble)
library(forcats)

flights <- read_rds("~/Research/paper-tsibble/data/flights.rds") %>% 
  print(width = 40)

## ---- try-tsibble
flights_ts <- flights %>% 
  as_tsibble(
    key = id(flight_num), 
    index = sched_dep_datetime,
    regular = FALSE
  )

## ---- find-duplicate
flights %>% 
  duplicates(key = id(flight_num), index = sched_dep_datetime)

## ---- find-duplicate-lgl
dup_lgl <- are_duplicated(flights, key = id(flight_num), 
  index = sched_dep_datetime, from_last = TRUE)

## ---- tsibble
flights_ts <- flights %>% 
  filter(!dup_lgl) %>% 
  as_tsibble(
    key = id(flight_num), index = sched_dep_datetime, regular = FALSE
  )

## ---- sel-flights
sel_delay <- flights_ts %>% 
  filter(origin %in% c("JFK", "KOA", "LAX")) %>% 
  group_by(origin) %>% 
  index_by(sched_dep_date = as_date(sched_dep_datetime)) %>% 
  summarise(pct_delay = sum(dep_delay > 15) / n())

## ---- sel-flights-plot
sel_delay %>% 
  ggplot(aes(x = sched_dep_date, y = pct_delay)) +
  geom_line() +
  facet_grid(origin ~ .) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  xlab("Date") +
  ylab("Departure delay")
