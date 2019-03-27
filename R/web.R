## ---- load-web
source("R/theme.R")
library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
theme_set(theme_remark())

web <- read_csv("data/website-ga.csv", skip = 5, n_max = 1100) %>% 
  mutate(`Day Index` = mdy(`Day Index`)) %>% 
  rename(Date = `Day Index`) %>% 
  print()

## ---- web-tsibble
web_ts <- web %>% 
  as_tsibble(
    key = id(), index = Date
  )

## ---- web-ggplot
web_ts %>% 
  ggplot(aes(x = Date, y = Users)) #<<

## ---- web-geom
web_ts %>% 
  ggplot(aes(x = Date, y = Users)) +
  geom_area(colour = "#3182bd", fill = "#9ecae1") #<<

## ---- web-tsibble-select
web_ts %>% select(Users)

## ---- web-dplyr-select
web %>% select(Users)

## ---- web-tsibble-summarise
web_ts %>% 
  summarise(Users = sum(Users))

## ---- web-dplyr-summarise
web %>% 
  summarise(Users = sum(Users))

## ---- web-year
web_year <- web_ts %>% 
  index_by(Year = year(Date)) %>% 
  summarise(Users = sum(Users))

## ---- web-year-bar
web_year %>% 
  ggplot(aes(x = Year, y = Users)) +
  geom_col()

## ---- web-fcast
# transform
web_mth <- web_ts %>% 
  index_by(YearMonth = yearmonth(Date)) %>% 
  summarise(Users = sum(Users))
# model and forecast
web_fcast <- web_mth %>% 
  model(ets = ETS(Users)) %>% 
  forecast(h = 9) %>% 
  mutate(type = "forecast")
# visualise
web_mth %>% 
  mutate(type = "data") %>% 
  rbind(web_fcast) %>% 
  group_by(type) %>% 
  index_by(Year = year(YearMonth)) %>% 
  summarise(Users = sum(Users)) %>% 
  mutate(type = fct_rev(type)) %>% 
  ggplot(aes(x = Year, y = Users)) +
  geom_col(aes(fill = type))

## ---- xkcd-man
dat <- web_mth %>% 
  mutate(type = "data") %>% 
  rbind(web_fcast) %>% 
  group_by(type) %>% 
  index_by(Year = year(YearMonth)) %>% 
  summarise(Users = sum(Users)) %>% 
  mutate(type = fct_rev(type))
library(xkcd)
library(gganimate)
xrange <- range(dat$Year)
yrange <- range(dat$Users)
ratioxy <- diff(xrange) / diff(yrange)

mapping <- aes(
  x = x,
  y = y,
  scale = scale,
  ratioxy = ratioxy,
  angleofspine = angleofspine,
  anglerighthumerus = anglerighthumerus,
  anglelefthumerus = anglelefthumerus,
  anglerightradius = anglerightradius,
  angleleftradius = angleleftradius,
  anglerightleg =  anglerightleg,
  angleleftleg = angleleftleg,
  angleofneck = angleofneck
)

id <- c(1, 1:2, 1:3, 1:4, 1:5)
grp_id <- c(1, rep(2, 2), rep(3, 3), rep(4, 4), rep(5, 5))
dataman <- data.frame(
  x = dat$Year,
  y = c(dat$Users[1:4], sum(dat$Users[4:5])) + 450,
  group = 1:5,
  scale = 200,
  ratioxy = ratioxy,
  angleofspine = -pi / 2,
  anglerighthumerus = -pi / 6,
  anglelefthumerus = pi + pi / 6,
  anglerightradius = 0,
  angleleftradius = -pi / 4,
  angleleftleg = 3 * pi / 2  + pi / 12 ,
  anglerightleg = 3 * pi / 2  - pi / 12,
  angleofneck = 3 * pi / 2 - pi / 10
  )
web_data <- dat[id, ] %>% 
  mutate(group = grp_id)
p_anim <- web_data %>% 
  ggplot(aes(x = Year, y = Users, group = group)) +
  geom_col(aes(fill = type)) +
  xkcdman(mapping, dataman) +
  theme_xkcd() +
  transition_manual(group)
anim_save("xkcd-bar.gif", p_anim, path = "img", nframes = 100, fps = 20, width = 1000, height = 800)

## ---- calendar-layout
library(sugrrants)
pedestrian %>%
  filter(
    Sensor_Name %in% c("Flagstaff Station", "Melbourne Convention Exhibition Centre"),
    Date < as.Date("2016-06-01"), Date >= as.Date("2016-04-01")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts, colour = Sensor_Name)) +
  facet_calendar(~ Date) +
  theme(axis.text = element_blank())

## ---- calendar-plot
pedestrian %>%
  filter(
    Sensor_Name %in% c("Flagstaff Station", "Melbourne Convention Exhibition Centre"),
    Date < as.Date("2016-06-01"), Date >= as.Date("2016-04-01")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts, colour = Sensor_Name)) +
  geom_line() +
  facet_calendar(~ Date) +
  theme(axis.text = element_blank())
