## ---- load-web
source("R/theme.R")
library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
theme_set(theme_remark())

web <- read_csv("data/website-ga.csv", skip = 5, n_max = 1096) %>% 
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

## ---- web-year
web_year <- web_ts %>% 
  index_by(Year = year(Date)) %>% 
  summarise(Users = sum(Users))

## ---- web-year-bar
web_year %>% 
  ggplot(aes(x = Year, y = Users)) +
  geom_col()

## ---- web-fcast
web_mth <- web_ts %>% 
  index_by(YearMonth = yearmonth(Date)) %>% 
  summarise(Users = sum(Users))

web_fcast <- web_mth %>% 
  model(ets = ETS(Users)) %>% 
  forecast(h = 9) %>% 
  as_tsibble() %>% 
  mutate(type = "forecast")

web_mth %>% 
  mutate(type = "data") %>% 
  rbind(web_fcast) %>% 
  mutate(type = fct_rev(type)) %>% 
  group_by(type) %>% 
  index_by(Year = year(YearMonth)) %>% 
  summarise(Users = sum(Users)) %>% 
  ggplot(aes(x = Year, y = Users, fill = type)) +
  geom_col()
