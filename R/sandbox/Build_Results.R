library(dplyr)
library(lubridate)
library(stringr)

load("././data/hourly_rain.RData")
load("././data/hourly_flow.RData")


rain_ <- hourly_rain %>%
  filter(station=="BUCKMN")
  #filter(station=="SOWEST")

tmp <- hourly_flow %>%
  filter(str_detect(location, "Car")) %>%
  mutate(date=date(datehour)) %>%
  filter(date>mdy("10/1/2018")) %>%
  filter(date<mdy("01/01/2020")) %>%
  left_join(rain_, by=c("datehour"="datetime"))


date <- tmp$datehour
flow <- tmp$flow
rain <- tmp$rainfall_in

rmarkdown::render("R/md/hourly_html.Rmd"
   , params = list(
          date = tmp$datehour
        , flow = tmp$flow
        , rain = tmp$rainfall_in
        , gwi_pct=0.80
   )
)
