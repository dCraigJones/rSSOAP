library(dplyr)
library(lubridate)
library(stringr)

load("././data/hourly_rain.RData")
load("././data/hourly_flow.RData")


rain_ <- hourly_rain %>%
  filter(station=="SOWEST")

tmp <- hourly_flow %>%
  filter(str_detect(location, "Tow")) %>%
  mutate(date=date(datehour)) %>%
  filter(date>mdy("9/1/2018")) %>%
  filter(date<mdy("01/01/2020")) %>%
  left_join(rain_, by=c("datehour"="datetime"))


date <- tmp$datehour
flow <- tmp$flow
rain <- tmp$rainfall_in

rmarkdown::render("R/md/Sample_Workflow_Hourly.Rmd"
   , params = list(
          date = tmp$datehour
        , flow = tmp$flow
        , rain = tmp$rainfall_in
        , gwi_pct=0.95
   )
)
