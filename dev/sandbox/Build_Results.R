.libPaths("G:/Financial Services/Corporate Planning/Hydraulic Model Files/RWD/4-0")

library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)

# load("././data/hourly_rain.RData")
# load("././data/hourly_flow.RData")


# rain_ <- hourly_rain %>%
#   filter(station=="BUCKMN")
#   #filter(station=="SOWEST")
#
# tmp <- hourly_flow %>%
#   filter(str_detect(location, "sta")) %>%
#   mutate(date=date(datehour)) %>%
#   filter(date>mdy("10/1/2018")) %>%
#   filter(date<mdy("01/01/2020")) %>%
#   left_join(rain_, by=c("datehour"="datetime"))

model <- read_csv("G:/Financial Services/Corporate Planning/Planning Group/Craig/_PEC/175-39S Walnut St/2020-03-10 Bypass Flow/scada/model.csv")

model %>%
  mutate(datetime=mdy_hm(Timestamp)) %>%
  mutate(datehour=round_date(datetime, "1h")) %>%
  group_by(datehour) %>%
  summarize(flow=mean(Boulevard)) -> tmp

rain <- read_csv("G:/Financial Services/Corporate Planning/Planning Group/Craig/_PEC/175-39S Walnut St/2020-03-10 Bypass Flow/scada/rain.csv")

rain %>%
  mutate(datetime=dmy_hms(timestamp)) %>%
  right_join(tmp, by=c("datetime"="datehour")) -> tmp


rmarkdown::render("R/templates/hourly_html.Rmd"
   , params = list(
          date = tmp$datetime
        , flow = tmp$flow*1.15
        , rain = tmp$`rain_in/hr`
        , gwi_pct=0.50
   )
)



model %>%
  mutate(datetime=mdy_hm(Timestamp)) %>%
  mutate(datehour=round_date(datetime, "1h")) %>%
  group_by(datehour) %>%
  summarize(flow=mean(`32nd St`)) -> tmp

rain <- read_csv("G:/Financial Services/Corporate Planning/Planning Group/Craig/_PEC/175-39S Walnut St/2020-03-10 Bypass Flow/scada/rain.csv")

rain %>%
  mutate(datetime=dmy_hms(timestamp)) %>%
  right_join(tmp, by=c("datetime"="datehour")) -> tmp




rmarkdown::render("R/templates/hourly_html.Rmd"
                  , params = list(
                    date = tmp$datetime
                    , flow = tmp$flow*1.2
                    , rain = tmp$`rain_in/hr`
                    , gwi_pct=0.40
                  )
)


model %>%
  mutate(datetime=mdy_hm(Timestamp)) %>%
  mutate(datehour=round_date(datetime, "1h")) %>%
  group_by(datehour) %>%
  summarize(flow=mean(Norwood)) -> tmp

rain <- read_csv("G:/Financial Services/Corporate Planning/Planning Group/Craig/_PEC/175-39S Walnut St/2020-03-10 Bypass Flow/scada/rain.csv")

rain %>%
  mutate(datetime=dmy_hms(timestamp)) %>%
  right_join(tmp, by=c("datetime"="datehour")) -> tmp


rmarkdown::render("R/templates/hourly_html.Rmd"
                  , params = list(
                    date = tmp$datetime
                    , flow = tmp$flow
                    , rain = tmp$`rain_in/hr`
                    , gwi_pct=0.80
                  )
)




model <- read_csv("G:/Financial Services/Corporate Planning/Planning Group/Craig/_PEC/175-39S Walnut St/2020-03-10 Bypass Flow/scada/mcm.csv")

model %>%
  #mutate(datetime=mdy_hm(Timestamp)) %>%
  mutate(datetime=dmy_hms(Timestamp)) %>%
  mutate(datehour=round_date(datetime, "1h")) %>%
  group_by(datehour) %>%
  summarize(flow=mean(McMillan)) -> tmp

rain <- read_csv("G:/Financial Services/Corporate Planning/Planning Group/Craig/_PEC/175-39S Walnut St/2020-03-10 Bypass Flow/scada/rain.csv")

rain %>%
  mutate(datetime=dmy_hms(timestamp)) %>%
  right_join(tmp, by=c("datetime"="datehour")) -> tmp


rmarkdown::render("R/templates/hourly_html.Rmd"
                  , params = list(
                    date = tmp$datetime
                    , flow = tmp$flow
                    , rain = tmp$`rain_in/hr`
                    , gwi_pct=0.25
                  )
)
