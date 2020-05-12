.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")

load("G:/Financial Services/Corporate Planning/Planning Group/Craig/_PEC/180-59 Townsend Bv/2020-05-08 Historical Flow/data/monterey.RData")
load("G:/Financial Services/Corporate Planning/Planning Group/Craig/_PEC/180-59 Townsend Bv/2020-05-08 Historical Flow/data/tidy_rain.RData")

rain_ <- tidy_rain %>%
  filter(station=="BUCKMN") %>%
  mutate(date=date(datetime)) %>%
  group_by(date) %>%
  summarize(rain=sum(rainfall_in, na.rm=TRUE))

rain_[is.na(rain_)]=0

tmp <- export %>%
  filter(str_detect(location, "Mer")) %>%
  mutate(date=date(date_qtr)) %>%
  filter(date<mdy("01/01/2020")) %>%
  filter(date>mdy("08/06/2018")) %>%
  group_by(date) %>%
  summarize(df=sum(flow_gpm, na.rm=TRUE)) %>%
  left_join(rain_, by="date")
