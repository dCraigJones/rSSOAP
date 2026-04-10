theme_set(theme_bw())
library(zoo)

date <- tmp$datehour
flow <- tmp$McCoy*460/60
rain <- tmp$rain

rmarkdown::render("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/rSSOAP/R/hourly_html.Rmd"
                  , params = list(
                    date = date
                    , flow = flow
                    , rain = rain
                    , gwi_pct=0.8
                    , qq_error=0.1
                    , qq_offset=5
                    , minimum_storm = 0.1
                    , init_abstraction=0
                  )
                  , output_dir=getwd()
                  , output_file="McCoy.html"
)

# NOTES:
# can gwi_pct and init_abstraction vary from say seq(0.5, 1, 0.1) and seq(0,1,0.1) respectively,
# then use best (i.e. least RMSE) per month?

hf %>%
  mutate(error=(model-scrub)/scrub) %>%
  mutate(date=date(datetime)) %>%
  group_by(date) %>%
  summarize(e=mean(error)*100) %>%
  #filter(date>mdy("01/01/2020")) %>%
  #filter(date<mdy("06/01/2020")) %>%
  ggplot(aes(date, e)) +
  geom_line() +
  labs(x="", y="mean percent error (%)")

prev <- uh


hf %>%
  mutate(date=date(datetime)) %>%
  group_by(date) %>%
  summarize(m=mean(model), f=mean(scrub), r=sum(rain)) %>%
  filter(r>0.5) %>%
  ggplot(aes(x=f, y=m)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept=0,slope=1, lwd=1, lty=2) +
  labs(x="observed peak flow (gpm)", y="simulated peak flow (gpm)")


hf %>%
  #mutate(date=date(datetime)) %>%
  #group_by(date) %>%
  #summarize(r=sum(rdi), p=sum(rain)) %>%
  mutate(cr=cumsum(rdi), cp=cumsum(rain)) %>%
  ggplot(aes(x=cp, y=cr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x="cumulative rainfall (inches/hr)", y="cumulative rdi (gallons/hr)")

hf %>%
  mutate(cg=rollapply(gwi, 15*24, sum, fill=NA, align="center", na.rm=TRUE)) %>%
  mutate(cr=rollapply(rain, 15*24, sum, fill=NA, align="center", na.rm=TRUE)) %>%
  mutate(month=month(datetime)) %>%
  group_by(month) %>%
  summarize(r=mean(cr), g=mean(gwi)) %>%
  ggplot(aes(r, g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

hf %>%
  mutate(error=(model-scrub)/scrub) %>%
  mutate(date=date(datetime)) %>%
  group_by(date) %>%
  summarize(e=mean(error)*100) %>%
  ggplot(aes(date, e)) +
  geom_line() +
  labs(x="", y="mean percent error (%)")

hf %>%
  mutate(delta=(model-scrub)) %>%
  ggplot(aes(datetime, delta)) +
  geom_line()

hf %>%
  ggplot(aes(datetime, gwi)) +
  geom_line()
