
build_idf_curve <- function(datetime, value, start_date=ymd(today())-days(8), end_date=ymd(today())-days(1)) {
  data.frame(datetime, p=value) %>%
    mutate(datehour=round_date(datetime, "1h")) %>%
    mutate(date=date(datehour)) %>%

    # Get Rolling Sums
    mutate(h2=zoo::rollapply(p, 2, sum, by.column=FALSE, fill=NA, align="right")) %>%
    mutate(h3=zoo::rollapply(p, 3, sum, by.column=FALSE, fill=NA, align="right")) %>%
    mutate(h6=zoo::rollapply(p, 6, sum, by.column=FALSE, fill=NA, align="right")) %>%
    mutate(h12=zoo::rollapply(p, 12, sum, by.column=FALSE, fill=NA, align="right")) %>%
    mutate(h24=zoo::rollapply(p, 24, sum, by.column=FALSE, fill=NA, align="right")) %>%
    mutate(d2=zoo::rollapply(p, 24*2, sum, by.column=FALSE, fill=NA, align="right")) %>%
    mutate(d3=zoo::rollapply(p, 24*3, sum, by.column=FALSE, fill=NA, align="right")) %>%
    mutate(d4=zoo::rollapply(p, 24*4, sum, by.column=FALSE, fill=NA, align="right")) %>%
    mutate(d7=zoo::rollapply(p, 24*7, sum, by.column=FALSE, fill=NA, align="right")) %>%
    mutate(d10=zoo::rollapply(p, 24*10, sum, by.column=FALSE, fill=NA, align="right")) %>%
    mutate(d20=zoo::rollapply(p, 24*20, sum, by.column=FALSE, fill=NA, align="right")) %>%
    mutate(d30=zoo::rollapply(p, 24*30, sum, by.column=FALSE, fill=NA, align="right")) %>%

    # Filter for
    filter(date<end_date) %>%
    filter(date>=start_date) %>%

    summarize(
      h1=max(p, na.rm=TRUE)
      , h2=max(h2, na.rm=TRUE)
      , h3=max(h3, na.rm=TRUE)
      , h6=max(h6, na.rm=TRUE)
      , h12=max(h12, na.rm=TRUE)
      , h24=max(h24, na.rm=TRUE)
      , d2=max(d2, na.rm=TRUE)
      , d3=max(d3, na.rm=TRUE)
      , d4=max(d4, na.rm=TRUE)
      , d7=max(d7, na.rm=TRUE)
      , d10=max(d10, na.rm=TRUE)
      , d20=max(d20, na.rm=TRUE)
      , d30=max(d30, na.rm=TRUE)
    ) %>%

    # Reformat
    pivot_longer(everything()) %>%
    mutate(d=c(1,2,3,6,12,24,24*2,24*3, 24*4, 24*7, 24*10, 24*20, 24*30)) %>%
    dplyr::select(duration_hr=3, precipitation_in=2)
}


# -------------------------------------------------------------------------
graph_idf_curve <- function(idf_df) {
  atlas14 %>%
    ggplot(aes(x=duration_hr, y=precipitation_in, color=recurrence_yr)) +
    geom_line(lwd=1) +
    scale_x_log10(
      breaks=c(1,2,3,6,12,24,24*2,24*3, 24*4, 24*7, 24*10, 24*20, 24*30, 24*45, 24*60)
      , labels=c("1h", "2h", "3h", "6h", "12h", "1d", "2d", "3d", "4d", "7d", "10d", "20d", "30d", "", "60d")
    ) +
    scale_y_log10(breaks=c(1, 5, seq(10,60,by=10)), minor_breaks=c(0:10, seq(10,60,by=5))) +
    labs(x="Duration", y="Precipitation depth (in)") + #, title="Mindanao Rain Gauge (ADS RainAlert III)", subtitle="September 2024") +
    scale_color_discrete(name="Recurrence Interval (yrs)") +
    theme(legend.position="bottom") +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    geom_point(aes(color=NA), data=idf_df, color="black", size=2)
}


# -------------------------------------------------------------------------
# example

rain <- get_mesanet_rain(
      location="JAX"
    , start_date=mdy("01/1/2025")
    , end_date=mdy("1/01/2015")
    )

build_idf_curve(rain$datehour, rain$p, mdy("01/01/2015"), mdy("1/01/2025")) %>%
graph_idf_curve()

read_csv("f:/temp/mindanao.csv") %>%
  mutate(datehour=mdy_hm(DateTime)) -> raw

idf <- build_idf_curve(raw$datehour, raw$p, mdy("09/01/2024"), mdy("09/30/2024"))

graph_idf_curve(idf)
