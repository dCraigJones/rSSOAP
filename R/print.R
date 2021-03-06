SCS_6_hr <- c(0.042,0.068,0.298,0.466,0.076,0.050,rep(0,72-6))

pf_ma <- 3.99
pf_5yr <- 4.90
pf_25yr <- 7.56
pf_100yr <- 10.6
pf_500yr <- 15.3





print_summary <- function(hf, diurnal, uh) {
  bwf_kGPD <- diurnal %>%
    mutate(day=floor(wday)) %>%
    group_by(day) %>%
    summarize(adf_gpm=mean(dwf)) %>%
    mutate(isWkDay=ifelse(day==1|day==7, "Weekend", "Weekday")) %>%
    group_by(isWkDay) %>%
    summarize(adf_kGPD=max(adf_gpm)*1.44)

  pwf_gpm <- hf %>%
    mutate(dwf=scrub-gwi-rdi) %>%
    mutate(wday=wday(datetime)+hour(datetime)/24) %>%
    group_by(wday) %>%
    summarize(p90=quantile(dwf, probs=0.9)
              , p95=quantile(dwf, probs=0.95)
              , p99=quantile(dwf, probs=0.99)
    ) %>%
    mutate(day=floor(wday)) %>%
    mutate(isWkDay=ifelse(day==1|day==7, "Weekend", "Weekday")) %>%
    group_by(isWkDay) %>%
    summarize(phf90=max(p90)
              , phf95=max(p95)
              , phf99=max(p99)
    )

  peak_factor <- full_join(bwf_kGPD, pwf_gpm, by="isWkDay") %>%
    mutate(adf_gpm=adf_kGPD*1e3/1440) %>%
    mutate(pf90 = phf90/adf_gpm) %>%
    mutate(pf95 = phf95/adf_gpm) %>%
    mutate(pf99 = phf99/adf_gpm) %>%
    dplyr::select(isWkDay, pf90, pf95, pf99)

  pmin_gpm <- hf %>%
    mutate(dwf=scrub-gwi-rdi) %>%
    filter(dwf>0) %>%
    mutate(wday=wday(datetime)+hour(datetime)/24) %>%
    group_by(wday) %>%
    summarize(p10=quantile(dwf, probs=0.1)
              , p5=quantile(dwf, probs=0.05)
              , p1=quantile(dwf, probs=0.01)
    ) %>%
    mutate(day=floor(wday)) %>%
    mutate(isWkDay=ifelse(day==1|day==7, "Weekend", "Weekday")) %>%
    group_by(isWkDay) %>%
    summarize(mhf10=quantile(p10, probs=0.10)
              , mhf5=quantile(p5, probs=0.05)
              , mhf1=quantile(p1, probs=0.01)
    )

  minimum_flow <- full_join(bwf_kGPD, pmin_gpm, by="isWkDay") %>%
    mutate(adf_gpm=adf_kGPD*1e3/1440) %>%
    mutate(mf10 = mhf10/adf_gpm) %>%
    mutate(mf5 = mhf5/adf_gpm) %>%
    mutate(mf1 = mhf1/adf_gpm) %>%
    dplyr::select(isWkDay, mf10, mf5, mf1)

  gwi_kGPD <- quantile(hf$gwi*1.44, probs=c(.10, .95, .99))

  rdi_gpm <- data.frame(
      return=c("MA", "5-YR", "25-YR", "100-YR", "500-YR")
    , rain=c(pf_ma, pf_5yr, pf_25yr, pf_100yr, pf_500yr)
    , flow_gpm=rep(NA,5))

  for (i in seq_along(rdi_gpm$rain)) {
    rdi_gpm$flow_gpm[i] <- max(model_hydrograph(SCS_6_hr*rdi_gpm$rain[i], uh), na.rm=TRUE)
  }

    bsf_wkday <- as.numeric(bwf_kGPD[1,2])
    bsf_wkend <- as.numeric(bwf_kGPD[2,2])
    pf_wkday_90p <- as.numeric(peak_factor[1,2])
    pf_wkday_95p <- as.numeric(peak_factor[1,3])
    pf_wkday_99p <- as.numeric(peak_factor[1,4])
    pf_wkend_90p <- as.numeric(peak_factor[2,2])
    pf_wkend_95p <- as.numeric(peak_factor[2,3])
    pf_wkend_99p <- as.numeric(peak_factor[2,4])
    mf_wkday_5p <- as.numeric(minimum_flow[1,2])
    mf_wkend_5p <- as.numeric(minimum_flow[2,2])
    gwi_5p <- as.numeric(gwi_kGPD[1])
    gwi_95p <- as.numeric(gwi_kGPD[2])
    gwi_99p <- as.numeric(gwi_kGPD[3])
    rdi_ma <- as.numeric(rdi_gpm[1,3])
    rdi_5yr <- as.numeric(rdi_gpm[2,3])
    rdi_25yr <- as.numeric(rdi_gpm[3,3])
    rdi_100yr <- as.numeric(rdi_gpm[4,3])

    min_5p <- min(bsf_wkday/1.44*mf_wkday_5p, bsf_wkend/1.44*mf_wkend_5p)+gwi_5p/1.44
    dwf_95p <- max(bsf_wkday/1.44*pf_wkday_95p, bsf_wkend/1.44*pf_wkend_95p)+gwi_95p/1.44
    wwf_99p_25yr <- max(bsf_wkday/1.44*pf_wkday_99p, bsf_wkend/1.44*pf_wkend_99p)+gwi_99p/1.44+rdi_25yr
    volume_inflow <- mean(uh)*1440

  cat(
  " --- Base Sewer Flow (kGPD) -----------------","\n",
  "Weekday: ", round(bsf_wkday, 1),"\n",
  "Weekend: ", round(bsf_wkend, 1),"\n",
  "\n",
  "--- Peaking Factor -------------------------","\n",
  "             min     95%     99%","\n",
  "Weekday:   ", round(mf_wkday_5p, 2), "  ", round(pf_wkday_95p, 2), "  ", round(pf_wkday_99p, 2),"\n",
  "Weekend:   ", round(mf_wkend_5p, 2), "  ", round(pf_wkend_95p, 2), "  ", round(pf_wkend_99p, 2),"\n",
  "\n",
  "--- Ground Water Infiltration (kGPD) -------","\n",
  " 5%: ", format(round(gwi_5p, 1), nsmall=1), "(", format(round(gwi_5p/1.44, 1), nsmall=1), "gpm )","\n",
  "95%: ", format(round(gwi_95p, 1), nsmall=1), "(", format(round(gwi_95p/1.44, 1), nsmall=1), "gpm )","\n",
  "99%: ", format(round(gwi_99p, 1), nsmall=1), "(", format(round(gwi_99p/1.44, 1), nsmall=1), "gpm )","\n",
  "\n",
  "--- Rainfall Derived Inflow (GPM) ----------","\n",
  "      6-hour SCS Type-II Storm","\n",
  "    MA: ", round(rdi_ma, 1),"\n",
  "  5-YR: ", round(rdi_5yr, 1),"\n",
  " 25-YR: ", round(rdi_25yr, 1),"\n",
  "\n",
  "Total Volume:", round(volume_inflow/1000, 2), "kGal/inch",  "(",round(volume_inflow*7.4805/43560*12, 2) ,"acre )", "\n",
  "\n",
  "--- Peak Hourly Flow (GPM) -----------------","\n",
  "min  (5%): ", prettyNum(round(min_5p, 1), big.mark = ","), "\n",
  "DWF (95%): ", prettyNum(round(dwf_95p, 1), big.mark = ","), "\n",
  "WWF (99%): ", prettyNum(round(wwf_99p_25yr, 1), big.mark = ","), "(25-YR 6-HR)")
}
