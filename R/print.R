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

  gwi_kGPD <- quantile(hf$gwi*1.44, probs=c(.9, .95, .99))

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
    gwi_90p <- as.numeric(gwi_kGPD[1])
    gwi_95p <- as.numeric(gwi_kGPD[2])
    gwi_99p <- as.numeric(gwi_kGPD[3])
    rdi_ma <- as.numeric(rdi_gpm[1,3])
    rdi_5yr <- as.numeric(rdi_gpm[2,3])
    rdi_25yr <- as.numeric(rdi_gpm[3,3])
    rdi_100yr <- as.numeric(rdi_gpm[4,3])

    dwf_95p <- max(bsf_wkday/1.44*pf_wkday_95p, bsf_wkend/1.44*pf_wkend_95p)+gwi_95p/1.44
    wwf_99p_25yr <- max(bsf_wkday/1.44*pf_wkday_99p, bsf_wkend/1.44*pf_wkend_99p)+gwi_99p/1.44+rdi_25yr

  cat(
  "--- Base Sewer Flow (kGPD) ---------------","\n",
  "Weekday: ", round(bsf_wkday, 1),"\n",
  "Weekend: ", round(bsf_wkend, 1),"\n",
  "\n",
  "--- Peaking Factor -----------------------","\n",
  "             90%     95%     99%","\n",
  "Weekday:   ", round(pf_wkday_90p, 2), "  ", round(pf_wkday_95p, 2), "  ", round(pf_wkday_99p, 2),"\n",
  "Weekend:   ", round(pf_wkend_90p, 2), "  ", round(pf_wkend_95p, 2), "  ", round(pf_wkend_99p, 2),"\n",
  "\n",
  "--- Ground Water Infiltration (kGPD) -----","\n",
  "90%: ", round(gwi_90p, 1), "(", round(gwi_90p/1.44, 1), "gpm )","\n",
  "95%: ", round(gwi_95p, 1), "(", round(gwi_95p/1.44, 1), "gpm )","\n",
  "99%: ", round(gwi_99p, 1), "(", round(gwi_99p/1.44, 1), "gpm )","\n",
  "\n",
  "--- Rainfall Derived Inflow (GPM) --------","\n",
  "      6-hour SCS Type-II Storm","\n",
  "MA:	 ", round(rdi_ma, 1),"\n",
  "5-YR:	 ", round(rdi_5yr, 1),"\n",
  "25-YR:  ", round(rdi_25yr, 1),"\n",
  "100-YR: ", round(rdi_100yr, 1),"\n",
  "\n",
  "--- Peak Hourly Flow (GPM) ---------------","\n",
  "DWF (95%): ", round(dwf_95p, 1), "\n",
  "WWF (99%): ", round(wwf_99p_25yr, 1), "(25-YR 6-HR)")
}
