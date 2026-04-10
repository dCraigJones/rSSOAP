# move to constants folder
SCS_6_hr <- c(0.042,0.068,0.298,0.466,0.076,0.050,rep(0,72-6))

SCS_6h_TypeII <- c(4.70266904418216E-02,7.20613625428095E-02,0.480965724151595,0.279895842178257,7.15518949364581E-02,4.84984857490588E-02,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
SCS_12h_TypeII <- c(2.25921521997622E-02,2.49702734839477E-02,3.21046373365041E-02,4.04280618311534E-02,6.42092746730083E-02,0.508917954815696,0.129607609988109,5.70749108204518E-02,3.98335315101072E-02,3.15101070154578E-02,2.58620689655172E-02,2.28894173602854E-02,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
SCS_18h_TypeII <- c(1.41885137531844E-02,1.55213741360592E-02,0.017628153450926,1.97779282620145E-02,2.19277030731031E-02,2.61842571990584E-02,3.42566616146959E-02,4.88858792041534E-02,0.14958133135554,0.426730300001075,6.08816226500274E-02,3.89001752066471E-02,3.07417797985661E-02,2.43999441058549E-02,2.14977481108855E-02,1.88105295970247E-02,0.016123311083164,0.01396278739802,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
SCS_24h_TypeII <- c(0.0105,0.0115,0.0125,0.0135,0.015,0.017,0.019,0.021,0.027,0.034,0.054,0.428,0.109,4.79999999999999E-02,3.35000000000001E-02,0.0265,0.02175,0.01925,1.67499999999999E-02,0.01425,0.01275,0.01225,0.01175,0.01125,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)


# Point Precipitation Frequency (PF) Estimates
# NOAA Atlas 14
# 1 Precipitation frequency (PF) estimates in this table are based on frequency analysis of partial duration series (PDS).
# Location is centroid of Jacksonville, FL.
pf_ma <- 3.99 #2.99
pf_5yr <- 4.90 #4.14
pf_25yr <- 7.56 #6.01
pf_100yr <- 10.6 #8.10
pf_500yr <- 15.3 #11.1

# Conversion Factors
kgpd_to_gpm <- 1000/1440
gpm_to_kgpd <- 1440/1000

# Storm Definitions
pf_ma <- 3.99
pf_5yr <- 4.90
pf_25yr <- 7.56
pf_100yr <- 10.6
pf_500yr <- 15.3

source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/rSSOAP/R/def.R")

# combine print_summary and print_data_table calculation fields

print_summary <- function(hf, diurnal, uh) {
  bwf_kGPD <- diurnal %>%
    mutate(day=floor(wday)) %>%
    group_by(day) %>%
    summarize(adf_gpm=mean(dwf)) %>%
    mutate(isWkDay=ifelse(day==1|day==7, "Weekend", "Weekday")) %>%
    group_by(isWkDay) %>%
    summarize(adf_kGPD=max(adf_gpm)*gpm_to_kgpd)

  pwf_gpm <- hf %>%
    mutate(dwf=scrub-gwi-rdi) %>%
    mutate(wday=wday(datetime)+hour(datetime)/24) %>%
    group_by(wday) %>%
    summarize(p90=quantile(dwf, probs=0.9, na.rm=TRUE)
              , p95=quantile(dwf, probs=0.95, na.rm=TRUE)
              , p99=quantile(dwf, probs=0.99, na.rm=TRUE)
    ) %>%
    mutate(day=floor(wday)) %>%
    mutate(isWkDay=ifelse(day==1|day==7, "Weekend", "Weekday")) %>%
    group_by(isWkDay) %>%
    summarize(phf90=max(p90)
              , phf95=max(p95)
              , phf99=max(p99)
    )

  peak_factor <- full_join(bwf_kGPD, pwf_gpm, by="isWkDay") %>%
    mutate(adf_gpm=adf_kGPD*kgpd_to_gpm) %>%
    mutate(pf90 = phf90/adf_gpm) %>%
    mutate(pf95 = phf95/adf_gpm) %>%
    mutate(pf99 = phf99/adf_gpm) %>%
    dplyr::select(isWkDay, pf90, pf95, pf99)

  pmin_gpm <- hf %>%
    mutate(dwf=scrub-gwi-rdi) %>%
    filter(dwf>0) %>%
    mutate(wday=wday(datetime)+hour(datetime)/24) %>%
    group_by(wday) %>%
    summarize(p10=quantile(dwf, probs=0.1, na.rm=TRUE)
              , p5=quantile(dwf, probs=0.05, na.rm=TRUE)
              , p1=quantile(dwf, probs=0.01, na.rm=TRUE)
    ) %>%
    mutate(day=floor(wday)) %>%
    mutate(isWkDay=ifelse(day==1|day==7, "Weekend", "Weekday")) %>%
    group_by(isWkDay) %>%
    summarize(mhf10=quantile(p10, probs=0.10, na.rm=TRUE)
              , mhf5=quantile(p5, probs=0.05, na.rm=TRUE)
              , mhf1=quantile(p1, probs=0.01, na.rm=TRUE)
    )



  minimum_flow <- full_join(bwf_kGPD, pmin_gpm, by="isWkDay") %>%
    mutate(adf_gpm=adf_kGPD*kgpd_to_gpm) %>%
    mutate(mf10 = mhf10/adf_gpm) %>%
    mutate(mf5 = mhf5/adf_gpm) %>%
    mutate(mf1 = mhf1/adf_gpm) %>%
    dplyr::select(isWkDay, mf10, mf5, mf1)



  gwi_kGPD <- quantile(hf$gwi*gpm_to_kgpd, probs=c(.10, 0.5, .95, .99), na.rm=TRUE)



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
    gwi_50p <- as.numeric(gwi_kGPD[2])
    gwi_95p <- as.numeric(gwi_kGPD[3])
    gwi_99p <- as.numeric(gwi_kGPD[4])
    rdi_ma <- as.numeric(rdi_gpm[1,3])
    rdi_5yr <- as.numeric(rdi_gpm[2,3])
    rdi_25yr <- as.numeric(rdi_gpm[3,3])
    rdi_100yr <- as.numeric(rdi_gpm[4,3])

    min_5p <- min(bsf_wkday*kgpd_to_gpm*mf_wkday_5p, bsf_wkend*kgpd_to_gpm*mf_wkend_5p)+gwi_5p*kgpd_to_gpm
    dwf_95p <- max(bsf_wkday*kgpd_to_gpm*pf_wkday_95p, bsf_wkend*kgpd_to_gpm*pf_wkend_95p)+gwi_95p*kgpd_to_gpm
    dwf_50p <- max(bsf_wkday*kgpd_to_gpm*pf_wkday_95p, bsf_wkend*kgpd_to_gpm*pf_wkend_95p)+gwi_50p*kgpd_to_gpm
    wwf_95p_5yr <- max(bsf_wkday*kgpd_to_gpm*pf_wkday_95p, bsf_wkend*kgpd_to_gpm*pf_wkend_95p)+gwi_95p*kgpd_to_gpm+rdi_5yr
    wwf_99p_25yr <- max(bsf_wkday*kgpd_to_gpm*pf_wkday_99p, bsf_wkend*kgpd_to_gpm*pf_wkend_99p)+gwi_99p*kgpd_to_gpm+rdi_25yr
    volume_inflow <- mean(uh[1:15])*1440

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
  "50%: ", format(round(gwi_50p, 1), nsmall=1), "(", format(round(gwi_5p*kgpd_to_gpm, 1), nsmall=1), "gpm )","\n",
  "95%: ", format(round(gwi_95p, 1), nsmall=1), "(", format(round(gwi_95p*kgpd_to_gpm, 1), nsmall=1), "gpm )","\n",
  "99%: ", format(round(gwi_99p, 1), nsmall=1), "(", format(round(gwi_99p*kgpd_to_gpm, 1), nsmall=1), "gpm )","\n",
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
  "DWF (50%): ", prettyNum(round(dwf_50p, 1), big.mark = ","), "\n",
  "WWF (95%): ", prettyNum(round(wwf_95p_5yr, 1), big.mark = ","), "(5-YR 6-HR)")
}


print_data_table <- function(hf, diurnal, uh) {
  bwf_kGPD <- diurnal %>%
    mutate(day=floor(wday)) %>%
    group_by(day) %>%
    summarize(adf_gpm=mean(dwf)) %>%
    mutate(isWkDay=ifelse(day==1|day==7, "Weekend", "Weekday")) %>%
    group_by(isWkDay) %>%
    summarize(adf_kGPD=max(adf_gpm)*gpm_to_kgpd)

  pwf_gpm <- hf %>%
    mutate(dwf=scrub-gwi-rdi) %>%
    mutate(wday=wday(datetime)+hour(datetime)/24) %>%
    group_by(wday) %>%
    summarize(p90=quantile(dwf, probs=0.9, na.rm=TRUE)
              , p95=quantile(dwf, probs=0.95, na.rm=TRUE)
              , p99=quantile(dwf, probs=0.99, na.rm=TRUE)
    ) %>%
    mutate(day=floor(wday)) %>%
    mutate(isWkDay=ifelse(day==1|day==7, "Weekend", "Weekday")) %>%
    group_by(isWkDay) %>%
    summarize(phf90=max(p90)
              , phf95=max(p95)
              , phf99=max(p99)
    )

  peak_factor <- full_join(bwf_kGPD, pwf_gpm, by="isWkDay") %>%
    mutate(adf_gpm=adf_kGPD*kgpd_to_gpm) %>%
    mutate(pf90 = phf90/adf_gpm) %>%
    mutate(pf95 = phf95/adf_gpm) %>%
    mutate(pf99 = phf99/adf_gpm) %>%
    dplyr::select(isWkDay, pf90, pf95, pf99)

  pmin_gpm <- hf %>%
    mutate(dwf=scrub-gwi-rdi) %>%
    filter(dwf>0) %>%
    mutate(wday=wday(datetime)+hour(datetime)/24) %>%
    group_by(wday) %>%
    summarize(p10=quantile(dwf, probs=0.1, na.rm=TRUE)
              , p5=quantile(dwf, probs=0.05, na.rm=TRUE)
              , p1=quantile(dwf, probs=0.01, na.rm=TRUE)
    ) %>%
    mutate(day=floor(wday)) %>%
    mutate(isWkDay=ifelse(day==1|day==7, "Weekend", "Weekday")) %>%
    group_by(isWkDay) %>%
    summarize(mhf10=quantile(p10, probs=0.10, na.rm=TRUE)
              , mhf5=quantile(p5, probs=0.05, na.rm=TRUE)
              , mhf1=quantile(p1, probs=0.01, na.rm=TRUE)
    )

  minimum_flow <- full_join(bwf_kGPD, pmin_gpm, by="isWkDay") %>%
    mutate(adf_gpm=adf_kGPD*kgpd_to_gpm) %>%
    mutate(mf10 = mhf10/adf_gpm) %>%
    mutate(mf5 = mhf5/adf_gpm) %>%
    mutate(mf1 = mhf1/adf_gpm) %>%
    dplyr::select(isWkDay, mf10, mf5, mf1)

  gwi_kGPD <- quantile(hf$gwi*gpm_to_kgpd, probs=c(.10, 0.5, .95, .99), na.rm=TRUE)

  rdi_gpm <- data.frame(
    return=c("MA", "5-YR", "25-YR", "100-YR", "500-YR")
    , rain=c(pf_ma, pf_5yr, pf_25yr, pf_100yr, pf_500yr)
    , flow_gpm=rep(NA,5))

  for (i in seq_along(rdi_gpm$rain)) {
    rdi_gpm$flow_gpm[i] <- max(model_hydrograph(SCS_6_hr*rdi_gpm$rain[i], uh), na.rm=TRUE)
  }

  pct_passing = print_error(hf$scrub, hf$model)

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
  gwi_50p <- as.numeric(gwi_kGPD[2])
  gwi_95p <- as.numeric(gwi_kGPD[3])
  gwi_99p <- as.numeric(gwi_kGPD[4])
  rdi_ma <- as.numeric(rdi_gpm[1,3])
  rdi_5yr <- as.numeric(rdi_gpm[2,3])
  rdi_25yr <- as.numeric(rdi_gpm[3,3])
  rdi_100yr <- as.numeric(rdi_gpm[4,3])

  min_5p <- min(bsf_wkday*kgpd_to_gpm*mf_wkday_5p, bsf_wkend*kgpd_to_gpm*mf_wkend_5p)+gwi_5p*kgpd_to_gpm
  dwf_95p <- max(bsf_wkday*kgpd_to_gpm*pf_wkday_95p, bsf_wkend*kgpd_to_gpm*pf_wkend_95p)+gwi_95p*kgpd_to_gpm
  wwf_99p_25yr <- max(bsf_wkday*kgpd_to_gpm*pf_wkday_99p, bsf_wkend*kgpd_to_gpm*pf_wkend_99p)+gwi_99p*kgpd_to_gpm+rdi_25yr
  volume_inflow <- mean(uh)*1440

  export <- data.frame(  bsf_wkday_gpd = bsf_wkday
             , bsf_wkend_gpd = bsf_wkend
             , pf_wkday_95p
             , pf_wkend_95p
             , gwi_50p_gpm = gwi_50p*kgpd_to_gpm
             , gwi_95p_gpm = gwi_95p*kgpd_to_gpm
             , rdi_5yr_gpm = rdi_5yr
             , rdi_25yr_gpm = rdi_25yr
             , rt_acres = round(volume_inflow*7.4805/43560*12, 2)
             , pct_passing
           )

  return(export)

}

print_error <- function(field, model, error=0.10, minVal=2) {
  fx <- quantile(field, probs=(2:99/100), na.rm=TRUE)
  my <- quantile(model, probs=(2:99/100), na.rm=TRUE)

  res <- as.numeric(abs(my-fx))
  mpe <- as.numeric(res/fx)

  pts_failed <- sum(res>minVal & mpe>error)

  return(100-pts_failed)

}
