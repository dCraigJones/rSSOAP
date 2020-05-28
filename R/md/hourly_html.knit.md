---
output: html_document
params:
  date: NA
  flow: NA
  rain: NA
  gwi_pct: 0.8
---
\pagenumbering{gobble}



```
## Registered S3 method overwritten by 'xts':
##   method     from
##   as.zoo.xts zoo
```

```
## Registered S3 method overwritten by 'quantmod':
##   method            from
##   as.zoo.data.frame zoo
```

<img src="hourly_html_files/figure-html/unnamed-chunk-2-1.png" width="672" />

```
##  --- Base Sewer Flow (kGPD) ----------------- 
##  Weekday:  248.9 
##  Weekend:  274 
##  
##  --- Peaking Factor ------------------------- 
##               min     95%     99% 
##  Weekday:    0.06    2.54    2.73 
##  Weekend:    0.04    2.31    2.62 
##  
##  --- Ground Water Infiltration (kGPD) ------- 
##   5%:  41.6 ( 28.9 gpm ) 
##  95%:  145.7 ( 101.2 gpm ) 
##  99%:  187.3 ( 130.1 gpm ) 
##  
##  --- Rainfall Derived Inflow (GPM) ---------- 
##        6-hour SCS Type-II Storm 
##      MA:  54.7 
##    5-YR:  67.1 
##   25-YR:  103.6 
##  
##  Total Volume: 5.06 kGal/inch ( 10.42 acre ) 
##  
##  --- Peak Hourly Flow (GPM) ----------------- 
##  min  (5%):  36.1 
##  DWF (95%):  541 
##  WWF (99%):  731.8 (25-YR 6-HR)
```
