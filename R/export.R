source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/rSSOAP/R/clean.R")
source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/rSSOAP/R/shape.R")
source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/rSSOAP/R/isolate.R")
source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/rSSOAP/R/model.R")
source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/rSSOAP/R/draw.R")
source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/rSSOAP/R/util.R")
source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/rSSOAP/R/print.R")
source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/rSSOAP/R/def.R")


library(dplyr)
library(zoo)
library(lubridate)
library(vars)


export_data_frame <- function(datetime, flow, rain, gwi_pct=0.8) {

    hf <- data.frame(datetime, flow, rain)

    hf$scrub <- remove_outliers(hf$flow)

    hf$gwi <- isolate_hourly_gwi(hf$datetime, hf$scrub, hf$rain,  gwi_pct)

    diurnal <- isolate_hourly_dwf(hf$datetime, hf$scrub-hf$gwi, hf$rain)

    hf$bsf <- isolate_hourly_bsf(hf$datetime, hf$scrub-hf$gwi, hf$rain, diurnal)

    uh <- shape_hourly_hydrograph(
      hf$datetime
      , hf$scrub-hf$gwi-hf$bsf
      , hf$rain
      , 24
      , 0.1
      , 1)

    hf$rdi <- model_hydrograph(hf$rain, uh)

    hf$model <- hf$bsf+hf$gwi+hf$rdi

    #draw_ii(hf$datetime, hf$scrub, hf$gwi, hf$model, diurnal, uh, ,params$qq_error, params$qq_offset)

    return(print_data_table(hf, diurnal, uh))
}
