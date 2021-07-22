# rSSOAP

## Overview
Analysis using the [EPA SSOAP](https://www.epa.gov/water-research/sanitary-sewer-overflow-analysis-and-planning-ssoap-toolbox) toolbox typically relies on weeks or months of flow meter and rain gauge data at timesteps ranging from 15-min to 1-hr. At this resolution, data collection can be costly, time-consuming, and contain noisy measurement errors.  Instead, rSSOAP relies on long-term (1 or 2 years) of daily flow and rain gauge data.

## Installation
```
library(devtools)
devtools::install_github("dCraigJones/rSSOAP")
```

<img src="fig/hourly_summary.jpg" alt="drawing" style="width:600px;align:middle;"/>

## Background

Wastewater flow patterns are a complex phenomenon that are often sub-divided into Dry-Weather Flow and Wet-Weather Flow.  **Dry-Weather Flow (DWF)** is the combination of **Base Sewer Flow (BSF)** and **Ground Water Intrustion (GWI)**.  BSF is the collected wastewater from customers that follows a predictable daily pattern in aggregate, called a diurnal.  GWI is the relatively constant flow from leaky infrasturcture during periods of high groundwater table.  **Wet-Weather Flow (WWF)** is the addition of **Rainfall Derived Inflow (RDI)** to DWF.  RDI is increased flow following rain events, usually defined by an unit hydrograph.  The final component is an error term ($\epsilon$), which includes iid noise and allows for uncertainty during special events such as holidays, abnormal weather, and mechanical issues.

<img src="https://latex.codecogs.com/gif.latex?\text{flow}%20=%20\underbrace{\overbrace{BSF%20+%20GWI}^{DWF}%20+%20RDI}_{WWF}%20+%20\epsilon" alt="drawing" style="width:300px;align:middle;"/>

In general, BSF should correspond to a percentage of water consumption.  GWI is often measured in terms of upstream gravity pipelines, in terms of GPD/IDM (or Gallons/Day per Inch-Diameter Miles).  RDI is usually measured as a percentage of total runoff.

```
 --- Base Sewer Flow (kGPD) ----------------
 Weekday: 623.8 
 Weekend: 651.5 
 
 --- Peaking Factor ------------------------
           90%  95%  99% 
 Weekday: 1.98 2.05 2.32 
 Weekend: 1.87 1.94 2.09 
 
 --- Ground Water Infiltration (kGPD) ------
 90%: 543.2 ( 377.2 gpm ) 
 95%: 614.7 ( 426.9 gpm ) 
 99%: 721.2 ( 500.8 gpm ) 
 
 --- Rainfall Derived Inflow (GPM) ---------
 6-hour SCS Type-II Storm 
 MA:       93.1 
 5-YR:    114.3 
 25-YR:   176.4 
 100-YR:  247.3 
 
 Total Volume: 8.56 kGal/inch ( 17.64 acre ) 
 
 --- Peak Hourly Flow (GPM) ----------------
 DWF (95%): 1315.2 
 WWF (99%): 1680.5 (25-YR 6-HR)
```


## Usage
*work in progress*

  


## References

[1](https://www.researchgate.net/publication/287852048_Rainfall_Derived_Inflow_and_Infiltration_Modeling_Approaches) Mikalson, Daley & Guo, Yiping & J. Adams, Barry. (2012). Rainfall Derived Inflow and Infiltration Modeling Approaches. Journal of Water Management Modeling. 10.14796/JWMM.R245-08. 

[2](https://nepis.epa.gov/Adobe/PDF/P1008BBP.pdf) US EPA. (2007). Computer Tools for Sanitary Sewer System Capacity Analysis and Planning. Publication No. EPA/600/R-07/111.

[3](https://www3.epa.gov/region1/sso/pdfs/Guide4EstimatingInfiltrationInflow.pdf) US EPA. (2014). Guide for Estimating Infiltration and Inflow.

## Other Packages
This package is part of a water/wastewater planning toolset.  Other packages include:

- [n185](https://www.github.com/dCraigJones/n185/), for visual fire flow analysis.
- [pumpR](https://www.github.com/dCraigJones/pumpR/), for pumpstation hydraulic analysis.
- [rSSOAP](https://www.github.com/dCraigJones/rSSOAP/), for Inflow & Infiltration Analysis.
