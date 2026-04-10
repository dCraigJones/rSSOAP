# rSSOAP Pipeline

rSSOAP decomposes raw sewer flow into three components:

> **Flow = BSF + GWI + RDI**
> - **BSF** — Base Sewer Flow (dry-weather diurnal pattern)
> - **GWI** — Groundwater Intrusion (slow groundwater baseline)
> - **RDI** — Rainfall-Derived Inflow/Infiltration (storm response)

---

## Hourly Pipeline (`export.R`)

```mermaid
graph TD
    input["datetime + flow + rain"]

    subgraph clean.R
        remove_outliers
    end

    subgraph isolate.R
        isolate_hourly_gwi
        isolate_hourly_dwf
        isolate_hourly_bsf
    end

    subgraph shape.R
        shape_hourly_hydrograph
    end

    subgraph model.R
        model_hydrograph
    end

    subgraph print.R
        print_data_table
    end

    input --> remove_outliers
    remove_outliers --> isolate_hourly_gwi
    remove_outliers --> isolate_hourly_dwf
    isolate_hourly_gwi --> isolate_hourly_bsf
    isolate_hourly_dwf --> isolate_hourly_bsf
    isolate_hourly_bsf --> shape_hourly_hydrograph
    shape_hourly_hydrograph --> model_hydrograph
    model_hydrograph --> print_data_table
```

---

## Daily Pipeline (`SSOAP.R`)

For daily resolution analysis, functions can be called individually:

```mermaid
graph TD
    input["date + flow + rain"]

    infer_daily_dwf
    infer_daily_wwf
    infer_daily_gwi
    infer_daily_rdii
    lag_rain
    model_hydrograph

    input --> infer_daily_dwf
    input --> infer_daily_wwf
    infer_daily_dwf --> infer_daily_wwf
    infer_daily_wwf --> infer_daily_gwi
    infer_daily_gwi --> infer_daily_rdii
    infer_daily_rdii --> lag_rain
    lag_rain --> model_hydrograph
```

---

## File Dependency Map

```mermaid
graph LR
    export.R --> clean.R
    export.R --> isolate.R
    export.R --> shape.R
    export.R --> model.R
    export.R --> print.R

    SSOAP.R --> model.R
    SSOAP.R --> util.R

    shape.R --> model.R

    print.R --> Draw.R

    idf.R --> def.R
    idf.R --> atlas14.R
```

---

## Supporting Files

| File | Role |
|---|---|
| `clean.R` | Outlier removal and baseline correction |
| `isolate.R` | Extracts BSF, GWI, RDI components from hourly flow |
| `shape.R` | Fits unit hydrograph using VAR impulse response |
| `model.R` | Convolves rainfall matrix with unit hydrograph |
| `print.R` | Summary tables, peaking factors, model fit validation |
| `Draw.R` | Visualization — pipeline QA, QQ plots, 4-panel summaries |
| `idf.R` | Builds intensity-duration-frequency curves from hourly rain |
| `def.R` | Unit conversion constants |
| `atlas14.R` | NOAA Atlas 14 reference precipitation frequency data |
| `util.R` | Shared utilities (`lag_rain`) |
