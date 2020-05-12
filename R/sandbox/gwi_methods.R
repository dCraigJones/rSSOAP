# In general work from
# decomposition --> Hydrograph --> Trianglar Hydrograph


# Long-Term Max Value -----------------------------------------------------

plot(density(df$gwi))
quantile(df$gwi, probs=0.95)

# Long-Term Regression (by Month) DO NOT USE ------------------------------



df %>%
  mutate(monthdate=month(date)) %>%
  group_by(monthdate) %>%
  summarize(month_rain=sum(rain, na.rm=TRUE),
            month_gwi=sum(gwi),
            month_rate=month_gwi/month_rain) %>%
  ggplot(aes(month_rain, month_rate)) +
           geom_point()



# Long-Term Regression ----------------------------------------------------



GWI_DUR <- 30


a <- df %>%
  mutate(lag=rollapply(
      rain
    , GWI_DUR
    , sum
    , align="right"
    , na.rm=TRUE
    , fill=0)) %>%
  mutate(lag_gwi=rollapply(
      gwi
    , GWI_DUR
    , sum
    , align="right"
    , fill=0))

quantile(a$lag_gwi/a$lag, na.rm=TRUE, probs=0.95)/GWI_DUR

a %>%
  ggplot(aes(lag, lag_gwi/lag/GWI_DUR)) +
  geom_point()

plot(density(df$gwi))
abline(v=quantile(a$lag_gwi/a$lag, na.rm=TRUE, probs=0.95)/GWI_DUR)

quantile(df$gwi, probs=0.95)
quantile(a$lag_gwi/a$lag, na.rm=TRUE, probs=0.95)/GWI_DUR




# Long-Term Hydrograph ----------------------------------------------------



r <- df$rain
r[is.na(r)]=0

# r <- rollapply(
#   r
#   , 3
#   , sum
#   , align="right"
#   , fill=0)

# Translate to time series for VAR
ii <- ts(cbind(r, df$gwi))

# Estimate the model
var.1 <- VAR(ii, 2, type = "none")

# Calculate the IRF
ir.1 <- irf(var.1, impulse = "r", response = "X", n.ahead = 60, ortho = FALSE)

# Return upper limit
uh <- ir.1$Upper$r
#uh <- ir.1$r

PU <- lag_rain(r)
UH <- uh
U <- matrix(c(UH, rep(0,ncol(PU)-length(UH))), ncol=1)
Q.m <- PU%*%U

plot(df$date, df$gwi); lines(df$date, Q.m)


# start UH2 ---------------------------------------------------------------

tmp <- df %>% mutate(gwi_adj=gwi-Q.m)
uh <- infer_daily_hydrograph(tmp$date, tmp$df_adj-tmp$gwi_adj, df$rain)

PU <- lag_rain(r)
UH <- uh
U <- matrix(c(UH, rep(0,ncol(PU)-length(UH))), ncol=1)
Q.m <- PU%*%U

plot(tmp$date, tmp$df_adj, type="l")
lines(tmp$date, tmp$bsf+tmp$gwi_adj+Q.m, col="red")

# try to recalc BSF with reduced UH GWI
