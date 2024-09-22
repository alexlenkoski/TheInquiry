## How does out of sample performance work if go by month?
rm(list = ls())

library(NordpoolDemand)
library(gam)
library(parallel)

feature_lag = 30
forecast_horizon = 15:21
dt_swm = load_swm_factor_loadings()
dt_swm[, year := year(date)]
dt_swm[, month := month(date)]
dt_swm[, day := mday(date)]

year_start = 1990
dates_run = dt_swm[year >=  year_start, unique(date)]
year_stop = dt_swm[, max(year)]

dt_pred = list()

dt_swm[, "nao1_past" := c(rep(NA, feature_lag * 24 - 1), zoo::rollmean(FL1_mslp, feature_lag * 24))]
run_bt = function(d,
                  dt_swm){
    if(d %% 200 == 0)print(d)
    date_d = dates_run[d]

    
    dt_train = copy(dt_swm[date <= date_d])
    dt_clim = dt_train[, .("FL_temp_climatology" = mean(FL1_temp)),
                       .(month, day, hour)]
    dt_y = merge(dt_train, dt_clim, by = c("month", "day","hour"))
    setkey(dt_y, "date")
    dt_y[, FL_anomaly := FL1_temp - FL_temp_climatology]
    dt_y[, FL_anomaly_30 := c(rep(NA, feature_lag * 24 - 1), zoo::rollmean(FL_anomaly, feature_lag * 24))]
    dt_y_daily = dt_y[, .("FL_anomaly" = mean(FL_anomaly),
                          "FL_anomaly_30" = mean(FL_anomaly_30),
                          "nao1_past" = mean(nao1_past)),
                      date]
    f_length = diff(range(forecast_horizon)) + 1
    dt_y_daily[, mean_f := c(rep(NA, f_length - 1), zoo::rollmean(FL_anomaly, f_length))]
    dt_y_daily[, FL_target := shift(mean_f, n = max(forecast_horizon), NA, "lead")]
    f = "FL_target ~ as.factor(month(date)) * year(date) + FL_anomaly + FL_anomaly_30"
    mod = lm(as.formula(f), data = dt_y_daily)
    f = "FL_target ~ as.factor(month(date)) * year(date)"
    mod_year = lm(as.formula(f), data = dt_y_daily)
    dt_test = merge(copy(dt_swm[ date %in% (date_d + forecast_horizon)]),
                    dt_clim,
                    by = c("month", "day", "hour"))
    dt_test[, FL_anomaly := FL1_temp - FL_temp_climatology]
    dt_pred = data.table(date = date_d,
                         y_hat = predict(mod, newdata = tail(dt_y_daily,1))[[1]],
                         y_hat_year = predict(mod_year, newdata = tail(dt_y_daily,1))[[1]],
                         y_obs = dt_test[,mean(FL_anomaly)])
    return(dt_pred)
}

dt_all = mclapply(1:1000,##seq_along(dates_run),
                  "run_bt",
                  dt_swm = dt_swm,
                  mc.cores = 30,
                  mc.silent = FALSE)
dt_res = rbindlist(dt_all)[is.finite(y_obs)]


dt_score = dt_res[, .("score_clim" = mean(y_obs^2),
                      "score_mod1" = mean( (y_obs - y_hat)^2),
                      "score_mod2" = mean( (y_obs - y_hat_year)^2)),
                  .(year(date), month(date))]
dt_score = dt_res[, .("score_clim" = mean(y_obs^2),
                      "score_mod1" = mean( (y_obs - y_hat)^2),
                      "score_mod2" = mean( (y_obs - y_hat_year)^2)),
                  .(month(date))]


