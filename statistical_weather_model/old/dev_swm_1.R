## THIS IS THE SCRIPT TO FOCUS ON
rm(list = ls())

library(NordpoolDemand)
library(gam)

mslp_lag = 30

dt_all = load_pca_framework() ## put out_dir in this with your path
dt_all[, nao1_past := c(rep(NA, mslp_lag * 24 - 1), zoo::rollmean(FL1_mslp, mslp_lag * 24))]
dt_all[, nao2_past := c(rep(NA, mslp_lag * 24 - 1), zoo::rollmean(FL2_mslp, mslp_lag * 24))]
dt_all[, sib1_past := c(rep(NA, mslp_lag * 24 - 1), zoo::rollmean(FL1_mslp_siberia, mslp_lag * 24))]
dt_all[, F_temp_past := c(rep(NA, mslp_lag * 24 - 1), zoo::rollmean(FL1_temp, mslp_lag * 24))]
dt_all[, F_temp_past2 := c(rep(NA, mslp_lag * 24 - 1), zoo::rollmean(FL2_temp, mslp_lag * 24))]
dt_all[, year := year(date)]
dt_all[, month := month(date)]
dt_all[, day := mday(date)]

years_all = dt_all[, unique(year(date))]
dt_score_all = list()
dt_test_all = list()
k = 1
for(y in seq_along(years_all)){
    year_y = years_all[y]
    dt_clim = dt_all[year != year_y,
                     .("FL_temp_climatology" = mean(FL1_temp),
                       "FL_temp_quantile" = quantile(FL1_temp, .9)),
                     .(month, day, hour)]
    dt_y = merge(dt_all, dt_clim, by = c("month", "day","hour"))
    setkey(dt_y, "date")
    dt_y[, FL_anomaly := FL1_temp - FL_temp_climatology]
    dt_y[, FL_quantile_anomaly := FL_temp_quantile - FL_temp_climatology]
    dt_y[, FL_anomaly_30 := c(rep(NA, mslp_lag * 24 - 1), zoo::rollmean(FL_anomaly, mslp_lag *24))]
    for(days_ahead in 1:60){
        dt_y[, anomaly_future := shift(FL_anomaly,
                                       n = days_ahead * 24,
                                       NA,
                                       "lead")]
        dt_y[, anomaly_quantile_future := shift(FL_quantile_anomaly,
                                                n = days_ahead * 24,
                                                NA,
                                                "lead")]
        dt_train = dt_y[ year != year_y]
        dt_test = dt_y[year == year_y]
        f = "anomaly_future ~ FL_anomaly + FL_anomaly_30 + as.factor(month) *  nao1_past"
##    f = "anomaly_future ~ as.factor(month) + FL_anomaly + FL_anomaly_30"
##        f = "anomaly_future ~ FL_anomaly + as.factor(month) * FL_anomaly_30"
        mod = lm(as.formula(f), data = dt_train)
        dt_test[, lead_time := days_ahead]
        dt_test[, y_hat := predict(mod, newdata = dt_test)]
        dt_test_all[[k]] = dt_test[, .(date, hour, lead_time, y_hat, anomaly_future)]
        k = k + 1
    }
}

dt_test = rbindlist(dt_test_all)

dt_weekly = dt_test[lead_time %in% 21:21, .("y_hat_weekly" = mean(y_hat),
                                            "anomaly_weekly" = mean(anomaly_future)),
                    date][!is.na(y_hat_weekly) & !is.na(anomaly_weekly)]
dt_weekly[, 1 - mean( (y_hat_weekly - anomaly_weekly)^2) / mean(anomaly_weekly^2), month(date)]

dt_weekly = dt_test[lead_time %in% 31:60, .("y_hat_weekly" = mean(y_hat),
                                            "anomaly_weekly" = mean(anomaly_future)),
                    date][!is.na(y_hat_weekly) & !is.na(anomaly_weekly)]
dt_weekly[, 1 - mean( (y_hat_weekly - anomaly_weekly)^2) / mean(anomaly_weekly^2), month(date)]

