## Does direct modeling on the weekly average work better than individual lead time modeling aggregated?
rm(list = ls())

library(NordpoolDemand)
library(gam)

mslp_lag = 30

dt_all = load_swm_factor_loadings()

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
                     .("FL_temp_climatology" = mean(FL1_temp)),
                     .(month, day, hour)]
    dt_y = merge(dt_all, dt_clim, by = c("month", "day","hour"))
    setkey(dt_y, "date")
    dt_y[, FL_anomaly := FL1_temp - FL_temp_climatology]
    dt_y[, FL_anomaly_30 := c(rep(NA, mslp_lag * 24 - 1), zoo::rollmean(FL_anomaly, mslp_lag *24))]
    dt_y_weekly_all = list()
    dt_test_weekly_all = list()
    for(days_ahead in 60:90){
        dt_y[, anomaly_future := shift(FL_anomaly,
                                       n = days_ahead * 24,
                                       NA,
                                       "lead")]
        dt_train = dt_y[ year != year_y]
        dt_test = dt_y[year == year_y]
        dt_y_weekly_all[[days_ahead]] = dt_y
        dt_test_weekly_all[[days_ahead]] = dt_test
        
        f = "anomaly_future ~ FL_anomaly + as.factor(month) * FL_anomaly_30 + as.factor(month) * nao1_past + as.factor(month) * sib1_past"
        mod = lm(as.formula(f), data = dt_train)
        dt_test[, lead_time := days_ahead]
        dt_test[, y_hat := predict(mod, newdata = dt_test)]
    }

    dt_y_weekly = rbindlist(dt_y_weekly_all)
    dt_test_weekly = rbindlist(dt_test_weekly_all)

    dt_weekly_train = dt_y_weekly[, .("month" = head(month, 1),
                                      "FL_anomaly" = mean(FL_anomaly),
                                      "FL_anomaly_30" = mean(FL_anomaly_30),
                                      "nao1_past" = mean(nao1_past),
                                      "sib1_past" = mean(sib1_past),
                                      "anomaly_future" = mean(anomaly_future)),
                                  date][!is.na(FL_anomaly_30) & !is.na(anomaly_future)]
    dt_weekly_test = dt_test_weekly[, .("month" = head(month, 1),
                                        "FL_anomaly" = mean(FL_anomaly),
                                        "FL_anomaly_30" = mean(FL_anomaly_30),
                                        "nao1_past" = mean(nao1_past),
                                        "sib1_past" = mean(sib1_past),
                                        "anomaly_future" = mean(anomaly_future),
                                        "y_hat_individ" = mean(y_hat)),
                                    date][!is.na(FL_anomaly_30) & !is.na(anomaly_future)]
    mod = lm(as.formula(f), data = dt_weekly_train)
    dt_weekly_test[, y_hat := predict(mod, newdata = dt_weekly_test)]
    dt_test_all[[k]] = dt_weekly_test
    k = k + 1
}

dt_test = rbindlist(dt_test_all)

dt_score = dt_test[, .("score_clim" = mean(anomaly_future^2),
                       "score_individ" = mean( (anomaly_future - y_hat_individ)^2),
                       "score_agg" = mean( (anomaly_future - y_hat)^2)),
                   month(date)]

dt_skill = dt_score[, .("skill_agg" = 1 - score_agg/score_clim,
                        "skill_individ" = 1 - score_individ/score_clim),
                    month]
