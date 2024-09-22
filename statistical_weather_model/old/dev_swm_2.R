## Assess the performance of predictive quantiles for individual lead times
rm(list = ls())

library(NordpoolDemand)
library(gam)
library(quantreg)
library(parallel)

quant_model_helper = function(y,
                              years_all,
                              dt_all,
                              mslp_lag = 24,
                              f = "anomaly_future ~ q_clim + FL_anomaly + FL_anomaly_30 - 1"){

    year_y = years_all[y]
    print(paste0(year_y, " ", Sys.time()))
    dt_clim = dt_all[year != year_y,
                     .("FL_temp_climatology" = mean(FL1_temp)),
                     .(month, day, hour)]
    dt_y = merge(dt_all,
                 dt_clim,
                 by = c("month",
                        "day",
                        "hour"))
    setkey(dt_y, "date",  "hour")
    dt_y[, FL_anomaly := FL1_temp - FL_temp_climatology]
    dt_y[, FL_anomaly_30 := c(rep(NA, mslp_lag * 24 - 1), zoo::rollmean(FL_anomaly, mslp_lag *24))]
    dt_test_all = list()
    k = 1
    for(days_ahead in 1:60){
        dt_y[, anomaly_future := shift(FL_anomaly,
                                       n = days_ahead * 24,
                                       NA,
                                       "lead")]
        dt_train = dt_y[ year != year_y]
        dt_test = dt_y[year == year_y]
        dt_train[, "q_clim" := quantile(anomaly_future,
                                        .9,
                                        na.rm = TRUE),
                 .(month, day, hour)]
        f = "anomaly_future ~ q_clim + FL_anomaly + FL_anomaly_30 - 1"
        mod = quantreg::rq(as.formula(f), data = dt_train, tau = 0.9, method = "fn")
        dt_test[, lead_time := days_ahead]
        dt_test = merge(dt_test,
                        unique(dt_train[, .(month, day, hour, q_clim)]),
                        by = c("month", "day", "hour"))
        dt_test[, y_hat := predict(mod, newdata = dt_test)]
        dt_test_all[[k]] = dt_test[, .(date, hour, lead_time, y_hat, q_clim, anomaly_future)]
        k = k + 1
    }
    return(rbindlist(dt_test_all))
}

mslp_lag = 30
out_dir = "~/NR/ClimateFutures/RenewableEnergy/SPMM/Volume/MastersData/"
l_temp = load_pca_analysis(variable = "2m_temperature",
                           pressure_level = NA, 
                           descriptive_string = "nordic",
                           out_dir = out_dir)
dt_all = l_temp$dt_factor_loadings[, .(date, hour, 
                                       FL1_temp = V1,
                                       FL2_temp = V2)]
dt_all[, year := year(date)]
dt_all[, month := month(date)]
dt_all[, day := mday(date)]

years_all = dt_all[, unique(year(date))]
l = mclapply(seq_along(years_all),
             "quant_model_helper",
             years_all = years_all,
             dt_all = dt_all,
             mc.cores = 30,
             mc.silent = FALSE)
dt_test = rbindlist(l)

pbloss = function(Y, Yhat, tau){
    return( (Y-Yhat) * tau * (Y > Yhat) + (Yhat - Y) * (1 - tau) * (Yhat > Y))
}
dt_test[!is.na(y_hat) &
        !is.na(anomaly_future),
        1 - mean(pbloss(anomaly_future, y_hat, .9)) / mean(pbloss(anomaly_future, q_clim, .9)),
        lead_time]
dt_weekly = dt_test[lead_time %in% 3:10, .("y_hat_weekly" = mean(y_hat),
                                           "anomaly_weekly" = mean(anomaly_future)),
                    date][!is.na(y_hat_weekly) & !is.na(anomaly_weekly)]
dt_weekly[, 1 - mean( (y_hat_weekly - anomaly_weekly)^2) / mean(anomaly_weekly^2), month(date)]

dt_weekly = dt_test[lead_time %in% 31:60, .("y_hat_weekly" = mean(y_hat),
                                            "anomaly_weekly" = mean(anomaly_future)),
                    date][!is.na(y_hat_weekly) & !is.na(anomaly_weekly)]
dt_weekly[, 1 - mean( (y_hat_weekly - anomaly_weekly)^2) / mean(anomaly_weekly^2), month(date)]

