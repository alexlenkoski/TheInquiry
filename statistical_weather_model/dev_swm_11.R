rm(list = ls())

library(NordpoolDemand)

dt = load_swm_factor_loadings()
X = cbind(zoo::rollmean(dt_train[, anomaly_future], 30 * 24),
          zoo::rollmean(dt_train[, FL1_mslp], 30 * 24))
