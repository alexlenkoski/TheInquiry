rm(list = ls())

library(ERAData)

dt = load_era_daily_data(variable = "geopotential",
                         start_date = "1993-01-01",
                         end_date = "1993-04-01",
                         pressure = 500,
                         lon_range = c(10,10),
                         lat_range = c(60,60))

dt_mslp = load_era_daily_data(variable = "mean_sea_level_pressure",
                              start_date = "1993-01-01",
                              end_date = "1994-01-01",
                              lon_range = c(10,10),
                              lat_range = c(60,60))
l = load_pca_analysis(variable = "mean_sea_level_pressure", descriptive_string = "nordic")


a = l$dt_factor_loadings[year(date) == 1993]
