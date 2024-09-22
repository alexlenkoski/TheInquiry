rm(list = ls())

library(NordpoolDemand)

l = load_pca_analysis("2m_temperature", descriptive_string = "nordic")

dt_fl_temp = l$dt_factor_loadings
dt_fl_daily = l$dt_factor_loadings[, .("fl_daily" = mean(V1)), date]

dt_fl_daily[, month := month(date)]
dt_fl_daily[, day := mday(date)]
dt_fl_climatology = dt_fl_daily[, .("fl_climatology" = mean(fl_daily),
                                    "q_low" = quantile(fl_daily, .25),
                                    "q_high" = quantile(fl_daily, .75)),
                                .(month, day)]
dt_fl = merge(dt_fl_daily,
              dt_fl_climatology,
              by = c("month", "day"))
setkey(dt_fl, "date")

years = 2013
for(y in seq_along(years)){
    X11()
    dt_focus = dt_fl[date %in% as.Date(paste0(years[y], "-11-15")):as.Date(paste0(years[y] + 1, "-03-15"))]
    plot(dt_focus[, .(date, fl_daily)], type = "l", main = years[y], ylim = dt_fl[, range(fl_daily)])
    points(dt_focus[, .(date, fl_daily)], pch = 20)
    lines(dt_focus[, .(date, fl_climatology)], col = "red")
    lines(dt_focus[, .(date, q_low)], col = "red", lty = 2)
    lines(dt_focus[, .(date, q_high)], col = "red", lty = 2)

}

dt_res[, y_obs_shift := shift(y_obs, n = 15, NA, "lag")]

dt_wtf = merge(dt_res, dt_daily, by = "date")
dt_wtf[, V1_shift := V1 - shift(V1,1,NA, "lag")]

years = 1980:2021
dt_clim_all = list()
for(y in seq_along(years)){
    print(y)
    dt = load_era_daily_data(variable = "2m_temperature",
                             start_date = paste0(years[y], "-11-15"),
                             end_date = paste0(years[y] + 1, "-01-15"),
                             lon_range = c(0, 30),
                             lat_range = c(40, 77))
    dt_clim_all[[y]] = dt
}

dt = rbindlist(dt_clim_all)
dt_temp_daily = dt[, .("daily_temp" = mean(get("2m_temperature") - 273.15)),
                   .(date, lon, lat)]
dt_temp_daily[, month:= month(date)]
dt_temp_daily[, day := mday(date)]
dt_clim = dt_temp_daily[, .("climatology" = mean(daily_temp)),
                        .(lon, lat, month, day)]
dt_temp = merge(dt_temp_daily

                        



dd = dt_temp_daily[, unique(date)]

for(d in seq_along(dd)){
    date_d = as.Date(dd[d], origin = "1970-01-01")
    dt_day = dt_temp_daily[date == date_d]
    X11()
    plot_smooth(dt_day[, .(lon, lat, daily_temp)], mn = date_d)

}
