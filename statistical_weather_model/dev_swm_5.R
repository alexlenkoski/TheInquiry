rm(list = ls())

library(ERAData)
library(ForecastTools)

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
