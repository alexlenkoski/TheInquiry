rm(list = ls())

library(NordpoolDemand)
library(ERAData)

years = 1995
dt_clim_all = list()
for(y in seq_along(years)){
    print(y)
    dt = load_era_daily_data(variable = "u_component_of_wind",
                             pressure = 10,
                             start_date = paste0(years[y], "-11-15"),
                             end_date = paste0(years[y] + 1, "-01-15"),
##                             lon_range = c(-20, 40),
##                             lat_range = c(10, 85),
                             root_dir = "/bigdisk/pro/SSW/")
    dt_clim_all[[y]] = dt
}

dt_u = rbindlist(dt_clim_all)
dt_u_daily = dt_u[, .("u_daily" = mean(u_component_of_wind)), .(date, lon, lat)]
dt_u_daily[, u_daily := u_daily - mean(u_daily)]
dd = dt_u[, unique(date)]
for(d in seq_along(dd)){
    date_d = as.Date(dd[d], origin = "1970-01-01")
    dt_day = dt_u_daily[date == date_d]
    X11()
    plot_smooth(dt_day[, .(lon, lat, u_daily)], mn = date_d)
}
