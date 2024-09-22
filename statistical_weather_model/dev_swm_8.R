rm(list = ls())

library(NordpoolDemand)

l_u = load_pca_analysis(variable = "u_component_of_wind",
                      pressure_level = 10)

dt_fl_daily = l_u$dt_factor_loadings[, .("fl_daily" = mean(V1)), date]
dt_fl_daily[, month := month(date)]
dt_fl_daily[, day := mday(date)]
dt_fl_climatology = dt_fl_daily[, .("fl_climatology" = mean(fl_daily),
                                    "q_low" = quantile(fl_daily, .25),
                                    "q_high" = quantile(fl_daily, .75)),
                                .(month, day)]
dt_fl_u = merge(dt_fl_daily,
                dt_fl_climatology,
                by = c("month", "day"))
setkey(dt_fl_u, "date")

l_t = load_pca_analysis(variable = "2m_temperature", descriptive_string = "nordic")
dt_fl_daily = l_t$dt_factor_loadings[, .("fl_daily" = mean(V1)), date]
dt_fl_daily[, month := month(date)]
dt_fl_daily[, day := mday(date)]
dt_fl_climatology = dt_fl_daily[, .("fl_climatology" = mean(fl_daily),
                                    "q_low" = quantile(fl_daily, .25),
                                    "q_high" = quantile(fl_daily, .75)),
                                .(month, day)]
dt_fl_t = merge(dt_fl_daily,
                dt_fl_climatology,
                by = c("month", "day"))
setkey(dt_fl_t, "date")

years = 1979:2021
for(y in seq_along(years)){
    X11()
    par(mfrow = c(1,2))
    dt_focus = dt_fl_u[date %in% as.Date(paste0(years[y], "-11-15")):as.Date(paste0(years[y] + 1, "-03-15"))]
    plot(dt_focus[, .(date, fl_daily)], type = "l", main = years[y], ylim = dt_fl_u[, range(fl_daily)], ylab = "U Wind")
    points(dt_focus[, .(date, fl_daily)], pch = 20)
    lines(dt_focus[, .(date, fl_climatology)], col = "red")
    lines(dt_focus[, .(date, q_low)], col = "red", lty = 2)
    lines(dt_focus[, .(date, q_high)], col = "red", lty = 2)

    dt_focus = dt_fl_t[date %in% as.Date(paste0(years[y], "-11-15")):as.Date(paste0(years[y] + 1, "-03-15"))]
    plot(dt_focus[, .(date, fl_daily)], type = "l", main = years[y], ylim = dt_fl_t[, range(fl_daily)], ylab = "Temp")
    points(dt_focus[, .(date, fl_daily)], pch = 20)
    lines(dt_focus[, .(date, fl_climatology)], col = "red")
    lines(dt_focus[, .(date, q_low)], col = "red", lty = 2)
    lines(dt_focus[, .(date, q_high)], col = "red", lty = 2)
}
