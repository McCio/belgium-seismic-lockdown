
# var domains = {
# mems: [-7200,7200],
# uccs: [-350000,350000],
# ebn:  [-216000,216000],
# dou:  [-22500,22500],
# rch:  [-4000,4000],
# zev:  [-2000000,2000000],
# sti:  [-14400,14400],
# skq:  [-60000,60000],
# ostb: [-9400,9400]
# };

library(stringi) # %s+%
library(lubridate) # floor_date
library(xts) # zoo
library(fpp) # auto.arima
library(tidyverse) # map_df, read_csv, group_by, summarize

setwd("/media/data/Documents/Uni/spatiotemp/villalobos/project/")
# UCCS
domain <- c(-350000,350000); pattern <- "UCCS.*\\.csv"
# MEMS
domain <- c(-7200,7200); pattern="MEM.*.csv"
seis <-
  list.files(path="csv/", pattern=pattern) %>%
  map_df(~read_csv("csv/" %s+% .))


out_domain <- (seis$min < domain[1] | seis$max > domain[2])
invalid_values <- (seis$min == -1 & seis$max == 1) | (seis$min == 0 & seis$max == 0) | out_domain
seis <-
  seis %>%
  mutate(min=replace(min, invalid_values, NA), max=replace(max, invalid_values, NA)) %>%
  complete(utc_second=seq.POSIXt(from=min(seis$utc_second), to=max(seis$utc_second), by = as.difftime("0:0:1")))
# %>% mutate(belgian_second=with_tz(utc_second, "Europe/Brussels"))
remove(out_domain, invalid_values) # free ~300 Mb from RAM



summary(seis)
head(seis, n=5)
nrow(seis)
nrow(na.omit(seis))
nrow(seis) - nrow(na.omit(seis))



seis.h <- 
  na.exclude(seis) %>%
  filter(utc_second < with_tz(as_datetime('2020-03-01T00:00:00'), "Europe/Brussels") ) %>%
  group_by(utc_hour=floor_date(with_tz(utc_second, "Europe/Brussels"), "1 hour")) %>%
  # filter(between(utc_hour, as_datetime('2020-04-01T00:00:00'), as_datetime('2020-05-01T00:00:00') )) %>%
  summarize(total_movement=sum(max-min), mean_movement_s=mean(max-min), min=min(min), max=max(max), max_movement=max-min) # %>%
  #complete(utc_hour=seq.POSIXt(from=min(seis$utc_second), to=max(seis$utc_second), by = as.difftime("1:0:0")))
#  summarize(total_movement=sum(max-min))
head(seis.h, n = 1)
summary(seis.h)
              
plot(seis.h$utc_hour, seis.h$total_movement, type = "l")
plot(seis.h$utc_hour, seis.h$mean_movement_s, type = "l")
plot(seis.h$utc_hour, seis.h$max_movement, type = "l", ylim = c(0,100000))
seis.h[seis.h$total_movement < 1000000,]



seis.h <- 
  na.exclude(seis) %>%
  filter(utc_second < as_datetime('2020-03-01T00:00:00') ) %>%
  group_by(utc_hour=floor_date(with_tz(utc_second, "Europe/Brussels"), "1 day")) %>%
  summarize(
    total_movement=sum(max-min),
    mean_min=mean(min), mean_max=mean(max), mean_movement_s=mean(max-min),
    min=min(min), max=max(max), max_movement=max-min,
  ) %>%
  complete(utc_hour=seq.POSIXt(from=min(seis$utc_second), to=max(seis$utc_second), by = 'day'))
#  summarize(total_movement=sum(max-min))
head(seis.h, n = 1)
summary(seis.h)

plot(seis.h$utc_hour, seis.h$total_movement, type = "l")
plot(seis.h$utc_hour, seis.h$mean_movement_s, type = "l")
plot(seis.h$utc_hour, seis.h$mean_max, type = "l")
plot(seis.h$utc_hour, seis.h$max_movement, type = "l")
seis.h[seis.h$total_movement < 1000000,]


seis.h <- 
  na.exclude(seis) %>%
  filter(utc_second > as_datetime('2020-03-01T00:00:00') ) %>%
  group_by(utc_hour=wday(utc_second, week_start=1)) %>% #floor(hour(with_tz(utc_second, "Europe/Brussels")))) %>%
  summarize(total_movement=sum(max-min), mean_movement_s=mean(max-min), min=min(min), max=max(max), max_movement=max-min)
#  summarize(total_movement=sum(max-min))
head(seis.h, n = 1)
summary(seis.h)

plot(seis.h$utc_hour, seis.h$total_movement, type = "l")
plot(seis.h$utc_hour, seis.h$mean_movement_s, type = "l")
plot(seis.h$utc_hour, seis.h$max_movement, type = "l")
seis.h[seis.h$total_movement < 1000000,]



tbh <- seis[seis$utc_second >= as_datetime('2019-03-01T04:00:00') & seis$utc_second < as_datetime('2019-03-01T09:00:00'),]
plot(tbh$utc_second, tbh$max, type = "l")
plot(tbh$utc_second, -tbh$min, type = "l")
plot(tbh$utc_second, tbh$max-tbh$min, type = "l")
plot(tbh$utc_second, tbh$max+tbh$min, type = "l")



h.ts <- zoo(
  order.by = seis.h[['utc_hour']],
  x = seis.h[['mean_movement_s']],
  frequency = 1
)
acf(h.ts, na.action = na.pass)
str(h.ts)
length(h.ts)
length(ts(h.ts))
as.numeric(time(h.ts))
plot(h.ts)
frequency(h.ts) <- 1/28
stl
remove(seis, seis.h)
gc()
decompose(h.ts)


# Regression methods: polynomial trends
tt<-as.numeric(time(h.ts))
# Second order polynomial fit
fit2<-lm(h.ts~poly(tt,degree=2,raw=TRUE))
fit4<-lm(h.ts~poly(tt,degree=4,raw=TRUE))
plot(h.ts)
length(tt)
length(predict(fit2))
lines(tt,predict(fit2),col='red',lwd=2)
lines(tt,predict(fit4),col='blue',lwd=2)














plot(recife,ylab='Temperature (degree C)',
     main='Recife, Brazil Temperature Data')
f <- 12
weights.s <- c(0.5, rep(1, f - 1), 0.5)/f
trend.s <- stats::filter(recife, filter=weights.s,side=2)
lines(trend.s,col='red',lwd=2)







seis.h <- 
  na.exclude(seis) %>%
  filter(utc_second < with_tz(as_datetime('2020-03-01T00:00:00'), "Europe/Brussels") ) %>%
  group_by(utc_hour=floor_date(with_tz(utc_second, "Europe/Brussels"), "1 hour")) %>%
  # filter(between(utc_hour, as_datetime('2020-04-01T00:00:00'), as_datetime('2020-05-01T00:00:00') )) %>%
  summarize(total_movement=sum(max-min), mean_movement_s=mean(max-min), min=min(min), max=max(max), max_movement=max-min)
summary(seis.h)
plot(seis.h$utc_hour, seis.h$mean_movement_s, type = "l")


h.ts <- zoo(
  order.by = as.numeric(seis.h[['utc_hour']]) / 3600 / 24,
  x = seis.h[['mean_movement_s']],
  frequency = 24
)
str(h.ts)
length(h.ts)
length(ts(h.ts))
as.numeric(time(h.ts))
plot(h.ts)



remove(seis, seis.h)
gc()
h.ts.dec <- decompose(h.ts)
plot(h.ts.dec)
acf(h.ts.dec$random, na.action = na.pass)



dx<-diff(h.ts,lag=24,differences=1)
tsdisplay(dx)
h.ts.arima <- auto.arima(h.ts, ic="aicc", trace=T)
h.ts.arima
tsdisplay(residuals(h.ts.arima))






plot(h.ts)
f <- 168
weights.s <- c(0.5, rep(1, f - 1), 0.5)/f
trend.s <- stats::filter(h.ts, filter=weights.s,side=2)
lines(trend.s,col='red',lwd=2)
h.ts.stl <- stl(h.ts, s.window=168, na.action=na.pass)


# https://tanzu.vmware.com/content/blog/forecasting-time-series-data-with-multiple-seasonal-periods

# heatmap https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/ 

# (p <- ggplot(nba.m, aes(y1, y2)) + geom_tile(aes(fill = rescale), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue"))

# base_size <- 9
# p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = "grey50"))
