
library(stringi) # %s+%
library(lubridate) # floor_date
library(xts) # zoo
library(fpp) # auto.arima
library(ggplot2) # plots
library(ggrepel) # for nicer labels on ggplot
library(cowplot) # for plot_grid
library(qqplotr) # qqplot with ggplot
library(spec) # for periodgram
library(tidyverse) # map_df, read_csv, group_by, summarize...
library(tsbox) # https://cran.r-project.org/web/packages/tsbox/vignettes/tsbox.html

# Before starting we define this helper function for printing formatted decimals
print_dec <- function(x, decimals=2) trimws(format(round(x, decimals), nsmall=decimals))



# If needed, set working directory. The folder structure inside it should contain
# - csv/
#   - UCCS*.csv files
#   - MEMS*.csv files
# - proj.*.RData (optional)
# setwd("/media/data/Documents/Uni/spatiotemp/villalobos/project/")



# We define some variables that identify the dataset. Only the row about the wanted dataset should be run
# UCCS
domain <- c(-350000,350000); pattern <- "UCCS-.*\\.csv"; name <- "UCCS"; description <- "Uccle, Bruxelles"; proj_file <- 'proj.uccs.RData'
# MEMS
domain <- c(-7200,7200); pattern <- "MEMS-.*\\.csv"; name <- "MEMS"; description <- "Membach"; proj_file <- 'proj.mems.RData'

# If the corresponding RData file is present, we load it mainly to avoid refitting all models
if (exists("proj_file") & file.exists(proj_file)) {
  load(proj_file)
}



# Load all csv files in the folder corresponding to the defined pattern, station-dependant
seis <-
  list.files(path="csv/", pattern=pattern) %>%
  map_df(~read_csv("csv/" %s+% .))



# Prepare a description for plots and see print some information about the data
long_description <- description %s+% " (" %s+% name %s+% " station)"
cat(long_description, "\n")
cat("Domain values:", domain[1], "to", domain[2], "nm\n")
cat("Data from", format.POSIXct(min(seis$utc_second), "%Y-%m-%d %H:%M:%S"), "to", format.POSIXct(max(seis$utc_second), "%Y-%m-%d %H:%M:%S"), "\n")



# As a first step, we remove all values out of domain, and all invalid values: (-1,1) and (0,0) pairs
# This is done following these steps:
# - locate the values through boolean arrays
# - employ the dplyr's mutate method to replace these values with NA
# - dropping them from the dataset
out_domain <- (seis$min < domain[1] | seis$max > domain[2])
invalid_values <- (seis$min == -1 & seis$max == 1) | (seis$min == 0 & seis$max == 0) | out_domain
cat("Removing", sum(out_domain), "values out of domain,", (sum(invalid_values) - sum(out_domain)), "explicit NA\n")
seis <-
  seis %>%
  mutate(min=replace(min, invalid_values, NA), max=replace(max, invalid_values, NA)) %>%
  drop_na()
remove(out_domain, invalid_values) # free ~350 Mb from RAM



# We can now proceed to analyse missing values directly on the cleaned dataset with times in second.
# The procedure follows these steps:
# - complete the dataset with missing seconds
# - locate the missing seconds by row index
# - for logging purposes, we log the number of missing points
# - we follow the indexes of NA values and we keep each start-end index of each consecutive block
# - we can transform back the indexes to UTC instants by adding the equivalent seconds to the first second in our dataset
# - finally, we can display this data in a plot to better evaluate it

# - complete the dataset with missing seconds
seis.complete <- seis %>% complete(utc_second=seq.POSIXt(from=min(seis$utc_second), to=max(seis$utc_second), by = as.difftime("0:0:1")))
seis.first.second <- min(seis$utc_second)
# - locate the missing seconds by row index
seis.missing.ids <- which(is.na(seis.complete$min))
# - for logging purposes, we log the number of missing points
seis.present <- length(seis$utc_second)
seis.total <- length(seis.complete$utc_second)
seis.missing <- seis.total - seis.present
cat(seis.present, "present values,", seis.missing, "missing points (", print_dec(seis.present / seis.total * 100, 2), "% /", print_dec(seis.missing / seis.total * 100, 2), "% )\n")
# - we follow the indexes of NA values and we keep each start-end index of each consecutive block
block_start <- NULL
block_end <- NULL
blocks <- data.frame(start=integer(), end=integer())
for (i in 1:length(seis.missing.ids)) {
  if (is.null(block_start)) {
    block_start <- seis.missing.ids[i]
    block_end <- seis.missing.ids[i]
  } else if ((block_end + 1) == seis.missing.ids[i]) {
    block_end <- seis.missing.ids[i]
  } else {
    blocks[nrow(blocks) + 1, ] <- c(block_start, block_end)
    block_start <- seis.missing.ids[i]
    block_end <- seis.missing.ids[i]
  }
}
blocks[nrow(blocks) + 1, ] <- c(block_start, block_end)
# - we can transform back the indexes to UTC instants by adding the equivalent seconds to the first second in our dataset
blocks$interval <- seconds(blocks$end - blocks$start + 1)
blocks$start <- seis.first.second + seconds(blocks$start - 1)
blocks$end <- seis.first.second + seconds(blocks$end - 1)
# - finally, we can display this data in a plot to better evaluate it
plot(blocks$start, blocks$interval,
     col=ifelse(blocks$interval<3600, "darkgreen", "darkred"),
     main="Consecutive seconds of missing values",
     ylab="Seconds", xlab="Interval start",
     xaxt="n", xlim=c(min(seis$utc_second), max(seis$utc_second))
)
axis.POSIXct(1, at=seq.POSIXt(from=min(seis$utc_second), to=max(seis$utc_second), by = "1 month"), format="%m-%y", cex=0.8)
abline(h=3600, col="green", lty="dotted")
legend("topleft", legend=c("interval >= 1h", "interval < 1h", "1h"), col=c("darkred", "darkgreen", "green"), lty=c(NA,NA,3), pch=c(1,1,NA),cex=0.9, bty="n")
text(x=blocks$start[which(blocks$interval > 3600)], y=blocks$interval[which(blocks$interval > 3600)], labels = paste0(blocks$start[which(blocks$interval > 3600)], " - ", blocks$end[which(blocks$interval > 3600)]), cex=0.9, pos=4)
# remove from RAM copy of dataset and cleanup
remove(seis.complete, seis.missing.ids)
gc()



# We now proceed to aggregate the data into an hourly dataset.
# This is done by shifting the UTC time to Belgium zone, and then considering it as it were UTC
# That is done to enable usage of normal R methods that do not take into account the TZ component of a datetime
# This leads to an hour hole on summer time changes, since the local time skips an hour. That hole is filled with the mean of the hour before with the hour after.
# Values are the overall mean of the max-min difference for all values inside each group (hour)
bruxelles <- function(d) with_tz(d, "Europe/Brussels")
force_bruxelles <- function(d) force_tz(d, "Europe/Brussels")
force_utc <- function(d) force_tz(d, "UTC")
# Fix the utc_hour value, and summarize with the mean of max-min differences
seis.h <- 
  seis %>%
  group_by(utc_hour=floor_date(force_utc(bruxelles(utc_second)), "1 hour")) %>%
  summarize(mean_movement_s=mean(max-min))
# Locate hours before a missing bruxelles local time
seis.before <- 
  seis.h[which(is.na(force_bruxelles(seis.h$utc_hour) + hours(1))), ] %>%
  mutate(utc_hour=utc_hour + hours(1))
# Locate hours after a missing bruxelles local time
seis.after <- 
  seis.h[which(is.na(force_bruxelles(seis.h$utc_hour) - hours(1))), ] %>%
  mutate(utc_hour=utc_hour - hours(1))
# Group the located hours on UTC hour, by taking the mean of the mean_movement_s
seis.dst.fix <-
  merge(seis.before, seis.after, all=T) %>%
  group_by(utc_hour=utc_hour) %>%
  summarize(mean_movement_s=mean(mean_movement_s))
# Merge the fix for summer time into the hourly dataset
seis.h <- merge(seis.h, seis.dst.fix, all=T)
remove(seis.after, seis.before, seis.dst.fix)
gc()



# We can now proceed to analyse missing values on the aggregated hourly dataset. The procedure is the same as before
# - complete the dataset with missing hours
seis.complete <- seis.h %>% complete(utc_hour=seq.POSIXt(from=min(seis.h$utc_hour), to=max(seis.h$utc_hour), by = as.difftime("1:0:0")))
seis.first.second <- min(seis$utc_second)
# - locate the missing seconds by row index
seis.missing.ids <- which(is.na(seis.complete$mean_movement_s))
# - for logging purposes, we log the number of missing points
seis.present <- length(seis.h$utc_hour)
seis.total <- length(seis.complete$utc_hour)
seis.missing <- seis.total - seis.present
cat(seis.present, "present values,", seis.missing, "missing points (", print_dec(seis.present / seis.total * 100, 2), "% /", print_dec(seis.missing / seis.total * 100, 2), "% )\n")
# - we follow the indexes of NA values and we keep each start-end index of each consecutive block
block_start <- NULL
block_end <- NULL
blocks <- data.frame(start=integer(), end=integer())
for (i in 1:length(seis.missing.ids)) {
  if (is.null(block_start)) {
    block_start <- seis.missing.ids[i]
    block_end <- seis.missing.ids[i]
  } else if ((block_end + 1) == seis.missing.ids[i]) {
    block_end <- seis.missing.ids[i]
  } else {
    blocks[nrow(blocks) + 1, ] <- c(block_start, block_end)
    block_start <- seis.missing.ids[i]
    block_end <- seis.missing.ids[i]
  }
}
blocks[nrow(blocks) + 1, ] <- c(block_start, block_end)
# - we can transform back the indexes to UTC instants by adding the equivalent hours to the first instant in our dataset
blocks$interval <- hours(blocks$end - blocks$start + 1)
blocks$start <- seis.first.second + seconds(blocks$start - 1)
blocks$end <- seis.first.second + seconds(blocks$end - 1)
# - finally, we can display this data in a plot to better evaluate it
plot(c(rbind(blocks$start, blocks$end, NA)), rep(hour(blocks$interval), each=3),
     col=ifelse(blocks$interval<3600, "darkgreen", "darkred"),
     main="Consecutive hours of missing values",
     ylab="Hours", xlab="Interval start (mm-'yy)",
     type="l", lwd=10,
     xaxt="n", xlim=c(min(seis$utc_second), max(seis$utc_second))
)
axis.POSIXct(1, at=seq.POSIXt(from=min(seis$utc_second), to=max(seis$utc_second), by = "1 month"), format="%m-'%y", cex=0.8)
text(x=blocks$start, y=hour(blocks$interval), labels = paste0(format(blocks$start, "%y-%m-%d %H:%M")), cex=0.9, pos=2)
text(x=blocks$end, y=hour(blocks$interval), labels = paste0(format(blocks$end, "%y-%m-%d %H:%M")), cex=0.9, pos=4)
text(x=blocks$start + make_difftime(hour=sapply(hour(blocks$interval), "/", y=2)), y=hour(blocks$interval), labels = paste0(hour(blocks$interval), "H"), cex=0.9, pos=3)
text(x=blocks$start + make_difftime(hour=sapply(hour(blocks$interval), "/", y=2)), y=hour(blocks$interval), labels = paste0(hour(blocks$interval), "H"), cex=0.9, pos=1)
# remove from RAM copy of dataset and cleanup
remove(seis.complete, seis.missing.ids)
gc()



# We will now start working with the dataset as a timeseries object.
# In order to do so, we define the function build_ts to accomodate the different usages we will need for ts objects build, using ts_box functions
build_ts <- function(dset, periods=NULL, start=NULL, end=NULL, force_ms=T) {
  # first, we build a zoo ts, that better handles dates with granularity under the day
  dset.ts <- ts_zoo(dset)
  # if a valid span is defined, we filter only the data inside that span
  if (!is.null(start) | !is.null(end)) {
    dset.ts <- ts_span(dset.ts, start=start, end=end)
  }
  # we complete the ts with missing values where needed
  dset.ts <- ts_regular(dset.ts)
  # if no frequency is required, we just return the obtained ts
  if (is.null(periods)) {
    return(dset.ts)
  }
  # we then apply the frequency, inside the zoo ts, or transforming the ts into a msts, if multiple frequencies are defined (or force_msts=T)
  if (is.vector(periods) & length(periods) == 1) {
    periods <- periods[1]
  }
  if (is.integer(periods)){
    if (force_ms) {
      periods <- c(periods)
    } else {
      frequency(dset.ts) <- periods
      return(dset.ts)
    }
  }
  return(msts(dset.ts, periods))
}



# To analytically select the period, we follow these steps, using the periodgram
# Firstly, we compute the periodgram itself on the MA-smoothed dataset, creating the plot with ggplot so that we can more easily highlight some points
min_h_seasonality <- 24
min_h_seasonality <- 2
pgram <- spec.pgram(stats::filter(build_ts(seis.h, start = seis.train.start, end = seis.train.end), side=2, filter=c(0.5, rep(1,min_h_seasonality-1), 0.5)/min_h_seasonality),
                    detrend=F, demean=T, na.action=na.aggregate, plot=F)
pgram.data <- data.frame(freq=pgram$freq, spec=pgram$spec)
ggplot(pgram.data) +
  geom_line(aes(freq, log(spec))) +
  xlab("frequency") + ylab("log(spectrum)") +
  labs(title = "Periodgram", caption = paste0("bandwidth = ", pgram$bandwidth, "    df = ", pgram$df)) ->
  periodgram
# we then select the points with higher values of the spectrum, and transform the frequency into the corresponding hour value
# 1/freq/3600 comes from the data being second-based (we run spec.pgram on a pure zoo dataset)
start_ratio <- 2
filter_freq <- (pgram$spec > (mean(pgram$spec) + sd(pgram$spec)*start_ratio)) 
top95pc <- pgram$spec[filter_freq]
top95freq <- pgram$freq[filter_freq][order(top95pc, decreasing = T)]
top95pc <- pgram$spec[filter_freq][order(top95pc, decreasing = T)]
cat("SD ratio\n")
(top95pc - mean(pgram$spec))/sd(pgram$spec)
top95seas.s <- 1/top95freq
cat("Raw frequency by unit\n")
top95seas.s
top95seas.h <- top95seas.s/3600
cat("Frequency by unit\n")
top95seas.h
# we add to the periodgram plot two secondary axes and the highlight of the most significant periodicites
ratio.color <- function(x) ifelse(x >= (mean(pgram$spec) + sd(pgram$spec)*3), "red", ifelse(x >= (mean(pgram$spec) + sd(pgram$spec)*2), "blue", ifelse(x >= (mean(pgram$spec) + sd(pgram$spec)*1), "darkgreen", "orange")))
shown_labels <- 4
periodgram +
  scale_x_continuous(sec.axis = sec_axis(~1/./3600, breaks=c(1, 3, 2*c(1,2,3,4,6,12), 24*7), labels = as.integer, name="hours")) +
  scale_y_continuous(sec.axis = sec_axis(~exp(.), breaks=mean(pgram$spec) + sd(pgram$spec) * c(1,3), labels=c("1", "2", "3")[c(1,3)], name="spectrum/sd ratio")) +
  geom_hline(yintercept=log(mean(pgram$spec) + sd(pgram$spec) * c(1:3)), linetype="dotted", color = ratio.color(mean(pgram$spec) + sd(pgram$spec) * c(1:3))) +
  geom_point(data=data.frame(freq=top95freq, spec=top95pc), aes(freq, log(spec), color=ratio.color(spec)), shape="o", size=4, show.legend = F) +
  geom_label_repel(data=data.frame(freq=top95freq, spec=top95pc)[1:shown_labels,], aes(freq, log(spec), label=round(1/freq/3600), color=ratio.color(spec)), segment.color="grey", show.legend=F) +
  scale_color_manual(values = c("darkgreen", "blue", "red")[start_ratio:3])
gc()



# We now proceed selecting the dataset timespan for train and test subsets.
# We will use the first 52 weeks starting from 2018-10-29 as training, and the following 18 weeks until 2020-03-02 as test
seis.h.start <- as_datetime('2018-10-29T00:00:00')
seis.h.end <- as_datetime('2020-03-02T00:00:00') - seconds(1)
# Only weekly seasonality is kept, given further analysis done
# We save the seasonality definitions for simpler usage later.
# We already define just the weekly seasonality, even if the rational for not using the daily one comes from later code
seasonality.period <- 168
seasonality.xlab <- "Weeks"
seasonality.aggregate <- function(ds) na.aggregate(ds, floor)
# We noticed that forcing the ts to be a msts object would make various steps in the future analysis easier, so we are making sure msts is used most of the times
seis.h.ts <- build_ts(seis.h, periods = c(seasonality.period), start = seis.h.start, end = seis.h.end, force_ms = T)
# This is the splitting point for the two datasets, that equals to the starting second of the test set
seis.test.split <- seis.h.start + days(52*7)
# When building datasets with span, we make sure to stop one second before the next hour, so that ts_box's ts_span correctly excludes the "next hour"
seis.train.start <- seis.h.start
seis.train.end <- seis.test.split - seconds(1)
seis.h.train <- build_ts(seis.h, periods = c(seasonality.period), start = seis.train.start, end = seis.train.end, force_ms = T)
# We build the test set starting at the same date as the training, and then we window it cutting away the training set,
# in order to have the ts indexes corresponding to "next values" of the training set
seis.test.start <- seis.test.split
seis.test.end <- seis.test.start + days(7*18) - seconds(1)
seis.h.test <- build_ts(seis.h, periods = c(seasonality.period), start = seis.train.start, end = seis.test.end, force_ms = T)
seis.h.test <- window(seis.h.test, start = c(end(seis.h.train)[1] + 1, 1))
seis.test.length <- length(seis.h.test)



# From now on we will need to compare various pairs of timeseries, using visual and analytical methods
# We collected them in this function for future simplicity.
# It prints errors and returns two plots showing the overlapped values and the difference with the percentage for significativity
show_overlap_and_diff <- function(base, compare, base.desc, compare.desc, xlab, overlap.ylab, diff.title, diff.ylab, diff.ylab.sec, diff.ylab.sec.accuracy=.1) {
  # We first plot the overlapped values
  overlap_plot <-
    autoplot(compare, xlab=xlab, ylab=overlap.ylab) +
    geom_line(aes(y=base, col="red"), show.legend = F) +
    labs(title=paste0(base.desc, " vs ", compare.desc))
  # The we compare the difference
  diff <- compare-base
  # Here we compute and print some error statistics
  mse <- mean(diff^2, na.rm = T)
  mae <- mean(abs(diff), na.rm = T)
  mape <- mean(abs(diff/base) * 100, na.rm = T)
  rmse <- sqrt(mse)
  R2 <- 1-(sum((diff)^2, na.rm = T)/sum((base-mean(base))^2, na.rm = T))
  cat("Errors: ", compare.desc, "\n")
  cat("\t","MSE=", mse, "  MAE=", mae, "  MAPE=", mape, "  RMSE=", rmse, "  R2=", R2, "\n")
  # Then we plot the difference between the series showing on the right the percentage relative to the span of values
  span <- max(compare, base, na.rm = T) - min(compare, base, na.rm = T)
  accu.lims <- diff.ylab.sec.accuracy / 100 * span * c(-3,3)
  diff_plot <-
    autoplot(diff, ylab = paste0("Difference (", diff.ylab, ")"), xlab = xlab) +
    expand_limits(y=accu.lims) +
    labs(title = diff.title, subtitle = paste0(base.desc, " - ", compare.desc)) +
    scale_y_continuous(sec.axis = sec_axis(~./span, name=paste0("Difference (", diff.ylab.sec, ")"), labels = scales::percent_format(accuracy = diff.ylab.sec.accuracy))) +
    geom_hline(yintercept=mean(diff, na.rm=T), linetype="dashed", color = "red")
  return(list(overlap_plot, diff_plot))
}



# We plot the dataset for a first visual inspection
month.starts <- seq.POSIXt(from=floor_date(seis.h.start, 'month'), to=ceiling_date(seis.h.end, 'month'), by = "1 month")
hours.from.start <- as.numeric(month.starts-seis.h.start)*24+seasonality.period
autoplot(seis.h.ts, type="l") +
  scale_x_continuous(breaks=hours.from.start/seasonality.period, labels=format(month.starts, "%m-%y")) +
  ylab("mean hourly movement (nm)") + xlab(NULL) + ggtitle("Training set") + labs(caption = "Data in original scale")
# We see that the fluctuations doesn't seem to increase with the trend, so we use an additive decomposition
dec.addi <- decompose(seasonality.aggregate(seis.h.ts))
autoplot(dec.addi) + labs(title="Decomposition of additive time series", subtitle = paste0("Seasonality ", seasonality.period)) + xlab(seasonality.xlab)



# We now apply three different filters to check which one behaves better
# since we are going to compare the results with the decomposed trend, we will apply the filters on the missing values-free dataset

# First we apply a simple linear MA filter
p <- floor(seasonality.period/2)
weights <- rep(1/(2*p+1), times=2*p+1)
trend <- stats::filter(seasonality.aggregate(seis.h.train), sides=2, filter = weights)
plots <- show_overlap_and_diff(
  dec.addi$trend, trend, xlab=seasonality.xlab,
  base.desc = "additive decomposition trend",
  compare.desc = paste0("linear filtered trend with p=", p),
  overlap.ylab = "Trend (nm)",
  diff.title = "Difference between trends",
  diff.ylab = "nm", diff.ylab.sec="% over trend span"
)
plot_grid(plots[[1]], plots[[2]], nrow = 2)

# Then we apply Spencer's 15-points weights MA filter
weights.s <- c(-3,-6,-5,3,21,46,67)
weights.s <- c(weights.s, 74, rev(weights.s))
weights.s <- weights.s/sum(weights.s)
trend.s <- stats::filter(seasonality.aggregate(seis.h.train), side=2, filter=weights.s)
plots <- show_overlap_and_diff(
  dec.addi$trend, trend.s, xlab=seasonality.xlab,
  base.desc = "additive decomposition trend",
  compare.desc = "Spencer's 15-point MA",
  overlap.ylab = "Trend (nm)",
  diff.title = "Difference between trends",
  diff.ylab = "nm", diff.ylab.sec="% over trend span",
  diff.ylab.sec.accuracy = .01
)
plot_grid(plots[[1]], plots[[2]], nrow = 2)

# Finally, we apply the seasonality-gnostic MA filter
f <- seasonality.period
weights.s <- c(0.5, rep(1,f-1), 0.5)/f
trend.s <- stats::filter(seasonality.aggregate(seis.h.train), side=2, filter=weights.s)
plots <- show_overlap_and_diff(
  dec.addi$trend, trend.s, xlab=seasonality.xlab,
  base.desc = "additive decomposition trend",
  compare.desc = paste0("shift-filtered trend with f=", f),
  overlap.ylab = "Trend (nm)",
  diff.title = "Difference between trends",
  diff.ylab = "nm", diff.ylab.sec="% over trend span",
  diff.ylab.sec.accuracy = .01
)
plot_grid(plots[[1]], plots[[2]], nrow = 2)



# We choose to keep the trend obtained from the seasonality-gnostic filter
# Then, we extract the 168-hours seasonality figure (and we repeat it for the whole dataset)
# using the mean-over-period technique.
detrended <- seasonality.aggregate(seis.h.train)-trend.s
detrended <- window(detrended, end = c(end(detrended)[1], seasonality.period), extend=T)
mat <- t(matrix(data = detrended, nrow = seasonality.period))
seasonality.figure <- as.ts(colMeans(mat, na.rm = T))
seasonality <- rep(seasonality.figure, length(detrended)/length(seasonality.figure))[1:length(trend.s)]
plots <- show_overlap_and_diff(
  dec.addi$figure, seasonality.figure, xlab=seasonality.xlab,
  base.desc = "additive decomposition",
  compare.desc = paste0("mean-over-period after removal of shift-filtered trend with f=", f),
  overlap.ylab = "Seasonality (nm)",
  diff.title = paste0("Difference between seasonalities (f=", seasonality.period, ")"),
  diff.ylab = "nm", diff.ylab.sec="% over seasonality span",
  diff.ylab.sec.accuracy = .01
)
plot_grid(plots[[1]], plots[[2]], nrow = 2)



# Then, we can extract the residuals
residuals <- seasonality.aggregate(seis.h.train) - trend.s - seasonality
plots <- show_overlap_and_diff(
  dec.addi$random, residuals, xlab=seasonality.xlab,
  base.desc = "additive decomposition",
  compare.desc = paste0("mean-over-period after removal of shift-filtered trend with f=", f),
  overlap.ylab = "Residuals (nm)",
  diff.title = paste0("Difference between residuals (f=", seasonality.period, ")"),
  diff.ylab = "nm", diff.ylab.sec="% over residuals span",
  diff.ylab.sec.accuracy = .01
)
plot_grid(plots[[1]], plots[[2]], nrow = 2)



# After that single extraction, we try extracting two seasonalities at once.
# Since the procedure is fixed, we define a couple of functions to better handle it.
# trend.s_compute only apply the seasonality-gnostic symmetric MA filter
trend.s_compute <- function(ts.in, period) {
  f <- period
  weights.s <- c(0.5, rep(1,f-1), 0.5)/f
  trend.s <- stats::filter(ts.in, side=2, filter=weights.s)
  return(trend.s)
}
# season_by_mean computes the seasonality figure using the mean-over-period methodology
# it also computes the standard deviation and produces the complete seasonality on all ts length
season_by_mean <- function(ts.in, period) {
  freq <- frequency(ts.in)
  if (freq == period) {
    end <- c(end(ts.in)[1], period)
  } else {
    end <- c(((end(ts.in)[1] * freq) %/% period + 1) * (period/freq), freq)
  }
  extended <- window(ts.in, end = end, extend=T)
  matrix <- t(matrix(data = extended, nrow = period))
  seasonality.figure <- as.ts(colMeans(matrix, na.rm = T))
  seasonality.sd <- as.ts(apply(matrix, 2, sd, na.rm = T))
  seasonality <- rep(seasonality.figure, length(extended)/length(seasonality.figure))[1:length(ts.in)]
  return(list(figure=seasonality.figure, seasonal=seasonality, sd=seasonality.sd))
}
# trend_season applies the two functions to compute both trend and seasonality with one call
trend_season <- function(ts.in, period) {
  trend.s <- trend.s_compute(ts.in, period)
  season <- season_by_mean(ts.in-trend.s, period)
  season$trend <- trend.s
  return(season)
}



# We now use those functions to extract the 24/168 hours seasonalities together
aggregated <- seasonality.aggregate(seis.h.train)
# first iteration
season.24 <- trend_season(aggregated, 24)
season.168 <- trend_season(aggregated-season.24$seasonal, 168)
# second iteration
season.24 <- trend_season(aggregated-season.168$seasonal, 24)
season.168 <- trend_season(aggregated-season.24$seasonal, 168)
# this third iteration isn't actually needed, but we apply it to check convergence
season.24 <- trend_season(aggregated-season.168$seasonal, 24)
season.168 <- trend_season(aggregated-season.24$seasonal, 168)
# We now plot the extracted components, just like decompose/stl methods
plot_grid(
  autoplot(aggregated) + labs(title="Timeseries", cex=0.7) + ylab("") + xlab("") + theme(plot.margin=margin(b=-6, unit="pt")),
  autoplot(ts(season.24$figure, frequency=1)) + labs(title="Daily seasonality", cex=0.7) + ylab("") + xlab("") + theme(plot.margin=margin(r=10, b=-6, unit="pt")),
  autoplot(ts(season.168$trend, frequency=168)) + labs(title="Trend", cex=0.7) + ylab("") + xlab("") + theme(plot.margin=margin(l=4, b=-6, unit="pt")),
  autoplot(ts(season.168$figure, frequency=1)) + labs(title="Weekly seasonality", cex=0.7) + ylab("") + xlab("") + theme(plot.margin=margin(r=10, b=-6, unit="pt")),
  autoplot(aggregated-season.24$seasonal-season.168$seasonal-season.168$trend) + labs(title="Residuals", cex=0.7) + ylab("") + xlab("") + theme(plot.margin=margin(t=-6, b=-6, unit="pt")),
  autoplot(season.168$figure + rep(season.24$figure, 7)) + labs(title="Weekly seasonality (sum)", cex=0.7) + ylab("") + xlab("") + theme(plot.margin=margin(r=10, b=-6, unit="pt")),
  nrow = 3,
  greedy = T)
# we can also directly extract the 24/168 hours seasonalities from the dataset, and then compare it with the multi extraction
seasonbase.24 <- trend_season(aggregated, 24)
seasonbase.168 <- trend_season(aggregated, 168)
plot_grid(
  autoplot(ts(seasonbase.24$figure, frequency=1)) + ylab("") + xlab("Hours") + scale_x_continuous() + geom_line(aes(col="Direct daily"), size=1.5) +
    geom_line(aes(y=season.24$figure, col="Multi daily")) +
    scale_colour_manual("", values=c("purple", "yellow"), breaks=c("Direct daily", "Multi daily")),
  autoplot(ts(seasonbase.168$figure, frequency=1)) + ylab("") + xlab("Hours") + scale_x_continuous() + geom_line(aes(col="Direct weekly"), size=1.5) +
    geom_line(aes(y=season.168$figure + rep(season.24$figure, 7), col="Multi w+d")) +
    geom_line(aes(y=season.168$figure, col="Multi weekly")) +
    scale_colour_manual("", values=c("red", "purple", "blue", "yellow", "green"), breaks=c("Direct weekly", "Direct daily", "Multi" %s+% c(" weekly", " daily", " w+d"))),
  nrow = 2)
# the composed 168 hours seasonality equal to the directly extracted one
plots <- show_overlap_and_diff(
  seasonbase.168$figure, season.168$figure + rep(season.24$figure, 7), xlab="Hours",
  base.desc = "168 from ds",
  compare.desc = "168+24 from ds",
  overlap.ylab = "Mean hourly movement (nm)",
  diff.title = "Difference between 168",
  diff.ylab = "nm", diff.ylab.sec="% over season span",
  diff.ylab.sec.accuracy = .01
)
plot_grid(plots[[1]], plots[[2]], nrow = 2)



# We now proceed to find the needed transformations for our dataset to choose the order of the model and estimate the parameters
# Since this must be done specifically for each of the two datasets, we start with what we need for the Uccle station, then we proceed with the Membach station.
# Please note that the variable names are the same, so the Membach-specific code will overwrite and clash with the Uccle-specific code.
# If at the start the Uccle data (UCCS) was chosen, then follow the code delimited for that station and skip the Membach-specific.
# Follow and skip the other code when Membach data (MEMS) was chosen at the start.



# To plot the forecasts, we will use this shorthand to see in the plot the Box-Pierce test result on the residuals
plot_forecast <- function(forec) {
  BP <- Box.test(forec$model$residuals, type="Box-Pierce")
  autoplot(forec) +
    labs(title=forec$method, subtitle = paste0("Box-Pierce p-value=", print_dec(BP$p.value, decimals=4))) +
    autolayer(forec$mean, series="Forecast") -> plotted
  return(plotted)
}
# To plot the Q-Q plot with ggplot we will use this shorthand
gg_qqplot <- function(dataset) {
  if (is.ts(dataset))
    dataset <- as.numeric(dataset)
  ggplot(data.frame(values=dataset), aes(sample=values)) +
    stat_qq_band() + stat_qq_line(col="red") + stat_qq_point(shape = 1, size = 3) +
    xlab("Theoretical quantiles (95% conf.)")
}



#################### Uccle station START ####################



# We check possible transformations, without obtaining significant better distributions
month.starts <- seq.POSIXt(from=floor_date(seis.train.start, 'month'), to=ceiling_date(seis.train.end, 'month'), by = "1 month")
hours.from.train.start <- as.numeric(month.starts-seis.train.start)*24+168
# base dataset
autoplot(seis.h.train, type="l") +
  scale_x_continuous(breaks=hours.from.train.start/168, labels=format(month.starts, "%m-%y")) +
  ylab("mean hourly movement (nm)") + xlab(NULL) + ggtitle("Training set") + labs(caption = "Data in original scale")
gghistogram(seis.h.train, add.normal = T) + xlab("mean hourly movement (nm)") + labs(caption = "Data in original scale")
gg_qqplot(seis.h.train) + 
  labs(title="Q-Q plot for training dataset") + ylab("Sample quantiles") + labs(caption = "Data in original scale")
# log transformation
autoplot(log(seis.h.train), type="l") +
  scale_x_continuous(breaks=hours.from.train.start/168, labels=format(month.starts, "%m-%y")) +
  ylab("mean hourly movement (nm)") + xlab(NULL) + ggtitle("Training set") + labs(caption = "Data in log-scale")
gghistogram(log(seis.h.train), add.normal = T) + xlab("mean hourly movement (nm)") + labs(caption = "Data in log-scale")
gg_qqplot(log(seis.h.train)) + 
  labs(title="Q-Q plot for training dataset") + ylab("Sample quantiles") + labs(caption = "Data in log-scale")
# Box-Cox transformation with auto estimation of lambda by R
train.bc.lambda <- BoxCox.lambda(seis.h.train)
train.bc <- BoxCox(seis.h.train, train.bc.lambda)
autoplot(train.bc, type="l") +
  scale_x_continuous(breaks=hours.from.train.start/168, labels=format(month.starts, "%m-%y")) +
  ylab("mean hourly movement (nm)") + xlab(NULL) + ggtitle("Training set") + labs(caption = paste0("Data Box-Cox transformed with lambda = ", train.bc.lambda))
gghistogram(train.bc, add.normal = T) + xlab("mean hourly movement (nm)") + labs(caption = paste0("Data Box-Cox transformed with lambda = ", train.bc.lambda))
gg_qqplot(train.bc) + 
  labs(title="Q-Q plot for training dataset") + ylab("Sample quantiles") + labs(caption = paste0("Data Box-Cox transformed with lambda = ", train.bc.lambda))



# We check the behaviour of ACF and PACF by applying different diff operators
seis.h.train %>% ggtsdisplay(lag.max = 168*4)
seis.h.train %>% diff() %>% ggtsdisplay(lag.max = 168*4)
seis.h.train %>% diff(lag=168) %>% ggtsdisplay(lag.max = 168*4)
seis.h.train %>% diff() %>% diff(lag=168) %>% ggtsdisplay(lag.max = 168*4)
# Since applying the diff(lag=168) seems necessary, but diff() could be not, we zoom the last two
plot_grid(
  seis.h.train %>% diff(lag=168) %>%  acf(lag.max = 24,    plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  seis.h.train %>% diff(lag=168) %>%  acf(lag.max = 168*4, plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  seis.h.train %>% diff(lag=168) %>% pacf(lag.max = 24,    plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  seis.h.train %>% diff(lag=168) %>% pacf(lag.max = 168*4, plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  nrow = 2
) # from this we will try ARIMA(2,0,0)(0,1,1)[168]
plot_grid(
  seis.h.train %>% diff() %>% diff(lag=168) %>%  acf(lag.max = 24,    plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  seis.h.train %>% diff() %>% diff(lag=168) %>%  acf(lag.max = 168*4, plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  seis.h.train %>% diff() %>% diff(lag=168) %>% pacf(lag.max = 24,    plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  seis.h.train %>% diff() %>% diff(lag=168) %>% pacf(lag.max = 168*4, plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  nrow = 2
) # from this we will try ARIMA(0,1,0)(0,1,1)[168]



# Next, we fit these models and some others suggested by different auto.arima calls and ACF/PACF analysis
to_test <- list(
  c(0,1,0,0,1,1),  # hint from acf/pacf with diff/diff(lag=168)
  c(0,1,0,0,1,2),  # after previous hint the ACF + PACF
  c(2,0,0,0,1,0),  # hint from auto.arima with fixed D=1
  c(2,0,0,0,1,1),  # hint from acf/pacf with diff(lag=168), and after hint from auto.arima with fixed D=1
  c(5,0,0,0,1,0),  # hint from auto.arima with fixed D=1
  c(3,1,1,0,1,0),  # hint from auto.arima with fixed D=1 and d=1
  c(3,1,1,0,1,1),  # after previous hint then ACF + PACF
  c(4,1,2,0,1,0),  # hint from auto.arima(d=1, D=1, max.p=5, max.q=2, max.P=5, max.Q=2, max.order=10)
  c(4,1,2,0,1,1)  # after previous hint then ACF + PACF
)
# this check, as similar code inside the loop, are meant for saving fitted models in an .RData for not refitting them each time
if (!exists("models")){
  models <- list()
}
# for each model orders, we fit the model, forecast the 18 test weeks, and then we plot and print various data for model evaluation
for (model in to_test) {
  # model <- c(4,1,2,0,1,0)
  fit.name <- paste0("fit", paste0(model, collapse = ""))
  forecast.name <- paste0("forecast", paste0(model, collapse = ""))
  cat("SARIMA(", paste0(model[1:3], collapse = ","), ")(", paste0(model[4:6], collapse = ","), ")[", frequency(seis.h.train), "]", sep = "")
  # fit the model if it was not fitted before
  if (!(fit.name %in% names(models))) {
    cat(" fitting...")
    models[[fit.name]] <- Arima(seis.h.train, model[1:3], model[4:6])
    cat("DONE")
  }
  # forecast the test period if it was not forecasted already
  if (!(forecast.name %in% names(models))) {
    cat(" forecasting...")
    models[[forecast.name]] <- forecast(models[[fit.name]], h=168*18)
    cat("DONE")
  }
  cat("\n")
  plotting.model <- models[[fit.name]]
  plotting.forecast <- models[[forecast.name]]
  # show model parameters and AIC/AICc/BIC values
  print(plotting.model)
  # plot overlap/difference with train data
  plots <- show_overlap_and_diff(
    seis.h.train, plotting.forecast$fitted, xlab=seasonality.xlab,
    base.desc = "training set",
    compare.desc = paste0("refit with ", plotting.forecast$method),
    overlap.ylab = "Mean hourly movement (nm)",
    diff.title = "Difference between ts",
    diff.ylab = "nm", diff.ylab.sec="%",
    diff.ylab.sec.accuracy = .01
  )
  print(plot_grid(plots[[1]], plots[[2]], nrow = 2))
  # check how residuals behave as distribution and as acf/pacf
  checkresiduals(plotting.model)
  print(gg_qqplot(plotting.model$residuals) + 
          labs(title=paste0("Q-Q plot for ", plotting.forecast$method, " residuals")) + ylab("Residuals quantiles")
  )
  residuals.acf <- autoplot(acf(plotting.model$residuals, na.action = na.pass, plot=F, lag.max=168*4)) + scale_x_continuous() + labs(title = paste0("Residuals of ", plotting.forecast$method))
  residuals.pacf <- autoplot(pacf(plotting.model$residuals, na.action = na.pass, plot=F, lag.max=168*4)) + scale_x_continuous() + labs(title = paste0("Residuals of ", plotting.forecast$method))
  print(plot_grid(residuals.acf, residuals.pacf, nrow=2))
  # also check zoomed residuals acf/pacf for more insights
  print(plot_grid(
    autoplot(acf(plotting.model$residuals, na.action = na.pass, plot=F, lag.max=24)) + scale_x_continuous() + labs(title = paste0("Residuals of ", plotting.forecast$method)),
    residuals.acf,
    autoplot(pacf(plotting.model$residuals, na.action = na.pass, plot=F, lag.max=24)) + scale_x_continuous() + labs(title = paste0("Residuals of ", plotting.forecast$method)),
    residuals.pacf,
    nrow = 2
  ))
  # plot forecasts
  print(plot_forecast(plotting.forecast) + autolayer(seis.h.test, series="Original") + ylab("Mean hourly movement (nm)"))
  # print stats about data falling inside 80% and 90% CIs
  cat("Test set inside the CI\n")
  cat("80% CI \t ", print_dec(mean(plotting.forecast$lower[,1] < seis.h.test & seis.h.test < plotting.forecast$upper[,1], na.rm = T)*100), "%\n")
  cat("95% CI \t ", print_dec(mean(plotting.forecast$lower[,2] < seis.h.test & seis.h.test < plotting.forecast$upper[,2], na.rm = T)*100), "%\n")
  # plot overlap/difference with test data
  plots <- show_overlap_and_diff(
    seis.h.test, plotting.forecast$mean, xlab=seasonality.xlab,
    base.desc = "test set",
    compare.desc = paste0("forecast with ", plotting.forecast$method),
    overlap.ylab = "Mean hourly movement (nm)",
    diff.title = "Difference between ts",
    diff.ylab = "nm", diff.ylab.sec="%",
    diff.ylab.sec.accuracy = .01
  )
  print(plot_grid(plots[[1]], plots[[2]], nrow = 2))
  # print accuracy statistics (different errors, mainly MAPE is considered)
  print(accuracy(plotting.forecast, seis.h.test))
}
# we finally set the chosen model, with Box-Cox corresponding lambda value (NULL since we are not transforming the data)
chosen.model <- c(2,0,0,0,1,1)
chosen.lambda <- NULL



#################### Uccle station END ####################



# The procedure for the Membach station is quite similar, but for clarity we are going to reproduce the code.
# The main differences are the initial transformation and ACF/PACF analysis, and then in the models cycle the added parameter lambda to Arima and forecast calls



#################### Membach station START ####################



# We check possible transformations, without obtaining significant better distributions
month.starts <- seq.POSIXt(from=floor_date(seis.train.start, 'month'), to=ceiling_date(seis.train.end, 'month'), by = "1 month")
hours.from.train.start <- as.numeric(month.starts-seis.train.start)*24+168
# base dataset
autoplot(seis.h.train, type="l") +
  scale_x_continuous(breaks=hours.from.train.start/168, labels=format(month.starts, "%m-%y")) +
  ylab("mean hourly movement (nm)") + xlab(NULL) + ggtitle("Training set") + labs(caption = "Data in original scale")
gghistogram(seis.h.train, add.normal = T) + xlab("mean hourly movement (nm)") + labs(caption = "Data in original scale")
gg_qqplot(seis.h.train) + 
  labs(title="Q-Q plot for training dataset") + ylab("Sample quantiles") + labs(caption = "Data in original scale")
# log transformation
autoplot(log(seis.h.train), type="l") +
  scale_x_continuous(breaks=hours.from.train.start/168, labels=format(month.starts, "%m-%y")) +
  ylab("mean hourly movement (nm)") + xlab(NULL) + ggtitle("Training set") + labs(caption = "Data in log-scale")
gghistogram(log(seis.h.train), add.normal = T) + xlab("mean hourly movement (nm)") + labs(caption = "Data in log-scale")
gg_qqplot(log(seis.h.train)) + 
  labs(title="Q-Q plot for training dataset") + ylab("Sample quantiles") + labs(caption = "Data in log-scale")
# Box-Cox transformation with auto estimation of lambda by R
train.bc.lambda <- BoxCox.lambda(seis.h.train)
train.bc <- BoxCox(seis.h.train, train.bc.lambda)
autoplot(train.bc, type="l") +
  scale_x_continuous(breaks=hours.from.train.start/168, labels=format(month.starts, "%m-%y")) +
  ylab("mean hourly movement (nm)") + xlab(NULL) + ggtitle("Training set") + labs(caption = paste0("Data Box-Cox transformed with lambda = ", train.bc.lambda))
gghistogram(train.bc, add.normal = T) + xlab("mean hourly movement (nm)") + labs(caption = paste0("Data Box-Cox transformed with lambda = ", train.bc.lambda))
gg_qqplot(train.bc) + 
  labs(title="Q-Q plot for training dataset") + ylab("Sample quantiles") + labs(caption = paste0("Data Box-Cox transformed with lambda = ", train.bc.lambda))



# We check the behaviour of ACF and PACF by applying different diff operators on the Box-Cox transformed data
train.bc %>% ggtsdisplay(lag.max = 168*4)
train.bc %>% diff() %>% ggtsdisplay(lag.max = 168*4)
train.bc %>% diff(lag=168) %>% ggtsdisplay(lag.max = 168*4)
train.bc %>% diff() %>% diff(lag=168) %>% ggtsdisplay(lag.max = 168*4)
# Since applying the diff(lag=168) seems necessary, but diff() could be not, we zoom the last two
plot_grid(
  train.bc %>% diff(lag=168) %>%  acf(lag.max = 24,    plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  train.bc %>% diff(lag=168) %>%  acf(lag.max = 168*4, plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  train.bc %>% diff(lag=168) %>% pacf(lag.max = 24,    plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  train.bc %>% diff(lag=168) %>% pacf(lag.max = 168*4, plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  nrow = 2
) # from this we will try ARIMA(2,0,0)(0,1,1)[168]
plot_grid(
  train.bc %>% diff() %>% diff(lag=168) %>%  acf(lag.max = 24,    plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  train.bc %>% diff() %>% diff(lag=168) %>%  acf(lag.max = 168*4, plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  train.bc %>% diff() %>% diff(lag=168) %>% pacf(lag.max = 24,    plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  train.bc %>% diff() %>% diff(lag=168) %>% pacf(lag.max = 168*4, plot = F, na.action = na.pass) %>% autoplot() + labs(title = NULL) + scale_x_continuous(),
  nrow = 2
) # from this we will try ARIMA(0,1,0)(0,1,1)[168]



# Next, we fit these models and some others suggested by ACF/PACF analysis
to_test <- list(
  c(0,1,0,0,1,1),  # from ACF/PACF of data with diff/diff(lag=168)
  c(0,1,1,0,1,1),  # after previous + ACF/PACF on residuals
  c(0,1,2,0,1,1),  # after previous + ACF/PACF on residuals
  c(2,0,0,0,1,1),  # from ACF/PACF of data with diff(lag=168)
  c(3,0,0,0,1,1)  # after previous + ACF/PACF on residuals
)
# this check, as similar code inside the loop, are meant for saving fitted models in an .RData for not refitting them each time
if (!exists("models")){
  models <- list()
}
# for each model orders, we fit the model, forecast the 18 test weeks, and then we plot and print various data for model evaluation
for (model in to_test) {
  # model <- c(4,1,2,0,1,0)
  fit.name <- paste0("fit", paste0(model, collapse = ""))
  forecast.name <- paste0("forecast", paste0(model, collapse = ""))
  cat("SARIMA(", paste0(model[1:3], collapse = ","), ")(", paste0(model[4:6], collapse = ","), ")[", frequency(seis.h.train), "]", sep = "")
  # fit the model if it was not fitted before
  if (!(fit.name %in% names(models))) {
    cat(" fitting...")
    models[[fit.name]] <- Arima(seis.h.train, model[1:3], model[4:6], lambda=train.bc.lambda)  # <-- note the added argument lambda=train.bc.lambda
    cat("DONE")
  }
  # forecast the test period if it was not forecasted already
  if (!(forecast.name %in% names(models))) {
    cat(" forecasting...")
    models[[forecast.name]] <- forecast(models[[fit.name]], h=168*18, lambda=train.bc.lambda)  # <-- note the added argument lambda=train.bc.lambda
    cat("DONE")
  }
  cat("\n")
  plotting.model <- models[[fit.name]]
  plotting.forecast <- models[[forecast.name]]
  # show model parameters and AIC/AICc/BIC values
  print(plotting.model)
  # plot overlap/difference with train data
  plots <- show_overlap_and_diff(
    seis.h.train, plotting.forecast$fitted, xlab=seasonality.xlab,
    base.desc = "training set",
    compare.desc = paste0("refit with ", plotting.forecast$method),
    overlap.ylab = "Mean hourly movement (nm)",
    diff.title = "Difference between ts",
    diff.ylab = "nm", diff.ylab.sec="%",
    diff.ylab.sec.accuracy = .01
  )
  print(plot_grid(plots[[1]], plots[[2]], nrow = 2))
  # check how residuals behave as distribution and as acf/pacf
  checkresiduals(plotting.model)
  print(gg_qqplot(plotting.model$residuals) + 
          labs(title=paste0("Q-Q plot for ", plotting.forecast$method, " residuals")) + ylab("Residuals quantiles")
  )
  residuals.acf <- autoplot(acf(plotting.model$residuals, na.action = na.pass, plot=F, lag.max=168*4)) + scale_x_continuous() + labs(title = paste0("Residuals of ", plotting.forecast$method))
  residuals.pacf <- autoplot(pacf(plotting.model$residuals, na.action = na.pass, plot=F, lag.max=168*4)) + scale_x_continuous() + labs(title = paste0("Residuals of ", plotting.forecast$method))
  print(plot_grid(residuals.acf, residuals.pacf, nrow=2))
  # also check zoomed residuals acf/pacf for more insights
  print(plot_grid(
    autoplot(acf(plotting.model$residuals, na.action = na.pass, plot=F, lag.max=24)) + scale_x_continuous() + labs(title = paste0("Residuals of ", plotting.forecast$method)),
    residuals.acf,
    autoplot(pacf(plotting.model$residuals, na.action = na.pass, plot=F, lag.max=24)) + scale_x_continuous() + labs(title = paste0("Residuals of ", plotting.forecast$method)),
    residuals.pacf,
    nrow = 2
  ))
  # plot forecasts
  print(plot_forecast(plotting.forecast) + autolayer(seis.h.test, series="Original") + ylab("Mean hourly movement (nm)"))
  # print stats about data falling inside 80% and 90% CIs
  cat("Test set inside the CI\n")
  cat("80% CI \t ", print_dec(mean(plotting.forecast$lower[,1] < seis.h.test & seis.h.test < plotting.forecast$upper[,1], na.rm = T)*100), "%\n")
  cat("95% CI \t ", print_dec(mean(plotting.forecast$lower[,2] < seis.h.test & seis.h.test < plotting.forecast$upper[,2], na.rm = T)*100), "%\n")
  # plot overlap/difference with test data
  plots <- show_overlap_and_diff(
    seis.h.test, plotting.forecast$mean, xlab=seasonality.xlab,
    base.desc = "test set",
    compare.desc = paste0("forecast with ", plotting.forecast$method),
    overlap.ylab = "Mean hourly movement (nm)",
    diff.title = "Difference between ts",
    diff.ylab = "nm", diff.ylab.sec="%",
    diff.ylab.sec.accuracy = .01
  )
  print(plot_grid(plots[[1]], plots[[2]], nrow = 2))
  # print accuracy statistics (different errors, mainly MAPE is considered)
  print(accuracy(plotting.forecast, seis.h.test))
}
# we finally set the chosen model, with Box-Cox corresponding lambda value
chosen.model <- c(2,0,0,0,1,1)
chosen.lambda <- train.bc.lambda



#################### Membach station END ####################


