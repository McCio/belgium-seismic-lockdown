
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


