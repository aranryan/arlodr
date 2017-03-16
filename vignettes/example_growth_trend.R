library(nycflights13)
library(zoo)
library(tidyverse)
library(arlodr)

head(flights)

# Creates daily data for flights from three origin airports
# includes count of flights and average air time
# Overwrites February and December data with NA
dat_1 <- flights %>%
  select(year, month, day, origin, air_time) %>%
  # create date
  mutate(date = lubridate::ymd(paste(year,month,day,sep="_"))) %>%
  select(date, origin, air_time) %>%
  # fill NAs in air_time
  mutate(air_time = ifelse(is.na(air_time), 0, air_time)) %>%
  group_by(date, origin) %>%
  summarize(count=n(), air_time=mean(air_time)) %>%
  ungroup() %>%
  # introduce NAs
  mutate(count = ifelse(lubridate::month(date) %in% c(2,12), NA, count)) %>%
  mutate(air_time = ifelse(lubridate::month(date) %in% c(2,12), NA, air_time))

head(dat_1)



dat_2 <- dat_1 %>%
  # this gives a trend in days, essentially days since the origin
  # date in lubridate
  mutate(period = lubridate::as.period(lubridate::interval(as.Date(lubridate::origin), date), unit="days")) %>%
  mutate(trend_days = as.numeric(period, "days")) %>%
  select(-period)

head(dat_2)

# put in a long format, convert value to log
dat_3 <- dat_2 %>%
  gather(variable, value, -date, -origin, -trend_days) %>%
  mutate(y = log(value)) %>%
  mutate(x = trend_days)

# adds a slope column
# The width argument is the number of observations being used in the rolling window.
# The partial is a number that provides the minimum window to attempt a calculation.
# So if we set it at 5, it will attempt the calculation when there are at least 5 data
# points to input.
# We've set February data to be missing. So this keeps estimating a slope even when it
# gets into the missing data, but once you get to the 14th of February, you only have
# one data point and you get a NA result. Those continue until you get to the second of
# March, where you have two data points (and 5 in the window, even though they are NA).
# I went down a long road trying to develop this approach that could be used on a grouped
# data frame. Didn't seem like it should be that difficult. I think one problem is that
# I don't have a great sense of what gets returned by rollapply. In any case,
# the following approach works for a data frame that has some grouping variables, and
# then has been set up to have a y variable already in log terms, and a x variable that
# is a time trend. It handles converting to an annualized growth rate by using the
# periods_year input.

dat_4 <- dat_3 %>%
  group_by(origin, variable) %>%
  do({
    slope <- rollapply(.[c("x", "y")],width = 15, arlodr::log_growth_1, periods_year=365.25,
                       by.column = FALSE, align = "right", fill = NA, partial=5)
    data.frame(., slope)
  })



# example without using function
dat_4 <- dat_3 %>%
  group_by(origin, variable) %>%
  do({
    slope <- zoo::rollapply(.[c("x", "y")],width = 50, function(m) coef(lm(formula = y ~ x, data = as.data.frame(m)))[2],
                            by.column = FALSE, align = "right", fill = NA)
    data.frame(., slope)
  })









