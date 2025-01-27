library(marinecs100b)

# Extracting data ---------------------------------------------------------

# P1 How did you extract the temperature and exposure from the hottest day?
# Copy-paste the code here.
install.packages("tidyverse")
install.packages("lubridate")
library(lubridate)



kefj_interval <- kefj_datetime[2:length(kefj_datetime)]-kefj_datetime[1:length(kefj_datetime)-1]
table(kefj_interval)
hottest_idx <- which.max(kefj_temperature)
hottest_time <- kefj_datetime[hottest_idx]
hottest_day <- as_date(hottest_time)
hottest_site <- kefj_site[hottest_idx]
hotday_start <- as.POSIXct(paste(hottest_day, "0:00"), tz = "Etc/GMT+8")
hotday_end <- as.POSIXct(paste(hottest_day +1, "0:00"), tz = "Etc/GMT+8")
#put day end as start of next day because otherwise it gets cut off early
hotday_idx <- which(kefj_site == hottest_site &
                      kefj_datetime >= hotday_start &
                      kefj_datetime <= hotday_end)
hotday_datetime <- kefj_datetime[hotday_idx]
hotday_temperature <- kefj_temperature[hotday_idx]
hotday_exposure <- kefj_exposure[hotday_idx]
plot_kefj(hotday_datetime, hotday_temperature, hotday_exposure)
#for some reason the exposure at the end of the day does not show up on the plot
#but the same code works fine for the cold section
hotday_exposure

# Writing a utility function ----------------------------------------------
table(kefj_site)
# P2: Fill in the blanks below to write the Alaskan datetime utility function.

time_function <- function(datetime) {
  return(as.POSIXct(datetime, tz = "Etc/GMT+8"))
}

time_in_day <- function(site, date){
  start <- as.POSIXct(paste(time_function(date), "0:00"), tz = "Etc/GMT+8")
  end <- as.POSIXct(paste(time_function(date), "23:59"), tz = "Etc/GMT+8")
  idx <- which(kefj_site == site &
                        kefj_datetime >= start &
                        kefj_datetime <= end)
  return(idx)
}
# P3: Make a copy of your code from P1 and edit it to plot the temperature and
# exposure for "Aialik" on 2012-06-01
P3_start <- as.POSIXct(paste("2012-06-01", "0:00"), tz = "Etc/GMT+8")
P3_end <- as.POSIXct(paste("2012-06-01", "23:59"), tz = "Etc/GMT+8")
P3_idx <- which(kefj_site == "Aialik" &
                      kefj_datetime >= P3_start &
                      kefj_datetime <= P3_end)
P3_datetime <- kefj_datetime[P3_idx]
P3_temperature <- kefj_temperature[P3_idx]
P3_exposure <- kefj_exposure[P3_idx]
plot_kefj(P3_datetime, P3_temperature, P3_exposure)

# P4: Make a copy of your code from P3 and edit it to plot the temperature and
# exposure for "Harris" on 2016-04-05.
P4_start <- as.POSIXct(paste("2016-04-05", "0:00"), tz = "Etc/GMT+8")
P4_end <- as.POSIXct(paste("2016-04-05", "23:59"), tz = "Etc/GMT+8")
P4_idx <- which(kefj_site == "Harris" &
                  kefj_datetime >= P4_start &
                  kefj_datetime <= P4_end)
P4_datetime <- kefj_datetime[P4_idx]
P4_temperature <- kefj_temperature[P4_idx]
P4_exposure <- kefj_exposure[P4_idx]
plot_kefj(P4_datetime, P4_temperature, P4_exposure)

# P5: Compare your solutions for P3 and P4 - what variables changed?
#datetime and location changed

#P6
#name: temp_extract
#parameters: datetime and place


# Writing extraction functions --------------------------------------------

# P7 Write your temperature extraction function here
temp_extract <- function(place, datetime){
  idx <- time_in_day(place, datetime)
  return(kefj_temperature[idx])
}

time_function <- function(datetime) {
  return(as.POSIXct(datetime, tz = "Etc/GMT+8"))
}

time_in_day <- function(site, date){
  start <- as.POSIXct(paste(time_function(date), "0:00"), tz = "Etc/GMT+8")
  end <- as.POSIXct(paste(time_function(date), "23:59"), tz = "Etc/GMT+8")
  idx <- which(kefj_site == site &
                 kefj_datetime >= start &
                 kefj_datetime <= end)
  return(idx)
}

# P8
#Write your exposure extraction function here
exposure_extract <- function(place, datetime){
  idx <- time_in_day(place, datetime)
  return(kefj_exposure[idx])
}

# Write your datetime extraction function here
datetime_extract <- function(place, date){
  idx <- time_in_day(place, date)
  return(kefj_datetime[idx])
}


#P9 Export your annotated screenshot as a JPEG called "annotated_function.jpg"
# and add it to your copy of the module repository. (It should be in the same
# folder as this file.)

# P10 Visualize Nuka Pass temperatures on 2018-07-01
plot <- function(place, date){
  return(plot_kefj(datetime_extract(place, date), temp_extract(place, date), exposure_extract(place, date)))
}
plot("Nuka_Pass", "2018-07-01")

# P11: Save a copy of the Nuka Pass plot as "nuka_pass_2018-07-01.png" in this
# repo


# P12: Compare the code you wrote to create the plot in this module to the code
# you wrote in PROG101. Qualitatively, how do they compare? Which one is easier
# to read and why?
#This code is much more organized and concise than what we wrote in PROG101.
#This code is easier to read and easy to repeat with different parameters




# Functions within functions (optional) -----------------------------------

# How would you parameterize the visualization code into its own function?
# Specifically, what would you call that function and its parameters?

# Write your wrapper function for plot_kefj(). Call it to compare the
# visualizations for Nuka Pass on July 1, 2018 versus January 1, 2018.
