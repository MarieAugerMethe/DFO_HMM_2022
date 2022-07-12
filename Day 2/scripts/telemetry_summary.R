# summarize and evaluate data quality
# setup work space
library(tidyverse)
library(lubridate)

# location data
{
  # import location data 
  tracks <- read_csv("../raw_data/narwhal_location_level_1.csv") %>% 
    mutate(DeployID = PTT,
           time = paste(Date, Time) %>% dmy_hms() %>%  # add time column (date and time)
             round_date("1 mins")) %>%   # and round it to the nearest minute
    arrange(DeployID, time) %>% # sort data by ID than time 
    mutate(dt = difftime(lead(time), time, units="mins")) %>%  # calculate difference in time in minutes
    select(DeployID, time, dt, Longitude, Latitude, Loc.Class) 
  
  # explore location frequency and identify temporal resolution
  {
    # plot histogram 
    hist(as.numeric(tracks$dt) %>% {.[.>0]}, # omit values <= 0
         1000000, xlim = c(0,120), main = "", xlab = "time difference (m)")
    
    # identify the most frequent dt
    tracks %>% 
      filter(dt > 0) %>% 
      {table(.$dt)} %>% 
      {sort(., decreasing = T)[1:10]}
    
    # define resolution to use
    # here, I selected 10 minutes as it is relatively high resolution, 
    # and represents a significant number of the time steps 
    res_m <- 10
  }
  # summarise
  track_summary <-
    tracks %>% group_by(DeployID) %>% 
    filter(dt > 0) %>% # remove duplicate time (just for this stage)
    summarise(t_0 = first(time),  # first location
              t_max = last(time),  # last location
              n_loc = n(), # number of locations
              res_m = res_m, # resolution 
              n_day = length(unique(round_date(time, "1 day"))),  # number of days with locations
              max_n_loc = length(seq(t_0, t_max, by = as.difftime(res_m, units="mins"))),  # max n of locs given date range and res_h
              n_NA = max_n_loc - n_loc,  # number of missing locations
              p_NA = n_NA/max_n_loc)  # proportion of missing locations
  
  # save as csv 
  write_csv(track_summary, "data/track_summary.csv")
}
# #  172064 172063 172069 172066 172062
# dat <- which(dives$DeployID==172062)
# plot(dat)
# dat.1 <- dat[dat>3400000]
# plot(dat.1)
# depth data
{
  # import dive data 
  dives <- read_csv("../raw_data/WC_series_4Ron.txt") %>% 
    {.[3662010:nrow(.),]} %>% 
    filter(DeployID %in% track_summary$DeployID) %>%  # filter only tags with location data
    mutate(time = paste(Day, Time) %>% dmy_hms()) %>%   # add time column (date and time)
    select(DeployID, time, Depth) %>% 
    arrange(DeployID, time) %>% # sort data by ID than time 
    mutate(dt = difftime(lead(time), time, units="mins"))  # calculate difference in time in minutes
  
  # explore location frequency and identify temporal resolution
  {
    # plot histogram 
    hist(as.numeric(dives$dt) %>% {.[.>0]}, # omit values <= 0
         1000000, xlim = c(0,5), main = "", xlab = "time difference (m)")
    
    # identify the most frequent dt
    dives %>% 
      filter(dt > 0) %>% 
      {table(.$dt)} %>% 
      {sort(., decreasing = T)[1:10]}
    
    # define resolution to use
    res_m <- 1.25  
    }
  # summarise
  dive_summary <-
    dives %>% group_by(DeployID) %>% 
    filter(dt > 0) %>% # remove duplicate time (just for this stage)
    summarise(t_0 = first(time),  # first location
              t_max = last(time),  # last location
              n_loc = n(), # number of locations
              res_m = res_m, # resolution 
              n_day = length(unique(round_date(time, "1 day"))),  # number of days with locations
              max_n_loc = length(seq(t_0, t_max, by = as.difftime(res_m, units="mins"))),  # max n of locs given date range and res_h
              n_NA = max_n_loc - n_loc,  # number of missing locations
              p_NA = n_NA/max_n_loc)  # proportion of missing locations
 
  
  # save as csv 
  write_csv(dive_summary, "data/dive_summary.csv")
}

# top tags
{
  # sort by preportion of missing locations
  sprintf("based on locations frequency, the top 5 tags are %s",
          paste(arrange(track_summary, p_NA)$DeployID[1:5], collapse = ", "))
  
  # sort by preportion of missing locations
  sprintf("based on dive frequency, the top 5 tags are %s",
          paste(arrange(dive_summary, p_NA)$DeployID[1:5], collapse = ", "))
  
  arrange(track_summary, p_NA)$DeployID[1:5]
  arrange(dive_summary, p_NA)$DeployID[1:5]
  
  # 172064 5 2 = 7
  # 172063 4 1 = 5
  # 172069 3
  # 172066 2 3 = 5
  # 172062 1 5 = 6
  # 172067   4
}
