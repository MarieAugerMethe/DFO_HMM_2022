# subset data
library(tidyverse)
library(lubridate)

# define IDs to include 
select_ID <- c(172064, 172062, 172066)
  
# import track data
tracks <- read_csv("../raw_data/narwhal_location_level_1.csv") %>% 
  filter(DeployID %in% select_ID) %>%  # filter IDs
  mutate(DeployID = PTT,
         time = paste(Date, Time) %>% dmy_hms() %>%  # add time column (date and time)
           round_date("1 mins")) %>%   # and round it to the nearest minute
  arrange(DeployID, time) %>% # sort data by ID than time 
  mutate(dt = difftime(lead(time), time, units="mins")) %>%  # calculate difference in time in minutes
  select(DeployID, time, dt, Longitude, Latitude, Loc.Class) %>% 
  filter(DeployID %in% select_ID)  # filter ID

# import dive data
dives <- read_csv("../raw_data/WC_series_4Ron.txt") %>% 
  {.[3662010:nrow(.),]} %>% 
  filter(DeployID %in% select_ID) %>%  # filter IDs
  mutate(time = paste(Day, Time) %>% dmy_hms()) %>%   # add time column (date and time)
  select(DeployID, time, Depth) %>% 
  arrange(DeployID, time) %>% # sort data by ID than time 
  mutate(dt = difftime(lead(time), time, units="mins"))  # calculate difference in time in minutes

# save data
write_csv(tracks, "data/tracks.csv")
write_csv(dives, "data/dives.csv")

# plot timeseries
plot(tracks$time, c(rep(1.1, sum(tracks$DeployID==172064)),
                    rep(2.1, sum(tracks$DeployID==172062)),
                    rep(3.1, sum(tracks$DeployID==172066))), pch=16, col = "red", ylim=c(0.9,3.2))
points(x = dives$time, c(rep(1, sum(dives$DeployID==172064)),
                         rep(2, sum(dives$DeployID==172062)),
                         rep(3, sum(dives$DeployID==172066))), pch=16)
