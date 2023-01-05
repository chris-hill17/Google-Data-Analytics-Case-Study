library(tidyverse)
library(readxl)
install.packages("rio")
library(rio)
install.packages("mosaic")
library(mosaic)
library(ggplot2)


#Merging all excel sheets into one
path <- "~/Desktop/Bike_Share_Case/xls/bike_share.xlsx"
bike_share <- import_list(path, rbind = TRUE)
bike_share[!duplicated(bike_share)]



######Cleaning######
bike_share_copy <- bike_share

bike_share_copy$started_at <- as.POSIXct(bike_share_copy$started_at, "%Y-%m-%d %H:%M:%S")
bike_share_copy$ended_at <- as.POSIXct(bike_share_copy$ended_at, "%Y-%m-%d %H:%M:%S")

#adding start hour to use for analysis
bike_share_copy <- bike_share_copy %>%  
  mutate(start_hour = strftime(bike_share_copy$ended_at, "%H"))
unique(bike_share_copy$start_hour)

bike_share_copy <- bike_share_copy %>% 
  mutate("ride_length" = as.numeric(bike_share_copy$ended_at - bike_share_copy$started_at) / 60)
summary(bike_share_copy$ride_length)

bike_share_copy <- bike_share_copy %>% 
  mutate("_file" = NULL) #get rid of extra column added by merging data

bike_share_copy <- bike_share_copy %>% 
  mutate(year_month = paste(strftime(bike_share_copy$started_at, "%Y"), "-",
                            strftime(bike_share_copy$started_at, "%m")))
unique(bike_share_copy$year_month)


#Exporting merged files
write.csv(bike_share_copy, "bike_share_merged.csv")



######Analysis######
summary(bike_share_copy[c("ride_length","member_casual","day_of_week")])

tally(~member_casual, data = bike_share_copy, margins = TRUE)
tally(~member_casual, data = bike_share_copy, margins = TRUE, format = "perc")

mean(~ride_length, data = bike_share_copy, na.rm = TRUE)
max(~ride_length, data = bike_share_copy, na.rm = TRUE)

#checking outliers 
ride_length_perc <- quantile(bike_share_copy$ride_length, seq(0, 1, by=0.05))
ride_length_perc

#Using 90% of the data
bike_share_90 <- bike_share_copy %>% 
  filter(ride_length >= as.numeric(ride_length_perc["5%"])) %>% 
  filter(ride_length <= as.numeric(ride_length_perc["95%"]))
print(paste("Removed", nrow(bike_share_copy) - nrow(bike_share_90), "rows as outliners" ))

ggplot(data = bike_share_copy) + geom_bar(mapping = aes(x=member_casual, fill = member_casual)) +
  labs(x = NULL, y = NULL, title = "Number of Members") 

ggplot(data = bike_share_copy) + geom_bar(mapping = aes(x=day_of_week, fill = member_casual)) +
  facet_grid(cols = vars(member_casual)) +
  labs(x="Day of Week", y = NULL, title = "Number of Member Types Per Weekday")

ggplot(data = bike_share_copy) + geom_bar(mapping = aes(y=year_month, fill=member_casual)) +
  facet_grid(cols = vars(member_casual)) +
  labs(title = "Ride Usage Per Month", y = "Month")

ggplot(data = bike_share_copy) + geom_bar(mapping = aes(x=start_hour, fill=member_casual)) +
  facet_grid(cols = vars(member_casual)) +
  labs(title = "Riders Per Hour", x= "Hour of the Day")

ggplot(data = bike_share_90) + geom_boxplot(mapping = aes(x=member_casual, y=ride_length, fill=member_casual)) +
  labs(title = "Ride Length Per Member Type", x=NULL, y="Ride Length")
bike_share_90 %>% 
  group_by(member_casual) %>% 
  summarize(mean = mean(ride_length), 
            "First_Quarter" = as.numeric(quantile(ride_length, .25)),
            "Median" = median(ride_length),
            "Third_Quarter" = as.numeric(quantile(ride_length, .75)),
            "IQR" = Third_Quarter - First_Quarter)

ggplot(data = bike_share_copy) + geom_bar(mapping = aes(x=rideable_type, fill=member_casual)) +
  facet_grid(cols = vars(member_casual)) +
  labs(title = "Rideable Type Distribution", x = NULL, y = NULL)
