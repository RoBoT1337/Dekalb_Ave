library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(lubridate)

Joint <- read.csv(".../Joint.csv")

Joint[,1] <- NULL

Joint <- Joint %>% gather("Direction", "Cars", 3:4)
Joint$Time <- as.POSIXct(strptime(Joint$Time, "%I:%M %p"))
Joint_spr <- spread(Joint, Date, Cars)

Joint_spr$avg <- ((Joint_spr[,3]) +
    (Joint_spr[,4]) +
    (Joint_spr[,5]) +
    (Joint_spr[,6]) +
    (Joint_spr[,7])) /5
Joint_spr$labels <- strftime(Joint_spr$Time, format = "%H:%M")
Joint_spr$labels <- factor(Joint_spr$labels, levels = unique(Joint_spr$labels))

traf <- ggplot(Joint_spr, aes(x=labels, group=Direction, y=avg)) +
  #geom_point(aes(color=Direction)) +
  geom_smooth(aes(color = Direction), method = "loess", span=0.2) +
  #geom_bar(aes(fill = Direction), position = "dodge", stat = "identity") +
  scale_x_discrete(breaks=c("00:00","03:00","06:00","09:00","12:00","15:00","18:00","21:00","24:00")) +
  labs(x="Time",y="Number of Cars",title="Dekalb Ave. Average Weekday 15-minute Interval Car Count, 2017 (n=95770)")
traf
#ggsave("...png", traf, width =10, height = 8)
