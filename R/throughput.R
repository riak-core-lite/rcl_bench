#!/usr/bin/env Rscript
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

title <- "put"
d <- read.csv(paste("../current/",title,"_single.csv", sep=""))

d$timestamp <- d$timestamp / 1000
d$timestamp <- as.POSIXct(d$timestamp, origin="1970-01-01")

d$tc <- cut(d$timestamp, breaks = "1 sec")
d = count(d, unit, tc)
d$tc <- as.POSIXct(d$tc, format="%Y-%m-%d %H:%M:%S")

ggplot(d, aes(x=tc, y=n)) + 
	geom_point(size=1)  +
	geom_smooth() +
	labs(title=paste(title, "throughput"), x = "Time", y = "Operations per second")
ggsave(paste(title, "throughput.png", sep="_")) 
