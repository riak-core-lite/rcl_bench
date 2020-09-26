#!/usr/bin/env Rscript
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

args <- commandArgs(trailingOnly = TRUE)

op_name <- args[1]
csv_path <- args[2]
img_path <- args[3]
d <- read.csv(csv_path)

d$timestamp <- d$timestamp / 1000
d$timestamp <- as.POSIXct(d$timestamp, origin="1970-01-01")

d$tc <- cut(d$timestamp, breaks = "1 sec")
d = count(d, unit, tc)
d$tc <- as.POSIXct(d$tc, format="%Y-%m-%d %H:%M:%S")

ggplot(d, aes(x=tc, y=n)) + 
	geom_point(size=1)  +
	geom_smooth() +
	labs(title=paste(op_name, "throughput"), x = "Time", y = "Operations per second")
ggsave(img_path) 
