#!/usr/bin/env Rscript
library(ggplot2)
args <- commandArgs(trailingOnly = TRUE)

op_name <- args[1]
csv_path <- args[2]
img_path <- args[3]
d <- read.csv(csv_path)

d$timestamp <- d$timestamp / 1000
d$timestamp <- as.POSIXct(d$timestamp, origin="1970-01-01")
d$microseconds[d$microseconds < 500] <- 501

ggplot(d, aes(x=timestamp,y=microseconds)) + 
	geom_point(size=0.3, stroke=0) + 
	geom_smooth() + 
	scale_y_log10(breaks=c(500,1000,10000,102997,105000), limits = c(500,105000), expand = c(0, 0)) +
	labs(title=paste(op_name, "latency"), x="Time in seconds",  y="Latency in microseconds")
ggsave(img_path)
