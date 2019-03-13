##################################
##Title: make-shots-charts-script.R
##Description:
##Inputs:
##Outputs:
###################################

library(grid)
library(jpeg)
library(ggplot2)
library(dplyr)

shots_data <- read.csv("../workout01/data/shots-data.csv", stringsAsFactors = FALSE)

court_file <- "../workout01/images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))


dat <- shots_data[shots_data$name == "Andre Iguodala", ]
str(dat)
pdf(file = "../workout01/images/andre-iguodala-shot-chart.pdf", width = 6.5, height = 5)
iguodala_scatterplot <- ggplot(data = dat) +
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
print(iguodala_scatterplot)
dev.off()

dat <- shots_data[shots_data$name == "Draymond Green", ]
str(dat)
pdf(file = "../workout01/images/draymond-green-shot-chart.pdf", width = 6.5, height = 5)
green_scatterplot <- ggplot(data = dat) +
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()
print(green_scatterplot)
dev.off()


dat <- shots_data[shots_data$name == "Kevin Durant", ]
str(dat)
pdf(file = "../workout01/images/kevin-durant-shot-chart.pdf", width = 6.5, height = 5)
durant_scatterplot <- ggplot(data = dat) +
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
print(durant_scatterplot)
dev.off()

dat <- shots_data[shots_data$name == "Klay Thompson", ]
str(dat)
pdf(file = "../workout01/images/klay-thompson-shot-chart.pdf", width = 6.5, height = 5)
thompson_scatterplot <- ggplot(data = dat) +
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
print(thompson_scatterplot)
dev.off()

dat <- shots_data[shots_data$name == "Stephen Curry", ]
str(dat)
pdf(file = "../workout01/images/stephen-curry-shot-chart.pdf", width = 8, height = 7)
curry_scatterplot <- ggplot(data = dat) +
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
print(curry_scatterplot)
dev.off()


dat <- shots_data
pdf(file = "../workout01/images/gsw-shot-charts.pdf", width = 6.5, height = 5)
print(facet_wrap_paginate(iguodala_scatterplot, green_scatterplot))
      #durant_scatterplot, thompson_scatterplot, curry_scatterplot))
dev.off()
