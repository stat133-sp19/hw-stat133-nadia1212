##################################
##Title: make-shots-charts-script.R
##Description:
##Inputs:
##Outputs:
###################################

library(grid)
library(jpeg)
library(ggplot2)


shots_data <- read.csv("../workout01/data/shots-data.csv", stringsAsFactors = FALSE)

court_file <- "../workout01/images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))
str(shots_data)
klay_scatterplot <- ggplot(data = shots_data[shots_data$name == "Klay Thompson"]) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()




