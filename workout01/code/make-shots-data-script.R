##################################
##Title: make-shots-data-script.R
##Description:
##Inputs:
##Outputs:
###################################

iguodala <- read.csv("../workout01/data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../workout01/data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../workout01/data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../workout01/data/klay-thompson.csv", stringsAsFactors = FALSE)
curry <- read.csv("../workout01/data/stephen-curry.csv", stringsAsFactors = FALSE)

iguodala$name <- "Andre Iguodala"
green$name <- "Draymond Green"
durant$name <- "Kevin Durant"
thompson$name <- "Klay Thompson"
curry$name <- "Stephen Curry"


       
iguodala[iguodala == "n"] <- "shot_made_no"
iguodala[iguodala == "y"] <- "shot_made_yes"
green[green == "n"] <- "shot_made_no"
green[green == "y"] <- "shot_made_yes"
durant[druant == "n"] <- "shot_made_no"
durant[durant == "y"] <- "shot_made_yes"
thompson[thompson == "n"] <- "shot_made_no"
thompson[thompson == "y"] <- "shot_made_yes"
curry[curry == "n"] <- "shot_made_no"
curry[curry == "y"] <- "shot_made_yes"

#minute = period * 12 - minutes_remaining
iguodala$period_12 <- as.integer(iguodala$period * 12)
iguodala$minute <- iguodala$period_12 - iguodala$minutes_remaining
green$period_12 <- as.integer(green$period * 12)
green$minute <- green$period_12 - green$minutes_remaining
durant$period_12 <- as.integer(durant$period * 12)
durant$minute <- durant$period_12 - durant$minutes_remaining
thompson$period_12 <- as.integer(thompson$period * 12)
thompson$minute <- thompson$period_12 - thompson$minutes_remaining
curry$period_12 <- as.integer(curry$period * 12)
curry$minute <- curry$period_12 - curry$minutes_remaining

sink(file="../workout01/output/andre-iguodala-summary.txt")
summary(iguodala)
sink()

