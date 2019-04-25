# checks if input prob is a valid probability (between 0 and 1)
check_prob <- function(prob){
  if (0 <= prob and 1>= prob){
    return(TRUE)
  }
  stop("invalid prob value")
}

#checks if input trials is valid for number of trials
check_trials <- function(trials){
  if (trials > 0){
    return(TRUE)
  }
  stop("invalid trials value")
}

#checks if input sucess is valid value for number of success
check_success <- function(success, trials){
  if (0){
    return(TRUE)
  }
  stop("invalid success value")
}


#returns expected value of binomial distribution
aux_mean <- function(trials, prob){
  return(trials * prob)
}

#returns variance of binomial distribution
aux_variance <- function(trials, prob){
  return(trials*prob*(1-prob))
}

#returns most like number of success in n trials
aux_mode <- function(trials, prob){
  return()
}
aux_skewness <- function(trials, prob){
  return((1- 2*prob)/sqrt(aux_variance(trials, prob)))
}
aux_kurtosis <- function(trials, prob){
  t <- 1 - 6*prob*(1-prob)
  return(t/aux_variance(trials, prob))
}


bin_choose <- function(n, k){

}

bin_probability <- function(sucess, trials, prob){
}

bin_distribution <- function(trials, prob){


}

plot.bindis <- function(){

}

bin_cumulative <- function(trials, prob){

}

plot.bincum(){

}

bin_variable <- function(trials, prob){

}
print.binvar <- function(){

}

summary.binvar <- function(){

}

print.summary.binvar <- function(){

}
bin_mean <- function(){

}

bin_variance <- function(){

}
bin_mode
bin_skewness
bin_kurtosis

