# checks if input prob is a valid probability (between 0 and 1)
check_prob <- function(prob){
  if (0 <= prob & 1>= prob){
    return(TRUE)
  }
  stop("invalid prob value")
}

#checks if input trials is valid for number of trials
check_trials <- function(trials){

  if (trials - round(trials) == 0 & trials > 0){
    return(TRUE)
  }
  stop("invalid trials value")
}

#checks if input sucess is valid value for number of success
check_success <- function(success, trials){

    for (i in success){
      if (i > trials){
        stop("success cannot be greater than trials")
      }
      if (i < 0){
        stop("success must be positive")
      }
    }

  return(TRUE)
}


#returns expected value of binomial distribution
aux_mean <- function(trials, prob){
  return(trials * prob)
}

#returns variance of binomial distribution
aux_variance <- function(trials, prob){
  return(trials*prob*(1-prob))
}


#returns most likely number of success in n trials
aux_mode <- function(trials, prob){
  x <- (trials*prob) + prob
  if (x%%1 == 0){
    return(c(x, x-1))
  }
  return(floor(x))
}

aux_skewness <- function(trials, prob){
  return((1- 2*prob)/sqrt(aux_variance(trials, prob)))
}
aux_kurtosis <- function(trials, prob){
  t <- 1 - 6*prob*(1-prob)
  return(t/aux_variance(trials, prob))
}

#' @title bin_choose
#' @description finds number of combinations in which k success can occur in n trials
#' @param n number of trials
#' @param k number of success
#' @return number of combinations
bin_choose <- function(n, k){
  if (length(k) == 1){
    if (k > n){
      stop("k cannot be greater than n")
    }
    return(factorial(n)/(factorial(k)*factorial(n-k)))
  }
  result <- rep(0, length(k))
  for (i in 1:length(k)){
    if (k[i] > n){
      stop("k cannot be greater than n")
    }
    result[i] <- (factorial(n)/(factorial(k[i])*factorial(n-k[i])))
  }
  return(result)
}

#' @title bin_probability
#' @description finds number of combinations in which k success can occur in n trials
#' @param trials number of trials
#' @param success number of success
#' @param prob probability of success
#' @return number of combinations
bin_probability <- function(success, trials, prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)

  return(bin_choose(trials, success) * (prob^success)*((1-prob)^(trials - success)))

}

#' @title bin_distribution
#' @description creates data frame of binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return data.frame with two classes: c("bindis", "data.frame")
bin_distribution <- function(trials, prob){
  col_names <- c("success", "probability")
  success <- seq(0, trials, 1)
  probability <- bin_probability(0:trials, trials, prob)
  df <- data.frame(success, probability)
  return(df)
}
#' @export
plot.bindis <- function(dis){
  library(ggplot2)
  ggplot(dis, aes(x=success, y=probability))+
    geom_bar(stat="identity")
}

#' @title bin_cumulative
#' @description creates data frame of binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return data.frame with two classes: c("bincum", "data.frame")
bin_cumulative <- function(trials, prob){
  col_names <- c("success", "probability", "cumulative")
  success <- seq(0, trials, 1)
  probability <- bin_probability(0:trials, trials, prob)
  cumulative <- probability
  for (i in 2:length(cumulative)){
    cumulative[i] <- cumulative[i - 1] + cumulative[i]
  }
  df <- data.frame(success, probability, cumulative)
  return(df)
}

#' @export
plot.bincum <- function(cum){
  library(ggplot2)
  ggplot(cum, aes(x=success, y=cumulative))+
    geom_line(stat="identity")
}


#' @title bin_variable
#' @description creates object of class binvar
#' @param trials number of trials
#' @param prob probability of success
#' @return object with list of trials and probs
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  binvar <- list(trials="trials", prob="prob")
  class(binvar) <- binvar
  return(binvar)
}

#' @export
print.binvar(){
  return("Binomial variable \n Parameters \n -number of trials: " + trials +
           "-prob of success: " + prob)
}


