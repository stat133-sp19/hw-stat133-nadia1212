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
  n <- round((trials*prob) + prob)
  return( n - 1)
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
  if (k > n){
    stop("k cannot be greater than n")
  }
  return(factorial(n)/(factorial(k)*factorial(n-k)))
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
  check_success(success)

  return(bin_choose(trials, success) * (prob^success)*((1-prob)^(trials - success)))

}



