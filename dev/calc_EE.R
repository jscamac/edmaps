# Calculate the probability of one or more establishment events occurring
#' 
#' Calculate the probability of one or more establishment events occurring as as
#' a function of estimated leakage numbers and the probability a leakage event
#' could result in establishment.
#' @param n_events Numeric vector containing lower and upper bounds (95% CI) for
#'   the number of leakage events.
#' @param p_establish Numeric vector containing the lower and upper bounds (95%
#'   CI) for the probability a leakage event could result in an establishment.
#' @param nsims Integer. Number of samples to be taken from event and
#'   establishment distributions. Default = 100000.
#' @return A \code{data.frame} containing the possible number of incursions that
#'   may occur, and their corresponding probabilities.
#' @author James Camac (\email{james.camac@gmail.com})
#' @importFrom dplyr arrange
#' @importFrom stats plogis qlogis qnorm rbinom rlnorm rnorm rpois
#' @export
calc_EE <- function(n_events, p_establish, nsims = 100000) {
  
  if(length(n_events) != 2) {
    stop('n_events must be a vector of two values.')
  }
  if(length(p_establish) != 2) {
    stop('p_establish must be a vector of two values.')
  }
  
  ## Calculate leakage number
  log_mean <- mean(log(n_events))
  log_sd <- (log_mean - log(min(n_events)))/stats::qnorm(0.975)
  
  # Sample lambda from lognormal
  lambda <- stats::rlnorm(n = nsims, meanlog = log_mean, sdlog = log_sd)
  
  # Sample leakage from poisson using lambda
  n <- stats::rpois(n = nsims,lambda = lambda)
  
  ## Calculate establishment probability
  logit_mean <- mean(qlogis(p_establish))
  logit_sd <- (logit_mean - stats::qlogis(min(p_establish)))/stats::qnorm(0.975)
  
  # Sample probability from logit normal
  logit_p <- stats::rnorm(n=nsims, mean = logit_mean, sd = logit_sd)
  
  # Sample incursion events from binomial
  
  est <- stats::rbinom(n = nsims, size = n, prob = stats::plogis(logit_p))
  
  est <- table(est) / length(est)
  
  data.frame(N_incursions = as.integer(names(est)),
             probability = c(est))
  
}

