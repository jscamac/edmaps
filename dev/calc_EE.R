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
#' @export
calc_EE <- function(n_events, p_establish, nsims = 100000) {
  ## Calculate leakage number
  log_mean <- sum(log(n_events))/2
  log_sd <- (log_mean - log(min(n_events)))/1.96
  
  # Sample lambda from lognormal
  lambda <- rlnorm(n = nsims, meanlog = log_mean, sdlog = log_sd)
  
  # Sample leakage from poisson using lambda
  n <- rpois(n = nsims,lambda = lambda)
  
  ## Calculate establishment probability
  logit_mean <- sum(qlogis(p_establish))/2
  logit_sd <- (logit_mean - qlogis(min(p_establish)))/1.96
  
  # Sample lambda from lognormal
  logit_p <- rnorm(n=nsims, mean = logit_mean, sd = logit_sd)
  
  # Sample incursion events from binomial
  
  est <- rbinom(n = nsims, size = n, prob = plogis(logit_p))
  
  est <-table(est) / length(est)
  
  data.frame(N_incursions = as.integer(names(est)),
             probability = as.numeric(est)) %>%
    dplyr::arrange()
  
}