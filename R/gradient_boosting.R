# Gradient Boosting -------------------------------------------------------

# this is our loss function
# y - the target
# yhat - the fitted values
loss <- function(y, yhat) return(mean(1/2*(y-yhat)^2))

# the derivative of our loss function is the gradient
# y - the target
# yhat - the fitted values
gradient <- function(y, yhat) {return(y - yhat)}

# grad_boost
#' Fit a boosted linear model
#'
#' @param formula an object of class formula
#' @param data a data.frame or matrix
#' @param nu a numeric within the range of [0,1], the learning rate
#' @param stop a numeric value determining the total boosting iterations
#' @param loss.fun a convex loss function which is continuously differentiable
#' @param grad.fun a function which computes the gradient of the loss function
#' @param yhat.init a numeric value determining the starting point
#'
#' @return \itemize{
#' \item theta - this is our estimator
#' \item u - this is the last gradient value
#' \item fit - our fitted values, i.e. X %*% theta
#' \item formula - the underlying formula
#' \item data - the underlying data
#' }
#' @export
#'
#' @examples # Complete runthrough see: www.github.com/andrebleier/cheapml
#'# Gradient Boosting Runthrough --------------------------------------------
#'
#'# load data simulation tool
#'# library(devtools)
#'# devtools::install_github('andrebleier/Xy')
#'library(Xy)
#'
#'# simulate data
#'sim <- Xy(n = 1000, # 1000 observations
#'         numvars = c(2, 0), # 2 linear variables / 0 nonlinear
#'         noisevars = 0, # no noise variables
#'         catvars = 0, # omit dummy variables
#'         stn = 30, # signal to noise ratio 30:1
#'         intercept = TRUE
#'         )
#'
#'# simulation overview
#'sim
#'
#'# get the formula
#'eq <- sim$eq
#'
#'# get the formula
#'model_df <- sim$data
#'
#'# source the function
#'source("algorithms/grad_boost.R")
#'
#'# fit
#'mod <- grad_boost(formula = eq, data = model_df,
#'                  nu = 0.1, stop = 100,
#'                  grad.fun = gradient,
#'                  loss.fun = loss)
#'
#'# our estimator
#'mod$theta
#'
#'# the true underlying effect
#'coef(sim)
#'
grad_boost <- function(formula, data, nu = 0.01, stop, 
                       grad.fun, loss.fun, yhat.init = 0) {
  
  # coerce to data.frame
  data <- as.data.frame(data)
  
  # handle formula
  formula <- terms.formula(formula)
  
  # get the design matrix
  X <- model.matrix(formula, data)
    
  # extract target
  y <- data[, as.character(formula)[2]]

  # initial fit
  fit <- yhat.init
  
  # initialize the gradient with yhat.init
  u <- grad.fun(y = y, yhat = fit)
  
  # initial working parameter (our theta)
  # this is just an empty body
  theta <- rep(0, ncol(X))
  
  # track loss
  loss <- c()
  
  # boost from 1 to stop
  for (i in 1:stop) {
    
    # estimate working parameter (via OLS)
    # theta_i = (X'X)^-1 X'y
    # This is the (base procedure) where you can literally place 
    # any optimization algorithm
    base_prod <- lm.fit(x = X, y = u)
    theta_i <- coef(base_prod)
  
    # update our theta
    theta <- theta + nu*as.vector(theta_i)
    
    # new fitted values
    fit <- fit + nu * fitted(base_prod)
    
    # update gradient
    u <- grad.fun(y = y, yhat = fit)
    
    # update loss 
    loss <- append(loss, loss.fun(y = y, yhat = fit))
  }  
  
  # naming the estimator
  names(theta) <- colnames(X)
  
  # define return
  return(list(theta = theta, u = u, fit = fit, loss = loss, 
              formula = formula, data = data))
}

