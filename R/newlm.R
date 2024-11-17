# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hw3_lm <- function(formula, data){
  # Identify the name of response variable
  response_var <- all.vars(formula)[attr(terms(formula), "response")]
  # Extract the data of response & explanatory variables
  y <- data[[response_var]]
  x <- model.matrix(formula, data)
  # Identify coefficients of the model
  coef <- solve(t(x) %*% x) %*% t(x) %*% y
  model <- list(coefficients = coef, fitted.values = x %*% coef,
                residuals = y - x %*% coef, call = match.call())
  class(model) <- "hw3_lm"
  return(model)
}

print.hw3_lm <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}






