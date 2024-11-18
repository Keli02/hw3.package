#'hw3_lm
#'
#'Fitting Linear Models
#'
#'@param formula an object of class "formula": a symbolic description of the model to be fitted
#'@param data an data frame containing the variables in the model.
#'
#'@return coefficients
#'@return residuals
#'@return fitted.values
#'
#'@examples
#'hw3_lm(y~x1+x2,test_data)
#'
#'@export
#'

hw3_lm <- function(formula, data){
  # Identify the name of response variable
  response_var <- all.vars(formula)[attr(terms(formula), "response")]

  # Extract the data of response & explanatory variables
  y <- data[[response_var]]
  x <- model.matrix(formula, data)

  # Identify n and p of the model
  n <- nrow(x)
  p <- ncol(x)

  # Find the coefficients of the model
  bhat <- solve(t(x) %*% x) %*% t(x) %*% y

  # Residuals
  yhat <- x %*% bhat
  resid <- y - yhat
  min_resid <- min(resid)
  oneq_resid <- quantile(resid, 0.25, names = FALSE)
  med_resid <- median(resid)
  threeq_resid <- quantile(resid, 0.75, names = FALSE)
  max_resid <- max(resid)

  # Estimated sigma squared
  sigma2 <- t(resid) %*% resid / (n-p)

  # Variance and standard error
  var <- diag(solve(t(x) %*% x)) * c(sigma2)
  se <- sqrt(var)

  # Inference
  t_stat <- c(bhat / se)
  p_val <- c(2 * (1 - pt(q = abs(t_stat), df = n - p)))

  # Final Residuals
  final_resid <- c(Min = min_resid, `1Q` = oneq_resid,
                       Median = med_resid, `3Q` = threeq_resid,
                       Max = max_resid)

  # Final coefficients
  final_coef <- cbind(Estimate = c(bhat), `Std.Err` = se,
                      `t value` = t_stat, `Pr(>|t|)` = p_val)

  # Find residual standard error
  sse <- sum(resid^2)
  resid_se <- sqrt(sse / (n - p))
  df <- n - p

  # Create a list to store data
  model <- list(residuals = final_resid, coefficients = final_coef,
                fitted.values = yhat,
                residuals = resid, residual_se = resid_se, df = df,
                call = match.call())
  class(model) <- "hw3_lm"
  return(model)
}

print.hw3_lm <- function(x, ...) {
  cat("Call:\n")
  print(x$call)

  cat("\nResiduals:\n")
  print(x$residuals)

  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients, digits = 4, signif.stars = TRUE)

  cat("\nResidual standard error:", format(x$residual_se, digits = 4),
      "on", x$df, "degrees of freedom\n")
}

