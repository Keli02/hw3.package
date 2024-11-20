#' Fitting Linear Models
#'
#' \code{hw3_lm} is used to fit linear regression model and return a summary table including the coefficients, residuals, and R-squared of the model.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted
#' @param data an data frame containing the variables in the model.
#'
#' @return \item{coefficients}{a named vector of coefficients.}
#' @return \item{residuals}{the residuals.}
#' @return \item{fitted.values}{the fitted mean values.}
#' @return \item{call}{the matched call.}
#'
#' @examples
#' hw3_lm(mpg ~ wt + hp + cyl, data = mtcars)
#' hw3_lm(mpg ~ wt + hp * cyl, data = mtcars)
#'
#' @export
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
  resid <- as.numeric(y - yhat)
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

  # Final residuals
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

  # Find multiple & adjusted R squared
  ybar <- mean(y)
  ssr <- sum((yhat - ybar)^2)
  m_r2 <- ssr / (ssr + sse)
  a_r2 <- 1 - ((1 - m_r2) * (n - 1) / (n - p))

  # Find F-statistic
  msr <- ssr / (p - 1)
  mse <- sse / df
  fstat <- msr / mse
  f_p_val <- 1 - pf(fstat, df1 = p - 1, df2 = df)

  # Create a list to store data
  model <- list(residuals = resid, resid_sum = final_resid,
                coefficients = final_coef, fitted.values = yhat,
                residual_se = resid_se, df = df,
                m_r2 = m_r2, a_r2 = a_r2, fstat = fstat,
                f_p_val = f_p_val, call = match.call())
  class(model) <- "hw3_lm"
  return(model)
}

#' print.hw3_lm
#'
#' Prints a summary of the hw3_lm object
#'
#' @param x An object of class "hw3_lm".
#' @param ... Additional arguments passed to the print function.
#'
#' @export
print.hw3_lm <- function(x, ...) {
  cat("Call:\n")
  print(x$call)

  cat("\nResiduals:\n")
  print(x$resid_sum)

  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients, digits = 4, signif.stars = TRUE)

  cat("\nResidual standard error:", format(x$residual_se, digits = 4),
      "on", x$df, "degrees of freedom\n")

  cat("Multiple R-squared: ", format(x$m_r2, digits = 4),
      ",  Adjusted R-squared: ", format(x$a_r2, digits = 4))

  cat("\nF-statistic:", format(x$fstat, digits = 4), "on",
      ncol(x$coefficients) - 1, "and", x$df,
      "DF,  p-value:", format(x$f_p_val, digits = 4), "\n")}
