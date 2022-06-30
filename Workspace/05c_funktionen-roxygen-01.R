#' main estimation function
#'
#' Takes in a matrix x of predictors and computes linear model
#' of y on x.
#' @param x numeric perdictor matrix
#' @param y numeric vector. Dependent variable.
#' @return List of 4 elements
#'  \itemize{
#'     \item coefficients coefficients of parameter estimation
#'     \item vcov variance covaraiance matrix
#'     \item sigma residual standard deviation
#'     \item df degrees of freedom
#'  }
#' @export
linmodEst <- function(x, y){
  ## compute QR-decomposition of x
  qx <- qr(x)

  ## compute (x'x)^(-1) x'y
  coef <- solve.qr(qx, y)

  ## degrees of freedom and standard deviation of residuals
  df <- nrow(x)-ncol(x)
  sigma2 <- sum((y - x%*%coef)^2)/df

  ## compute sigma^2 * (x'x)^-1
  vcov <- sigma2 * chol2inv(qx$qr)
  colnames(vcov) <- rownames(vcov) <- colnames(x)

  list(coefficients = coef,
       vcov = vcov,
       sigma = sqrt(sigma2),
       df = df)
}

# Setter-Method
linmod <- function(x, ...) UseMethod("linmod")

linmod.default <- function(x, y, ...){
  x <- as.matrix(x)
  y <- as.numeric(y)

  est <- linmodEst(x, y)

  est$fitted.values <- as.vector(x %*% est$coefficients)
  est$residuals <- y - est$fitted.values
  est$call <- match.call()

  class(est) <- "linmod"
  est
}

# Print-method -- preexists in base; therefor just add class method
print.linmod <- function(x, ...){

  cat("Call:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  print(x$coefficients)
}

# plot-/summary-/coef-/predict-method...
# print.summary.linmod
