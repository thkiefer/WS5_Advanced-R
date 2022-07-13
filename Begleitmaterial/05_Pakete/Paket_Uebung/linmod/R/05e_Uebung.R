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

#' @param \dots further objects to be passed to methods
#' @export
#' @rdname linmodEst
linmod <- function(x, ...) UseMethod("linmod")

#' @param y numeric vector. Dependent variable.
#' @export
#' @rdname linmod-internal
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

#' Internal methods
#'
#' preexists in base; therefor just add class method
#' @param x object of class linmod
#' @param \dots further objects to be passed to methods
#' @export
#' @rdname linmod-internal
#' @method print linmod
print.linmod <- function(x, ...){

  cat("Call:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  print(x$coefficients)
}

#' @export
#' @rdname linmod-internal
#' @param object object of class linmod for coef-method. Argument titled `object`
#' instead of `x` in order to be consistent with stats::coef.
#' Otherwise CHECK --as-cran throws a warning.
#' @method coef linmod
coef.linmod <- function(object, ...){
  object$coefficients
}

# @importFrom MASS cats geht nicht, weil Datensätze nicht exportiert werden
# If it’s not used in the code but is used in examples, vignettes, or tests ->
# Description: Suggests

# weitere:
# plot-/summary-/predict-method...
# print.summary.linmod
