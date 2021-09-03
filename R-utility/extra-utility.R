library(itses) # For itses implementation (and helper functions)
library(asus) # For SureShrink implementation.
source("R-utility/data-generation.R")

#' SURE risk of ST
#' Stein's unbiased risk estiamtor with the soft-threshold estimator.
risk.hat <-  function(y, sd, lambda) {
  sum(sd^2-2*sd^2*(abs(y)<=lambda)+sapply(y, function(x) min(x^2,lambda^2)))
}

#' SURE threshold
#' Returns risk-minimizing sure-threshold with the soft threshold estimator
get.sure.threshold <- function(y, sd = 1) {
  visu <- sd*itses:::get.visu.threshold(y)
  lambda.grid <- c(0, abs(y)[abs(y)<=visu], visu)
  est.risks <- sapply(lambda.grid, function(lambda) {
    risk.hat(y, sd, lambda)
  })
  lambda.index <- which.min(est.risks)
  lambda <- lambda.grid[lambda.index]
  lambda
}

#' SureShrink threshold
#' @return SureShrink (SS) threshold.
#' @details
#' Wrapper function to the ASUS package.
get.sureshrink.threshold <- function(y) {
  lambda <- asus::sureshrink(y, v.d = 1)$t
  lambda
}

#' Loss oracle threshold
#' @return Loss minimizing oracle threshold.
get.loss.oracle.threshold <- function(theta0, y, sd = 1, method = "ST", also.check = NULL) {
  y <- y/sd
  theta0 <- theta0/sd
  sse.fun <- function(theta) sum((theta0 - theta)^2)

  if(method == "ST") {
    estimator <- itses:::soft.threshold.estimator
  }else if(method == "HT") {
    estimator <- itses:::hard.threshold.estimator
  }
  loss.fun <- function(lambda) sse.fun(estimator(y, lambda))
  opt.lambda <- NA
  if(method == "ST") {
    opt <- optimize(loss.fun, interval = c(0,max(abs(y))))
    opt.lambda <- opt$minimum
  }else if(method == "HT") {
    lambdas <- c(0,abs(y),itses:::get.visu.threshold(y))
    risks <- sapply(lambdas, loss.fun)
    j <- which.min(risks)
    opt.lambda <- lambdas[j]
  }

  if(!is.null(also.check)) {
    extra.losses <- sapply(also.check, risk.fun)
    if(min(extra.losses) < loss.fun(opt.lambda)) {
      j <- which.min(extra.losses)
      opt.lambda <- also.check[j]
    }
  }
  return(sd*opt.lambda)
}
# Helper function used in plotting, get next char.
# By user rosscova on https://stackoverflow.com/a/44305519 Date: 01.07.2021
getnextchar <- function(char) {
  letters[which(letters == char) + 1L]
}

#' Wrapper of ITSES to save results in file.
itses_saved <- function(y,
                  sparse_mad = TRUE,
                        savepath = NULL,
                  ...) {
  result <- itses::itses(y,
                         sparse.mad = sparse_mad,
                         ...)
  # Store result if save path given.
  if(!is.null(savepath)) {
    save(result, file = savepath)
  }
  result
}
