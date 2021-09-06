source("R-utility/extra-utility.R")
library(itses)
unknown.variance.simulation <- function(n,
                            mean.length,
                                        b = 20,
                                      ...,
                                      seed = NA,
                                      k = 15) {
  # Will use sd = 1, but unknown to methods
  # Simulate means
  data <- just.give.me.some.data(n = n, mean.length = mean.length, seed = seed)
  y <- data$y
  theta <- data$theta

  data.info <- list(
    n_sparse = sum(theta == 0),
    sparsity = sum(theta == 0)/n,
    theta_l2_2 = sum(theta^2),
    y_l2_2 = sum(y^2),
    theta_l1 = sum(abs(theta)),
    y_l1 = sum(abs(y)),
    mse_mle = mean((theta-y)^2)
  )


  # Find sd2 startistic used in SS
  sd2 <- mean(y^2-1) 

  # Estimate sd with regular method
  sdest <- itses:::mad.estimator(y)

  # Find thresholds with different estimators
  # visu minimax threshold
  visu <- sdest*itses:::get.visu.threshold(y/sdest)

  sshrink <- sdest*get.sureshrink.threshold(y/sdest)
  

  iterative.st.res <- itses(y,
                            method = "ST",
                            minimizationmethod = "numeric",
                              ...,
                              sd = NA,
                              sparse.mad = TRUE,
                              k = k
                                )
  iterative.st <- iterative.st.res$lambda

  iterative.ht.res <- itses(y,
                               method = "HT",
                            minimizationmethod = "numeric",
                              ...,
                              sd = NA,
                              sparse.mad = TRUE,
                              k = k
  )

  iterative.ht <- iterative.ht.res$lambda

   iterative.st.res.sampling <- itses(y,
                            method = "ST",
                            minimizationmethod = "sampling",
                                      b = b,
                              ...,
                              sd = NA,
                              sparse.mad = TRUE,
                              k = k
                                )
  iterative.st.sampling <- iterative.st.res.sampling$lambda

  iterative.ht.res.sampling <- itses(y,
                               method = "HT",
                            minimizationmethod = "sampling",
                                     b = b,
                              ...,
                              sd = NA,
                              sparse.mad = TRUE,
                              k = k
  )

  iterative.ht.sampling <- iterative.ht.res.sampling$lambda
  
  
   # Find oracle threshold
  oracle.lambda.st.loss <- get.loss.oracle.threshold(theta = theta, y = y, method = "ST")
  oracle.lambda.ht.loss <- get.loss.oracle.threshold(theta, y, method = "HT")

  # Also check these
  also.check <- c(oracle.lambda.st.loss,
                  oracle.lambda.ht.loss,
                  visu,
                  iterative.st,
                  iterative.ht,
                  iterative.st.sampling,
                  iterative.ht.sampling,
                  sshrink)

  oracle.lambda.st.risk <- itses:::get.risk.oracle.threshold(theta = theta,
                                                             method = "ST",
                                                             max.threshold = Inf,
                                                             k =k, also.check = also.check)$lambda
  oracle.lambda.ht.risk <- itses:::get.risk.oracle.threshold(theta = theta,
                                                             method = "HT", k = k,
                                                             max.threshold = Inf,
                                                             also.check = also.check)$lambda

 # Store found thresholds
  thresholds <- list(
    lambda_oracle_st_loss = oracle.lambda.st.loss,
    lambda_oracle_ht_loss = oracle.lambda.ht.loss,
    lambda_oracle_st_risk = oracle.lambda.st.risk,
    lambda_oracle_ht_risk = oracle.lambda.ht.risk,
    lambda_visu = visu,
    lambda_iterative_st = iterative.st,
    lambda_iterative_ht = iterative.ht,
    lambda_iterative_st_sampling = iterative.st.sampling,
    lambda_iterative_ht_sampling = iterative.ht.sampling,
    lambda_ss = sshrink)

  # Store and calculate losses at different thresholds.  sd=1 so no adjustment is needded do theta/sd and lambda/sd else
  risks <- c(
    risk_oracle_st = itses:::risk.st(theta, oracle.lambda.st.risk),
    risk_oracle_ht = itses:::risk.ht(theta, oracle.lambda.ht.risk),
    risk_visu_st = itses:::risk.st(theta, visu),
    risk_visu_ht = itses:::risk.ht(theta, visu),
    risk_iterative_st = itses:::risk.st(theta, iterative.st),
    risk_iterative_ht = itses:::risk.ht(theta, iterative.ht),
    risk_iterative_st_sampling = itses:::risk.st(theta, iterative.st.sampling),
    risk_iterative_ht_sampling = itses:::risk.ht(theta, iterative.ht.sampling),
    risk_ss = itses:::risk.st(theta, sshrink)
  )

  losses <- c(
    loss_oracle_st = itses:::loss.w.st(theta, y, oracle.lambda.st.loss),
    loss_oracle_ht = itses:::loss.w.ht(theta, y, oracle.lambda.ht.loss),
    loss_visu_st = itses:::loss.w.st(theta, y, visu),
    loss_visu_ht = itses:::loss.w.ht(theta, y, visu),
    loss_iterative_st = itses:::loss.w.st(theta, y, iterative.st),
    loss_iterative_ht = itses:::loss.w.ht(theta, y, iterative.ht),
    loss_iterative_st_sampling = itses:::loss.w.st(theta, y, iterative.st.sampling),
    loss_iterative_ht_sampling = itses:::loss.w.ht(theta, y, iterative.ht.sampling),
    loss_ss = itses:::loss.w.st(theta, y, sshrink)
  )

  sd.ests <- list(
    sd_doh = sdest,
    sd_ht = iterative.ht.res$sd,
    sd_st = iterative.st.res$sd
  )

  params <- list(seed = seed, n = n, mean.length = mean.length, sd2 = sd2, k = k)
  # Return all data
  result <- c(params, sd.ests, thresholds, losses, risks, data.info)
  unlist(result)

}

