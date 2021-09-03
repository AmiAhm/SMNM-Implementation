source("R-utility/extra-utility.R")

known.variance.simulation <- function(n,
                            mean.length,
                                      seed = NA,
                                      k = 15,
                                      b = 20,
                                      ...,
                                      sampling = T,
                                      loss = T
                            ) {
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

  # visu minimax threshold
  visu <- itses:::get.visu.threshold(y)
  # ss threshold
  sshrink <- get.sureshrink.threshold(y)
  # iterative sampling with visu minimax start
  iterative.st <- itses(y,
                        method = "ST",
                        minimization.method  = "numeric" ,
                        ...,
                        sd = 1,
                        k = k
  )$lambda

    iterative.ht <- itses(y,
                          method = "HT",
                          minimization.method  = "numeric" ,
                          ...,
                          sd = 1,
                          k = k
    )$lambda


  if(sampling){

   iterative.st.res.sampling <- itses(y,
                              method = "ST",
                              minimization.method = "sampling",
                                        b = b,
                                ...,
                                sd = 1,
                                k = k
                                  )


    iterative.st.sampling <- iterative.st.res.sampling$lambda

    iterative.ht.res.sampling <- itses(y,
                                 method = "HT",
                              minimization.method = "sampling",
                                       b = b,
                                ...,
                                sd = 1,
                                k = k
    )

    iterative.ht.sampling <- iterative.ht.res.sampling$lambda
  }


  if(loss){
  # Find oracle threshold
  oracle.lambda.st.loss <- get.loss.oracle.threshold(theta = theta, y = y, method = "ST")
  oracle.lambda.ht.loss <- get.loss.oracle.threshold(theta, y, method = "HT")
  }
  # Also check these
  also.check <- c(if(loss) c(oracle.lambda.st.loss,
                  oracle.lambda.ht.loss) else NULL,
                  visu,
                  iterative.st,
                  iterative.ht,
                  if(sampling)
                  c(iterative.st.sampling,
                  iterative.ht.sampling) else NULL,
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
    lambda_oracle_st_loss = if(loss) oracle.lambda.st.loss else NULL,
    lambda_oracle_ht_loss = if(loss) oracle.lambda.ht.loss else NULL,
    lambda_oracle_st_risk = oracle.lambda.st.risk,
    lambda_oracle_ht_risk = oracle.lambda.ht.risk,
    lambda_visu = visu,
    lambda_iterative_st = iterative.st,
    lambda_iterative_ht = iterative.ht,
    lambda_iterative_st_sampling = if(sampling) iterative.st.sampling else NULL,
    lambda_iterative_ht_sampling = if(sampling) iterative.ht.sampling else NULL,
    lambda_ss = sshrink)

  # Store and calculate losses at different thresholds.  sd=1 so no adjustment is needded do theta/sd and lambda/sd else
  risks <- c(
    risk_oracle_st = itses:::risk.st(theta, oracle.lambda.st.risk),
    risk_oracle_ht = itses:::risk.ht(theta, oracle.lambda.ht.risk),
    risk_visu_st = itses:::risk.st(theta, visu),
    risk_visu_ht = itses:::risk.ht(theta, visu),
    risk_iterative_st = itses:::risk.st(theta, iterative.st),
    risk_iterative_ht = itses:::risk.ht(theta, iterative.ht),
    risk_iterative_st_sampling = if(sampling) itses:::risk.st(theta, iterative.st.sampling) else NULL,
    risk_iterative_ht_sampling = if(sampling) itses:::risk.ht(theta, iterative.ht.sampling) else NULL,
    risk_ss = itses:::risk.st(theta, sshrink)
  )


  losses <- if(loss) c(
    loss_oracle_st = itses:::loss.w.st(theta, y, oracle.lambda.st.loss),
    loss_oracle_ht = itses:::loss.w.ht(theta, y, oracle.lambda.ht.loss),
    loss_visu_st = itses:::loss.w.st(theta, y, visu),
    loss_visu_ht = itses:::loss.w.ht(theta, y, visu),
    loss_iterative_st = itses:::loss.w.st(theta, y, iterative.st),
    loss_iterative_ht = itses:::loss.w.ht(theta, y, iterative.ht),
    loss_iterative_st_sampling = itses:::loss.w.st(theta, y, iterative.st.sampling),
    loss_iterative_ht_sampling = itses:::loss.w.ht(theta, y, iterative.ht.sampling),
    loss_ss = itses:::loss.w.st(theta, y, sshrink)
  ) else NULL

  params <- list(seed = seed, n = n, mean.length = mean.length, sd2 = sd2, k = k)
  # Return all data
  result <- c(params, thresholds, losses, risks, data.info)
  unlist(result)
}

