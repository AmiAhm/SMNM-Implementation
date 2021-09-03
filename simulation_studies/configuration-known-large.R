source("simulation_studies/main-simulation-script_known-variance.R")

ns <- 5e3
mean.lengths <- c(0.5, 1, 2)

seeds <- 1:2500
iter.params <- expand.grid(n=ns, mean.length = mean.lengths, seed = seeds)
iter.params <- as.list(iter.params)


sim.params <- list(
  iter.params = iter.params,
  fn.params = list(
    m = 10,
    max_num_iters = 10,
    k = 1e3,
    sampling = F,
    loss = F
  ),
  fn = known.variance.simulation
)

