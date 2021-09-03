source("simulation_studies/main-simulation-script_unknown-variance.R")

ns <- c(10, 50, 500, 5000)
mean.lengths <- c(0.5, 1, 2)

seeds <- 1:1e4
iter.params <- expand.grid(n=ns, mean.length = mean.lengths, seed = seeds)
iter.params <- as.list(iter.params)


sim.params <- list(
  iter.params = iter.params,
  fn.params = list(
    m = 5,
    max_num_iters = 10,
    k = 15,
    b = 20
  ),
  fn = unknown.variance.simulation
)

