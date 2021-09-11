#  clusters <- 22
#  cl <- parallel::makeCluster(clusters)
#  doParallel::registerDoParallel(cl)
#  source("simulation_studies/simulation-runner-service.R")
#  ptm <- proc.time()
#
#
# print("Run simulation with known variance")
# source("simulation_studies/configuration-known.R")
# result <- do.call(run.simulation, sim.params)
# save(result, file = "output/main-simulation-data/sim-known-10k.RData")
# print(proc.time() - ptm)
#
# print("Run simulation with unknown variance")
# source("simulation_studies/configuration-unknown.R")
# result <- do.call(run.simulation, sim.params)
# save(result, file = "output/main-simulation-data/sim-unknown-10k.RData")
# print(proc.time() - ptm)

# print("Run large sim")
# source("simulation_studies/configuration-known-large.R")
# result <- do.call(run.simulation, sim.params)
# save(result, file = "output/main-simulation-data/simulation-known_large.RData")
# print(proc.time() - ptm)
#
# parallel::stopCluster(cl)
# print(proc.time() - ptm)

# # Produce plots of simulation with known variance
source("simulation_studies/simulation-plotter.R")

plot.simulation.result(data.path = "output/main-simulation-data/simulation-known-10k.RData",
                       out.path = "output/figures/simulation-known-10k.pdf")

plot.simulation.result(data.path = "output/main-simulation-data/simulation-unknown-10k.RData",
                       out.path = "output/figures/simulation-unknown-10k.pdf", ratio.max = 30)

plot.simulation.result(data.path = "output/main-simulation-data/simulation-known_large.RData",
                     out.path = "output/figures/simulation-known_large-10k.pdf", ratio.max = 5, ns = 5e3)


# # Small plot
plot.simulation.result(data.path = "output/main-simulation-data/simulation-known-10k.RData",
                       out.path = "output/figures/simulation-known-10k-small.pdf", n = 50, mean.lengths = c(0.5, 2),
full.a4 = F)


## Verify simultion data
library(testthat)
test_that("Simulation data is healthy", {
  source("simulation_studies/configuration-known.R")
  data.path <-"output/main-simulation-data/simulation-known-10k.RData"

  load(data.path)
  df <- as.data.frame(result)
  df$risk_ratio_visu_st <- df$risk_visu_st / df$risk_oracle_st
  df$risk_ratio_ss <- df$risk_ss / df$risk_oracle_st
  df$risk_ratio_itses_st <- df$risk_iterative_st / df$risk_oracle_st

  df$risk_ratio_visu_ht <- df$risk_visu_ht / df$risk_oracle_ht
  df$risk_ratio_itses_ht <- df$risk_iterative_ht / df$risk_oracle_ht

  expect_equal(nrow(df[df$risk_ratio_visu_st <1,]), 0)
  expect_equal(nrow(df[df$risk_ratio_ss <1,]), 0)
  expect_equal(nrow(df[df$risk_ratio_itses_st <1,]), 0)
  expect_equal(nrow(df[df$risk_ratio_visu_ht <1,]), 0)
  expect_equal(nrow(df[df$risk_ratio_itses_ht <1,]), 0)

  expect_equal(nrow(df),length(iter.params$mean.length))

  source("simulation_studies/configuration-unknown.R")
  data.path <-"output/main-simulation-data/simulation-unknown-10k.RData"
  df$risk_ratio_visu_st <- df$risk_visu_st / df$risk_oracle_st
  df$risk_ratio_ss <- df$risk_ss / df$risk_oracle_st
  df$risk_ratio_itses_st <- df$risk_iterative_st / df$risk_oracle_st

  df$risk_ratio_visu_ht <- df$risk_visu_ht / df$risk_oracle_ht
  df$risk_ratio_itses_ht <- df$risk_iterative_ht / df$risk_oracle_ht


  expect_equal(nrow(df[df$risk_ratio_visu_st <1,]), 0)
  expect_equal(nrow(df[df$risk_ratio_ss <1,]), 0)
  expect_equal(nrow(df[df$risk_ratio_itses_st <1,]), 0)
  expect_equal(nrow(df[df$risk_ratio_visu_ht <1,]), 0)
  expect_equal(nrow(df[df$risk_ratio_itses_ht <1,]), 0)

  expect_equal(nrow(df),length(iter.params$mean.length))

})