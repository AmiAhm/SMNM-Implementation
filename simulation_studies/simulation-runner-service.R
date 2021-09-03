
library(foreach)
run.simulation <- function(iter.params, fn.params, fn) {
  iter.params.names <- names(iter.params)
  params <- c(iter.params, list(.combine='rbind', .export = c("fn", "iter.params.names", "fn.params")))
  result <- do.call(foreach, params) %dopar% {
    library(itses)
    source("R-utility/extra-utility.R")
    args <- lapply(iter.params.names, function(name) get(name))
    names(args) <- iter.params.names
    fn.args <- c(args, fn.params)
    do.call(fn, fn.args)
  }
  result
}

