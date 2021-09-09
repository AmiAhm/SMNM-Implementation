#' Generate data
#' After specification in text. Two main methods:
#' 1. Random sparsity.
#' 2. Enforced sparsity.
#' @param seed Sets seed for reproducibility. `numeric`. Defaults to `NA`.
#' @param n Number of observations to generate.
#' @param mean.length Signal strength (`l`) of data.
#' @param sd Standard deviation of data. Defaults to `1`.
#' @param p Sparsity to enforce, `0<p<1`. If `NA` do not enforce sparsity. Defaults to `NA`.
#' @return Generated data after given specifcation as list.
just.give.me.some.speckle.data <- function(seed = NA,
                                   n = 50,
                                   mean.length = 1,
                                   sd.scale = 0.5,
                                   sd.add = 0.5,
                                            noise.mean = 0,
                                   p = NA) {
  # Set seed if given
  if(is.numeric(seed)) set.seed(seed)
  # Draw means
  theta <- rnorm(n, mean = 1, sd = 1)
  theta <- mean.length*sqrt(2*log(n))*theta
  # Set sparse means:
   if(!is.numeric(p)) {
     p <- runif(1) # If no sparsity given select sparsity
     zeros <- rbinom(n, size = 1, prob  = p)  # Draw zeros
     cheksum  <- sum(zeros)
     while ((cheksum == 0)|(cheksum==n)) {
       zeros <- rbinom(n, size = 1, prob  = p)
       cheksum  <- sum(zeros)
     }
  }else{
     # Is sparsity given? Then set sparsity to excatly that
     n.zeros <- floor(n*p)
     if(n - n.zeros < 0) stop("Invalid sparsity given")
     zeros <- c(rep(0, n.zeros), rep(1, n - n.zeros)) # Create vector of 1s and 0s
     zeros <- sample(zeros) # Shuffle
   }

  theta <- theta*zeros # Set selected means to zero
  y <- (1+rnorm(n, mean = noise.mean, sd = sd.scale))*theta  +rnorm(n, mean = 0, sd = sd.add)   # Sample data

  list(y = y, theta = theta)  # Return data

}
