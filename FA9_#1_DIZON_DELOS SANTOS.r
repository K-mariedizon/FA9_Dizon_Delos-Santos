# Define the gamma distribution function
gamma_density <- function(x, alpha, beta) {
  if (x > 0) {
    return((x^(alpha - 1) * exp(-x / beta)) / (beta^alpha * gamma(alpha)))
  } else {
    return(0)
  }
}

# Calculate the mean of the gamma distribution
mean_gamma <- function(alpha, beta) {
  return(alpha * beta)
}

# Calculate the variance of the gamma distribution
variance_gamma <- function(alpha, beta) {
  return(alpha * beta^2)
}

# Set alpha and beta values
alpha <- 3
beta <- 2

# Calculate the mean and variance using the defined functions
mean_value <- mean_gamma(alpha, beta)
variance_value <- variance_gamma(alpha, beta)

# Print the results
cat("Mean of the gamma distribution (μ) = ", mean_value, "\n")
cat("Variance of the gamma distribution (σ^2) = ", variance_value, "\n")

