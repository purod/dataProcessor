scaleNegPos1 <- function(x) {
  # Scale x to 0-1 range
  x_scaled <- (x - min(x)) / (max(x) - min(x))
  
  # Rescale to -1 to 1
  x_scaled <- x_scaled * 2 - 1
  return(x_scaled)
}