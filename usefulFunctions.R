# Function to compute a sliding window 
sliding.window <- function(v, winwdt = 7, pos = 4, na.rm = TRUE, FUN = mean){
  # v vector to be averaged/summed
  # winwdt width of the window 
  # pos position of the focal day in the window
  # FUN function to apply
  n <- length(v)
  # Initialize output vector
  out <- 0 * v + (-1)
  out[1:(pos-1)] <- NA
  out[(n + 1 - winwdt + pos) : n] <- NA
  
  for(i in pos : (n - winwdt + pos)){
    out[i] <- FUN(v[(i - pos + 1):(i + winwdt - pos)], na.rm = na.rm)
  }
  return(out[1:n])
}

