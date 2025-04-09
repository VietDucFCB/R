# Bai2:
create_sphere_volume_dataframe <- function() {
  radius_values <- 3:20
  
  volume_values <- (4/3) * pi * (radius_values^3)
  
  df <- data.frame(
    radius = radius_values,
    volume = volume_values
  )
  
  return(df)
}

sphere_df <- create_sphere_volume_dataframe()
print(sphere_df)

#Bai4:
data <- read.csv("C:/Users/kkagi/Downloads/data11.csv")

calculate_statistics <- function(data) {
  lower_bounds <- data$a
  upper_bounds <- data$b
  frequencies <- data$n
  
  midpoints <- (lower_bounds + upper_bounds) / 2
  
  total_freq <- sum(frequencies)
  
  min_height <- min(lower_bounds)
  max_height <- max(upper_bounds)
  
  mean_height <- sum(midpoints * frequencies) / total_freq
  
  variance <- sum(frequencies * (midpoints - mean_height)^2) / (total_freq - 1)
  
  return(list(
    minimum_height = min_height,
    maximum_height = max_height,
    sample_mean = mean_height,
    corrected_sample_variance = variance
  ))
}

results <- calculate_statistics(data)
print(results)

#Bai5:
phanvi <- function(X, p) {

  X_sorted <- sort(X)

  n <- length(X_sorted)
  
  i <- (p/100) * n
  
  if (i %% 1 == 0) {
    if (i + 1 <= n) {
      return((X_sorted[i] + X_sorted[i+1]) / 2)
    } else {
      return(X_sorted[i])
    }
  } else {
    i_rounded <- round(i)
    
    i_rounded <- min(max(i_rounded, 1), n)
    
    return(X_sorted[i_rounded])
  }
}

# Example 1: A small vector
X1 <- c(5, 1, 9, 3, 7, 4, 6, 8, 2)
cat("Vector X1:", X1, "\n")
cat("25th percentile:", phanvi(X1, 25), "\n")
cat("50th percentile:", phanvi(X1, 50), "\n")
cat("75th percentile:", phanvi(X1, 75), "\n")

# Example 2: A vector with even number of elements
X2 <- c(10, 20, 30, 40, 50, 60)
cat("\nVector X2:", X2, "\n")
cat("50th percentile:", phanvi(X2, 50), "\n")