##Scale. Note: This min-max scales, rather than say, mean/std scales or normalizing.
scale_var <- function(x){
  min <- min(x)
  max <- max(x)
  
  return((x - min)/(max - min))
}

##Center
center_var <- function(x){
  mean <- mean(x)
  x %>%
    subtract(mean) %>%
    return()
}

scale_and_center <- function(df){
  df %>%
    mutate_all(scale_var) %>%
    mutate_all(center_var) %>%
    return()
}
