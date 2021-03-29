library(magrittr)
#Models

linear_regression <- function(X,Y){
  #Add a column of 1's to the beginning of X for
  #the intercept.
  X %>%
    as.matrix() %>%
    cbind(matrix(rep(1,nrow(.)),ncol=1),.)
  
  #Beta estimates (X^T*X)^-1*x^T*Y
  X %>%
    t() %>%
    multiply_by_matrix(X) %>%
    raise_to_power(-1) %>%
    multiply_by_matrix(t(X)) %>%
    multiply_by_matrix(Y) %>%
    as.vector() %>%
    return()
}
