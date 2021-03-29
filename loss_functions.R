#Loss Functions

#Mean Squared Error, L2 Loss
MSE <- function(Y,Yhat){
  Y %>%
    subtract(Yhat) %>%
    raise_to_power(2) %>%
    sum() %>%
    divide_by(length(Y)) %>%
    return()
}

#Mean Absolute Error, L1 Loss
MAE <- function(Y,Yhat){
  Y %>%
    subtract(Yhat) %>%
    abs() %>%
    sum() %>%
    divide_by(length(Y)) %>%
    return()
}


#Huber Loss, Less sensitive to outliers, but still differntiable at 0.
#Delta determines what's considered an outlier.
huber_loss <- function(Y,Yhat,delta=1){
  library(dplyr)
  #Need to make these two separate chains or else the mutate evaluates incorrectly
  #for some reason.
  u <- Y-Yhat %>%
    as_tibble()
  
  u %>%
    mutate(value = case_when(abs(value) <= delta~.5*(value^2),
                             T~(delta*abs(value))-(.5*delta^2))) %>%
    sum() %>%
    divide_by(length(y_test)) %>%
    return()
}

#Log-Cash Loss
#Twice differentiable, unlike Huber loss.
log_cash_loss <- function(Y,Yhat){
 Yhat %>%
    subtract(Y) %>%
    cosh() %>%
    log() %>%
    sum() %>%
    divide_by(length(Y)) %>%
    return()
}

#Hinge Loss
#Usually used for SVMs
hinge_loss <- function(Y,Yhat){
  u <- Y-(1-2*Y)*Yhat
  
  sapply(u,max,0) %>%
    sum() %>%
    divide_by(length(Y)) %>%
    return()
}


#0-1 Loss
zero_one_loss <- function(Y,Yhat){
  Y %>%
    equals(Yhat) %>%
    sum() %>%
    divide_by(length(Y)) %>%
    return()
}
