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

k_nearest_neighbors <- function(X,Y,query,k=3,center=T,scale=T){
  
}


scaled <- x_test %>%
  scale()



dist <- rep(0,ncol(x_test))
index <- rep(0,ncol(x_test))
k <- 1

#Calculates euclidean distance for each point
for(i in 1:nrow(x_test)){
  dist[i] <- query %>%
    subtract(x_test[i,]) %>%
    raise_to_power(2) %>%
    sum() %>%
    sqrt()
  
  index[i] <- i
}


tibble(dist,index,classes) %>%
  arrange(dist) %>%
  extract(1:k,) %>%
  mutate(classes = as.factor(classes)) %>%
  summarise(mode = names(which.max(table(classes))))


