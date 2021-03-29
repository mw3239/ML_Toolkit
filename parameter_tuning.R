#Brute force approach to maximize k in a knn classification problem.
#Only checks up to k=21 because anything larger than that (and even that)
#tends to be unreasonable.

#For later: Add regression too.
knn_optimize_k <- function(x_train,y_train,x_test,y_test) {
  k <- seq(1,max(21,length(x_train)),by=2)
  accuracy <- rep(NA,length(k))
  results <- tibble(k,accuracy)
  
  for(i in 1:length(k)){
    results$accuracy[i] <- zero_one_loss(k_nearest_neighbors(x_train,y_train,x_test,k=k[i]),y_test)
  }
  
  results %>%
    arrange(desc(accuracy),k) %>%
    slice_head(n=1) %>%
    unlist() %>%
    return()
}

knn_optimize_k(x_train,y_train,x_test,y_test)
