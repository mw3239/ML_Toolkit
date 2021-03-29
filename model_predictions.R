#######Linear Regression##############

linear_reg_prediction <- function(X,Beta){
  #Add a column of 1's to the beginning of X for
  #the intercept.
  X %>%
    as.matrix() %>%
    cbind(matrix(rep(1,nrow(.)),ncol=1),.)
  
  betas %>%
    multiply_by_matrix(t(X)) %>%
    t() %>%
    apply(1,sum) %>% #sum the rows
    return()
}

#######K-Nearest Neighbors##############

#Helper: Calculates Euclidean distance between two equal-length vectors
#X is an nxp matrix, while query is 1xp.
calc_distance <- function(X,query,Y){
  dist <- rep(0,ncol(X))
  index <- rep(0,ncol(X))
  
  #Calculates euclidean distance for each point
  for(i in 1:nrow(X)){
    dist[i] <- query %>%
      subtract(X[i,]) %>%
      raise_to_power(2) %>%
      sum() %>%
      sqrt()
    
    index[i] <- i
  }
  
  return(tibble(dist,index,classes=Y))
}

#Helper: Determines the class of a KNN classification problem
knn_class <- function(df,k){
  df %>%
    arrange(dist) %>% #Sort by distance
    magrittr::extract(1:k,) %>% #Extract 1st k rows
    mutate(classes = as.factor(classes)) %>% #Classes need to be factors to find mode
    summarise(mode = names(which.max(table(classes)))) %>% #Find mode. Ties are unfortunately broken
                                                           #on priority rather than randomly, so
                                                           #try to avoid them
    unlist %>%
    return()
}

#Helper: Determines the mean of a KNN Regression problem
knn_reg <- function(df,k){
  df %>%
    arrange(dist) %>%
    magrittr::extract(1:k,) %>%
    summarise(mean = mean(classes)) %>%
    unlist %>%
    return()
}

#Helper: Determines if a classification or regression procedure should be
#used.
knn_set_type <- function(type,Y){
  if(mode(Y)=="character"){
    return("Classification")
  } 
  else{
    return("Regression")
  }
}

k_nearest_neighbors <- function(X,Y,query,k=3,scale=T,type=NULL){
  if(is.null(type)) {
    type <- knn_set_type(type,Y) 
  }
  
  if(scale){
    scaled <- X %>%
      rbind(query) %>%
      scale()
    
    scaled_x <- scaled[1:nrow(X),]
    scaled_query <- scaled[-(1:nrow(X)),]
  }
  
  
  final_predictions <- rep(NA,nrow(query))
  
  for(i in 1:nrow(query)){
    if(scale){
      results <- calc_distance(scaled_x,scaled_query[i,],Y)
      
    }
    else{
      results <- calc_distance(X,query[i,],Y)
    }
    
    if(type == "Classification"){
      final_predictions[i] <- knn_class(results,k)
    }
    else if(type == "Regression"){
      final_predictions[i] <- knn_reg(results,k)
    }
  }
  return(final_predictions)
}