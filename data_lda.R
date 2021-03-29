library(dplyr)
library(readr)
library(magrittr)
library(stringr)

####Linear Discriminant Analysis
#df = a data frame containing the full dataset with a single categorical y column.
#y is the name of the column with the classes labels
#matrix_type can be either "covariance" or "scatter", which sets how the variance
#matrices are calculated. The eigenvectors that they return will be identical provided
#that the there are no near zero variance predictors within each class. If both matrix
#types produce wildly different coefficients, then the data likely needs to be
#transformed in some fashion.
#graph determines whether the linear discriminants should be plotted. In the
#2-class case, a jitter plot is used to better visualize the 1D data.
#In multi-class cases, only the first 2 linear discriminants are plotted.
my_lda <- function(df,y,matrix_type="scatter",graph=T){
  lda_check_col_types(df,y)
  
  #There will be problems if any of the predictors have a column named "y"
  #unless we change it's name.
  if(any(str_detect(names(df),"y"))){
    found_new_name <- F
    while(!found_new_name){
      #Randomly picking a string of 6 characters to use for a new name and
      #then checking to make sure that string isn't a column name already seems
      #like overkill, but you never know.
      new_name <- str_flatten(sample(letters,6,replace=T))
      if(sum(str_detect(names(diamonds),new_name))==0){
        found_new_name <- T
        new_name_index <- which(names(df)=="y")
        names(df)[new_name_index] <- new_name
      }
    }
  }
  
  
  #If the y column is not already type character, make it so. (Sometimes it's
  #a factor.)
  df <- df %>%
    mutate_at(vars(y),funs(as.character(.)))
  
  class_means <- get_class_means(df,y)
  
  within_cov <- within_class_cov(df,class_means,y,matrix_type)
  between_cov <- between_class_cov(df,class_means,y,matrix_type)
  
  tryCatch({
    A <- solve(within_cov) %*% between_cov  
  },error = function(e) stop(str_c("Within class covariance matrix is singular and can not be inverted.",
                             "This is likely a result of linearly dependent columns. Check for",
                             "linear dependencies and try again."))
  )
  
  lin_discs <- eigen(A)
  
  #Return only the # of labels - 1 eigenvectors OR the number of columns in
  #X, whichever is smaller.
  num_labels <- df %>%
    dplyr::select(paste(y)) %>%
    unique() %>%
    nrow() %>%
    subtract(1)
  
  num_predictors <- ncol(df) - 1
  
  if(num_predictors<num_labels){
    return_length <- num_predictors  
  }
  else{
    return_length <- num_labels
  }
  
  
  
  final_eigenvecs<- Re(lin_discs$vectors[,1:return_length])
  
  df_y <- df[,which(colnames(df)==y)]
  df_x <- df[,which(colnames(df)!=y)] %>% as.matrix()
  df_x <- apply(df_x,2,as.numeric)
  
  final_lds <- df_x %*% final_eigenvecs %>%
    as_tibble() %>%
    `colnames<-` (str_replace(names(.),"V","LD"))
  
  final_lds <- cbind(df_y,final_lds)
  names(final_lds)[1] <- y
    
  
  return_priors <- get_prior_probs(df,y)
  
  if(graph){
    graph_lda(final_lds,y)
  }
  
  return(list("eigenvalues"=Re(lin_discs$values[1:return_length]),
              "priors"=return_priors,
              "group_means "=class_means,
              "eigenvectors"=final_eigenvecs,
              "transformed_data"=final_lds))
}

lda_check_col_types <- function(df,y){
  acceptable_classes <- c("numeric","integer","double","logical","float")
  
  df_x <- df %>%
    dplyr::select(-paste(y))
  
  col_types <- sapply(df_x,class) %>% unlist()
  
  if(!all(col_types %in% acceptable_classes)){
    stop(message="One or more columns is categorical. Remove it or encode it with dummy variables and try again.")
  }
}

get_class_means <- function(df,y){
  df %>%
    group_by(!!as.name(all_of(y))) %>%
    summarize_all(mean) %>%
    return()
}


within_class_cov <- function(df,means,y,matrix_type="scatter"){
  dims <- ncol(df)-1
  result <- matrix(0L,dims,dims)
  
  for(i in 1:nrow(means)){
    class_label <- means[i,1]
    class_mean <- means[i,-1]
    
    class_x <- df %>%
      filter(!!as.name(y)==paste(class_label)) %>%
      dplyr::select(-!!as.name(y))
    
    class_size <- nrow(class_x)
    
    for(j in 1:nrow(class_x)){
      current_row <- class_x[j,]
      
      difference = t(t(t(current_row))) - t(t(t(class_mean)))
      if(matrix_type=="covariance"){
        new_result <- (1/(class_size-1)) * (difference %*% t(difference))  
      }
      else{
        new_result <- (difference %*% t(difference))
      }
      result <- new_result + result
    }
  }
  return(result)
}

between_class_cov <- function(df,means,y,matrix_type="scatter"){
  dims <- ncol(df)-1
  result <- matrix(0L,dims,dims)
  
  overall_means <- df %>% dplyr::select(-paste(y)) %>%
    summarize_all(mean,.groups="keep") %>%
    t()
  
  for(i in 1:nrow(means)){
    class_label <- means[i,1]
    
    class_mean <- means[i,-1] %>%
      as.matrix() %>% #Converting tibble -> vector directly doesn't work.
      as.vector()
    
    class_size <- df %>%
      filter(!!as.name(y)==paste(class_label)) %>%
      nrow()
    
    n <- nrow(df)
    difference <- class_mean - overall_means
    new_res <- class_size * (difference%*%t(difference))
    result <- result + new_res
  }
  if(matrix_type=="covariance"){
   result <- result * (1/(nrow(df)-1)) 
  }
  return(result)
}

get_prior_probs <- function(df,y){
  df %>%
    group_by(!!as.name(y)) %>%
    summarise(n()/nrow(df),.groups="keep") %>%
    return()
}

graph_lda <- function(df,y){
  library(ggplot2)
  #Only 1 LD case
  if(ncol(df)==2){
    p <- ggplot(df,aes(x=LD1,y=!!as.name(y),color=!!as.name(y))) +
      geom_jitter()
  }
  #3 or more LDs.
  else{
    if(ncol(df)>=4){
      message("Warning: More than 3 linear discriminants. Only the first 2 will be graphed in order to fit on a 2D plot.") 
    }
    #2 or more LDs
    p <- ggplot(df,aes(x=LD1,y=LD2,color=!!as.name(y))) +
      geom_point()
  }
  print(p)
}

#model is the object returned by my_lda
#x is the matrix of predictors that you wish to predict.
my_lda_predict <- function(model,x){
  y <- names(model$transformed_data[1])
  labels <- unique(model$transformed_data[1])
  
  label_table <- data.frame(label=labels,id=seq(1:nrow(labels)))
  
  class_priors <- as.matrix(model$priors[,2])
  class_means <- get_class_means(model$transformed_data,y)
  within_cov <- within_class_cov(model$transformed_data,class_means,y)
  new_x <- as.matrix(x) %*% model$eigenvectors
  
  predictions <- data.frame(id=NA,prob=NA)
  for(i in 1:nrow(new_x)){
    current_row <- new_x[i,]
    
    class_probs <- rep(NA,nrow(class_means))
    for(j in 1:nrow(class_means)){
      current_mean <- as.matrix(class_means[j,-1])
      
      class_probs[j] <- multivar_norm(current_row,current_mean,within_cov)*class_priors[j]
    }
    class_probs <- class_probs/sum(class_probs)
    max_class <- data.frame(id=which(class_probs==max(class_probs)),prob=max(class_probs))
    
    predictions[i,] <- max_class
  }
  predictions %>%
    left_join(label_table,by="id") %>%
    dplyr::select(!!as.name(y),prob) %>%
    return()
}

multivar_norm <- function(x,mu,sigma){
  f <- (1/((2*pi)^(length(mu)/2)*sqrt(det(sigma))))*exp((-1/2)*(x-mu)%*%solve(sigma)%*%t(x-mu))
  return(f)
}

