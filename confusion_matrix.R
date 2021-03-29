confusion_matrix <- function(predictions,test_y){
  conmat <- table(predictions[,1],test_y)
  
  accuracy <- sum(diag(conmat))/sum(conmat)
  precision <- diag(conmat)/apply(conmat,2,sum)
  recall <- diag(conmat)/apply(conmat,1,sum)
  
  return(list("confusion_matrix"=conmat,
              "accuracy"=accuracy,
              "precision"=precision,
              "recall"=recall))
}