#logistf()で作成したモデルはpredictに対応していないので
#自作関数を用いて分類を行う. 

lf_p <- function(lo, y, t){
  
  ans <- c()
  
  for(i in 1:nrow(y)){
    if(lo$preict[i] > t){
      ans <- c(ans, 1)
    }
    else{
      ans <- c(ans, 0)
    }
  }
  
  return(ans)
}