#logistf()�ō쐬�������f����predict�ɑΉ����Ă��Ȃ��̂�
#����֐���p���ĕ��ނ��s��. 

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