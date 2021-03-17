#logistf()‚Åì¬‚µ‚½ƒ‚ƒfƒ‹‚Ípredict‚É‘Î‰‚µ‚Ä‚¢‚È‚¢‚Ì‚Å
#©ìŠÖ”‚ğ—p‚¢‚Ä•ª—Ş‚ğs‚¤. 

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