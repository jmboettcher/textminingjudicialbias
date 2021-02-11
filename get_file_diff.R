# quick algorithm for getting a sense of the level of difference between two
# court decisions
# returns distinct portions of each decision (inputted as s,r) separated by a "|||"
quickh<-function(s,r) {
  s<-unlist(strsplit(s,""))
  r<-unlist(strsplit(r,""))
  i<-1
  j <- 1
  
  while(i <= length(s) & j <= length(r)) {
    if(s[i]==r[j]) {
      s[i] <- NA
      r[j] <- NA
      i<-i+1
      j<-j+1
    } else {
      continue = TRUE
      
      itry<-i
      
      while(itry < i + 50 & itry + 4 <= length(s) & continue) {
        iblock<-s[itry:itry+4]
        
        jtry<-j
        while (jtry < j + 50 & jtry + 4 <= length(r) & continue) {
          if(s[itry]==r[jtry] & s[itry+1]==r[jtry+1] & s[itry+2]==r[jtry+2] & s[itry+3]==r[jtry+3]& s[itry+4]==r[jtry+4]) {
            s[itry]=NA
            r[jtry]=NA 
            s[itry+1]=NA
            r[jtry+1]=NA
            s[itry+2]=NA
            r[jtry+2]=NA 
            s[itry+3]=NA
            r[jtry+3]=NA 
            s[itry+4]=NA
            r[jtry+4]=NA
            i<-itry+5
            j<-jtry+5
            continue<-FALSE
          }
          jtry<-jtry+1
        }
        itry<-itry+1
      }
      
      if (continue) {
        i<-length(s)+1
        j<-length(r)+1
      }
      
    }
  }
  
  finals<-paste(s[complete.cases(s)],collapse="")
  toreturn<-paste(finals,"  |||  ")
  finalr<-paste(r[complete.cases(r)],collapse="")
  toreturn<-paste(toreturn,finalr)
  return(toreturn)
}