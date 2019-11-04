ccss <-
function(x, size, number){
  
    ccs.sample<-function(x, size){
     
      if ( size == 1){
        return( sample(x,1))
      }
      if( size <= 0)stop("Invalid sample size entry")
      
      if (length(x)%%size == 0 && size != 1){
        x<-x[-sample(1:length(x),1)]
      }
      
      gcd <- function(x, y) {
        while(y) {
          temp = y
          y = x %% y
          x = temp
        }
        return(x)
      }
      
      L<- gcd(length(x),size)
      N<-length(x)
      M<-N/L
      n<-size
      m<-n/L
      cluster<- matrix(x, nrow= L, ncol= M) 
      cl<- as.matrix(c(1:ncol(cluster)))
      syst<-function(x, n){
        
        suppressWarnings( b1<-matrix(x, nrow= length(x)/n+1, ncol=n)) #The matrix formed will be in a circularl systematic order starting right from the begining of the sequence
        b2<-b1[sample(nrow(b1),1),]
        return(b2)
      }
      b3<-as.matrix(syst(cl, m))
      return(sample(as.vector(cluster[,b3])))
    }
    
     ite = 1
    final<- NULL
    
    while(ite <= number){
      set.seed(ite+ Sys.time())
      final<- cbind(ccs.sample(x, size), final)
      ite = ite+1
      
    }
    final<- as.matrix(final)
    colnames(final) <- paste("sample no.", 1:ncol(final), sep = "")
    return(final)
  }
