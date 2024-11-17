# 2.2 winsorization
#'  Winsorize continuous variables 
#'
#'  \code{winsorize} return winsorized continuous variable.
#'
#'  @details
#'  The values of the continuous variable is checked to see if it is greater than 
#'  some set number of MADs (median absolute deviations) from its median.  If so, 
#'  the value is converted to the set number of MADs from median (winsorization).
#'
#'  @param in.data :  data values of continuous variable
#'  @param nMAD : number of median absolute deviations (default : 4)
#'  @param countWinsorize : instead of peforming the actual winsorization on the data,
#'                      summarize the number of necessary winsorized values (default : FALSE)
#'
#'  @return  winsorized continuous variable data values (countWinsorize=FALSE) or
#'              summary of the number of winsorizable values (countWinsorize=TRUE)
#'
#'  @export
#'

winsorize <- function(in.data1, nMAD = 4,countWinsorize = FALSE){
  
  in.data <- as.numeric(as.character(unlist(in.data1)))
  
  upp.lim <- median(in.data,na.rm=T) + nMAD*mad(in.data,na.rm=T)
  low.lim <- median(in.data,na.rm=T) - nMAD*mad(in.data,na.rm=T)
  
  ## summary of winsorizable values
  if ( countWinsorize ){
    ## MAD==0, no winsorization
    if ( mad(in.data,na.rm=T)==0 ){
      return(0)
    }else{
      ## count number of values above or below upp.lim or low.lim, respectively.
      no.winsor <- length(which(in.data > upp.lim | in.data < low.lim))
      return(no.winsor)
    }
    
  }else{
    
    ## MAD==0, no need for winsorization
    if ( mad(in.data,na.rm=T)==0 ){
      
      return(in.data)
    }else{
      
      ## winsorize data value to upp.lim if above or to low.lim if below  
      winsor.data <- sapply(in.data,function(x)if (is.na(x)){NA}
                            else if ( x > upp.lim){upp.lim}else if ( x < low.lim ){
                              low.lim}else{x})
      
      return(winsor.data)
    }
    
  }
  
}