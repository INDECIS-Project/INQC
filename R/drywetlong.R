drywetlong<-function(x,ret=300,sueco=9.9,dry=TRUE,wet=TRUE){

  #' Detect wet and dry days
  #' @description This function detects episodes of too many consecutive wet or dry days
  #' @param x vector with values
  #' @param ret pseudo-return period (pareto-based) to compute the maximum tolerable spell
  #' @param sueco threshold for dividing dry and wet. This is useful to label other binary sequences, e.g. for 0 radiation. Now it is <= and >, instead of < and >=
  #' @param dry if set to TRUE, dry sequences are sent to result; if FALSE, omitted
  #' @param wet same as previous, for wet sequences
  #' @return list of positions which do not pass qc (bad)
  #' @export

  todo<-NULL  
  seco<-which( x<= sueco) # using the 1 mm threshold as in RClimdex
  mojao<-which(x >sueco)
  nohay<-which(is.na(x))
  y<-x
  ## creating a binary variable
  y[seco]<-0  ### dry values set to 0
  y[nohay]<-9 ### NAs set to 9
  y[mojao]<-1 ### wet days set to 1
  nyu<-rle(y)
  wetspell<-nyu$lengths[which(nyu$values==1)]
  if(length(wetspell) > 0){ ### some VERY short series do not have even a wetspell
    nyi<-potpareto(wetspell)
    if(!is.null(nyi)){wetlim<-returnpotpareto(nyi,ret)}else{wetlim<-999999999999999} ### when the paretp apparel does not find a value higher than the quantile, fails. Solved (needs to be improved)
    wetchungo<-which(nyu$lengths > wetlim & nyu$values == 1)
  }else{wetchungo=NULL}
  dryspell<-nyu$lengths[which(nyu$values==0)]
  if(length(dryspell) > 0){ ### some VERY short series do not have even a dryspell
    nyi<-potpareto(dryspell)
    if(!is.null(nyi)){drylim<-returnpotpareto(nyi,ret)}else{drylim<-999999999999999}### when the paretp apparel does not find a value higher than the quantile, fails. Solved (needs to be improved)
    drychungo<-which(nyu$lengths > drylim & nyu$values == 0)
  }else{drychungo=NULL}
  if(dry & wet){chungos<-c(wetchungo,drychungo)}
  if(dry & !wet){chungos<-drychungo}
  if(!dry & wet){chungos<-wetchungo}
  if(length(chungos) !=0 ){
    ene<-length(chungos)
    #### ACHTUUUNG!!! Corrected in January 2019: it was erroneously considered that rle "values" were signaling begining of strike and they are END OF STRIKE.
    for(i in 1:ene){
      rocha<-nyu$lengths[chungos[i]] ### the value exceeding the tolerated strike
      end<-sum(nyu$lengths[1:chungos[i]]) 
      start<-end-rocha+1
      if(i==1){todo<-c(start:end)}else{todo<-c(todo,start:end)}
    }
    return(todo)
  }
}