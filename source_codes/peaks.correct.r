# Find local maximum with span = 3;
peaks <- function(series){ 
    maxind <- rep(0, length(series))
    for(i in 2: (length(series)-1)){
        if((series[i-1] < series[i] && series[i+1] < series[i])==T){
            maxind[i] <- 1
        }
    }
return(maxind)
} 


