
## Generation of Average Correlation ##
# --------------------------------------------------------------------------
# For generating correlation landscape
# Input:
# 	  ind = the given order of variables. Default value is 1,2,...,p;
#     cor.x = pairwise correlation matrix from the original dataset;
#	  k = window size;
# Output:
#     cor.ori = average correlation vector.
#     
# ------------------------------------------------------------------------
#        ** Xu Han - Baylor College of Medicine (2011) **
#


 
Cor.stat <- function(ind, cor.x, k){   
    cor.ori <- c()
	p <- dim(cor.x)[1]
    for(pj in 1 : (p - k + 1)){ 
        ind.pj <- ind[pj: (pj + k -1)]
        cor.pj <- cor.x[ind.pj, ind.pj]
        s.ori <- mean(cor.pj[lower.tri(cor.pj)]) 
        cor.ori <- c(cor.ori, s.ori)
    }
    return(cor.ori)
}
