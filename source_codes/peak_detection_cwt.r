# This file demonstrates some of the basic functionalities of msProcess, 
# the spectra processing component of the S-PLUS software S+Proteome:
#
# * denoising
# * local noise estimation
# * baseline correction
# * intensity normalization
# * peak detection
#
# NOTE: The parameter setting for the following processing functions 
# might not be optimal for the dataset used. A user is encouraged to 
# try out different parameter settings. 

# st.n is n * p data matrix;


# load the msProcess package
Peak_cwt <- function(st.n, ppm, thresh.scale = 2, snr = 2, snr.thresh=10, scale.min = 0.5, 
						snr.min = 1, length.min=6, noise.span=NULL, noise.fun="quantile", 
						noise.min=NULL, n.octave.min=1, tolerance=0.0, holder=TRUE, process="msPeakCWT"){
  p = length(ppm)
  
  avg.c <- apply(st.n, 2, mean)
#------------------------------------------------------------------------------ 

  #set dataset to the specific class (an msSet object)
  z1 <- msSet(t(st.n), mz = ppm, data.name = NULL)
  
#------------------------------------------------------------------------------  
  # denoising 
  # the output will be an msSet object with an additional element "noise"
  # NOTE: we are overwriting the input with the output  
  z2 <- msDenoise(z1, FUN="wavelet", thresh.scale= thresh.scale)

#------------------------------------------------------------------------------	
  # local noise estimation
  #	the output will be an msSet object with an additional element "noise.local"
  z3 <- msNoise(z2, FUN="mean")
  
#------------------------------------------------------------------------------				
  # baseline correction
  #	the output will be an msSet object with an additional element "baseline"
  z4 <- msDetrend(z3, FUN="monotone")

#------------------------------------------------------------------------------			
  # intensity normalization
  #	the output will be an msSet object with an additional element "tic"
  z5 <- msNormalize(z4)
  
  # visualize how the intensity normalization algorithm performed within a certain mass range   
 	
	mm <- z5$mz #mm is the ppm
	intensity <- z5$intensity  
	me.ints <- as.vector(apply(intensity, 1, mean))  #average spectrum

#------------------------------------------------------------------------------     
  # peak detection
  # the output will be an msSet object with additional elements

z6 <- msPeakCWT(mm, me.ints, scale.min = scale.min, snr.min = snr.min, length.min = length.min,
  noise.span = noise.span, noise.fun = noise.fun, noise.min = noise.min,
  n.octave.min = n.octave.min, tolerance = tolerance, holder = holder, process = process)

  print("Number of peaks detected:")
  print(length(z6$tick.loc))

  plot(mm, me.ints, type = "l") + 
  points(x = mm[z6$tick.loc], y = me.ints[z6$tick.loc], col = "red")
	
	
#------------------------------------------------------------------------------
  # extract information from z6. 
  pk.class <- z6
  pk.class <- as.data.frame(pk.class)
  idx.pk <- round(pk.class$tick.loc)
  idx.left <- pk.class$tick.left
  idx.right <- pk.class$tick.right
  ppm.pk <- pk.class$mass.loc
  ppm.left <- pk.class$mass.left
  ppm.right <- pk.class$mass.right
  
  clus.mem <- rep(0, p)
  clus.col <- rep(0, p)
  for(i in 1:dim(pk.class)[1]){
	clus.mem[idx.left[i] : idx.right[i]] <- i
	clus.col[idx.left[i] : idx.right[i]] <- 2
  }
  # Visualization of buckets:
  qplot(ppm, avg.c, geom = "line", col = clus.col) + 
  geom_vline(xintercept = ppm.pk, col = "green")
  return(list(clus.mem = clus.mem, clus.col = clus.col, idx.pk = idx.pk, idx.pk = idx.pk, 
  idx.right = idx.right, idx.left = idx.left))
}
#==============================================================================

