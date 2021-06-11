################################################
# S+Proteome peak detection functions.
#
# Mother Functions
#  msPeak
#
# Children Functions
#  msPeakCWT
#  msPeakMRD
#  msPeakSearch
#  msPeakSimple
#
# Utility Functions
#  msPeakInfo
#  msPeakPrune
#
################################################


################################################
##  Children peak detection functions
################################################




"msPeakCWT" <- function(x, y, n.scale = 100, snr.min = 3, scale.min = 4, length.min = 10,
  noise.span = NULL, noise.fun = "quantile", noise.min = NULL,
  n.octave.min = 1, tolerance = 0.0, holder = TRUE, process = "msPeakCWT")
{
  # check input arguments. let the CWT function check the related arguments.
  checkVectorType(y,"numeric")

  # calculate the CWT
  W <- wavCWT(y, n.scale=n.scale, wavelet="gaussian2", variance=1)

  # form the CWT tree
  W.tree <- wavCWTTree(W, n.octave.min=n.octave.min, tolerance=tolerance, type="maxima")

  # isolate the peaks
	noise.min.raw <- quantile(abs(attr(W.tree,"noise")), 
		prob=if (is.null(noise.min)) {0.05} else {noise.min})
  p <- wavCWTPeaks(W.tree, snr.min=snr.min, scale.range=c(scale.min, length(x)), length.min=length.min,
    noise.span=noise.span, noise.fun=noise.fun, noise.min=noise.min.raw)

  # calculate corresponding Holder exponents
  if (holder){
    cusps <- holderSpectrum(W.tree)
  }

  # write history information (only once)
  if (!isProcessRecorded(process)){

    report <- list(process=process,
       scale.min=scale.min,
       noise.fun=noise.fun,
       noise.min=noise.min,
       noise.span=noise.span,
       n.octave.min=n.octave.min,
       tolerance=tolerance,
       snr.min=snr.min,
       wavelet="Mexican hat (gaussian2)",
       holder=holder)

    assignEvent(report, process)
  }

  # create index.max vector
  nmz <- length(y)
  index.min <- index.max <- rep(FALSE, nmz)
  imax <- attr(p,"peaks")[["iendtime"]]
  index.max[imax] <- TRUE

  # create index.min vector
  # NOTE: local minima are not isolated in the wavCWTPeaks function so
  # we estimate using the midpoint of adjacent maxima.
  if (length(imax) > 1){
    dmax <- round(diff(imax)/2)
    nmax <- length(imax)
    imin <- c(max(1,imax[1]-dmax[1]), imax[1:(nmax-1)] + dmax, min(nmz, imax[nmax]+ dmax[nmax-1]))
  }
  else{
    imin <- c(max(1, imax-2), min(nmz, imax+2))
  }
  index.min[imin] <- TRUE

  # wrap the peaks for output
  z <- msPeakInfo(x, y, index.min=index.min, index.max=index.max, snr.thresh = 0)

  if (holder){
    ibranch <- intersect(attr(p,"peaks")[["branch"]], cusps$branch)
    z[["holder"]] <- cusps$exponent[ibranch]
  }

  z
}



################################################
##  General peak detection functions
################################################

###
# msPeakInfo
###

"msPeakInfo" <- function(x, y, index.min, index.max, noise.local=NULL, snr.thresh=2)
{
  # check inputs
  nvar <- length(x)
  checkVectorType(x,"numeric")
  checkVectorType(y,"numeric")
  checkVectorType(index.min,"logical")
  checkVectorType(index.max,"logical")
  checkScalarType(snr.thresh,"numeric")
  if (!all(c(length(y), length(index.min), length(index.max)) == nvar))
    stop("x, y, index.min, and index.max must be vectors of equal length")
  if (!is.null(noise.local)){
    checkVectorType(noise.local,"numeric")
    if (length(noise.local) != nvar)
      stop("noise.local must be a numeric vector of length equal to that of the original mass spectrum")
  }

  # gather peak information

  # calculate signal to noise ratio
  # use intensity if noise.local is NULL, i.e., treat noise.local as 1
  # NOTE: snr is a vector containing only the snr values estimated for each peak,
  # i.e., its length will typically be must less than the length of nvar
  imax <- which(index.max)
  snr <- ifelse1(is.null(noise.local), y[imax], y[imax] / noise.local[imax])

  # remove peaks with low snr
  good.snr <- (snr > snr.thresh)
  snr <- snr[good.snr]

  tick.loc <- imax[good.snr]

  # find the tick.left and tick.right bounds of each peak
  # TODO: is there any way to get rid of the for loop?
  if ((npeak=length(tick.loc))==0) return(data.frame())
  tick.left <- tick.right <- rep(1, length(tick.loc))

  for (i in 1:length(tick.loc)) {
    for (j in tick.loc[i]:1) {
      if (index.min[j]){
        tick.left[i] <- j
        break
      }
    }

    for (j in tick.loc[i]:length(x)) {
      if (index.min[j]){
        tick.right[i] <- j
        break
      }
    }

#    # NOTE: the above two for loops could be replaced with the following code
#    # it is simpler but much slower
#    if(any(index.min[1:tick.loc[i]]))
#      tick.left[i] <- max(which(index.min[1:tick.loc[i]]))
#    if(any(index.min[tick.loc[i]:length(x)]))
#      tick.right[i] <- min(which(index.min[tick.loc[i]:length(x)])) + tick.loc[i] - 1
  }

  # special handling for the span of the last peak
  if (tick.right[length(tick.loc)]==1)
    tick.right[length(tick.loc)] <- length(x)

  # remove duplicated peaks (due to plateaus) by keeping only the most left
 
  keep <- !duplicated(tick.left)

  # gather peak information
  tick.loc   <- tick.loc[keep]
  tick.left  <- tick.left[keep]
  tick.right <- tick.right[keep]
  mass.left  <- x[tick.left]
  mass.right <- x[tick.right]

  # Return peak info
  data.frame(
    tick.loc, tick.left, tick.right,
    tick.span = tick.right - tick.left + 1,
    snr = snr[keep], intensity = y[tick.loc],
    mass.loc = x[tick.loc], mass.left, mass.right,
    mass.span = mass.right - mass.left,
    row.names = NULL)
}
