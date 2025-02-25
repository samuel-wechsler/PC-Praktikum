# Some helpful functions for the APE experiment
# ERME 10/2014

#########################################################################

# read.WAV(): Reading audio signal from a mono channel WAV file
# package 'tuneR' must be installed and loaded

read.WAV <- function(filename) { # filename: name of WAV file to be read
  wavedata <- readWave(filename)
  cat("WAV file", filename, "read.\n")
  fs <- wavedata@samp.rate                            # sampling frequency (Hz)
  sdata <- wavedata@left / (2^(wavedata@bit-1) - 1)   # audio data s(t) from left mono channel
  dt <- 1 / fs                                        # time between samples (s)
  N.s <- length(sdata)                                # number of data points
  T.s <- (N.s - 1) * dt                               # duration of measurement (s)
  t <- seq(from=0, by=dt, length=N.s)                 # time (s)
  list("Time"=t, "Signal"=sdata, "Rate"=fs, "Length"=N.s, "Duration"=T.s)
}

#########################################################################

# write.WAV(): Writing audio signal to a mono channel WAV file
# package 'tuneR' must be installed and loaded

write.WAV <- function(filename, s, sr, bit) { # filename: name of WAV file to be written
                                              # s: audio signal s(t)
                                              # sr: sampling rate (Hz)
                                              # bit: A/D bits conversion
  s <- round(s * (2^(bit-1) - 1))
  w <- Wave(s, samp.rate=sr, bit=bit)
  writeWave(w, filename)
}

#########################################################################

# trigger(): Returns the time when signal rises above trigger level

trigger <- function(t, s, level){
  t[min(which(abs(s)>level))]
}

#########################################################################

# f2t(): Mapping frequency to time in sweep experiments
# t2f(): Mapping time to frequency in sweep experiments

f2t <- function(sweeptime, f.min, f.max, freq){
  approx(c(f.min,f.max), c(0,sweeptime), freq)$y
}

t2f <- function(sweeptime, f.min, f.max, time){
  approx(c(0,sweeptime), c(f.min,f.max), time)$y
}

#########################################################################

Envelope <- function(x, y, sampl=1, w=1) {
  # Calculates upper and lower y envelope of y(x) data.
  # The width w can be used to average noisy data. Minimum is w=1,
  # w=0 resamples original data.
  # The factor sampl multiplies length of resampled vectors. (Note that
  # factor<1 may give strange results.)
  # For normal use set sampl <= w.
  # The function returns a list of 3 vectors (x, lower y and upper y)
  # of the same length.
  M <- approx(x, y, n=sampl*length(x))  # resampling
  x <- M$x
  y <- M$y
  Nx <- length(x)
  zu <- zl <- rep(NA, Nx)
  for (i in (w+1):(Nx-w)) {
    ysel <- y[(i-w):(i+w)]
    zu[i] <- max(ysel)
    zl[i] <- min(ysel)
  }
  list("x"=x, "upper"=zu, "lower"=zl)
}

#########################################################################

# zoom(): Zoom function
# Clicking within the plot region zooms in the selected frame or zooms out.
# Clicking outside the plot region exits the zoom function.

zoom <- function(x, y, lpch="l"){
  cat("Click edges to zoom in or out!\n")
  xyrange <- par("usr")
  p <- locator(2)
  while (p$x[1]>xyrange[1] & p$x[2]<xyrange[2] & p$y[1]>xyrange[3] & p$y[2]<xyrange[4]) { #zoom in/out
    if (p$x[1]<p$x[2]) { # zoom in
      newxrange <- p$x
      newyrange <- p$y
    }
    else { # zoom out
      newxrange <- c(xyrange[1]-diff(xyrange[1:2]), xyrange[2]+diff(xyrange[1:2]))
      newyrange <- c(xyrange[3]-diff(xyrange[3:4]), xyrange[4]+diff(xyrange[3:4]))
    }
    plot(x, y, type=lpch, xlim=newxrange, ylim=newyrange)
    cat("Click edges to zoom in!\n")
    xyrange <- par("usr")
    p <- locator(2)
  }
}

#########################################################################

# myconv(): Convolution, according to Matlab/Octave definition
#
# Note that (with type="open") the total length of vectors to be convoluted
# should equal to 2^n + 1 for efficient calculation.
# Vector x is zero-padded if neccessary.

myconv <- function(x, y) {
  Nx.Ny <- length(x) + length(y)
  N <- 2^floor(log(Nx.Ny, base=2)+1)
  No.of.zeros <- N - Nx.Ny + 1
  x <- c(x, rep(0, No.of.zeros))
  convolve(x, rev(y), type="open")
}

#########################################################################

# myacf(): Autocorrelation

myacf <- function(s, max_lag, dt) { # s: signal to be correlated, 
                                    # max_lag: maximum lag,
                                    # dt: time between points
  F <- acf(s, lag.max=max_lag, type="correlation", demean=FALSE, plot=FALSE)
  f <- c(rev(F$acf[-1]), F$acf)           # alles zusammen
  tau_max <- dt*(length(f)-1)/2   # maximale Verschiebung
  tau <- seq(from=-tau_max, to=tau_max, by=dt)
  list("tau"=tau, "ACF"=f)
}

#########################################################################

# Fourier Transform
# myfft.R: positive frequencies only
# myfft2.R: positive and negative frequencies

myfft <- function(s, sr) {# s: signal s(t), sr: sampling rate (Hz)
  fNyq <- sr/2
  if (length(s)%%2==1) { s <- s[-length(s)] }
  n <- length(s)
  S <- fft(s)[1:(n/2+1)]
  f <- seq(from=0, to=fNyq, length.out=(n/2+1))
  list("Freq"=f, "Complex"=S, "Abs"=abs(S), "Pow"=abs(S)^2, "Re"=Re(S), "Im"=Im(S))
}

myfft2 <- function(s, sr) {# s: signal s(t), sr: sampling rate (Hz)
  fNyq <- sr/2
  if (length(s)%%2==1) { s <- s[-length(s)] }
  n <- length(s)
  S <- fft(s)
  fpos <- seq(from=0, to=fNyq, length.out=(n/2+1))
  fneg <- -seq(from=fpos[n/2], to=fpos[2], length.out=(n/2-1))
  f <- c(fpos,fneg)
  fsort.idx <- sort(f,index.return=TRUE)$ix
  f <- f[fsort.idx]
  S <- S[fsort.idx]
  list("Freq"=f, "Complex"=S, "Abs"=abs(S), "Pow"=abs(S)^2, "Re"=Re(S), "Im"=Im(S))
}

#########################################################################

