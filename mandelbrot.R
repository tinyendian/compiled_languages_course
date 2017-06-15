# Build instructions for shared library
#
# Linux and Windows:
# gfortran -c -fPIC mandelbrot.f90
# gfortran -shared -o mandelbrot.so mandelbrot.o
#
# AIX:
# gfortran -maix64 -c -fPIC mandelbrot.f90
# gfortran -maix64 -static-libgcc -shared -o mandelbrot.so mandelbrot.o
# Add underscore to symbol name

# Blue colour palette
blue <- colorRampPalette(c("#000000", "#0000FF"))

# Implementation of Mandelbrot using vectorised R
mandel_R <- function(nx, ny, maxiter, window, cut, magnitude) {

  # Set axes
  xaxis <- seq(window[1], window[3], length.out=nx)
  yaxis <- seq(window[2], window[4], length.out=ny)

  # Initial values for mandelbrot set
  C <- matrix(rep(xaxis, ny), nx, ny, byrow=FALSE)
  C <- C + 1i*matrix(rep(yaxis, ny), nx, ny, byrow=TRUE)

  # Iteration, only compute elements below cutoff
  Z <- matrix(as.complex(0), nx, ny)
  for (k in 1:maxiter) {
    idx <- (abs(Z) <= cut)
    if (sum(idx) == 0) {break}
    Z[idx] <- Z[idx]*Z[idx]+C[idx]
  }
  magnitude <- abs(Z)
  return(magnitude)
}

# Implementation of Mandelbrot using Fortran
mandel_fortran <- function(nx, ny, maxiter, window, cut, magnitude) {
  result <- .Fortran("mandelbrot", nx = as.integer(nx), ny = as.integer(ny),
                    maxiter = as.integer(maxiter), window = as.double(window),
                    cut = as.double(cut), magnitude = as.double(magnitude))
  return(result$magnitude)
}

# Set parameters
nx <- 800
ny <- 800
maxiter <- 100
cut <- 4.0
window <- c(-2.2, -1.2, 1.0, 1.2)
magnitude <- matrix(0.0, nx, ny)

# Run R version and plot result
starttime <- as.numeric(Sys.time())
magnitude_R <- mandel_R(nx, ny, maxiter, window, cut, magnitude)
stoptime <- as.numeric(Sys.time())
print(paste("R version:", stoptime-starttime))
image(matrix(magnitude_R, nx, ny), col = blue(256))
title("Mandelbrot - R version")

# Run Fortran library version and plot result
dyn.load("mandelbrot.so")
starttime <- as.numeric(Sys.time())
magnitude_fortran <- mandel_fortran(nx, ny, maxiter, window, cut, magnitude)
stoptime <- as.numeric(Sys.time())
print(paste("Fortran version:", stoptime-starttime))
image(matrix(magnitude_fortran, nx, ny), col = blue(256))
title("Mandelbrot - Fortran library version")
dyn.unload("mandelbrot.so")
