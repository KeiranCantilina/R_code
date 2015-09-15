# Clear workspace
rm(list=ls())

# Load libraries if just starting

library(caTools)

# Set working directory
dir <-"C:\\Users\\Ross\\Desktop\\Workspace\\"

         
jet.colors <- colorRampPalette(c("green", "blue", "red", "cyan", "#7FFF7F",
                                 "yellow", "#FF7F00", "red", "#7F0000"))
m <- 2000                # define size
C <- complex( real=rep(seq(-1.8,0.6, length.out=m), each=m ),
              imag=rep(seq(-1.2,1.2, length.out=m), m ) )
C <- matrix(C,m,m)       # reshape as square matrix of complex numbers
Z <- 0                   # initialize Z to zero
X <- array(0, c(m,m,20)) # initialize output 3D array
for (k in 1:20) {        # loop with 20 iterations
  Z <- Z^2+C             # the central difference equation
  X[,,k] <- exp(-abs(Z)) # capture results
}
dir_pic <- paste(dir,"Mandelbrot.gif",sep="")

picture <- write.gif(X, dir_pic, col=jet.colors, delay=900)

