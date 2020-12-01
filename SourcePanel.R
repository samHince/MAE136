# Code written for the source pannel method report assignment in MAE 136

# Samuel Hince 19295309
# 11/29/2020

# Flow around a circular cylinder
# References: I used some of the work outlined by Josh the Engineer. His MATLAB and article may be found at 
# 				http://www.joshtheengineer.com/2020/02/29/source-panel-method-circular-cylinder/

# import useful libraries
library(ggplot2)

### Lets first define some of the needed constants 
r <- 1			# Radius of body
Vinf <- 1		# Free flow velocity 
nPanels <- 64 	# number of panels 
aoa <- 0 		# angle of free stream relative to horizontal 

### create genometry 

# general
theta <- seq(0,360,(360/(nPanels)))
theta <- theta[-length(theta)]

# for vertex
shift <- (360/(nPanels))/2

thetaVertex <- theta - shift
thetaVertex <- thetaVertex%%360

thetaVertex <- thetaVertex * (pi/180)

x <- r * cos(thetaVertex)
y <- r * sin(thetaVertex)

x <- x * -1

vertex <- data.frame(x,y)

# midpoints
X <- (c(x[-1], x[1]) + x)/2
Y <- (c(y[-1], y[1]) + y)/2

midpoint <- data.frame(X,Y)

# angles
dx <- c(x[-1], x[1]) - x
dy <- c(y[-1], y[1]) - y

phiDeg <- atan2(dy,dx) * (180/pi)
phiDeg <- ifelse(phiDeg < 0, phiDeg + 360, phiDeg)
phi <- phiDeg * (pi/180)

betaDeg <- phiDeg + 90 - aoa
betaDeg <- ifelse(betaDeg > 360, betaDeg - 360, betaDeg)
beta <- betaDeg * (pi/180)

# lenght
S <- (dx^2 + dy^2)^0.5

## Calcularte integrals
I <- matrix(nrow = nPanels, ncol = nPanels)
J <- matrix(nrow = nPanels, ncol = nPanels)

for (i in 1:nPanels){ 
	for (j in 1:nPanels){
		if (j != i){                     
			A  <- -(X[i] - x[j]) * cos(phi[j]) - (Y[i] - y[j]) * sin(phi[j])    # A term
			B  <- (X[i] - x[j])^2 + (Y[i] - y[j])^2                           	# B term
			Cn <- sin(phi[i] - phi[j])                                          # C term (normal)
			Dn <- -(X[i] - x[j]) * sin(phi[i]) + (Y[i]-y[j]) * cos(phi[i])      # D term (normal)
			Ct <- -cos(phi[i]-phi[j])                                           # C term (tangential)
			Dt <- (X[i] - x[j]) * cos(phi[i]) + (Y[i]-y[j]) * sin(phi[i])       # D term (tangential)
			E  <- sqrt(B-A^2)                                                   # E term
			if (E == "NaN"){
				E <- 0
			}

			# Compute I for normal velocity
			term1  <- 0.5*Cn*log((S[j]^2+2*A*S[j]+B)/B)                     # First term in I equation
			term2  <- ((Dn-A*Cn)/E)*(atan2((S[j]+A),E) - atan2(A,E))        # Second term in I equation
			I[i,j] <- term1 + term2                                         # Compute I integral

			# Compute J for tangential velocity
			term1  <- 0.5*Ct*log((S[j]^2+2*A*S[j]+B)/B)                     # First term in J equation
			term2  <- ((Dt-A*Ct)/E)*(atan2((S[j]+A),E) - atan2(A,E))        # Second term in J equation
			J[i,j] <- term1 + term2                                         # Compute J integral
		}
	}
}

# Zero out any NANs, INFs, or imaginary numbers
I[is.na(I)] <- 0
J[is.na(J)] <- 0

# find lambda 
A <- I
diag(A) <- pi

b <- -Vinf * 2 * pi * cos(beta)

lambda <- solve(A, b)

sum(lambda) # should be near zero

# Find velocities and Cp
Vt <- Vinf * sin(beta) + J %*% lambda / (2 * pi)
Cp <- 1 - (Vt / Vinf)^2

# Analytical solution
CpAna = 1 - 4 * (sin(beta)^2) 

# Create plot
pl3 <- ggplot(data.frame(Cp, CpAna, panel = 1:nPanels), aes(x=panel)) + geom_point(aes(y=Cp)) + geom_line(aes(y=CpAna))
print(pl3)
