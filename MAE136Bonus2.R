# MAE 136 Bonus Assignment 2

# Sam Hince
# 11/13/2020

# import required packages
library(dplyr)
library(ggplot2)

### constants ###
Uinf = 1
Lambda = 1

# create df
x <- seq(-1, 1, 0.01)
y <- seq(-1, 1, 0.01)

# fill in stream function for each point
df <- data.frame()

for (i in x){
	for (j in y){ 
		r = sqrt(i^2 + j^2)
		theta = atan2(j, i)
		
		stream <- ((Lambda/(2*pi))*theta) + Uinf*r*sin(theta)
		df <- rbind(df, c(stream, i, j))
	}
}
names(df) <- c("stream", "x", "y")

### body ###
thetas <- seq(0, 2*pi, 0.01)

wing <- data.frame()
Cpdf <- data.frame()


for (theta in thetas){
	r <- (0.5 - (theta / (2 * pi))) / sin(theta)
	wing <- rbind(wing, c(x = r*cos(theta), y = r*sin(theta)))
	
	# for part b #
	Vr <- (1/r)*((Lambda/(2*pi))+(Uinf*r*cos(theta)))
	Vtheta <- -Uinf*sin(theta)
	V <- sqrt((Vr^2) + (Vtheta^2))
	Cp <- 1 - ((V/Uinf)^2)
	
	Cpdf <- rbind(Cpdf, c(Cp, theta))
}
names(wing) <- c("x", "y")
wing <- filter(wing, x <= 1)

names(Cpdf) <- c("Cp", "theta")

### create plots ###
pl1 <- ggplot() + 
	#geom_tile(data = df, aes(x = x, y = y, z = stream, fill = stream)) + 
	geom_contour(data = df, aes(x = x, y = y, z = stream), binwidth = 0.05) + # geom_tile() #geom_point(aes(colour = stream))
	geom_path(data = wing, aes(x = x, y = y), size=1.5) +
	xlab("") +
	ylab("") +
	scale_x_continuous(breaks=c()) +
	scale_y_continuous(breaks=c()) +
	theme_bw()
print(pl1)


pl2 <- ggplot(Cpdf, aes(x = theta, y = Cp)) +
	geom_path(size=1.5) +
	xlab(expression(theta[body])) +
	ylab(expression(Cp[body])) +
	theme_bw()+ 
	theme(axis.title.y = element_text(size = 16), 
		  axis.title.x = element_text(size = 16))
print(pl2)
### End of script ###