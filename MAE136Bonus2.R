# MAE 136 Bonus Assignment 2

# import required packages

library(dplyr)
library(ggplot2)

# constants

Uinf = 1
Lambda = 1

# create df
x <- seq(-1, 1, 0.01)
y <- seq(-1, 1, 0.01)


# fill in source


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

###############
thetas <- seq(0, 2*pi, 0.01)

wing <- data.frame()

for (theta in thetas){
	#theta = atan2(j, i)
	
	r <- (0.5 - (theta / (2 * pi))) / sin(theta)
	wing <- rbind(wing, c(x = r*cos(theta), y = r*sin(theta)))
}

names(wing) <- c("x", "y")

wing <- filter(wing, x <= 1)
###############


#pl <- ggplot(df, aes(x = r*cos(theta), y = r*sin(theta), fill = stream)) + geom_tile() #geom_contour()
pl <- ggplot(df, aes(x = x, y = y, z = stream, fill = stream)) + geom_contour(binwidth = 0.05)# geom_tile() #geom_point(aes(colour = stream))
print(pl)

#pl <- ggplot(df, aes(x = x, y = y, z = stream, fill = stream)) + geom_tile()
#print(pl)

pl1 <- ggplot() + 
	#geom_tile(data = df, aes(x = x, y = y, z = stream, fill = stream)) + 
	geom_contour(data = df, aes(x = x, y = y, z = stream), binwidth = 0.05) + # geom_tile() #geom_point(aes(colour = stream))
	geom_path(data = wing, aes(x = x, y = y), size=2)

print(pl1)