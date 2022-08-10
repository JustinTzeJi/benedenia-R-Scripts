#input your .csv files. One csv file per species
benA <- read.csv(file="benAhardtest.csv")
benB <- read.csv(file="benBhard.csv")
neo <- read.csv(file="neoBhardtest.csv")

pool <- rbind(benA,benB,neo)
dim(benA); dim(benB); dim(neo)

#fill the dimensions in based on what you get from the above line of command
index.benA <- 1:157
index.benB <- 158:256
index.neo <- 257:430



#don't change anything here
pool.pca <- princomp(pool[,-c(1,2)])
summary (pool.pca)

#don't change anything here
lambda <- pool.pca$sdev[1:2] * sqrt(pool.pca$n.obs)
variable <- t ( t(pool.pca$loadings[,c(1,2)]) * lambda )
scores <- t ( t(pool.pca$scores[,c(1,2)]) / lambda )

#change according to the dimensions in line 15-20, but leave the '1' and '2' there 
centroid.benA <- c(mean(scores[1:157,1]) , mean(scores[1:157,2]))
centroid.benB <- c(mean(scores[158:256,1]), mean(scores[158:256,2]))
centroid.neo <- c(mean(scores[257:430,1]), mean(scores[257:430,2]))

#remember to match this with the xlim and ylim in the plot function
xhist <- hist(scores[,1], breaks=seq(-.15,.15,0.01), plot=FALSE)
yhist <- hist(scores[,2], breaks=seq(-.2,.15,0.01), plot=FALSE)

#don't change anything here
nf <- layout(matrix(c(2,0,1,3),2,2, byrow=TRUE), c(6,1), c(1,6), TRUE)
layout.show(nf)
round (variable,0)

#this is the plotting function. Change the dimensions as per lines 15-20
##change the x and y lables according to what you get from 'round(variable,0)'
plot(scores[1:157,1], scores[1:157,2], xlab="PC1 (66%)", ylab="PC2 (19%)", pch=16, col=1, xlim=c(-.15,.15), ylim=c(-.2,.15))
points(scores[157:255,1], scores[157:255,2], pch=8, col=2)
points(scores[257:430,1], scores[257:430,2], pch=17, col=3)

#this is your legend, you can change the fonts, font size, color, position of legend box etc...
abline(h=0, lty=3) ; abline(v=0, lty=3)
legend(-.15, -.08, cex=0.8, pch=c(16,8,17,15), col = (1:4), expression(paste(italic("Benedenia A")), paste(italic("Benedenia B")), paste(italic("Neobenedenia B"))))

#no need to change anything here
par(mar = c(0,3.5,1,1))
barplot(xhist$counts, axes=FALSE, space=0, main="")
par(mar=c(5,0,3.5,1))
barplot(yhist$counts, axes=FALSE, space=0, main="", horiz=TRUE)
