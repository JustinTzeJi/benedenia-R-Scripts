#input your .csv files. One csv file per species
benA <- read.csv(file="benAovaant.csv")
benB <- read.csv(file="benBovaant.csv")
neo <- read.csv(file="neoBovaant.csv")
benL <- read.csv(file="benLovaant.csv")

pool <- rbind(benA,benB,neo,benL)
dim(benA); dim(benB); dim(neo); dim(benL)

#fill the dimensions in based on what you get from the above line of command
index.benA <- 1:160
index.benB <- 161:259
index.neo <- 260:435
index.benL <- 435:440


#don't change anything here
pool.pca <- princomp(pool[,-c(1,2)])
summary (pool.pca)

#don't change anything here
lambda <- pool.pca$sdev[1:2] * sqrt(pool.pca$n.obs)
variable <- t ( t(pool.pca$loadings[,c(1,3)]) * lambda )
scores <- t ( t(pool.pca$scores[,c(1,3)]) / lambda )

#change according to the dimensions in line 15-20, but leave the '1' and '2' there 
centroid.benA <- c(mean(scores[1:160,1]) , mean(scores[1:160,2]))
centroid.benB <- c(mean(scores[161:259,1]), mean(scores[161:259,2]))
centroid.neo <- c(mean(scores[260:435,1]), mean(scores[260:435,2]))
centroid.benL <- c(mean(scores[435:440,1]), mean(scores[435:440,2]))

#remember to match this with the xlim and ylim in the plot function
xhist <- hist(scores[,1], breaks=seq(-.15,.15,0.01), plot=FALSE)
yhist <- hist(scores[,2], breaks=seq(-.15,.1,0.01), plot=FALSE)

#don't change anything here
nf <- layout(matrix(c(2,0,1,3),2,2, byrow=TRUE), c(6,1), c(1,6), TRUE)
layout.show(nf)
round (variable,0)

#this is the plotting function. Change the dimensions as per lines 15-20
##change the x and y lables according to what you get from 'round(variable,0)'
plot(scores[1:160,1], scores[1:160,2], xlab="PC1 (66%)", ylab="PC2 (19%)", pch=16, col=1, xlim=c(-.15,.15), ylim=c(-.15,.10))
points(scores[161:259,1], scores[161:259,2], pch=8, col=2)
points(scores[260:435,1], scores[260:435,2], pch=17, col=3)
points(scores[435:440,1], scores[435:440,2], pch=15, col=4)


#this is your legend, you can change the fonts, font size, color, position of legend box etc...
abline(h=0, lty=3) ; abline(v=0, lty=3)
legend(-.15, -.07, cex=0.8, pch=c(16,8,17,15), col = (1:4), expression(paste(italic("Benedenia A")), paste(italic("Benedenia B")), paste(italic("Neobenedenia B")), paste(italic("Benedenia lutjani")) ))

#no need to change anything here
par(mar = c(0,3.5,1,1))
barplot(xhist$counts, axes=FALSE, space=0, main="")
par(mar=c(5,0,3.5,1))
barplot(yhist$counts, axes=FALSE, space=0, main="", horiz=TRUE)
