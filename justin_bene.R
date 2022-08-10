#input your .csv files. One csv file per species
benA <- read.csv(file="benA1.csv")
benB <- read.csv(file="benB.csv")
neo <- read.csv(file="neoB.csv")
benL <- read.csv(file="benL.csv")

pool <- rbind(benA,benB,neo,benL)
dim(benA); dim(benB); dim(neo); dim(benL)

#fill the dimensions in based on what you get from the above line of command
index.benA <- 1:131
index.benB <- 132:205
index.neo <- 206:319
index.benL <- 320:324


#don't change anything here
pool.pca <- princomp(pool[,-c(1,2)])
summary (pool.pca)

#don't change anything here
lambda <- pool.pca$sdev[1:2] * sqrt(pool.pca$n.obs)
variable <- t ( t(pool.pca$loadings[,c(1,2)]) * lambda )
scores <- t ( t(pool.pca$scores[,c(1,2)]) / lambda )

#change according to the dimensions in line 15-20, but leave the '1' and '2' there 
centroid.benA <- c(mean(scores[1:131,1]) , mean(scores[1:131,2]))
centroid.benB <- c(mean(scores[132:205,1]), mean(scores[132:205,2]))
centroid.neo <- c(mean(scores[206:319,1]), mean(scores[206:319,2]))
centroid.benL <- c(mean(scores[320:324,1]), mean(scores[320:324,2]))

#remember to match this with the xlim and ylim in the plot function
xhist <- hist(scores[,1], breaks=seq(-0.2,0.2,0.01), plot=FALSE)
yhist <- hist(scores[,2], breaks=seq(-0.15,0.2,0.01), plot=FALSE)

#don't change anything here
nf <- layout(matrix(c(2,0,1,3),2,2, byrow=TRUE), c(3,1), c(1,3), TRUE)
layout.show(nf)
round (variable,0)

#this is the plotting function. Change the dimensions as per lines 15-20
##change the x and y lables according to what you get from 'round(variable,0)'
plot(scores[1:131,1], scores[1:131,2], xlab="PC1 (62%)", ylab="PC2 (22%)", pch=1, xlim=c(-.2,.2), ylim=c(-.15,.2))
points(scores[132:205,1], scores[132:205,2], pch=3)
points(scores[206:319,1], scores[206:319,2], pch=2)
points(scores[320:324,1], scores[320:324,2], pch=12)


#this is your legend, you can change the fonts, font size, color, position of legend box etc...
abline(h=0, lty=3) ; abline(v=0, lty=3)
legend(0.025, 0.2, cex=0.7, pch=c(1,3,2,12), expression(paste(italic("Benedenia A")), paste(italic("Benedenia B")), paste(italic("Neobenedenia B")), paste(italic("Benedenia lutjani")) ))

#no need to change anything here
par(mar = c(0,3.5,1,1))
barplot(xhist$counts, axes=FALSE, space=0, main="")
par(mar=c(5,0,3.5,1))
barplot(yhist$counts, axes=FALSE, space=0, main="", horiz=TRUE)
