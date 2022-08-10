#input your .csv files. One csv file per species
hk <- read.csv(file="neoBhk.csv")
my <- read.csv(file="neoBmy.csv")


pool <- rbind(hk,my)
dim(hk); dim(my)

#fill the dimensions in based on what you get from the above line of command
index.hk <- 1:13
index.my <- 14:176



#don't change anything here
pool.pca <- princomp(pool[,-c(1,2)])
summary (pool.pca)

#don't change anything here
lambda <- pool.pca$sdev[1:2] * sqrt(pool.pca$n.obs)
variable <- t ( t(pool.pca$loadings[,c(1,3)]) * lambda )
scores <- t ( t(pool.pca$scores[,c(1,3)]) / lambda )

#change according to the dimensions in line 15-20, but leave the '1' and '2' there 
centroid.hk <- c(mean(scores[1:13,1]) , mean(scores[1:13,2]))
centroid.my <- c(mean(scores[14:176,1]), mean(scores[14:176,2]))


#remember to match this with the xlim and ylim in the plot function
xhist <- hist(scores[,1], breaks=seq(-.2,.2,0.01), plot=FALSE)
yhist <- hist(scores[,2], breaks=seq(-.2,.2,0.01), plot=FALSE)

#don't change anything here
nf <- layout(matrix(c(2,0,1,3),2,2, byrow=TRUE), c(6,1), c(1,6), TRUE)
layout.show(nf)
round (variable,0)

#this is the plotting function. Change the dimensions as per lines 15-20
##change the x and y lables according to what you get from 'round(variable,0)'
plot(scores[1:13,1], scores[1:13,2], xlab="PC1 (73%)", ylab="PC3 (5%)", pch=16, col=1, xlim=c(-.2,.2), ylim=c(-.2,.2))
points(scores[14:176,1], scores[14:176,2], pch=8, col=2)

#this is your legend, you can change the fonts, font size, color, position of legend box etc...
abline(h=0, lty=3) ; abline(v=0, lty=3)
legend(-.2, .2, cex=0.8, pch=c(16,8), col = (1:2), expression(paste(italic("Hong Kong")), paste(italic("Malaysia")) ))

#no need to change anything here
par(mar = c(0,3.5,1,1))
barplot(xhist$counts, axes=FALSE, space=0, main="")
par(mar=c(5,0,3.5,1))
barplot(yhist$counts, axes=FALSE, space=0, main="", horiz=TRUE)
