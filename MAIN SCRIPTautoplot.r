pool <- read.csv(file="goodnoout.csv")

library(ggplot2)
library(ggfortify)
library(cluster)

df <- pool[c(5:10)]
autoplot(prcomp(df), data = pool, colour = 'Monogenea', shape = FALSE, label.size = 3)

autoplot(prcomp(df) ,data = pool, colour = 'Genus', frame = TRUE, frame.type = 't',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


pca <- prcomp(df) 

summary (pca)

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

## get the name of the top 10 measurements (genes) that contribute
## most to pc1.
loading_scores <- pca$rotation[,1]
gene_scores <- abs(loading_scores) ## get the magnitudes
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked)

top_10_genes ## show the names of the top 10 genes

pca$rotation[top_10_genes,1] ## show the scores (and +/- sign)

##pc2
loading_scores2 <- pca$rotation[,2]
gene_scores2 <- abs(loading_scores2) ## get the magnitudes
gene_score_ranked2 <- sort(gene_scores2, decreasing=TRUE)
top_10_genes2 <- names(gene_score_ranked2)

top_10_genes2 ## show the names of the top 10 genes

pca$rotation[top_10_genes2,2] ## show the scores (and +/- sign)

pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

pcaCharts(pca)
print(pca.var)
print(pca.var.per)