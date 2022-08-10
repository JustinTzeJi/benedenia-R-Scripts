pool <- read.csv(file="bigtestno.csv")

library(ggplot2)
library(ggfortify)
library(cluster)

df <- pool[c(17:23)]
autoplot(prcomp(df), data = pool, colour = 'SP', shape = FALSE, label.size = 3)

autoplot(prcomp(df) ,data = pool, colour = 'SP', frame = TRUE, frame.type = 't')

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