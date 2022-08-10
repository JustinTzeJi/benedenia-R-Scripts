pool <- read.csv(file="bigtest.csv")

#to select only columns of the variable
df <- pool[c(2:21)] 

library(ggplot2)
library(ggfortify)

#PCA plot
autoplot(prcomp(df), data = pool, colour = 'SP')

##for plot with specimen name and loading score
#autoplot(prcomp(df), data = pool, colour = 'SP', shape = FALSE, label.size = 3,
#         loadings = TRUE, loadings.colour = 'blue',
#         loadings.label = TRUE, loadings.label.size = 3)

## changing pc
#autoplot(prcomp(df), x=1, y=3 , data = pool, colour = 'SP')


#PCA plot with eclispe

library(cluster)

autoplot(pam(df[-5], 3),data = pool, colour = 'SP', frame = TRUE, frame.type = 'norm')


#for PCA data
pca <- prcomp(pool[2:21], scale=TRUE) 
summary (pca)

#can be used to make scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

## get the name of the measurements that contribute most to pc1.
loading_scores <- pca$rotation[,1]
part_scores <- abs(loading_scores) ## get the magnitudes
part_score_ranked <- sort(part_scores, decreasing=TRUE)
part_rank <- names(part_score_ranked)

part_rank ## show the rank of parts

pca$rotation[part_rank,1] ## show the scores (and +/- sign)