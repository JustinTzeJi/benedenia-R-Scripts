library(FSA)
library(rcompanion)
library(dunn.test)


kruskal <- read.csv(file="compkrus.csv")


dunnTest(AS ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)
dunnTest(AH ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)
dunnTest(PH ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)
dunnTest(TTW ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)
dunnTest(TTL ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)
dunnTest(OVL ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)
dunnTest(OVW ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)
dunnTest(PEL ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)
dunnTest(SW ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)
dunnTest(SL ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)
dunnTest(PHW ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)
dunnTest(PHL ~ SP, data=kruskal, method="bonferroni", two.sided =TRUE)