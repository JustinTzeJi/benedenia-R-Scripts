library(FSA)

kruskal <- read.csv(file="compilewhitN.csv")


wilcox.test(AS ~ SP, data=kruskal)
wilcox.test(AH ~ SP, data=kruskal)
wilcox.test(PH ~ SP, data=kruskal)
wilcox.test(TTW ~ SP, data=kruskal)
wilcox.test(TTL ~ SP, data=kruskal)
wilcox.test(OVW ~ SP, data=kruskal)
wilcox.test(OVL ~ SP, data=kruskal)
wilcox.test(PEL ~ SP, data=kruskal)
wilcox.test(SW ~ SP, data=kruskal)
wilcox.test(SL ~ SP, data=kruskal)
wilcox.test(PHW ~ SP, data=kruskal)
wilcox.test(PHL ~ SP, data=kruskal)