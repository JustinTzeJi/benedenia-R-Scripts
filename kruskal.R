library(agricolae)
library(coin)

kruskal <- read.csv(file="compkrus.csv")

kruskal.test(AS ~ SP, data = kruskal)
kruskal(kruskal$AS, kruskal$SP, console = TRUE)


kruskal.test(AH ~ SP, data = kruskal)
kruskal(kruskal$AH, kruskal$SP, console = TRUE)


kruskal.test(PH ~ SP, data = kruskal)
kruskal(kruskal$PH, kruskal$SP, console = TRUE)


kruskal.test(TTW ~ SP, data = kruskal)
kruskal(kruskal$TTW, kruskal$SP, console = TRUE)


kruskal.test(TTL ~ SP, data = kruskal)
kruskal(kruskal$TTL, kruskal$SP, console = TRUE)


kruskal.test(OVL ~ SP, data = kruskal)
kruskal(kruskal$OVL, kruskal$SP, console = TRUE)


kruskal.test(OVW ~ SP, data = kruskal)
kruskal(kruskal$OVW, kruskal$SP, console = TRUE)


kruskal.test(PEL ~ SP, data = kruskal)
kruskal(kruskal$PEL, kruskal$SP, console = TRUE)


kruskal.test(SW ~ SP, data = kruskal)
kruskal(kruskal$SW, kruskal$SP, console = TRUE)


kruskal.test(SL ~ SP, data = kruskal)
kruskal(kruskal$SL, kruskal$SP, console = TRUE)


kruskal.test(PHW ~ SP, data = kruskal)
kruskal(kruskal$PHW, kruskal$SP, console = TRUE)


kruskal.test(PHL ~ SP, data = kruskal)
kruskal(kruskal$PHL, kruskal$SP, console = TRUE)