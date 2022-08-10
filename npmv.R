pool <- read.csv(file="compilewhitN.csv")

library(npmv)
library(ggfortify)
library(cluster)

nonpartest(AS|AH|PH|TTW|TTL|OVW|OVL|SW|SL|PHW|PHL|PEL~SP,data=pool, permreps = 5000)
