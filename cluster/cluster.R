setwd("~/Code/r/BigDataLabs/cluster")

df <- read.table("assess.dat", header=TRUE, row.names = "NR")
summary(df)


dst <- dist(df, method="euclidean")

hc <- hclust(dst, method="ward.D2")

plot(hc, hang=1)

rect.hclust(hc, k=4)
clusters <- cutree(hc, k=4)
print(clusters)

aggregate(df, by=list(cluster=clusters), mean)

