protein <- read.table("protein.txt", sep="\t", header=TRUE)
ndata <- nrow(protein)
summary(protein)
vars.to.use <- colnames(protein)[-1]
pmatrix <- scale(protein[,vars.to.use])

pclusters <- kmeans(pmatrix, 5, nstart=100, iter.max=100)
summary(pclusters) 

kList <- seq(1,10,1)
get_ch_index <- function(k) {
  pclusters <- kmeans(pmatrix, k, nstart=100, iter.max=100)
  wss <- sum(pclusters$withinss)
  tss <- pclusters$totss
  bss <- pclusters$betweenss
  bss == tss - wss
  ch.index <- (bss/(k-1)) / (wss/(ndata-k))
  ch.index
}
getWSS <- function(i) {
  pclusters <- kmeans(pmatrix, i, nstart=100, iter.max=100)
  sum(pclusters$withinss)
}
chIndexList <- sapply(kList, get_ch_index)
plot(x=kList, y=chIndexList, xlab="k", ylab="CH-Index", type="l")
wssList <- sapply(kList, getWSS)
plot(x=kList, y=wssList, xlab="k", ylab="WSS", type="l")