library(spdep)
library(maptools)

chicago <- readShapePoly("chicensus_merge.shp")

summary(chicago)

plot(chicago,border=gray(0.5))

pcvarnames <- c("PC1","PC2","PC3","PC4",
                "PC5", "PC6", "PC7", "PC8",
                "PC9", "PC10")

dat <- data.frame(chicago@data[,pcvarnames])

sdat <- scale(dat)
chicago.nb <- poly2nb(chicago)

plot(chicago, border=gray(0.5))
plot(chicago.nb,coordinates(chicago),col="blue",add=TRUE)

lcosts <- nbcosts(chicago.nb,sdat)
chicago.w <- nb2listw(chicago.nb,lcosts,style="B")
chicago.mst <- mstree(chicago.w)

plot(chicago.mst, coordinates(chicago), col="blue", cex.lab=0.7)
plot(chicago,border=gray(.5),add=TRUE)

clus4 <- skater(chicago.mst[,1:2],sdat,3)
ccs4 <- clus4$groups

plot(clus4, coordinates(chicago), cex.lab=0.7, groups.colors=c("red","green","blue","brown"))
plot(chicago,border=gray(.5),add=TRUE)

plot(chicago,col=c("red","green","blue","brown")[clus4$groups])