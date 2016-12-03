library(foreign)
library(ggplot2)

chicensus <- read.csv("chicensus.csv")

varnames <- c("PER.CAPITA.INCOME",
              "PERCENT.HOUSEHOLDS.BELOW.POVERTY",
              "PERCENT.AGED.UNDER.18.OR.OVER.64",
              "PERCENT.AGED.16..UNEMPLOYED",
              "PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA",
              "PCINCBELPOV", 
              "PCASIAN", 
              "PCAFRICAN", 
              "PCLATIN", 
              "PCEUROPEAN", 
              "PCOTHER", 
              "PCFORBORN", 
              "PCHIGHSCHOOLPLUS", 
              "PCBAPLUS",
              "PCOWNEROCC",
              "PCINC10",
              "PCINC15",
              "PCINC20",
              "PCINC25",
              "PCINC30",
              "PCINC35",
              "PCINC40",
              "PCINC50",
              "PCINC60",
              "PCINC75",
              "PCINC100",
              "PCINC125",
              "PCINC150",
              "PCINC200",
              "PCINC200PLUS",
              "POPTOTAL"
               )

vd1 <- chicensus[,varnames]
vds <- scale(vd1)

prc <- prcomp(vds)

str(prc)

summary(prc)

scree_plot <- function(princ,cumulative=FALSE)
{
  pv <- princ$sdev^2 
  pve <- pv / sum(pv) 
  mtitle="Scree Plot" 
  if (cumulative){
    pve <- cumsum(pve)
    mtitle="Cumulative Variance Proportion" 
  }
  plot(pve,type="b",main=mtitle,xlab="Principal Components", ylab="Proportion Variance Explained")
}

scree_plot(prc, cumulative=TRUE)

biplot(prc, scale = 0)

vdiss <- dist(vds)

pcscores <- prc$x
pcs1 <- as.data.frame(pcscores)
pcs1$COMMAREANO <- chicensus$COMMAREANO
pcvarnames <- c("PC1","PC2","PC3","PC4",
                "PC5", "PC6", "PC7", "PC8",
                "PC9", "PC10")

vd <- pcs1[,pcvarnames]
write.csv(pcs1[,c(pcvarnames, "COMMAREANO")], "censuspcs.csv")
rowSums(vd)
write.csv(data.frame(cbind(chicensus$COMMAREANO, rowSums(vd))), "pc.csv")

vds <- scale(vd)
vdist <- dist(vds)

hc1 <- hclust(vdist,method="complete")

plot(hc1,main="Complete linkage",xlab="",sub="")

hc1_4 <- cutree(hc1,4)

table(hc1_4)

hcdata  <- data.frame(chicensus$COMMAREANO, hc1_4)

write.csv(hcdata, "hc.csv")

set.seed(1234567)

km1_4 <- kmeans(vds,4,nstart=25, iter.max = 1000)
km1_4

kmdat <- as.data.frame(cbind(chicensus$COMMAREANO, as.numeric(km1_4$cluster)))
write.csv(kmdat, "km4.csv")