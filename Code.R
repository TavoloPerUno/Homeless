library(git2r)
library(xlsx)

if(!require("ghit")){
  install.packages("ghit")
}
# elsewhere
ghit::install_github(c("ropenscilabs/tabulizerjars", "ropenscilabs/tabulizer"))

library(tabulizer)

test <- extract_tables('2016PITSummaryReportFinal091216.10.pdf')


homeless <- data.frame(COMMAREA = character(77))
homeless$COMMAREA <- trimws(c(test[[1]][3:48,1], test[[1]][3:33,11]))
homeless[57,] <- "O'HARE"

homeless$PCHOMELESS <- as.numeric(unlist(strsplit(as.character(rbind(filter(data.frame(x = unlist(strsplit(as.vector(test[[1]][3:48,9]), " "))), grepl("%", x)),
                                                                     filter(data.frame(x = unlist(strsplit(as.vector(test[[1]][3:48,20]), " "))), grepl("%", x)))$x), '%')))

census <- read.xlsx2("Census.xlsx", sheetIndex = 1)
census$COMMAREA <- toupper(colsplit(census$COMMAREA, ". ", c("Num", "Name"))$Name)
census[57,]$COMMAREA <- "ARCHER HEIGHTS"
census[76,]$COMMAREA <- "O'HARE"

mergedcensus <- merge(census, homeless, "COMMAREA")

hardship <- read.csv("Hardship.csv")
hardship$COMMAREA <- toupper(hardship$COMMAREA)
hardship[57,]$COMMAREA <- "ARCHER HEIGHTS"
hardship[18,]$COMMAREA <- "MONTCLARE"
hardship[73,]$COMMAREA <- "WASHINGTON HEIGHTS"
hardship[76,]$COMMAREA <- "O'HARE"
hardship <- hardship[-78,]

mergedcensus <- merge(mergedcensus, hardship, "COMMAREA")

write.csv(mergedcensus, file="chicensus.csv")
remove(census, hardship, test)
