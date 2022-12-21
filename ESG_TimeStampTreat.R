ESGtime2008 <- esgTEMP[Ano == 2008]
ESGtime2010 <- esgTEMP[Ano == 2010]
ESGtime2012 <- esgTEMP[Ano == 2012]
ESGtime2013 <- esgTEMP[Ano == 2013]
ESGtime2014 <- esgTEMP[Ano == 2014]
ESGtime2015 <- esgTEMP[Ano == 2015]
ESGtime2016 <- esgTEMP[Ano == 2016]
ESGtime2017 <- esgTEMP[Ano == 2017]
ESGtime2018 <- esgTEMP[Ano == 2018]
Score2014_15 <- ESGtime2014[, ESG_Score ,.(Stock)]
colnames(Score2014_15) <- c("Stock","ESG_2014")
auxscore <- ESGtime2015[, ESG_Score ,.(Stock)]
colnames(auxscore) <- c("Stock","ESG_2015")

auxscore <- auxscore[!duplicated(auxscore$Stock)]
Score2014_15 <- Score2014_15[!duplicated(Score2014_15$Stock)]
datamerged <- merge(Score2014_15, auxscore, by="Stock")

EMPList <- as.data.frame(dtICC_D$RIC)
colnames(EMPList) <- c("Stock")
datamerged2 <- merge(datamerged, EMPList, by = "Stock", all.y = TRUE)
colnames(datamerged2) <- c("RIC","ESG_2014","ESG_2015")
datamerged2$ESG_2014 <- -1*datamerged2$ESG_2014
datamerged3 <- as.data.table(rowSums(datamerged2[, c("ESG_2015", "ESG_2014")]), value.name = "DeltaESG")
datamerged2[,DeltaESG := datamerged3$V1]
datamerged2[DeltaESG >= 0, DeltaHL := "Alto"]
datamerged2[DeltaESG < 0, DeltaHL := "Baixo"]
datamerged2[DeltaESG >= quantile(DeltaESG,.5, na.rm=TRUE), HLAbove := "Alto"]
datamerged2[DeltaESG < quantile(DeltaESG,.5, na.rm=TRUE), HLBelow := "Baixo"]
datamerged2 <- datamerged2[!duplicated(datamerged2$RIC)]
dtICCCopy <- copy(dtICC_D)
dtICCCopy <- dtICCCopy[!duplicated(dtICCCopy$RIC)]
merge_aux <- merge(dtICCCopy, datamerged2, by = "RIC" ,all = TRUE)

merge_aux <- merge_aux[!is.na(merge_aux$DeltaESG)]


















