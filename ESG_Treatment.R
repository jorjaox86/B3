###Inserindo as colunas E S e G na tabela de ESG, testar na esgB3
BetaNames <- data.table(read_xlsx("BetaNames.xlsx"))
esgTEMP <- data.table(read_xlsx("dados/ESG_Score.xlsx"))
esgTEMP[ESG_Score == "NULL",ESG_Score := NA]
esgTEMP[Environmental_Score == "NULL",Environmental_Score := NA]
esgTEMP[Governance_Score == "NULL",Governance_Score := NA]
esgTEMP[Social_Score == "NULL",Social_Score := NA]
esgTEMP <- esgTEMP[!is.na(Environmental_Score)]
esgTEMP <- esgTEMP[!is.na(Governance_Score)]
esgTEMP <- esgTEMP[!is.na(Social_Score)]
esgALT <- esgTEMP
esgTEMP[,Ano := year(Data)]
esgTEMP[,ESG_Score := as.numeric(ESG_Score)]
esgTEMP[,Environmental_Score := as.numeric(Environmental_Score)]
esgTEMP[,Governance_Score := as.numeric(Governance_Score)]
esgTEMP[,Social_Score := as.numeric(Social_Score)]
esgTEMP1 <- esgTEMP[,.(ESG_Score = mean(ESG_Score)),.(Stock)]
esgTEMP2 <- esgTEMP[,.(Environmental_Score = mean(Environmental_Score)),.(Stock)]
esgTEMP3 <- esgTEMP[,.(Governance_Score = mean(Governance_Score)),.(Stock)]
esgTEMP4 <- esgTEMP[,.(Social_Score = mean(Social_Score)),.(Stock)]

esgTEMP1[,Environmental_Score := esgTEMP2$Environmental_Score]
esgTEMP1[,Governance_Score := esgTEMP3$Governance_Score]
esgTEMP1[,Social_Score := esgTEMP4$Social_Score]
esgTEMP5 <- rowSums(esgTEMP1[, c("Environmental_Score", "Governance_Score","Social_Score")])
esgTEMP6 <- as.data.table(esgTEMP5)
esgTEMP1[,E_S_G := esgTEMP6$esgTEMP5]
esgB3 <-copy(esgTEMP1)

dsBR <- data.table(read_xlsx("dados/Tickers.xlsx",sheet = "DataStream"))
dsBR <- dsBR[RIC %in% empB3$Ticker]
dsBR <- dsBR[,.(Name,Symbol,RIC,Hist.,Sector)]

tb6_new <- as.data.table(tb6)

#################Código antigo##########################################################

esgB3 <- data.table(read_xlsx("dados/ESG_Score.xlsx"))
esgB3[ESG_Score == "NULL",ESG_Score := NA]
esgB3 <- esgB3[!is.na(ESG_Score)]
esgALT <- esgB3
esgB3[,Ano := year(Data)]
esgB3[,ESG_Score := as.numeric(ESG_Score)]
esgB3 <- esgB3[,.(ESG_Score = mean(ESG_Score)),.(Stock)]
esgB3
