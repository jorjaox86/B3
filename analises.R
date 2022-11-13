library(readxl)
library(rootSolve)
library(data.table)
library(pbapply)
library(ggplot2)

source('apoio.R')

### Empresas B3
empB3 <- data.table(read_xlsx("dados/Tickers.xlsx"))
dsBR <- data.table(read_xlsx("dados/Tickers.xlsx",sheet = "DataStream"))
dsBR <- dsBR[RIC %in% empB3$Ticker]

esgB3 <- data.table(read_xlsx("dados/ESG_Score.xlsx"))
esgB3[ESG_Score == "NULL",ESG_Score := NA]
esgB3 <- esgB3[!is.na(ESG_Score)]
esgB3[,Ano := year(Data)]
esgB3[,ESG_Score := as.numeric(ESG_Score)]
esgB3 <- esgB3[,.(ESG_Score = mean(ESG_Score)),.(Stock)]


### Planilha de dados do datastream
dados <- data.table(read_xlsx("dados/datastream.xlsx",sheet="dados"))
endtimecol <- ncol(dados)
anos <- names(dados)[3:endtimecol]
anos <- substr(as.numeric(anos)- 10 + as.Date("1900-01-01"),1,4)
names(dados)[3:endtimecol] <- anos
ref <- data.table(read_xlsx("dados/datastream.xlsx",sheet="ref",range = "B1:C26",col_names = c("CODE","VAR")))
dados[,Empresa := unlist(lapply(strsplit(dados$Name," - "),function(x)x[1]))]
dados[,VarCode := gsub('^.*\\(|\\)','',Code)]
dados[,EmpCode := gsub('\\(.*','',Code)]
dadosn <- data.table::melt(dados,id.vars = c("Empresa","EmpCode","VarCode"),measure = 3:endtimecol,
                           value.name = "Valor",variable.name = "Ano",variable.factor = F)
dadosn[,Ano := as.numeric(Ano)]
dadosn <- dadosn[order(Empresa,VarCode,Ano)]
suppressWarnings(dadosn[,Valor := as.numeric(Valor)])
dadosn <- dadosn[complete.cases(dadosn)]
dadosn <- ref[dadosn,on = .(CODE = VarCode)]
dadosn[,CODE := NULL]

### Tratamento dados Previstos - Completando com previsoes futuras caso faltantes
dt <- copy(dadosn)
dtY <- dcast(dt[Ano %in% 2015:2018],EmpCode + VAR ~ Ano,value.var = "Valor")
dtref <- data.table(expand.grid(unique(dtY$EmpCode),ref$VAR))
names(dtref) <- c("EmpCode","VAR")
dtY <- dtY[dtref,on=c("EmpCode","VAR")]
dtY <- dtY[order(EmpCode,VAR)]
dtYF <- dtY[VAR %in% ref$VAR[-(1:6)]]
dtYF[,RefVAR := substr(VAR,1,3)]
dtYF[!is.na(`2015`),VC := .N,.(EmpCode,RefVAR)]
dtYF[,VC := mean(VC,na.rm=T),.(EmpCode,RefVAR)]
dtYF <- dtYF[,EVC := all(VC >= 3),.(EmpCode)]
dtYF <- dtYF[EVC == T]
dtYF[,D2016:= shift(`2016`,n = 3,type = "lag"),.(EmpCode,RefVAR)]
dtYF[,D2017 := shift(`2017`,type = "lag"),.(EmpCode,RefVAR)]
dtYF[,D2018:= shift(`2018`,n = 2,type = "lag"),.(EmpCode,RefVAR)]
dtYF[is.na(`2015`),`2015` := ifelse(!is.na(D2016),D2016,ifelse(!is.na(D2017),D2017,ifelse(!is.na(D2018),D2018,NA_real_)))]
dtYF <- dtYF[!EmpCode %in% unique(dtYF[is.na(`2015`)]$EmpCode)]
dtYF[,Ano := as.numeric(substr(VAR,4,4))+2015]
setnames(dtYF,"2015","Valor")
#### Dados Previstos tratados
dadosF <- dtYF[,.(EmpCode,RefVAR,VAR,Ano,Valor)]
dadosF <- dcast(dadosF,EmpCode + Ano ~ RefVAR,value.var = "Valor")
dadosF_chk <- dadosF[Ano %in% c(2016,2017),.(EmpCode,Ano,EPS)]
dadosF_chk[,chk := all(EPS>0),.(EmpCode)]
dadosF_chk <- dadosF_chk[chk==T]
dadosF_chk[Ano == 2016,EPS := 1/EPS]
dadosF_chk <- dadosF_chk[,.(EPS = prod(EPS)),.(EmpCode)]
dadosF_chk <- dadosF_chk[EPS > 1]
dadosF <- dadosF[EmpCode %in% dadosF_chk$EmpCode]
dadosI <- dadosn[VAR %in% c("PRC","BPS","EPS","DPS") & Ano == 2015 & EmpCode %in% unique(dadosF$EmpCode)]
dadosI <- dcast(dadosI,EmpCode ~ VAR,value.var = "Valor")
### Empresas
stocks <- unique(dadosF$EmpCode)


### Dados ROE
ROEtip <- ROEestimado(data.table(read_xlsx("dados/datastream.xlsx",sheet="dadosROE")))

### ICC - GLS
# Calcula ICC GLS
dtGLS_ICC <- rbindlist(lapply(stocks,ICC_GLS,dadosF,dadosI,ROEtip))
#pltGLS <- lapply(stocks,plotGLS,dadosF_0,dadosI_0,ROEtip)


### ICC - CT
## Claus usa um gae na formula correspondente a expectativa de inflacao futura e ICC > gae
dtCT_ICC <- rbindlist(lapply(stocks,ICC_CT,dadosF,dadosI,gae = 0.05))
#pltCT <- lapply(stocks,plotCT,dadosF,dadosI)

### ICC - OJ
dtOJ_ICC <- rbindlist(lapply(stocks,ICC_OJ,dadosF,dadosI,gma = 1.02))

### ICC - E
dtE_ICC <- rbindlist(lapply(stocks,ICC_E,dadosF,dadosI))


dtICC <- copy(dtGLS_ICC)
dtICC <- dtICC[dtCT_ICC,on="EmpCode",nomatch=0]
dtICC <- dtICC[dtOJ_ICC,on="EmpCode",nomatch=0]
dtICC <- dtICC[dtE_ICC,on="EmpCode",nomatch=0]
dtICC <- dtICC[!EmpCode %in% c("BR:SAB","BR:RG3")]
dtICC <- dtICC[dsBR[,.(Symbol,RIC)],on=.(EmpCode=Symbol),nomatch=0]
dtICC <- dtICC[esgB3,on = .(RIC = Stock),nomatch=0]
dtICC[,Median := median(ESG_Score)]
dtICC[ESG_Score >= Median,Group := "Alto"]
dtICC[ESG_Score < Median,Group := "Baixo"]

dtICCg <- dtICC[,lapply(.SD,mean),by=.(Group),.SDcols=grep('ICC.*',colnames(dtICC),value = T)]
View(dtICCg)
