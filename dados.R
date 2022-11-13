library(readxl)
library(data.table)
library(vars)
library(urca)



### Empresas B3
empB3 <- data.table(read_xlsx("dados/Tickers.xlsx"))
dsBR <- data.table(read_xlsx("dados/Tickers.xlsx",sheet = "DataStream"))
dsBR <- dsBR[RIC %in% empB3$Ticker]


### ESG Grupo por ano
esgB3 <- data.table(read_xlsx("dados/ESG_Score.xlsx"))
esgB3[ESG_Score == "NULL",ESG_Score := NA]
esgB3 <- esgB3[!is.na(ESG_Score)]
esgB3[,Ano := year(Data)]
esgB3[,ESG_Score := as.numeric(ESG_Score)]
esgB3[,Median := median(ESG_Score),.(Ano)]
esgB3 <- esgB3[,.(ESG_Score = mean(ESG_Score)),.(Stock,Ano,Median)]
esgB3[ESG_Score >= Median,Group := "Alto"]
esgB3[ESG_Score < Median,Group := "Baixo"]


### Preco
priceB3 <- data.table(read_xlsx("dados/PriceClose.xlsx"))
priceB3 <- priceB3[!Price_Close == "NULL"]
priceB3[,Price_Close := as.numeric(Price_Close)]
priceB3[,Ano := year(Data)]
priceB3[,UltReg := max(Data),.(Stock,Ano)]
priceB3 <- priceB3[Data == UltReg]
priceB3 <- unique(priceB3)
priceB3[,Cnt := .N,.(Stock)]
priceB3[,UltReg := max(Data),Stock]
priceB3[,UltAno := year(UltReg)]
### Filtrando acoes com mais de 10 anos e terminam em 2020
priceB3 <- priceB3[Cnt >= 10 & UltAno == 2020]

### Filtrando ESG
esgB3 <- esgB3[Stock %in% unique(priceB3$Stock)]

### Dividendos
divdB3_a <- data.table(read_xlsx("dados/2 TR.CashDividendsPaid A.xlsx"))
divdB3_b <- data.table(read_xlsx("dados/2 TR.CashDividendsPaid B.xlsx"))
divdB3 <- rbind(divdB3_a,divdB3_b)[Stock %in% unique(priceB3$Stock),.(Stock,Data,Dividends = Total_Cash_Dividends_Paid)]
rm(divdB3_a,divdB3_b)
divdB3 <- divdB3[!Dividends == "NULL"]
divdB3[,Dividends := as.numeric(Dividends)]
divdB3[Dividends <= 0,Dividends := 1]
divdB3[,Ano := year(Data)]

### Ganhos
earnB3_a <- data.table(read_xlsx("dados/2 TR.NetIncome A.xlsx"))
earnB3_b <- data.table(read_xlsx("dados/2 TR.NetIncome B.xlsx"))
earnB3 <- rbind(earnB3_a,earnB3_b)[Stock %in% unique(priceB3$Stock),.(Stock,Data,Earnings = Net_Income)]
rm(earnB3_a,earnB3_b)
earnB3 <- earnB3[!Earnings == "NULL"]
earnB3[,Earnings := as.numeric(Earnings)]
earnB3[Earnings <= 0,Earnings := 1]
earnB3[,Ano := year(Data)]
earnB3[,ern := log(Earnings)]

### Assets
assetB3_a <- data.table(read_xlsx("dados/2 TR.TotalCurrentAssets A.xlsx"))
assetB3_b <- data.table(read_xlsx("dados/2 TR.TotalCurrentAssets B.xlsx"))
assetB3 <- rbind(assetB3_a,assetB3_b)[Stock %in% unique(priceB3$Stock),.(Stock,Data,Assets = Total_Current_Assets)]
rm(assetB3_a,assetB3_b)
assetB3 <- assetB3[!Assets == "NULL"]
assetB3[,Assets := as.numeric(Assets)]
assetB3[,Ano := year(Data)]

### Liabilities
liabB3_a <- data.table(read_xlsx("dados/2 TR.TotalCurrLiabilities A.xlsx"))
liabB3_b <- data.table(read_xlsx("dados/2 TR.TotalCurrLiabilities B.xlsx"))
liabB3 <- rbind(liabB3_a,liabB3_b)[Stock %in% unique(priceB3$Stock),.(Stock,Data,Liabilities = Total_Current_Liabilities)]
rm(liabB3_a,liabB3_b)
liabB3 <- liabB3[!Liabilities == "NULL"]
liabB3[,Liabilities := as.numeric(Liabilities)]
liabB3[,Ano := year(Data)]

### Book Value
bookB3 <- assetB3[liabB3[,.(Stock,Ano,Liabilities)],on = c("Stock","Ano"),nomatch=0]
bookB3[,BookValue := Assets - Liabilities]
bookB3[BookValue <= 0,BookValue := 1]
bookB3[,bv := log(BookValue)]

### ROE
roeB3 <- earnB3[bookB3[,.(Stock,Ano,bv)],on = c("Stock","Ano"),nomatch=0]
roeB3[,ROE := ern/bv]
roeB3[is.infinite(ROE),ROE := NA_real_]
roeB3[is.nan(ROE),ROE := NA_real_]
roeB3[,ROEavg := mean(ROE,na.rm=T),.(Stock)]
roeB3[is.na(ROE) | ROE == 0,ROE := ROEavg]

num_anosteste <- 3
### ROE Forecast
stocks <- unique(roeB3$Stock)
ts_roe <- lapply(stocks,getTS,"ROE",num_anosteste,roeB3)
ts_roe <- lapply(ts_roe,forecastTS)
roeB3 <- rbindlist(lapply(ts_roe,forecastDT))

### BV Forecast
stocks <- unique(bookB3$Stock)
ts_bv <- lapply(stocks,getTS,"BookValue",num_anosteste,roeB3)
ts_bv <- lapply(ts_roe,forecastTS)
roeB3 <- rbindlist(lapply(ts_roe,forecastDT))






### ICC - GLS


dtICC_GLS <- rbindlist(lapply(stocks,getICC_GLS,roeB3))
dtICC_GLS[,ICC_GLS := round(ICC_GLS,3)]








### ICCS
ano = 2016

dt_ICCs <- dtICC_GLS[esgB3[Ano == ano,.(Stock,Group)], on = "Stock"]



dt_ICCs_sum <- dt_ICCs[,lapply(.SD,mean,na.rm = T),by = Group,.SDcols = c("ICC_GLS")]


### Stock Data
stockData <- copy(esgB3)
stockData <- stockData[priceB3[,.(Stock,Ano,Price_Close)],on = c("Stock","Ano"),nomatch = 0]
stockData <- stockData[divdB3[,.(Stock,Ano,Dividends)],on = c("Stock","Ano"),nomatch = 0]
stockData <- stockData[earnB3[,.(Stock,Ano,Earnings)],on = c("Stock","Ano"),nomatch = 0]
stockData <- stockData[order(Stock,Ano)]
stockData <- unique(stockData)
stockData[,ret := log(Price_Close/shift(Price_Close,1,type = "lag")),.(Stock)]
stockData[,prc := log(Price_Close)]
stockData[,dvd := log(Dividends)]
stockData[,ear := log(Earnings)]


getVAR_Matrix <- function(stock,stockData){
   tryCatch({
      dt <- stockData[Stock == stock]
      dt[,delta := shift(dvd,1,0,"lag")-prc]
      dt[,epsil := shift(ear,1,0,"lag")-prc]
      dt[,lag := shift(dvd,1,NA,"lag")]
      dt <- dt[-1]
      dt2 <- dt[,.(ret,delta,lag,epsil)]
      dt <- dt[,.(delta,lag,epsil)]
      var.model <- VAR(dt)
      lin.reg <- lm(formula = ret ~ delta + lag + epsil, data = dt2)
      A = t(matrix(c(
         var.model$varresult$delta$coefficients,
         var.model$varresult$lag$coefficients,
         var.model$varresult$epsil$coefficients,
         c(lin.reg$coefficients[2:4],lin.reg$coefficients[1])
      ),nrow = 4))
      U = A[,4]
      A = A[,1:3]
      R2 = c(summary(var.model$varresult$delta)$r.squared,summary(var.model$varresult$lag)$r.squared,
             summary(var.model$varresult$epsil)$r.squared,summary(lin.reg)$r.squared)
     list(stock = stock,A = A, U = U, R2 = R2)
   }, error = function(e) {
     list(stock = stock,A = NULL, U = NULL, R2 = NULL)
   })
}

VAR_Matrizes <- lapply(unique(stockData$Stock),getVAR_Matrix,stockData)
names(VAR_Matrizes) <- unique(stockData$Stock)




