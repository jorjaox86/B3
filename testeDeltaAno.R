for(i in 1:5){
   anos2 <- anos+i-1
   print(anos2)

   dtICC_D5 <- GeraDeltaDAno(anos2)
   dtICC_0 <- dtICC_D5[[1]]
   dtICC_1 <- dtICC_D5[[2]]
   dtICC_D <- dtICC_D5[[3]]

   cutoff30_0 <- subset(dtICC_0, ESG_Score < 30)
   cutoff70_0 <- subset(dtICC_0, ESG_Score > 70)

   cutoff40_0 <- subset(dtICC_0, ESG_Score < 40)
   cutoff60_0 <- subset(dtICC_0, ESG_Score > 60)


   cutoff37_0 <- rbind(cutoff30_0,cutoff70_0)
   cutoff46_0 <- rbind(cutoff40_0,cutoff60_0)


   cutoff30_1 <- subset(dtICC_1, ESG_Score < 30)
   cutoff70_1 <- subset(dtICC_1, ESG_Score > 70)

   cutoff40_1 <- subset(dtICC_1, ESG_Score < 40)
   cutoff60_1 <- subset(dtICC_1, ESG_Score > 60)


   cutoff37_1 <- rbind(cutoff30_1,cutoff70_1)
   cutoff46_1 <- rbind(cutoff40_1,cutoff60_1)

   dtICC_D_clean <- deltaICCcut(dtICC_0,dtICC_1)
   dtICC_D_37 <- deltaICCcut(cutoff37_0,cutoff37_1)
   dtICC_D_46 <- deltaICCcut(cutoff46_0,cutoff46_1)

   assign(paste0("tb5xx_",i),treatTab5(anos2, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46))
   # tb5_1 <- treatTab5(anos2, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46)
}

# ano <- ano_0
# dadosR_5_0 <- calculaRetorno(dadosn,ano)
#
# tb5_clean <- decomposicaoRetornoOutroArquivo(dtICC_D_clean,dadosR_5_0)[ICC == "ICC_GLS"]
# tb5_L <- decomposicaoRetornoOutroArquivo(dtICC_D_37,dadosR_5_0)[ICC == "ICC_GLS"]
# tb5_H <- decomposicaoRetornoOutroArquivo(dtICC_D_46,dadosR_5_0)[ICC == "ICC_GLS"]

ano <- ano_0+1
dadosR_5_1 <- calculaRetorno(dadosn,ano)

tb5_clean_1 <- decomposicaoRetornoOutroArquivo(dtICC_D_clean,dadosR_5_1)[ICC == "ICC_GLS"]
tb5_L_1 <- decomposicaoRetornoOutroArquivo(dtICC_D_37,dadosR_5_1)[ICC == "ICC_GLS"]
tb5_H_1 <- decomposicaoRetornoOutroArquivo(dtICC_D_46,dadosR_5_1)[ICC == "ICC_GLS"]

# tb5_clean[,CASE := "Base Case"]
# tb5_L[,CASE := "L=[0-3] H=[7-10]"]
# tb5_H[,CASE := "L=[0-4] H=[6-10]"]
#
# tb5_1 <- rbind(tb5_clean,tb5_H,tb5_L)

tb5_clean_1[,CASE := "Base Case"]
tb5_L_1[,CASE := "L=[0-3] H=[7-10]"]
tb5_H_1[,CASE := "L=[0-4] H=[6-10]"]

tb5_2 <- rbind(tb5_clean_1,tb5_H_1,tb5_L_1)

# tb5_1[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
# tb5_1[,Valor := round(Valor*100,2)]
# tb5_1 <- dcast(tb5_1,VARF~CASE,value.var = "Valor")
# tb5_1 <- as.data.frame(tb5_1)
# rownames(tb5_1) <- tb5_1$VARF

tb5_2[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
tb5_2[,Valor := round(Valor*100,2)]
tb5_2 <- dcast(tb5_2,VARF~CASE,value.var = "Valor")
tb5_2 <- as.data.frame(tb5_2)
rownames(tb5_2) <- tb5_2$VARF

tb5_3 <- treatTab5(ano_0+2, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46)
tb5_4 <- treatTab5(ano_0+3, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46)
tb5_5 <- treatTab5(ano_0+4, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46)
tb5_6 <- treatTab5(ano_0+5, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46)

for(i in 1:6){
   assign(paste0("tauxx",i),tb5_3$`Base Case`)
}

taux <- copy(tb5_1)
taux1 <- copy(tb5_2)
taux2 <- copy(tb5_3)
taux3 <- copy(tb5_4)
taux4 <- copy(tb5_5)
taux5 <- copy(tb5_6)


x <- c(taux$`Base Case`[c(12)],taux1$`Base Case`[c(12)],taux2$`Base Case`[c(12)],taux3$`Base Case`[c(12)],taux4$`Base Case`[c(12)],taux5$`Base Case`[c(12)])
mediaX <- mean(x)
u <- as.numeric(length(x))
variancia <- ((x[1]-mediaX)^2+(x[2]-mediaX)^2+(x[3]-mediaX)^2+(x[4]-mediaX)^2+(x[5]-mediaX)^2+(x[6]-mediaX)^2)/(u-1)
varia <- 0
for(n in x) {
   varia <- varia+(n-mediaX)^2
}
varia <- varia/(u-1)

variancia_11 <- var(x, y=NULL, use = "everything")

covCF_10 <- ((taux1$`Base Case`[c(11)])-(taux1$`Base Case`[c(11)]+ taux$`Base Case`[c(11)])/2 )*((taux1$`Base Case`[c(6)])-(taux1$`Base Case`[c(6)]+ taux$`Base Case`[c(6)])/2 )+((taux$`Base Case`[c(11)])-(taux1$`Base Case`[c(11)]+ taux$`Base Case`[c(11)])/2 )*((taux$`Base Case`[c(6)])-(taux1$`Base Case`[c(6)]+ taux$`Base Case`[c(6)])/2 )
variancia_10 <- (taux$`Base Case`[c(12)]-(taux1$`Base Case`[c(12)]+ taux$`Base Case`[c(12)])/2 )^2+(taux1$`Base Case`[c(12)]-(taux1$`Base Case`[c(12)]+ taux$`Base Case`[c(12)])/2 )^2

covDR_10 <- ((taux1$`Base Case`[c(11)])-(taux1$`Base Case`[c(11)]+ taux$`Base Case`[c(11)])/2 )*((taux1$`Base Case`[c(2)])-(taux1$`Base Case`[c(2)]+ taux$`Base Case`[c(2)])/2 )+((taux$`Base Case`[c(11)])-(taux1$`Base Case`[c(11)]+ taux$`Base Case`[c(11)])/2 )*((taux$`Base Case`[c(2)])-(taux1$`Base Case`[c(2)]+ taux$`Base Case`[c(2)])/2 )

covCF_20 <- ((taux1$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux1$`L=[0-3] H=[7-10]`[c(6)])-(taux1$`L=[0-3] H=[7-10]`[c(6)]+ taux$`L=[0-3] H=[7-10]`[c(6)])/2 )+((taux$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux$`L=[0-3] H=[7-10]`[c(6)])-(taux1$`L=[0-3] H=[7-10]`[c(6)]+ taux$`L=[0-3] H=[7-10]`[c(6)])/2 )
variancia_20 <- (taux$`L=[0-3] H=[7-10]`[c(12)]-(taux1$`L=[0-3] H=[7-10]`[c(12)]+ taux$`L=[0-3] H=[7-10]`[c(12)])/2 )^2+(taux1$`L=[0-3] H=[7-10]`[c(12)]-(taux1$`L=[0-3] H=[7-10]`[c(12)]+ taux$`L=[0-3] H=[7-10]`[c(12)])/2 )^2

covDR_20 <- ((taux1$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux1$`L=[0-3] H=[7-10]`[c(2)])-(taux1$`L=[0-3] H=[7-10]`[c(2)]+ taux$`L=[0-3] H=[7-10]`[c(2)])/2 )+((taux$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux$`L=[0-3] H=[7-10]`[c(2)])-(taux1$`L=[0-3] H=[7-10]`[c(2)]+ taux$`L=[0-3] H=[7-10]`[c(2)])/2 )

covCF_30 <- ((taux1$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux1$`L=[0-4] H=[6-10]`[c(6)])-(taux1$`L=[0-4] H=[6-10]`[c(6)]+ taux$`L=[0-4] H=[6-10]`[c(6)])/2 )+((taux$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux$`L=[0-4] H=[6-10]`[c(6)])-(taux1$`L=[0-4] H=[6-10]`[c(6)]+ taux$`L=[0-4] H=[6-10]`[c(6)])/2 )
variancia_30 <- (taux$`L=[0-4] H=[6-10]`[c(12)]-(taux1$`L=[0-4] H=[6-10]`[c(12)]+ taux$`L=[0-4] H=[6-10]`[c(12)])/2 )^2+(taux1$`L=[0-4] H=[6-10]`[c(12)]-(taux1$`L=[0-4] H=[6-10]`[c(12)]+ taux$`L=[0-4] H=[6-10]`[c(12)])/2 )^2

covDR_30 <- ((taux1$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux1$`L=[0-4] H=[6-10]`[c(2)])-(taux1$`L=[0-4] H=[6-10]`[c(2)]+ taux$`L=[0-4] H=[6-10]`[c(2)])/2 )+((taux$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux$`L=[0-4] H=[6-10]`[c(2)])-(taux1$`L=[0-4] H=[6-10]`[c(2)]+ taux$`L=[0-4] H=[6-10]`[c(2)])/2 )

BetaCF_10 <- covCF_10/variancia_10
BetaDR_10 <- covDR_10/variancia_10

BetaCF_20 <- covCF_20/variancia_20
BetaDR_20 <- covDR_20/variancia_20

BetaCF_30 <- covCF_30/variancia_30
BetaDR_30 <- covDR_30/variancia_30


tbaux_10 <- rbind(BetaCF_10 ,BetaDR_10)
tbaux_20 <- rbind(BetaCF_20 ,BetaDR_20)
tbaux_30 <- rbind(BetaCF_30 ,BetaDR_30)

nameTab <- BetaNames
# print(sprintf("beta calculado"))
# covCF_10 <- ((taux1$'Base Case' [c(11)]))
# print(BetaDR_10)
aux <- cbind(nameTab,tbaux_10,tbaux_20, tbaux_30)
colnames(aux)<-c("VARF",	"Base Case",	"L=[0-3] H=[7-10]",	"L=[0-4] H=[6-10]")
tabfinal <- rbind(taux,aux)

geraDeltaDAno <- function(ano_0){
   dadosF_0 <- trataPrevisao(dadosn,ref,ano = ano_0,vars = c("BPS","DPS","EPS","ROE"))
   dadosF_1 <- trataPrevisao(dadosn,ref,ano = ano_0+1,vars = c("BPS","DPS","EPS","ROE"))


   stocks <- unique(dadosF_1$EmpCode)
   dadosI_0 <- dadosn[VAR %in% c("PRC","BPS","EPS","DPS") & Ano == ano_0 & EmpCode %in% stocks]
   dadosI_0 <- dcast(dadosI_0,EmpCode ~ VAR,value.var = "Valor")
   dadosI_1 <- dadosn[VAR %in% c("PRC","BPS","EPS","DPS") & Ano == ano_0+1 & EmpCode %in% stocks]
   dadosI_1 <- dcast(dadosI_1,EmpCode ~ VAR,value.var = "Valor")

   dadosR <- calculaRetorno(dadosn,ano_0+1)

   dadosr <- data.table(read_xlsx("dados/datastream.xlsx",sheet="dadosROE"))
   ROE_historio_mediana <- ROEestimado(dadosr)

   dtGLS_ICC_0 <- rbindlist(lapply(stocks,ICC_GLS,dadosF_0,dadosI_0,ROE_historio_mediana))
   dtGLS_ICC_1 <- rbindlist(lapply(stocks,ICC_GLS,dadosF_1,dadosI_1,ROE_historio_mediana))

   dtICC_0 <- comparaICCs(dtGLS_ICC_0,dtCT_ICC_0,dtOJ_ICC_0,dtE_ICC_0,dsBR,esgB3)
   dtICC_1 <- comparaICCs(dtGLS_ICC_1,dtCT_ICC_1,dtOJ_ICC_1,dtE_ICC_1,dsBR,esgB3)

   dtICC_0 <- dtICC_0[EmpCode != "BR:RG3"]
   dtICC_1 <- dtICC_1[EmpCode != "BR:RG3"]

   dtICCg_1 <- dtICC_1[,lapply(.SD,mean,na.rm=T),by=.(Group50),.SDcols=grep('ICC.*',colnames(dtICC_0),value = T)]

   colid <- names(dtICC_1)[-grep('ICC.*',names(dtICC_1))]
   dtICC_D <- melt(dtICC_1,id.vars = colid,variable.name = "ICC",variable.factor = F,value.name = "Valor")
   dtICC_D_0 <- melt(dtICC_0,id.vars = "EmpCode",measure.vars = grep('ICC.*',names(dtICC_0),value = T),variable.name = "ICC",variable.factor = F,value.name = "V0")
   dtICC_D_0 <- dtICC_D_0[!is.na(V0)]
   dtICC_D <- dtICC_D[dtICC_D_0,on = c("EmpCode","ICC")]
   dtICC_D[,Delta := Valor-V0]
   return(list( dtICC_0, dtICC_1,dtICC_D))
}

ano_0 = 2012 #era 2014
dadosF_0 <- trataPrevisao(dadosn,ref,ano = ano_0,vars = c("BPS","DPS","EPS","ROE"))
dadosF_1 <- trataPrevisao(dadosn,ref,ano = ano_0+1,vars = c("BPS","DPS","EPS","ROE"))


stocks <- unique(dadosF_1$EmpCode)
dadosI_0 <- dadosn[VAR %in% c("PRC","BPS","EPS","DPS") & Ano == ano_0 & EmpCode %in% stocks]
dadosI_0 <- dcast(dadosI_0,EmpCode ~ VAR,value.var = "Valor")
dadosI_1 <- dadosn[VAR %in% c("PRC","BPS","EPS","DPS") & Ano == ano_0+1 & EmpCode %in% stocks]
dadosI_1 <- dcast(dadosI_1,EmpCode ~ VAR,value.var = "Valor")

dadosR <- calculaRetorno(dadosn,ano_0+1)

dadosr <- data.table(read_xlsx("dados/datastream.xlsx",sheet="dadosROE"))
ROE_historio_mediana <- ROEestimado(dadosr)

dtGLS_ICC_0 <- rbindlist(lapply(stocks,ICC_GLS,dadosF_0,dadosI_0,ROE_historio_mediana))
dtGLS_ICC_1 <- rbindlist(lapply(stocks,ICC_GLS,dadosF_1,dadosI_1,ROE_historio_mediana))

dtICC_0 <- comparaICCs(dtGLS_ICC_0,dtCT_ICC_0,dtOJ_ICC_0,dtE_ICC_0,dsBR,esgB3)
dtICC_1 <- comparaICCs(dtGLS_ICC_1,dtCT_ICC_1,dtOJ_ICC_1,dtE_ICC_1,dsBR,esgB3)

dtICC_0 <- dtICC_0[EmpCode != "BR:RG3"]
dtICC_1 <- dtICC_1[EmpCode != "BR:RG3"]

dtICCg_1 <- dtICC_1[,lapply(.SD,mean,na.rm=T),by=.(Group50),.SDcols=grep('ICC.*',colnames(dtICC_0),value = T)]

colid <- names(dtICC_1)[-grep('ICC.*',names(dtICC_1))]
dtICC_D <- melt(dtICC_1,id.vars = colid,variable.name = "ICC",variable.factor = F,value.name = "Valor")
dtICC_D_0 <- melt(dtICC_0,id.vars = "EmpCode",measure.vars = grep('ICC.*',names(dtICC_0),value = T),variable.name = "ICC",variable.factor = F,value.name = "V0")
dtICC_D_0 <- dtICC_D_0[!is.na(V0)]
dtICC_D <- dtICC_D[dtICC_D_0,on = c("EmpCode","ICC")]
dtICC_D[,Delta := Valor-V0]




x <- c(taux$`Base Case`[c(12)],taux1$`Base Case`[c(12)],taux2$`Base Case`[c(12)],taux3$`Base Case`[c(12)],taux4$`Base Case`[c(12)])
mediaX <- mean(x)
u <- as.numeric(length(x))
####variancia <- ((x[1]-mediaX)^2+(x[2]-mediaX)^2+(x[3]-mediaX)^2+(x[4]-mediaX)^2+(x[5]-mediaX)^2+(x[6]-mediaX)^2)/(u-1)

###gerando variancia_1, variancia_2 e variancia_3 para

#vetorizando os valores das tabelas - c(12) = UR_M
URM_1 <- c(taux$`Base Case`[c(12)],taux1$`Base Case`[c(12)],taux2$`Base Case`[c(12)],taux3$`Base Case`[c(12)],taux4$`Base Case`[c(12)])
media1 <- mean(URM_1)

URM_2 <- c(taux$`L=[0-3] H=[7-10]`[c(12)],taux1$`L=[0-3] H=[7-10]`[c(12)],taux2$`L=[0-3] H=[7-10]`[c(12)],taux3$`L=[0-3] H=[7-10]`[c(12)],taux4$`L=[0-3] H=[7-10]`[c(12)])
media2 <- mean(URM_2)

URM_3 <- c(taux$`L=[0-4] H=[6-10]`[c(12)],taux1$`L=[0-4] H=[6-10]`[c(12)],taux2$`L=[0-4] H=[6-10]`[c(12)],taux3$`L=[0-4] H=[6-10]`[c(12)],taux4$`L=[0-4] H=[6-10]`[c(12)])
media3 <- mean(URM_3)

u <- as.numeric(length(URM_1))

w <-list( x_1,x_2,x_3)
media <- list(media1,media2,media3)

for (i in 1:3){

   varia <- 0
   for(n in w[[i]]) {
      varia <- varia+(n-media[[i]])^2
   }
   varia <- varia/(u-1)
   assign(paste0("variancia_",i*10),varia)
}

####expressão da covariancia###############################################

URHL_1 <- c(taux$`Base Case`[c(11)],taux1$`Base Case`[c(11)],taux2$`Base Case`[c(11)],taux3$`Base Case`[c(11)],taux4$`Base Case`[c(11)])
URHL_2 <- c(taux$`L=[0-3] H=[7-10]`[c(11)],taux1$`L=[0-3] H=[7-10]`[c(11)],taux2$`L=[0-3] H=[7-10]`[c(11)],taux3$`L=[0-3] H=[7-10]`[c(11)],taux4$`L=[0-3] H=[7-10]`[c(11)])
URHL_3 <- c(taux$`L=[0-4] H=[6-10]`[c(11)],taux1$`L=[0-4] H=[6-10]`[c(11)],taux2$`L=[0-4] H=[6-10]`[c(11)],taux3$`L=[0-4] H=[6-10]`[c(11)],taux4$`L=[0-4] H=[6-10]`[c(11)])
URHL <- list(URHL_1,URHL_2,URHL_3)

mediaURHL1 <- mean(URHL_1)
mediaURHL2 <- mean(URHL_2)
mediaURHL3 <- mean(URHL_3)
mediaURHL <- list(mediaURHL1,mediaURHL2,mediaURHL3)

NDRM_1 <- c(taux$`Base Case`[c(11)],taux1$`Base Case`[c(11)],taux2$`Base Case`[c(11)],taux3$`Base Case`[c(11)],taux4$`Base Case`[c(11)])
NDRM_2 <- c(taux$`L=[0-3] H=[7-10]`[c(11)],taux1$`L=[0-3] H=[7-10]`[c(11)],taux2$`L=[0-3] H=[7-10]`[c(11)],taux3$`L=[0-3] H=[7-10]`[c(11)],taux4$`L=[0-3] H=[7-10]`[c(11)])
NDRM_3 <- c(taux$`L=[0-4] H=[6-10]`[c(11)],taux1$`L=[0-4] H=[6-10]`[c(11)],taux2$`L=[0-4] H=[6-10]`[c(11)],taux3$`L=[0-4] H=[6-10]`[c(11)],taux4$`L=[0-4] H=[6-10]`[c(11)])
NDRM <- list(NDRM_1,NDRM_2,NDRM_3)

mediaNDRM1 <- mean(NDRM_1)
mediaNDRM2 <- mean(NDRM_2)
mediaNDRM3 <- mean(NDRM_3)
mediaNDRM <- list(mediaNDRM1,mediaNDRM2,mediaNDRM3)

NCFM_1 <- c(taux$`Base Case`[c(11)],taux1$`Base Case`[c(11)],taux2$`Base Case`[c(11)],taux3$`Base Case`[c(11)],taux4$`Base Case`[c(11)])
NCFM_2 <- c(taux$`L=[0-3] H=[7-10]`[c(11)],taux1$`L=[0-3] H=[7-10]`[c(11)],taux2$`L=[0-3] H=[7-10]`[c(11)],taux3$`L=[0-3] H=[7-10]`[c(11)],taux4$`L=[0-3] H=[7-10]`[c(11)])
NCFM_3 <- c(taux$`L=[0-4] H=[6-10]`[c(11)],taux1$`L=[0-4] H=[6-10]`[c(11)],taux2$`L=[0-4] H=[6-10]`[c(11)],taux3$`L=[0-4] H=[6-10]`[c(11)],taux4$`L=[0-4] H=[6-10]`[c(11)])
NCFM <- list(NCFM_1,NCFM_2,NCFM_3)

mediaNCFM1 <- mean(NCFM_1)
mediaNCFM2 <- mean(NCFM_2)
mediaNCFM3 <- mean(NCFM_3)
mediaNCFM <- list(mediaNCFM1,mediaNCFM2,mediaNCFM3)

##repetindo so por desencargo de consciência
u <- as.numeric(length(URHL_1))
####iteração para calculo da covariancia_10, covariancia_20 e covariancia_30##################################
for (i in 1:3){

   covaria <- 0
   for(n in 1:5) {
      covaria <- covaria+(URHL[[i]][n]-mediaURHL[[i]])*(NCFM[[i]][n]-mediaNCFM[[i]])
      #covaria <- covaria+((URHL[[i]][n])-mediaURHL[[i]])*((NCFM[[i]][n])-mediaNCFM[[i]])
   }
   covaria <- covaria/(u-1)
   assign(paste0("covCF_",i*10),covaria)
}

cov(URHL_1,NCFM_1,use = "everything")
########################################################################################################



print(sprintf("entrou função"))
for(i in 1:5){
   ano_selecionado <- anos+i-1

   ###gerador de deltas e ICCs para cada ano de 2013 a 2017###
   dtICC_D6 <- GeraDeltaDAno(ano_selecionado)
   ###separando a lista de dtICCs
   dtICC_0 <- dtICC_D6[[1]]
   dtICC_1 <- dtICC_D6[[2]]
   dtICC_D <- dtICC_D6[[3]]

   # dadosR_6_0 <- calculaRetorno(dadosn,ano)

   assign(paste0("tb6_",i),treatTab6(ano_selecionado, dadosn, dtICC_D))
}

tb6_1[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
tb6_1[,Valor := round(Valor*100,2)]
tb6_1 <- dcast(tb6_1,VARF~CASE,value.var = "Valor")
tb6_1 <- as.data.frame(tb6_1)
rownames(tb6_1) <- tb6_1$VARF

print("saiu função")

tb6 <- BetaTab6Calc(tb6_1,tb6_2)


ano_selecionado <- anos+i-1

###gerador de deltas e ICCs para cada ano de 2013 a 2017###
dtICC_D6 <- GeraDeltaDAno(ano_selecionado)
###separando a lista de dtICCs
dtICC_0 <- dtICC_D6[[1]]
dtICC_1 <- dtICC_D6[[2]]
dtICC_D <- dtICC_D6[[3]]
dadosR_6_0 <- calculaRetorno(dadosn,ano_selecionado)

tb6_1 <- decomposicaoRetornoOutroArquivo(dtICC_D6[[3]],dadosR_6_0)[ICC == "ICC_GLS"]
tb6_1 <- midprocessing_table(tb6_1)

anos <- ano_0
for(i in 1:5){
   ano_selecionado <- anos+i-1

   ###gerador de deltas e ICCs para cada ano de 2013 a 2017###
   dtICC_D6 <- GeraDeltaDAno(ano_selecionado)
   ###separando a lista de dtICCs
   dtICC_0 <- dtICC_D6[[1]]
   dtICC_1 <- dtICC_D6[[2]]
   dtICC_D <- dtICC_D6[[3]]

   assign(paste0("dtICCESG_",i),Tab3DeltaIndex(dtICC_D,esgTEMP,ano_selecionado))
   # dadosR_6_0 <- calculaRetorno(dadosn,ano)

   # assign(paste0("tb3_",i),treatTab3(ano_selecionado, dadosn, dtICCESG_1 ))

}

assign("tb3_1",treatTab3(ano_selecionado, dadosn, dtICCESG_1 ))
assign("tb3_2",treatTab3(ano_selecionado, dadosn, dtICCESG_2 ))
assign("tb3_3",treatTab3(ano_selecionado, dadosn, dtICCESG_3 ))
assign("tb3_4",treatTab3(ano_selecionado, dadosn, dtICCESG_4 ))
assign("tb3_5",treatTab3(ano_selecionado, dadosn, dtICCESG_5 ))

dadosR_6_0 <- calculaRetorno(dadosn,ano_selecionado)

tb3t_1 <- decomposicaoRetornoOutroArquivo(dtICCESG_1, dadosR_6_0)[ICC == "ICC_GLS"]
# tb6_1 <- midprocessing_table(tb6_1)

tb3t_Delta <- decomposicaoRetornoDeltaESG(dtICCESG_2, dadosR_6_0)[ICC == "ICC_GLS"]

tb3t_1[,CASE := "Base Case"]
tb3t_Delta[,CASE := "DeltaHL"]


tb3t<-rbind(tb3t_1,tb3t_Delta)

tb3t <- midprocessing_table6(tb3t)

tb3t[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
tb3t[,Valor := round(Valor*100,2)]
tb3t <- dcast(tb3t,VARF~CASE,value.var = "Valor")
tb3t <- as.data.frame(tb3t)
rownames(tb3t) <- tb6_1$VARF
