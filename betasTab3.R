betasTab3 <- function(dadosn,dt){
   dtICC_D <- copy(dt)
   ano<-ano_0
   dadosR_b <- dadosn[VAR %in% c("PRC","DPS") & Ano >= ano & Ano < ano + 5 & EmpCode %in% stocks]
   dadosR_b <- dcast(dadosR_b,EmpCode + Ano ~ VAR,value.var = "Valor")
   dadosR_b[, `:=` (P0 = shift(PRC),D0 = shift(DPS)),.(EmpCode)]
   dadosR_b <- dadosR_b[Ano != ano]
   dadosR_b[,R := ((PRC - D0)/P0)-1]
   dadosR_ano0 <- dadosR_b[,.(R = mean(R)),.(EmpCode)]

   ano <- ano_0+1
   dadosR_b <- dadosn[VAR %in% c("PRC","DPS") & Ano >= ano & Ano < ano + 5 & EmpCode %in% stocks]
   dadosR_b <- dcast(dadosR_b,EmpCode + Ano ~ VAR,value.var = "Valor")
   dadosR_b[, `:=` (P0 = shift(PRC),D0 = shift(DPS)),.(EmpCode)]
   dadosR_b <- dadosR_b[Ano != ano]
   dadosR_b[,R := ((PRC - D0)/P0)-1]
   dadosR_ano1 <- dadosR_b[,.(R = mean(R)),.(EmpCode)]

   taux <- decomposicaoRetornoDeltaESG(dtICC_D, dadosR_ano0)[ICC == "ICC_GLS"]

   taux[,Valor := round(Valor*100,2)]
   taux <- dcast(taux,VAR+VARF~ICC,value.var = "Valor")
   taux <- as.data.frame(taux)
   rownames(taux) <- taux$VARF

   taux1 <- decomposicaoRetornoDeltaESG(dtICC_D, dadosR_ano1)[ICC == "ICC_GLS"]

   taux1[,Valor := round(Valor*100,2)]
   taux1 <- dcast(taux1,VAR+VARF~ICC,value.var = "Valor")
   taux1 <- as.data.frame(taux1)
   rownames(taux1) <- taux1$VARF

   tb3 <- BetaTab3Calc(taux,taux1)

   return(tb3)
}

BetaTab3Calc <- function(taux, taux1){

   covCF_10 <- ((taux1$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux1$`ICC_GLS`[c(4)])-(taux1$`ICC_GLS`[c(4)]+ taux$`ICC_GLS`[c(4)])/2 )+((taux$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux$`ICC_GLS`[c(4)])-(taux1$`ICC_GLS`[c(4)]+ taux$`ICC_GLS`[c(4)])/2 )
   variancia_10 <- (taux$`ICC_GLS`[c(12)]-(taux1$`ICC_GLS`[c(12)]+ taux$`ICC_GLS`[c(12)])/2 )^2+(taux1$`ICC_GLS`[c(12)]-(taux1$`ICC_GLS`[c(12)]+ taux$`ICC_GLS`[c(12)])/2 )^2

   covDR_10 <- ((taux1$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux1$`ICC_GLS`[c(6)])-(taux1$`ICC_GLS`[c(6)]+ taux$`ICC_GLS`[c(6)])/2 )+((taux$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux$`ICC_GLS`[c(6)])-(taux1$`ICC_GLS`[c(6)]+ taux$`ICC_GLS`[c(6)])/2 )

   # covCF_20 <- ((taux1$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux1$`L=[0-3] H=[7-10]`[c(6)])-(taux1$`L=[0-3] H=[7-10]`[c(6)]+ taux$`L=[0-3] H=[7-10]`[c(6)])/2 )+((taux$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux$`L=[0-3] H=[7-10]`[c(6)])-(taux1$`L=[0-3] H=[7-10]`[c(6)]+ taux$`L=[0-3] H=[7-10]`[c(6)])/2 )
   # variancia_20 <- (taux$`L=[0-3] H=[7-10]`[c(12)]-(taux1$`L=[0-3] H=[7-10]`[c(12)]+ taux$`L=[0-3] H=[7-10]`[c(12)])/2 )^2+(taux1$`L=[0-3] H=[7-10]`[c(12)]-(taux1$`L=[0-3] H=[7-10]`[c(12)]+ taux$`L=[0-3] H=[7-10]`[c(12)])/2 )^2
   #
   # covDR_20 <- ((taux1$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux1$`L=[0-3] H=[7-10]`[c(2)])-(taux1$`L=[0-3] H=[7-10]`[c(2)]+ taux$`L=[0-3] H=[7-10]`[c(2)])/2 )+((taux$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux$`L=[0-3] H=[7-10]`[c(2)])-(taux1$`L=[0-3] H=[7-10]`[c(2)]+ taux$`L=[0-3] H=[7-10]`[c(2)])/2 )
   #
   # covCF_30 <- ((taux1$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux1$`L=[0-4] H=[6-10]`[c(6)])-(taux1$`L=[0-4] H=[6-10]`[c(6)]+ taux$`L=[0-4] H=[6-10]`[c(6)])/2 )+((taux$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux$`L=[0-4] H=[6-10]`[c(6)])-(taux1$`L=[0-4] H=[6-10]`[c(6)]+ taux$`L=[0-4] H=[6-10]`[c(6)])/2 )
   # variancia_30 <- (taux$`L=[0-4] H=[6-10]`[c(12)]-(taux1$`L=[0-4] H=[6-10]`[c(12)]+ taux$`L=[0-4] H=[6-10]`[c(12)])/2 )^2+(taux1$`L=[0-4] H=[6-10]`[c(12)]-(taux1$`L=[0-4] H=[6-10]`[c(12)]+ taux$`L=[0-4] H=[6-10]`[c(12)])/2 )^2
   #
   # covDR_30 <- ((taux1$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux1$`L=[0-4] H=[6-10]`[c(2)])-(taux1$`L=[0-4] H=[6-10]`[c(2)]+ taux$`L=[0-4] H=[6-10]`[c(2)])/2 )+((taux$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux$`L=[0-4] H=[6-10]`[c(2)])-(taux1$`L=[0-4] H=[6-10]`[c(2)]+ taux$`L=[0-4] H=[6-10]`[c(2)])/2 )
   #
   BetaCF_V_weight <- covCF_10/variancia_10
   BetaDR_V_weight <- covDR_10/variancia_10
   #
   # BetaCF_20 <- covCF_20/variancia_20
   # BetaDR_20 <- covDR_20/variancia_20

   # BetaCF_30 <- covCF_30/variancia_30
   # BetaDR_30 <- covDR_30/variancia_30


   tbaux_10 <- rbind(BetaCF_V_weight ,BetaDR_V_weight)
   # tbaux_20 <- rbind(BetaCF_20 ,BetaDR_20)
   # tbaux_30 <- rbind(BetaCF_30 ,BetaDR_30)

   nameTab <- data.frame(x1 = c("BetaCf","BetaDR"))
   aux <- cbind(nameTab,nameTab, tbaux_10)
   colnames(aux)<-c("VARF","VAR",	"DeltaHL")

   colnames(taux)<-c("VARF","VAR",	"DeltaHL")


   #aux2 <- rbind(taux,aux)
   return(rbind(taux,aux))
}
