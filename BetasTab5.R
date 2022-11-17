tab5Build <- function(dadosn, dtICC_D,ano){

   print(sprintf("entrou função"))
   dadosR <- calculaRetorno(dadosn,ano)
   P10 <- decomposicaoRetornoOutroArquivo(dtICC_D,dadosR,interv = list(H=c(.9,1),L=c(0,.1)))[ICC == "ICC_GLS"]
   print(P10)
   P20 <- decomposicaoRetornoOutroArquivo(dtICC_D,dadosR,interv = list(H=c(.8,1),L=c(0,.2)))[ICC == "ICC_GLS"]
   P30 <- decomposicaoRetornoOutroArquivo(dtICC_D,dadosR,interv = list(H=c(.7,1),L=c(0,.3)))[ICC == "ICC_GLS"]
   print(P30)
   P40 <- decomposicaoRetornoOutroArquivo(dtICC_D,dadosR,interv = list(H=c(.6,1),L=c(0,.4)))[ICC == "ICC_GLS"]
   P50 <- decomposicaoRetornoOutroArquivo(dtICC_D,dadosR,interv = list(H=c(.5,1),L=c(0,.5)))[ICC == "ICC_GLS"]

   print(P50)
   P10[,CASE := "10%"]
   P20[,CASE := "20%"]
   P30[,CASE := "30%"]
   P40[,CASE := "40%"]
   P50[,CASE := "50% (base case)"]
   print("saiu função")

   return(rbind(P10,P20,P30,P40,P50))
}

BetaTab5Calc <- function(taux, taux1){

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

   # print(sprintf("beta calculado"))
   # covCF_10 <- ((taux1$'Base Case' [c(11)]))
   # print(BetaDR_10)
   return(cbind(tbaux_30,tbaux_20, tbaux_10))
}
