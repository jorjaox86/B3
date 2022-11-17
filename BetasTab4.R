tab4Build <- function(dadosn, dtICC_D,ano){

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

BetaTab4Calc <- function(taux, taux1){
   covCF_10 <- ((taux1$`10%`[c(11)])-(taux1$`10%`[c(11)]+ taux$`10%`[c(11)])/2 )*((taux1$`10%`[c(6)])-(taux1$`10%`[c(6)]+ taux$`10%`[c(6)])/2 )+((taux$`10%`[c(11)])-(taux1$`10%`[c(11)]+ taux$`10%`[c(11)])/2 )*((taux$`10%`[c(6)])-(taux1$`10%`[c(6)]+ taux$`10%`[c(6)])/2 )
   variancia_10 <- (taux$`10%`[c(12)]-(taux1$`10%`[c(12)]+ taux$`10%`[c(12)])/2 )^2+(taux1$`10%`[c(12)]-(taux1$`10%`[c(12)]+ taux$`10%`[c(12)])/2 )^2

   covDR_10 <- ((taux1$`10%`[c(11)])-(taux1$`10%`[c(11)]+ taux$`10%`[c(11)])/2 )*((taux1$`10%`[c(2)])-(taux1$`10%`[c(2)]+ taux$`10%`[c(2)])/2 )+((taux$`10%`[c(11)])-(taux1$`10%`[c(11)]+ taux$`10%`[c(11)])/2 )*((taux$`10%`[c(2)])-(taux1$`10%`[c(2)]+ taux$`10%`[c(2)])/2 )

   covCF_20 <- ((taux1$`20`[c(11)])-(taux1$`20`[c(11)]+ taux$`20`[c(11)])/2 )*((taux1$`20`[c(6)])-(taux1$`20`[c(6)]+ taux$`20`[c(6)])/2 )+((taux$`20`[c(11)])-(taux1$`20`[c(11)]+ taux$`20`[c(11)])/2 )*((taux$`20`[c(6)])-(taux1$`20`[c(6)]+ taux$`20`[c(6)])/2 )
   variancia_20 <- (taux$`20`[c(12)]-(taux1$`20`[c(12)]+ taux$`20`[c(12)])/2 )^2+(taux1$`20`[c(12)]-(taux1$`20`[c(12)]+ taux$`20`[c(12)])/2 )^2

   covDR_20 <- ((taux1$`20`[c(11)])-(taux1$`20`[c(11)]+ taux$`20`[c(11)])/2 )*((taux1$`20`[c(2)])-(taux1$`20`[c(2)]+ taux$`20`[c(2)])/2 )+((taux$`20`[c(11)])-(taux1$`20`[c(11)]+ taux$`20`[c(11)])/2 )*((taux$`20`[c(2)])-(taux1$`20`[c(2)]+ taux$`20`[c(2)])/2 )

   covCF_30 <- ((taux1$`30`[c(11)])-(taux1$`30`[c(11)]+ taux$`30`[c(11)])/2 )*((taux1$`30`[c(6)])-(taux1$`30`[c(6)]+ taux$`30`[c(6)])/2 )+((taux$`30`[c(11)])-(taux1$`30`[c(11)]+ taux$`30`[c(11)])/2 )*((taux$`30`[c(6)])-(taux1$`30`[c(6)]+ taux$`30`[c(6)])/2 )
   variancia_30 <- (taux$`30`[c(12)]-(taux1$`30`[c(12)]+ taux$`30`[c(12)])/2 )^2+(taux1$`30`[c(12)]-(taux1$`30`[c(12)]+ taux$`30`[c(12)])/2 )^2

   covDR_30 <- ((taux1$`30`[c(11)])-(taux1$`30`[c(11)]+ taux$`30`[c(11)])/2 )*((taux1$`30`[c(2)])-(taux1$`30`[c(2)]+ taux$`30`[c(2)])/2 )+((taux$`30`[c(11)])-(taux1$`30`[c(11)]+ taux$`30`[c(11)])/2 )*((taux$`30`[c(2)])-(taux1$`30`[c(2)]+ taux$`30`[c(2)])/2 )

   covCF_40 <- ((taux1$`40`[c(11)])-(taux1$`40`[c(11)]+ taux$`40`[c(11)])/2 )*((taux1$`40`[c(6)])-(taux1$`40`[c(6)]+ taux$`40`[c(6)])/2 )+((taux$`40`[c(11)])-(taux1$`40`[c(11)]+ taux$`40`[c(11)])/2 )*((taux$`40`[c(6)])-(taux1$`40`[c(6)]+ taux$`40`[c(6)])/2 )
   variancia_40 <- (taux$`40`[c(12)]-(taux1$`40`[c(12)]+ taux$`40`[c(12)])/2 )^2+(taux1$`40`[c(12)]-(taux1$`40`[c(12)]+ taux$`40`[c(12)])/2 )^2

   covDR_40 <- ((taux1$`40`[c(11)])-(taux1$`40`[c(11)]+ taux$`40`[c(11)])/2 )*((taux1$`40`[c(2)])-(taux1$`40`[c(2)]+ taux$`40`[c(2)])/2 )+((taux$`40`[c(11)])-(taux1$`40`[c(11)]+ taux$`40`[c(11)])/2 )*((taux$`40`[c(2)])-(taux1$`40`[c(2)]+ taux$`40`[c(2)])/2 )

   covCF_50 <- ((taux1$`50`[c(11)])-(taux1$`50`[c(11)]+ taux$`50`[c(11)])/2 )*((taux1$`50`[c(6)])-(taux1$`50`[c(6)]+ taux$`50`[c(6)])/2 )+((taux$`50`[c(11)])-(taux1$`50`[c(11)]+ taux$`50`[c(11)])/2 )*((taux$`50`[c(6)])-(taux1$`50`[c(6)]+ taux$`50`[c(6)])/2 )
   variancia_50 <- (taux$`50`[c(12)]-(taux1$`50`[c(12)]+ taux$`50`[c(12)])/2 )^2+(taux1$`50`[c(12)]-(taux1$`50`[c(12)]+ taux$`50`[c(12)])/2 )^2

   covDR_50 <- ((taux1$`50`[c(11)])-(taux1$`50`[c(11)]+ taux$`50`[c(11)])/2 )*((taux1$`50`[c(2)])-(taux1$`50`[c(2)]+ taux$`50`[c(2)])/2 )+((taux$`50`[c(11)])-(taux1$`50`[c(11)]+ taux$`50`[c(11)])/2 )*((taux$`50`[c(2)])-(taux1$`50`[c(2)]+ taux$`50`[c(2)])/2 )

   BetaCF_10 <- covCF_10/variancia_10
   BetaDR_10 <- covDR_10/variancia_10

   BetaCF_20 <- covCF_20/variancia_20
   BetaDR_20 <- covDR_20/variancia_20

   BetaCF_30 <- covCF_30/variancia_30
   BetaDR_30 <- covDR_30/variancia_30

   BetaCF_40 <- covCF_40/variancia_40
   BetaDR_40 <- covDR_40/variancia_40

   BetaCF_50 <- covCF_50/variancia_50
   BetaDR_50 <- covDR_50/variancia_50

   # BetaCFAVG <- (BetaCF_10+BetaCF_40+BetaCF_20+BetaCF_30)/4
   # BetaDRAVG <- (BetaDR_10+BetaDR_40+BetaDR_20+BetaDR_30)/4

   tbaux_10 <- rbind(BetaCF_10 ,BetaDR_10)
   tbaux_20 <- rbind(BetaCF_20 ,BetaDR_20)
   tbaux_30 <- rbind(BetaCF_30 ,BetaDR_30)
   tbaux_40 <- rbind(BetaCF_40 ,BetaDR_40)
   tbaux_50 <- rbind(BetaCF_50, BetaDR_50)
   # print(sprintf("beta calculado"))
   # covCF_10 <- ((taux1$'10%' [c(11)]))
   # print(BetaDR_10)
return(cbind(tbaux_50,tbaux_40,tbaux_30,tbaux_20, tbaux_10))
}
