tab4Build <- function(dadosn, dtICC_D,anos){

   for(i in 1:5){
      ano_selecionado <- anos+i-1

      ###gerador de deltas e ICCs para cada ano de 2013 a 2017###
      dtICC_D6 <- GeraDeltaDAno(ano_selecionado)
      ###separando a lista de dtICCs
      dtICC_0 <- dtICC_D6[[1]]
      dtICC_1 <- dtICC_D6[[2]]
      dtICC_D <- dtICC_D6[[3]]

      # dadosR_6_0 <- calculaRetorno(dadosn,ano)

      assign(paste0("tb4_",i),treatTab4(ano_selecionado, dadosn, dtICC_D))

   }


   tb4t <- BetaTab4Calc(tb4_1, tb4_2, tb4_3, tb4_4, tb4_5, BetaNames)

return(tb4t)
}

BetaTab4Calc <- function(taux, taux1, taux2, taux3, taux4, BetaNames){


   URM_1 <- c(taux$`50% (base case)`[c(12)],taux1$`50% (base case)`[c(12)],taux2$`50% (base case)`[c(12)],taux3$`50% (base case)`[c(12)],taux4$`50% (base case)`[c(12)])
   media1 <- mean(URM_1)

   URM_2 <- c(taux$`20%`[c(12)],taux1$`20%`[c(12)],taux2$`20%`[c(12)],taux3$`20%`[c(12)],taux4$`20%`[c(12)])
   media2 <- mean(URM_2)

   URM_3 <- c(taux$`30%`[c(12)],taux1$`30%`[c(12)],taux2$`30%`[c(12)],taux3$`30%`[c(12)],taux4$`30%`[c(12)])
   media3 <- mean(URM_3)

   URM_4 <- c(taux$`40%`[c(12)],taux1$`40%`[c(12)],taux2$`40%`[c(12)],taux3$`40%`[c(12)],taux4$`40%`[c(12)])
   media4 <- mean(URM_2)

   URM_5 <- c(taux$`10%`[c(12)],taux1$`10%`[c(12)],taux2$`10%`[c(12)],taux3$`10%`[c(12)],taux4$`10%`[c(12)])
   media5 <- mean(URM_3)

   u <- as.numeric(length(URM_1))

   URM <-list( URM_1,URM_2,URM_3,URM_4,URM_5)
   media <- list(media1,media2,media3,media4,media5)

   for (i in 1:5){

      varia <- 0
      for(n in 1:5) {
         varia <- varia+(URM[[i]][n]-media[[i]])^2
      }
      varia <- varia/(u-1)
      assign(paste0("variancia_",i*10),varia)
   }

   URHL_1 <- c(taux$`50% (base case)`[c(11)],taux1$`50% (base case)`[c(11)],taux2$`50% (base case)`[c(11)],taux3$`50% (base case)`[c(11)],taux4$`50% (base case)`[c(11)])
   URHL_2 <- c(taux$`20%`[c(11)],taux1$`20%`[c(11)],taux2$`20%`[c(11)],taux3$`20%`[c(11)],taux4$`20%`[c(11)])
   URHL_3 <- c(taux$`30%`[c(11)],taux1$`30%`[c(11)],taux2$`30%`[c(11)],taux3$`30%`[c(11)],taux4$`30%`[c(11)])
   URHL_4 <- c(taux$`40%`[c(11)],taux1$`40%`[c(11)],taux2$`40%`[c(11)],taux3$`40%`[c(11)],taux4$`40%`[c(11)])
   URHL_5 <- c(taux$`10%`[c(11)],taux1$`10%`[c(11)],taux2$`10%`[c(11)],taux3$`10%`[c(11)],taux4$`10%`[c(11)])
   URHL <- list(URHL_1,URHL_2,URHL_3,URHL_4,URHL_5)

   mediaURHL1 <- mean(URHL_1)
   mediaURHL2 <- mean(URHL_2)
   mediaURHL3 <- mean(URHL_3)
   mediaURHL4 <- mean(URHL_4)
   mediaURHL5 <- mean(URHL_5)
   mediaURHL <- list(mediaURHL1,mediaURHL2,mediaURHL3,mediaURHL4,mediaURHL5)

   NDRM_1 <- c(taux$`50% (base case)`[c(2)],taux1$`50% (base case)`[c(2)],taux2$`50% (base case)`[c(2)],taux3$`50% (base case)`[c(2)],taux4$`50% (base case)`[c(2)])
   NDRM_2 <- c(taux$`20%`[c(2)],taux1$`20%`[c(2)],taux2$`20%`[c(2)],taux3$`20%`[c(2)],taux4$`20%`[c(2)])
   NDRM_3 <- c(taux$`30%`[c(2)],taux1$`30%`[c(2)],taux2$`30%`[c(2)],taux3$`30%`[c(2)],taux4$`30%`[c(2)])
   NDRM_4 <- c(taux$`40%`[c(2)],taux1$`40%`[c(2)],taux2$`40%`[c(2)],taux3$`40%`[c(2)],taux4$`40%`[c(2)])
   NDRM_5 <- c(taux$`10%`[c(2)],taux1$`10%`[c(2)],taux2$`10%`[c(2)],taux3$`10%`[c(2)],taux4$`10%`[c(2)])
   NDRM <- list(NDRM_1,NDRM_2,NDRM_3,NDRM_4,NDRM_5)

   mediaNDRM1 <- mean(NDRM_1)
   mediaNDRM2 <- mean(NDRM_2)
   mediaNDRM3 <- mean(NDRM_3)
   mediaNDRM4 <- mean(NDRM_4)
   mediaNDRM5 <- mean(NDRM_5)
   mediaNDRM <- list(mediaNDRM1,mediaNDRM2,mediaNDRM3,mediaNDRM4,mediaNDRM5)

   NCFM_1 <- c(taux$`50% (base case)`[c(6)],taux1$`50% (base case)`[c(6)],taux2$`50% (base case)`[c(6)],taux3$`50% (base case)`[c(6)],taux4$`50% (base case)`[c(6)])
   NCFM_2 <- c(taux$`20%`[c(6)],taux1$`20%`[c(6)],taux2$`20%`[c(6)],taux3$`20%`[c(6)],taux4$`20%`[c(6)])
   NCFM_3 <- c(taux$`30%`[c(6)],taux1$`30%`[c(6)],taux2$`30%`[c(6)],taux3$`30%`[c(6)],taux4$`30%`[c(6)])
   NCFM_4 <- c(taux$`40%`[c(6)],taux1$`40%`[c(6)],taux2$`40%`[c(6)],taux3$`40%`[c(6)],taux4$`40%`[c(6)])
   NCFM_5 <- c(taux$`10%`[c(6)],taux1$`10%`[c(6)],taux2$`10%`[c(6)],taux3$`10%`[c(6)],taux4$`10%`[c(6)])
   NCFM <- list(NCFM_1,NCFM_2,NCFM_3,NCFM_4,NCFM_5)

   mediaNCFM1 <- mean(NCFM_1)
   mediaNCFM2 <- mean(NCFM_2)
   mediaNCFM3 <- mean(NCFM_3)
   mediaNCFM4 <- mean(NCFM_4)
   mediaNCFM5 <- mean(NCFM_5)
   mediaNCFM <- list(mediaNCFM1,mediaNCFM2,mediaNCFM3,mediaNCFM4,mediaNCFM5)

   ##repetindo so por desencargo de consciência
   u <- as.numeric(length(URHL_1))
   ####iteração para calculo da covariancia_10, covariancia_20 e covariancia_30##################################
   for (i in 1:5){

      covaria <- 0
      for(n in 1:5) {
         covaria <- covaria+(URHL[[i]][n]-mediaURHL[[i]])*(NCFM[[i]][n]-mediaNCFM[[i]])
      }
      covaria <- covaria/(u-1)
      assign(paste0("covCF_",i*10),covaria)
   }

   for (i in 1:5){

      covaria <- 0
      for(n in 1:5) {
         covaria <- covaria+(URHL[[i]][n]-mediaURHL[[i]])*(NDRM[[i]][n]-mediaNDRM[[i]])
      }
      covaria <- covaria/(u-1)
      assign(paste0("covDR_",i*10),covaria)
   }


   ###########################################################

   covDR_p10 <- cov(URHL_1[URM_1>0],NDRM_1[URM_1>0])
   covCF_p10 <- cov(URHL_1[URM_1>0],NCFM_1[URM_1>0])
   variancia_p10 <- var(URM_1,y=NULL)

   covDR_p20 <- cov(URHL_2[URM_2>0],NDRM_2[URM_2>0])
   covCF_p20 <- cov(URHL_2[URM_2>0],NCFM_2[URM_2>0])
   variancia_p20 <- var(URM_2,y=NULL)

   covDR_p30 <- cov(URHL_3[URM_3>0],NDRM_3[URM_3>0])
   covCF_p30 <- cov(URHL_3[URM_3>0],NCFM_3[URM_3>0])
   variancia_p30 <- var(URM_3,y=NULL)

   covDR_p40 <- cov(URHL_4[URM_4>0],NDRM_4[URM_4>0])
   covCF_p40 <- cov(URHL_4[URM_4>0],NCFM_4[URM_4>0])
   variancia_p40 <- var(URM_4,y=NULL)

   covDR_p50 <- cov(URHL_5[URM_5>0],NDRM_5[URM_5>0])
   covCF_p50 <- cov(URHL_5[URM_5>0],NCFM_5[URM_5>0])
   variancia_p50 <- var(URM_5,y=NULL)

   ######################################################

   BetaCF_p10 <- covCF_p10/variancia_p10
   BetaDR_p10 <- covDR_p10/variancia_p10

   BetaCF_p20 <- covCF_p20/variancia_p20
   BetaDR_p20 <- covDR_p20/variancia_p20

   BetaCF_p30 <- covCF_p30/variancia_p30
   BetaDR_p30 <- covDR_p30/variancia_p30

   BetaCF_p40 <- covCF_p40/variancia_p40
   BetaDR_p40 <- covDR_p40/variancia_p40

   BetaCF_p50 <- covCF_p50/variancia_p50
   BetaDR_p50 <- covDR_p50/variancia_p50

   # covCF_10 <- ((taux1$`10%`[c(11)])-(taux1$`10%`[c(11)]+ taux$`10%`[c(11)])/2 )*((taux1$`10%`[c(6)])-(taux1$`10%`[c(6)]+ taux$`10%`[c(6)])/2 )+((taux$`10%`[c(11)])-(taux1$`10%`[c(11)]+ taux$`10%`[c(11)])/2 )*((taux$`10%`[c(6)])-(taux1$`10%`[c(6)]+ taux$`10%`[c(6)])/2 )
   # variancia_10 <- (taux$`10%`[c(12)]-(taux1$`10%`[c(12)]+ taux$`10%`[c(12)])/2 )^2+(taux1$`10%`[c(12)]-(taux1$`10%`[c(12)]+ taux$`10%`[c(12)])/2 )^2
   #
   # covDR_10 <- ((taux1$`10%`[c(11)])-(taux1$`10%`[c(11)]+ taux$`10%`[c(11)])/2 )*((taux1$`10%`[c(2)])-(taux1$`10%`[c(2)]+ taux$`10%`[c(2)])/2 )+((taux$`10%`[c(11)])-(taux1$`10%`[c(11)]+ taux$`10%`[c(11)])/2 )*((taux$`10%`[c(2)])-(taux1$`10%`[c(2)]+ taux$`10%`[c(2)])/2 )
   #
   # covCF_20 <- ((taux1$`20`[c(11)])-(taux1$`20`[c(11)]+ taux$`20`[c(11)])/2 )*((taux1$`20`[c(6)])-(taux1$`20`[c(6)]+ taux$`20`[c(6)])/2 )+((taux$`20`[c(11)])-(taux1$`20`[c(11)]+ taux$`20`[c(11)])/2 )*((taux$`20`[c(6)])-(taux1$`20`[c(6)]+ taux$`20`[c(6)])/2 )
   # variancia_20 <- (taux$`20`[c(12)]-(taux1$`20`[c(12)]+ taux$`20`[c(12)])/2 )^2+(taux1$`20`[c(12)]-(taux1$`20`[c(12)]+ taux$`20`[c(12)])/2 )^2
   #
   # covDR_20 <- ((taux1$`20`[c(11)])-(taux1$`20`[c(11)]+ taux$`20`[c(11)])/2 )*((taux1$`20`[c(2)])-(taux1$`20`[c(2)]+ taux$`20`[c(2)])/2 )+((taux$`20`[c(11)])-(taux1$`20`[c(11)]+ taux$`20`[c(11)])/2 )*((taux$`20`[c(2)])-(taux1$`20`[c(2)]+ taux$`20`[c(2)])/2 )
   #
   # covCF_30 <- ((taux1$`30`[c(11)])-(taux1$`30`[c(11)]+ taux$`30`[c(11)])/2 )*((taux1$`30`[c(6)])-(taux1$`30`[c(6)]+ taux$`30`[c(6)])/2 )+((taux$`30`[c(11)])-(taux1$`30`[c(11)]+ taux$`30`[c(11)])/2 )*((taux$`30`[c(6)])-(taux1$`30`[c(6)]+ taux$`30`[c(6)])/2 )
   # variancia_30 <- (taux$`30`[c(12)]-(taux1$`30`[c(12)]+ taux$`30`[c(12)])/2 )^2+(taux1$`30`[c(12)]-(taux1$`30`[c(12)]+ taux$`30`[c(12)])/2 )^2
   #
   # covDR_30 <- ((taux1$`30`[c(11)])-(taux1$`30`[c(11)]+ taux$`30`[c(11)])/2 )*((taux1$`30`[c(2)])-(taux1$`30`[c(2)]+ taux$`30`[c(2)])/2 )+((taux$`30`[c(11)])-(taux1$`30`[c(11)]+ taux$`30`[c(11)])/2 )*((taux$`30`[c(2)])-(taux1$`30`[c(2)]+ taux$`30`[c(2)])/2 )
   #
   # covCF_40 <- ((taux1$`40`[c(11)])-(taux1$`40`[c(11)]+ taux$`40`[c(11)])/2 )*((taux1$`40`[c(6)])-(taux1$`40`[c(6)]+ taux$`40`[c(6)])/2 )+((taux$`40`[c(11)])-(taux1$`40`[c(11)]+ taux$`40`[c(11)])/2 )*((taux$`40`[c(6)])-(taux1$`40`[c(6)]+ taux$`40`[c(6)])/2 )
   # variancia_40 <- (taux$`40`[c(12)]-(taux1$`40`[c(12)]+ taux$`40`[c(12)])/2 )^2+(taux1$`40`[c(12)]-(taux1$`40`[c(12)]+ taux$`40`[c(12)])/2 )^2
   #
   # covDR_40 <- ((taux1$`40`[c(11)])-(taux1$`40`[c(11)]+ taux$`40`[c(11)])/2 )*((taux1$`40`[c(2)])-(taux1$`40`[c(2)]+ taux$`40`[c(2)])/2 )+((taux$`40`[c(11)])-(taux1$`40`[c(11)]+ taux$`40`[c(11)])/2 )*((taux$`40`[c(2)])-(taux1$`40`[c(2)]+ taux$`40`[c(2)])/2 )
   #
   # covCF_50 <- ((taux1$`50`[c(11)])-(taux1$`50`[c(11)]+ taux$`50`[c(11)])/2 )*((taux1$`50`[c(6)])-(taux1$`50`[c(6)]+ taux$`50`[c(6)])/2 )+((taux$`50`[c(11)])-(taux1$`50`[c(11)]+ taux$`50`[c(11)])/2 )*((taux$`50`[c(6)])-(taux1$`50`[c(6)]+ taux$`50`[c(6)])/2 )
   # variancia_50 <- (taux$`50`[c(12)]-(taux1$`50`[c(12)]+ taux$`50`[c(12)])/2 )^2+(taux1$`50`[c(12)]-(taux1$`50`[c(12)]+ taux$`50`[c(12)])/2 )^2
   #
   # covDR_50 <- ((taux1$`50`[c(11)])-(taux1$`50`[c(11)]+ taux$`50`[c(11)])/2 )*((taux1$`50`[c(2)])-(taux1$`50`[c(2)]+ taux$`50`[c(2)])/2 )+((taux$`50`[c(11)])-(taux1$`50`[c(11)]+ taux$`50`[c(11)])/2 )*((taux$`50`[c(2)])-(taux1$`50`[c(2)]+ taux$`50`[c(2)])/2 )

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

   BetaCF_p10[is.na(BetaCF_p10)] <- 0.000
   BetaCF_p20[is.na(BetaCF_p20)] <- 0.000
   BetaCF_p30[is.na(BetaCF_p30)] <- 0.000
   BetaCF_p40[is.na(BetaCF_p40)] <- 0.000
   BetaCF_p50[is.na(BetaCF_p50)] <- 0.000

   BetaDR_p10[is.na(BetaDR_p10)] <- 0.000
   BetaDR_p20[is.na(BetaDR_p20)] <- 0.000
   BetaDR_p30[is.na(BetaDR_p30)] <- 0.000
   BetaDR_p40[is.na(BetaDR_p40)] <- 0.000
   BetaDR_p50[is.na(BetaDR_p50)] <- 0.000

   # BetaCFAVG <- (BetaCF_10+BetaCF_40+BetaCF_20+BetaCF_30)/4
   # BetaDRAVG <- (BetaDR_10+BetaDR_40+BetaDR_20+BetaDR_30)/4

   tbaux_10 <- rbind(BetaCF_10 ,BetaDR_10, BetaCF_p10 ,BetaDR_p10)
   tbaux_20 <- rbind(BetaCF_20 ,BetaDR_20, BetaCF_p20 ,BetaDR_p20)
   tbaux_30 <- rbind(BetaCF_30 ,BetaDR_30, BetaCF_p30 ,BetaDR_p30)
   tbaux_40 <- rbind(BetaCF_40 ,BetaDR_40, BetaCF_p40 ,BetaDR_p40)
   tbaux_50 <- rbind(BetaCF_50, BetaDR_50, BetaCF_p50 ,BetaDR_p50)

   nameTab <- rbind(BetaNames,BetaNamesP)
   # print(sprintf("beta calculado"))
   # covCF_10 <- ((taux1$'10%' [c(11)]))
   # print(BetaDR_10)
   aux <- cbind(nameTab,tbaux_50,tbaux_20,tbaux_30,tbaux_40,tbaux_10 )
   colnames(aux)<-c("VARF",	"10%",	"20%",	"30%", "40%", "50% (base case)")
return(rbind(taux,aux))
}

treatTab4 <- function(ano, dadosn, dtICC_D_4){

   # print(sprintf("entrou na função outro"))

   dadosR_4_0 <- calculaRetorno(dadosn,ano)
   P10 <- decomposicaoRetornoOutroArquivo(dtICC_D_4,dadosR_4_0,interv = list(H=c(.9,1),L=c(0,.1)))[ICC == "ICC_GLS"]
   # print(P10)
   P20 <- decomposicaoRetornoOutroArquivo(dtICC_D_4,dadosR_4_0,interv = list(H=c(.8,1),L=c(0,.2)))[ICC == "ICC_GLS"]
   P30 <- decomposicaoRetornoOutroArquivo(dtICC_D_4,dadosR_4_0,interv = list(H=c(.7,1),L=c(0,.3)))[ICC == "ICC_GLS"]
   # print(P30)
   P40 <- decomposicaoRetornoOutroArquivo(dtICC_D_4,dadosR_4_0,interv = list(H=c(.6,1),L=c(0,.4)))[ICC == "ICC_GLS"]
   P50 <- decomposicaoRetornoOutroArquivo(dtICC_D_4,dadosR_4_0,interv = list(H=c(.5,1),L=c(0,.5)))[ICC == "ICC_GLS"]

   P10[,CASE := "10%"]
   P20[,CASE := "20%"]
   P30[,CASE := "30%"]
   P40[,CASE := "40%"]
   P50[,CASE := "50% (base case)"]
   tb4 <- rbind(P10,P20,P30,P40,P50)

   tb4 <- midprocessing_table4(tb4)

   # print("saiu função")

   return(tb4)
}

midprocessing_table4 <- function(tb4_1){
   tb4_1[,Valor := round(Valor*100,2)]
   tb4_1 <- dcast(tb4_1,VARF~CASE,value.var = "Valor")
   tb4_1 <- as.data.frame(tb4_1)
   rownames(tb4_1) <- tb4_1$VARF
   return(tb4_1)
}
