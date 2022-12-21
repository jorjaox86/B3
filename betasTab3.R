betasTab3 <- function(dadosn,dt,anos){

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



   tb3 <- BetaTab3Calc(tb3_1,tb3_2,tb3_3,tb3_4,tb3_5)

   return(tb3)
   # return(list(tb3_1,tb3_2,tb3_3.tb3_4,tb3_5))
}

BetaTab3Calc <- function(taux, taux1, taux2, taux3, taux4){

   ###calculo das variâncias

   #vetorizando os valores das tabelas - c(12) = UR_M
   URM_1 <- c(taux$`DeltaHL`[c(12)],taux1$`DeltaHL`[c(12)],taux2$`DeltaHL`[c(12)],taux3$`DeltaHL`[c(12)],taux4$`DeltaHL`[c(12)])
   media1 <- mean(URM_1)

   URM_2 <- c(taux$`DeltaH`[c(12)],taux1$`DeltaH`[c(12)],taux2$`DeltaH`[c(12)],taux3$`DeltaH`[c(12)],taux4$`DeltaH`[c(12)])
   media2 <- mean(URM_2)

   URM_3 <- c(taux$`DeltaL`[c(12)],taux1$`DeltaL`[c(12)],taux2$`DeltaL`[c(12)],taux3$`DeltaL`[c(12)],taux4$`DeltaL`[c(12)])
   media3 <- mean(URM_3)

   u <- as.numeric(length(URM_1))

   URM <-list( URM_1,URM_2,URM_3)
   media <- list(media1,media2,media3)

   for (i in 1:3){

      varia <- 0
      for(n in 1:5) {
         varia <- varia+(URM[[i]][n]-media[[i]])^2
      }
      varia <- varia/(u-1)
      assign(paste0("variancia_",i*10),varia)
   }

   ######variancia_11 <- var(x, y=NULL, use = "everything")

   ####expressão da covariancia###############################################

   URHL_1 <- c(taux$`DeltaHL`[c(11)],taux1$`DeltaHL`[c(11)],taux2$`DeltaHL`[c(11)],taux3$`DeltaHL`[c(11)],taux4$`DeltaHL`[c(11)])
   URHL_2 <- c(taux$`DeltaH`[c(11)],taux1$`DeltaH`[c(11)],taux2$`DeltaH`[c(11)],taux3$`DeltaH`[c(11)],taux4$`DeltaH`[c(11)])
   URHL_3 <- c(taux$`DeltaL`[c(11)],taux1$`DeltaL`[c(11)],taux2$`DeltaL`[c(11)],taux3$`DeltaL`[c(11)],taux4$`DeltaL`[c(11)])
   URHL <- list(URHL_1,URHL_2,URHL_3)

   mediaURHL1 <- mean(URHL_1)
   mediaURHL2 <- mean(URHL_2)
   mediaURHL3 <- mean(URHL_3)
   mediaURHL <- list(mediaURHL1,mediaURHL2,mediaURHL3)

   NDRM_1 <- c(taux$`DeltaHL`[c(2)],taux1$`DeltaHL`[c(2)],taux2$`DeltaHL`[c(2)],taux3$`DeltaHL`[c(2)],taux4$`DeltaHL`[c(2)])
   NDRM_2 <- c(taux$`DeltaH`[c(2)],taux1$`DeltaH`[c(2)],taux2$`DeltaH`[c(2)],taux3$`DeltaH`[c(2)],taux4$`DeltaH`[c(2)])
   NDRM_3 <- c(taux$`DeltaL`[c(2)],taux1$`DeltaL`[c(2)],taux2$`DeltaL`[c(2)],taux3$`DeltaL`[c(2)],taux4$`DeltaL`[c(2)])
   NDRM <- list(NDRM_1,NDRM_2,NDRM_3)

   mediaNDRM1 <- mean(NDRM_1)
   mediaNDRM2 <- mean(NDRM_2)
   mediaNDRM3 <- mean(NDRM_3)
   mediaNDRM <- list(mediaNDRM1,mediaNDRM2,mediaNDRM3)

   NCFM_1 <- c(taux$`DeltaHL`[c(6)],taux1$`DeltaHL`[c(6)],taux2$`DeltaHL`[c(6)],taux3$`DeltaHL`[c(6)],taux4$`DeltaHL`[c(6)])
   NCFM_2 <- c(taux$`DeltaH`[c(6)],taux1$`DeltaH`[c(6)],taux2$`DeltaH`[c(6)],taux3$`DeltaH`[c(6)],taux4$`DeltaH`[c(6)])
   NCFM_3 <- c(taux$`DeltaL`[c(6)],taux1$`DeltaL`[c(6)],taux2$`DeltaL`[c(6)],taux3$`DeltaL`[c(6)],taux4$`DeltaL`[c(6)])
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
      }
      covaria <- covaria/(u-1)
      assign(paste0("covCF_",i*10),covaria)
   }

   for (i in 1:3){

      covaria <- 0
      for(n in 1:5) {
         covaria <- covaria+(URHL[[i]][n]-mediaURHL[[i]])*(NDRM[[i]][n]-mediaNDRM[[i]])
      }
      covaria <- covaria/(u-1)
      assign(paste0("covDR_",i*10),covaria)
   }


   # covCF_10 <- ((taux1$`DeltaHL`[c(11)])-(taux1$`DeltaHL`[c(11)]+ taux$`DeltaHL`[c(11)])/2 )*((taux1$`DeltaHL`[c(6)])-(taux1$`DeltaHL`[c(6)]+ taux$`DeltaHL`[c(6)])/2 )+((taux$`DeltaHL`[c(11)])-(taux1$`DeltaHL`[c(11)]+ taux$`DeltaHL`[c(11)])/2 )*((taux$`DeltaHL`[c(6)])-(taux1$`DeltaHL`[c(6)]+ taux$`DeltaHL`[c(6)])/2 )
   # variancia_10 <- (taux$`DeltaHL`[c(12)]-(taux1$`DeltaHL`[c(12)]+ taux$`DeltaHL`[c(12)])/2 )^2+(taux1$`DeltaHL`[c(12)]-(taux1$`DeltaHL`[c(12)]+ taux$`DeltaHL`[c(12)])/2 )^2
   #
   # covDR_10 <- ((taux1$`DeltaHL`[c(11)])-(taux1$`DeltaHL`[c(11)]+ taux$`DeltaHL`[c(11)])/2 )*((taux1$`DeltaHL`[c(2)])-(taux1$`DeltaHL`[c(2)]+ taux$`DeltaHL`[c(2)])/2 )+((taux$`DeltaHL`[c(11)])-(taux1$`DeltaHL`[c(11)]+ taux$`DeltaHL`[c(11)])/2 )*((taux$`DeltaHL`[c(2)])-(taux1$`DeltaHL`[c(2)]+ taux$`DeltaHL`[c(2)])/2 )
   #
   # covCF_20 <- ((taux1$`DeltaH`[c(11)])-(taux1$`DeltaH`[c(11)]+ taux$`DeltaH`[c(11)])/2 )*((taux1$`DeltaH`[c(6)])-(taux1$`DeltaH`[c(6)]+ taux$`DeltaH`[c(6)])/2 )+((taux$`DeltaH`[c(11)])-(taux1$`DeltaH`[c(11)]+ taux$`DeltaH`[c(11)])/2 )*((taux$`DeltaH`[c(6)])-(taux1$`DeltaH`[c(6)]+ taux$`DeltaH`[c(6)])/2 )
   # variancia_20 <- (taux$`DeltaH`[c(12)]-(taux1$`DeltaH`[c(12)]+ taux$`DeltaH`[c(12)])/2 )^2+(taux1$`DeltaH`[c(12)]-(taux1$`DeltaH`[c(12)]+ taux$`DeltaH`[c(12)])/2 )^2
   #
   # covDR_20 <- ((taux1$`DeltaH`[c(11)])-(taux1$`DeltaH`[c(11)]+ taux$`DeltaH`[c(11)])/2 )*((taux1$`DeltaH`[c(2)])-(taux1$`DeltaH`[c(2)]+ taux$`DeltaH`[c(2)])/2 )+((taux$`DeltaH`[c(11)])-(taux1$`DeltaH`[c(11)]+ taux$`DeltaH`[c(11)])/2 )*((taux$`DeltaH`[c(2)])-(taux1$`DeltaH`[c(2)]+ taux$`DeltaH`[c(2)])/2 )
   #
   # covCF_30 <- ((taux1$`DeltaL`[c(11)])-(taux1$`DeltaL`[c(11)]+ taux$`DeltaL`[c(11)])/2 )*((taux1$`DeltaL`[c(6)])-(taux1$`DeltaL`[c(6)]+ taux$`DeltaL`[c(6)])/2 )+((taux$`DeltaL`[c(11)])-(taux1$`DeltaL`[c(11)]+ taux$`DeltaL`[c(11)])/2 )*((taux$`DeltaL`[c(6)])-(taux1$`DeltaL`[c(6)]+ taux$`DeltaL`[c(6)])/2 )
   # variancia_30 <- (taux$`DeltaL`[c(12)]-(taux1$`DeltaL`[c(12)]+ taux$`DeltaL`[c(12)])/2 )^2+(taux1$`DeltaL`[c(12)]-(taux1$`DeltaL`[c(12)]+ taux$`DeltaL`[c(12)])/2 )^2
   #
   # covDR_30 <- ((taux1$`DeltaL`[c(11)])-(taux1$`DeltaL`[c(11)]+ taux$`DeltaL`[c(11)])/2 )*((taux1$`DeltaL`[c(2)])-(taux1$`DeltaL`[c(2)]+ taux$`DeltaL`[c(2)])/2 )+((taux$`DeltaL`[c(11)])-(taux1$`DeltaL`[c(11)]+ taux$`DeltaL`[c(11)])/2 )*((taux$`DeltaL`[c(2)])-(taux1$`DeltaL`[c(2)]+ taux$`DeltaL`[c(2)])/2 )


   covDR_p10 <- cov(URHL_1[URM_1>0],NDRM_1[URM_1>0])
   covCF_p10 <- cov(URHL_1[URM_1>0],NCFM_1[URM_1>0])
   variancia_p10 <- var(URM_1,y=NULL)

   covDR_p20 <- cov(URHL_2[URM_2>0],NDRM_2[URM_2>0])
   covCF_p20 <- cov(URHL_2[URM_2>0],NCFM_2[URM_2>0])
   variancia_p20 <- var(URM_2,y=NULL)

   covDR_p30 <- cov(URHL_3[URM_3>0],NDRM_3[URM_3>0])
   covCF_p30 <- cov(URHL_3[URM_3>0],NCFM_3[URM_3>0])
   variancia_p30 <- var(URM_3,y=NULL)


   ###Cálculo dos Betas##############################################

   BetaCF_10 <- covCF_10/variancia_10
   BetaDR_10 <- covDR_10/variancia_10

   BetaCF_20 <- covCF_20/variancia_20
   BetaDR_20 <- covDR_20/variancia_20

   BetaCF_30 <- covCF_30/variancia_30
   BetaDR_30 <- covDR_30/variancia_30

   BetaCF_p10 <- covCF_p10/variancia_p10
   BetaDR_p10 <- covDR_p10/variancia_p10

   BetaCF_p20 <- covCF_p20/variancia_p20
   BetaDR_p20 <- covDR_p20/variancia_p20

   BetaCF_p30 <- covCF_p30/variancia_p30
   BetaDR_p30 <- covDR_p30/variancia_p30

   BetaCF_p10[is.na(BetaCF_p10)] <- 0.000
   BetaCF_p20[is.na(BetaCF_p20)] <- 0.000
   BetaCF_p30[is.na(BetaCF_p20)] <- 0.000

   BetaDR_p10[is.na(BetaDR_p10)] <- 0.000
   BetaDR_p20[is.na(BetaDR_p20)] <- 0.000
   BetaDR_p30[is.na(BetaDR_p30)] <- 0.000

   tbaux_10 <- rbind(BetaCF_10 ,BetaDR_10,  BetaCF_p10, BetaDR_p10)
   tbaux_20 <- rbind(BetaCF_20 ,BetaDR_20,  BetaCF_p20, BetaDR_p20)
   tbaux_30 <- rbind(BetaCF_30 ,BetaDR_30,  BetaCF_p30, BetaDR_p30)


   # nameTab <- BetaNames
   # print(sprintf("beta calculado"))
   # covCF_10 <- ((taux1$'DeltaHL' [c(11)]))
   # print(BetaDR_10)


   # covCF_10 <- ((taux1$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux1$`ICC_GLS`[c(4)])-(taux1$`ICC_GLS`[c(4)]+ taux$`ICC_GLS`[c(4)])/2 )+((taux$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux$`ICC_GLS`[c(4)])-(taux1$`ICC_GLS`[c(4)]+ taux$`ICC_GLS`[c(4)])/2 )
   # variancia_10 <- (taux$`ICC_GLS`[c(12)]-(taux1$`ICC_GLS`[c(12)]+ taux$`ICC_GLS`[c(12)])/2 )^2+(taux1$`ICC_GLS`[c(12)]-(taux1$`ICC_GLS`[c(12)]+ taux$`ICC_GLS`[c(12)])/2 )^2
   #
   # covDR_10 <- ((taux1$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux1$`ICC_GLS`[c(6)])-(taux1$`ICC_GLS`[c(6)]+ taux$`ICC_GLS`[c(6)])/2 )+((taux$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux$`ICC_GLS`[c(6)])-(taux1$`ICC_GLS`[c(6)]+ taux$`ICC_GLS`[c(6)])/2 )
   #
   # # covCF_20 <- ((taux1$`DeltaH`[c(11)])-(taux1$`DeltaH`[c(11)]+ taux$`DeltaH`[c(11)])/2 )*((taux1$`DeltaH`[c(6)])-(taux1$`DeltaH`[c(6)]+ taux$`DeltaH`[c(6)])/2 )+((taux$`DeltaH`[c(11)])-(taux1$`DeltaH`[c(11)]+ taux$`DeltaH`[c(11)])/2 )*((taux$`DeltaH`[c(6)])-(taux1$`DeltaH`[c(6)]+ taux$`DeltaH`[c(6)])/2 )
   # # variancia_20 <- (taux$`DeltaH`[c(12)]-(taux1$`DeltaH`[c(12)]+ taux$`DeltaH`[c(12)])/2 )^2+(taux1$`DeltaH`[c(12)]-(taux1$`DeltaH`[c(12)]+ taux$`DeltaH`[c(12)])/2 )^2
   # #
   # # covDR_20 <- ((taux1$`DeltaH`[c(11)])-(taux1$`DeltaH`[c(11)]+ taux$`DeltaH`[c(11)])/2 )*((taux1$`DeltaH`[c(2)])-(taux1$`DeltaH`[c(2)]+ taux$`DeltaH`[c(2)])/2 )+((taux$`DeltaH`[c(11)])-(taux1$`DeltaH`[c(11)]+ taux$`DeltaH`[c(11)])/2 )*((taux$`DeltaH`[c(2)])-(taux1$`DeltaH`[c(2)]+ taux$`DeltaH`[c(2)])/2 )
   # #
   # # covCF_30 <- ((taux1$`DeltaL`[c(11)])-(taux1$`DeltaL`[c(11)]+ taux$`DeltaL`[c(11)])/2 )*((taux1$`DeltaL`[c(6)])-(taux1$`DeltaL`[c(6)]+ taux$`DeltaL`[c(6)])/2 )+((taux$`DeltaL`[c(11)])-(taux1$`DeltaL`[c(11)]+ taux$`DeltaL`[c(11)])/2 )*((taux$`DeltaL`[c(6)])-(taux1$`DeltaL`[c(6)]+ taux$`DeltaL`[c(6)])/2 )
   # # variancia_30 <- (taux$`DeltaL`[c(12)]-(taux1$`DeltaL`[c(12)]+ taux$`DeltaL`[c(12)])/2 )^2+(taux1$`DeltaL`[c(12)]-(taux1$`DeltaL`[c(12)]+ taux$`DeltaL`[c(12)])/2 )^2
   # #
   # # covDR_30 <- ((taux1$`DeltaL`[c(11)])-(taux1$`DeltaL`[c(11)]+ taux$`DeltaL`[c(11)])/2 )*((taux1$`DeltaL`[c(2)])-(taux1$`DeltaL`[c(2)]+ taux$`DeltaL`[c(2)])/2 )+((taux$`DeltaL`[c(11)])-(taux1$`DeltaL`[c(11)]+ taux$`DeltaL`[c(11)])/2 )*((taux$`DeltaL`[c(2)])-(taux1$`DeltaL`[c(2)]+ taux$`DeltaL`[c(2)])/2 )
   # #
   # BetaCF_10 <- covCF_10/variancia_10
   # BetaDR_10 <- covDR_10/variancia_10
   # #
   # # BetaCF_20 <- covCF_20/variancia_20
   # # BetaDR_20 <- covDR_20/variancia_20
   #
   # # BetaCF_30 <- covCF_30/variancia_30
   # # BetaDR_30 <- covDR_30/variancia_30
   #
   #
   # tbaux_10 <- rbind(BetaCF_10 ,BetaDR_10)
   # # tbaux_20 <- rbind(BetaCF_20 ,BetaDR_20)
   # # tbaux_30 <- rbind(BetaCF_30 ,BetaDR_30)

   nameTab <- rbind(BetaNames,BetaNamesP)
   aux <- cbind(nameTab,tbaux_10,tbaux_20, tbaux_30)
   colnames(aux)<-c("VARF",	"DeltaHL", "DeltaH", "DeltaL")

   colnames(taux)<-c("VARF",	"DeltaHL", "DeltaH", "DeltaL")


   #aux2 <- rbind(taux,aux)
   return(rbind(taux,aux))
}

treatTab3 <- function(ano, dadosn, dtICC_D_6){

   dadosR_6_0 <- calculaRetorno(dadosn,ano)

   # tb6_1 <- decomposicaoRetornoOutroArquivo(dtICC_D_6, dadosR_6_0)[ICC == "ICC_GLS"]
   # # tb6_1 <- midprocessing_table(tb6_1)

   tb6_DeltaHL <- decomposicaoRetornoDeltaESG(dtICC_D_6, dadosR_6_0)[ICC == "ICC_GLS"]
   tb6_DeltaH <- decomposicaoRetornoDeltaESG(dtICC_D_6, dadosR_6_0,interv = list(H=c(.75,1),L=c(.5,.75)))[ICC == "ICC_GLS"]
   tb6_DeltaL <- decomposicaoRetornoDeltaESG(dtICC_D_6, dadosR_6_0,interv = list(H=c(.25,.5),L=c(0,.25)))[ICC == "ICC_GLS"]


   tb6_DeltaHL[,CASE := "DeltaHL"]
   tb6_DeltaH[,CASE := "DeltaH"]
   tb6_DeltaL[,CASE := "DeltaL"]

   tb6<-rbind(tb6_DeltaHL, tb6_DeltaH, tb6_DeltaL)

   tb6 <- midprocessing_table3(tb6)


   return(tb6)
}

midprocessing_table3 <- function(tb6_1){
   tb6_1[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
   tb6_1[,Valor := round(Valor*100,2)]
   tb6_1 <- dcast(tb6_1,VARF~CASE,value.var = "Valor")
   tb6_1 <- as.data.frame(tb6_1)
   rownames(tb6_1) <- tb6_1$VARF
   return(tb6_1)
}
