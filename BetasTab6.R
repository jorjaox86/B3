tab6Build <- function(dadosn, dtICC_D_6,anos){

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


   tb6 <- BetaTab6Calc(tb6_1,tb6_2,tb6_3,tb6_4,tb6_5)
   # tb6_E <- BetaTab6Calc(tb6_1_E,tb6_2_E)
   # tb6_S <- BetaTab6Calc(tb6_1_S,tb6_2_S)
   # tb6_G <- BetaTab6Calc(tb6_1_G,tb6_2_G)
   # tb6_ESG <- BetaTab6Calc(tb6_1_ESG,tb6_2_ESG)

   #build expected return table part----------------------
   # tb6_final <- copy(tb6_1_E)
   # tb6_final <- as.data.table(tb6_final)
   # tb6_final[,S := tb6_1_S$ICC_GLS]
   # tb6_final[,G := tb6_1_G$ICC_GLS]
   # tb6_final[,"E+S+G" := tb6_1_ESG$ICC_GLS]
   # tb6_final[,ESG := tb6_1$ICC_GLS]
   # colnames(tb6_final) <- c("VAR","VARF","E","S","G","E+S+G","ESG")
   #
   #
   # #-------------------------------------------------------
   #
   # nameTab <- data.frame(x1 = c("BetaCf","BetaDR"))
   # aux <- cbind(nameTab,nameTab,tb6_E,tb6_S,tb6_G,tb6_ESG,tb6)
   # colnames(aux)<-c("VAR","VARF",	"E",	"S",	"G","E+S+G","ESG")
   # return(rbind(tb6_final,aux))
   return(tb6)
}

BetaTab6Calc <- function(taux, taux1, taux2, taux3, taux4){

   URM_1 <- c(taux$`Base Case`[c(12)],taux1$`Base Case`[c(12)],taux2$`Base Case`[c(12)],taux3$`Base Case`[c(12)],taux4$`Base Case`[c(12)])
   media1 <- mean(URM_1)

   URM_2 <- c(taux$`E`[c(12)],taux1$`E`[c(12)],taux2$`E`[c(12)],taux3$`E`[c(12)],taux4$`E`[c(12)])
   media2 <- mean(URM_2)

   URM_3 <- c(taux$`S`[c(12)],taux1$`S`[c(12)],taux2$`S`[c(12)],taux3$`S`[c(12)],taux4$`S`[c(12)])
   media3 <- mean(URM_3)

   URM_4 <- c(taux$`G`[c(12)],taux1$`G`[c(12)],taux2$`G`[c(12)],taux3$`G`[c(12)],taux4$`G`[c(12)])
   media4 <- mean(URM_2)

   URM_5 <- c(taux$`E+S+G`[c(12)],taux1$`E+S+G`[c(12)],taux2$`E+S+G`[c(12)],taux3$`E+S+G`[c(12)],taux4$`E+S+G`[c(12)])
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

   URHL_1 <- c(taux$`Base Case`[c(11)],taux1$`Base Case`[c(11)],taux2$`Base Case`[c(11)],taux3$`Base Case`[c(11)],taux4$`Base Case`[c(11)])
   URHL_2 <- c(taux$`E`[c(11)],taux1$`E`[c(11)],taux2$`E`[c(11)],taux3$`E`[c(11)],taux4$`E`[c(11)])
   URHL_3 <- c(taux$`S`[c(11)],taux1$`S`[c(11)],taux2$`S`[c(11)],taux3$`S`[c(11)],taux4$`S`[c(11)])
   URHL_4 <- c(taux$`G`[c(11)],taux1$`G`[c(11)],taux2$`G`[c(11)],taux3$`G`[c(11)],taux4$`G`[c(11)])
   URHL_5 <- c(taux$`E+S+G`[c(11)],taux1$`E+S+G`[c(11)],taux2$`E+S+G`[c(11)],taux3$`E+S+G`[c(11)],taux4$`E+S+G`[c(11)])
   URHL <- list(URHL_1,URHL_2,URHL_3,URHL_4,URHL_5)

   mediaURHL1 <- mean(URHL_1)
   mediaURHL2 <- mean(URHL_2)
   mediaURHL3 <- mean(URHL_3)
   mediaURHL4 <- mean(URHL_4)
   mediaURHL5 <- mean(URHL_5)
   mediaURHL <- list(mediaURHL1,mediaURHL2,mediaURHL3,mediaURHL4,mediaURHL5)

   NDRM_1 <- c(taux$`Base Case`[c(2)],taux1$`Base Case`[c(2)],taux2$`Base Case`[c(2)],taux3$`Base Case`[c(2)],taux4$`Base Case`[c(2)])
   NDRM_2 <- c(taux$`E`[c(2)],taux1$`E`[c(2)],taux2$`E`[c(2)],taux3$`E`[c(2)],taux4$`E`[c(2)])
   NDRM_3 <- c(taux$`S`[c(2)],taux1$`S`[c(2)],taux2$`S`[c(2)],taux3$`S`[c(2)],taux4$`S`[c(2)])
   NDRM_4 <- c(taux$`G`[c(2)],taux1$`G`[c(2)],taux2$`G`[c(2)],taux3$`G`[c(2)],taux4$`G`[c(2)])
   NDRM_5 <- c(taux$`E+S+G`[c(2)],taux1$`E+S+G`[c(2)],taux2$`E+S+G`[c(2)],taux3$`E+S+G`[c(2)],taux4$`E+S+G`[c(2)])
   NDRM <- list(NDRM_1,NDRM_2,NDRM_3,NDRM_4,NDRM_5)

   mediaNDRM1 <- mean(NDRM_1)
   mediaNDRM2 <- mean(NDRM_2)
   mediaNDRM3 <- mean(NDRM_3)
   mediaNDRM4 <- mean(NDRM_4)
   mediaNDRM5 <- mean(NDRM_5)
   mediaNDRM <- list(mediaNDRM1,mediaNDRM2,mediaNDRM3,mediaNDRM4,mediaNDRM5)

   NCFM_1 <- c(taux$`Base Case`[c(6)],taux1$`Base Case`[c(6)],taux2$`Base Case`[c(6)],taux3$`Base Case`[c(6)],taux4$`Base Case`[c(6)])
   NCFM_2 <- c(taux$`E`[c(6)],taux1$`E`[c(6)],taux2$`E`[c(6)],taux3$`E`[c(6)],taux4$`E`[c(6)])
   NCFM_3 <- c(taux$`S`[c(6)],taux1$`S`[c(6)],taux2$`S`[c(6)],taux3$`S`[c(6)],taux4$`S`[c(6)])
   NCFM_4 <- c(taux$`G`[c(6)],taux1$`G`[c(6)],taux2$`G`[c(6)],taux3$`G`[c(6)],taux4$`G`[c(6)])
   NCFM_5 <- c(taux$`E+S+G`[c(6)],taux1$`E+S+G`[c(6)],taux2$`E+S+G`[c(6)],taux3$`E+S+G`[c(6)],taux4$`E+S+G`[c(6)])
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


   # covCF_10 <- ((taux1$`Base Case`[c(11)])-(taux1$`Base Case`[c(11)]+ taux$`Base Case`[c(11)])/2 )*((taux1$`Base Case`[c(6)])-(taux1$`Base Case`[c(6)]+ taux$`Base Case`[c(6)])/2 )+((taux$`Base Case`[c(11)])-(taux1$`Base Case`[c(11)]+ taux$`Base Case`[c(11)])/2 )*((taux$`Base Case`[c(6)])-(taux1$`Base Case`[c(6)]+ taux$`Base Case`[c(6)])/2 )
   # variancia_10 <- (taux$`Base Case`[c(12)]-(taux1$`Base Case`[c(12)]+ taux$`Base Case`[c(12)])/2 )^2+(taux1$`Base Case`[c(12)]-(taux1$`Base Case`[c(12)]+ taux$`Base Case`[c(12)])/2 )^2
   #
   # covDR_10 <- ((taux1$`Base Case`[c(11)])-(taux1$`Base Case`[c(11)]+ taux$`Base Case`[c(11)])/2 )*((taux1$`Base Case`[c(2)])-(taux1$`Base Case`[c(2)]+ taux$`Base Case`[c(2)])/2 )+((taux$`Base Case`[c(11)])-(taux1$`Base Case`[c(11)]+ taux$`Base Case`[c(11)])/2 )*((taux$`Base Case`[c(2)])-(taux1$`Base Case`[c(2)]+ taux$`Base Case`[c(2)])/2 )
   #
   # covCF_20 <- ((taux1$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux1$`L=[0-3] H=[7-10]`[c(6)])-(taux1$`L=[0-3] H=[7-10]`[c(6)]+ taux$`L=[0-3] H=[7-10]`[c(6)])/2 )+((taux$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux$`L=[0-3] H=[7-10]`[c(6)])-(taux1$`L=[0-3] H=[7-10]`[c(6)]+ taux$`L=[0-3] H=[7-10]`[c(6)])/2 )
   # variancia_20 <- (taux$`L=[0-3] H=[7-10]`[c(12)]-(taux1$`L=[0-3] H=[7-10]`[c(12)]+ taux$`L=[0-3] H=[7-10]`[c(12)])/2 )^2+(taux1$`L=[0-3] H=[7-10]`[c(12)]-(taux1$`L=[0-3] H=[7-10]`[c(12)]+ taux$`L=[0-3] H=[7-10]`[c(12)])/2 )^2
   #
   # covDR_20 <- ((taux1$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux1$`L=[0-3] H=[7-10]`[c(2)])-(taux1$`L=[0-3] H=[7-10]`[c(2)]+ taux$`L=[0-3] H=[7-10]`[c(2)])/2 )+((taux$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux$`L=[0-3] H=[7-10]`[c(2)])-(taux1$`L=[0-3] H=[7-10]`[c(2)]+ taux$`L=[0-3] H=[7-10]`[c(2)])/2 )
   #
   # covCF_30 <- ((taux1$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux1$`L=[0-4] H=[6-10]`[c(6)])-(taux1$`L=[0-4] H=[6-10]`[c(6)]+ taux$`L=[0-4] H=[6-10]`[c(6)])/2 )+((taux$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux$`L=[0-4] H=[6-10]`[c(6)])-(taux1$`L=[0-4] H=[6-10]`[c(6)]+ taux$`L=[0-4] H=[6-10]`[c(6)])/2 )
   # variancia_30 <- (taux$`L=[0-4] H=[6-10]`[c(12)]-(taux1$`L=[0-4] H=[6-10]`[c(12)]+ taux$`L=[0-4] H=[6-10]`[c(12)])/2 )^2+(taux1$`L=[0-4] H=[6-10]`[c(12)]-(taux1$`L=[0-4] H=[6-10]`[c(12)]+ taux$`L=[0-4] H=[6-10]`[c(12)])/2 )^2
   #
   # covDR_30 <- ((taux1$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux1$`L=[0-4] H=[6-10]`[c(2)])-(taux1$`L=[0-4] H=[6-10]`[c(2)]+ taux$`L=[0-4] H=[6-10]`[c(2)])/2 )+((taux$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux$`L=[0-4] H=[6-10]`[c(2)])-(taux1$`L=[0-4] H=[6-10]`[c(2)]+ taux$`L=[0-4] H=[6-10]`[c(2)])/2 )

   ###Cálculo dos Betas##############################################

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

   ############################################################

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


   tbaux_10 <- rbind(BetaCF_10 ,BetaDR_10, BetaCF_p10 ,BetaDR_p10)
   tbaux_20 <- rbind(BetaCF_20 ,BetaDR_20, BetaCF_p20 ,BetaDR_p20)
   tbaux_30 <- rbind(BetaCF_30 ,BetaDR_30, BetaCF_p30 ,BetaDR_p30)
   tbaux_40 <- rbind(BetaCF_40 ,BetaDR_40, BetaCF_p40 ,BetaDR_p40)
   tbaux_50 <- rbind(BetaCF_50 ,BetaDR_50, BetaCF_p50 ,BetaDR_p50)

   nameTab <- rbind(BetaNames,BetaNamesP)
   # print(sprintf("beta calculado"))
   # covCF_10 <- ((taux1$'Base Case' [c(11)]))
   # print(BetaDR_10)
   aux <- cbind(nameTab,tbaux_10,tbaux_20, tbaux_30,tbaux_40, tbaux_50)
   colnames(aux)<-c("VARF",	"Base Case",	"E",	"S", "G", "E+S+G")
   return(rbind(taux,aux))

   # covCF_10 <- ((taux1$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux1$`ICC_GLS`[c(4)])-(taux1$`ICC_GLS`[c(4)]+ taux$`ICC_GLS`[c(4)])/2 )+((taux$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux$`ICC_GLS`[c(4)])-(taux1$`ICC_GLS`[c(4)]+ taux$`ICC_GLS`[c(4)])/2 )
   # variancia_10 <- (taux$`ICC_GLS`[c(12)]-(taux1$`ICC_GLS`[c(12)]+ taux$`ICC_GLS`[c(12)])/2 )^2+(taux1$`ICC_GLS`[c(12)]-(taux1$`ICC_GLS`[c(12)]+ taux$`ICC_GLS`[c(12)])/2 )^2
   #
   # covDR_10 <- ((taux1$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux1$`ICC_GLS`[c(6)])-(taux1$`ICC_GLS`[c(6)]+ taux$`ICC_GLS`[c(6)])/2 )+((taux$`ICC_GLS`[c(11)])-(taux1$`ICC_GLS`[c(11)]+ taux$`ICC_GLS`[c(11)])/2 )*((taux$`ICC_GLS`[c(6)])-(taux1$`ICC_GLS`[c(6)]+ taux$`ICC_GLS`[c(6)])/2 )
   #
   # # covCF_20 <- ((taux1$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux1$`L=[0-3] H=[7-10]`[c(6)])-(taux1$`L=[0-3] H=[7-10]`[c(6)]+ taux$`L=[0-3] H=[7-10]`[c(6)])/2 )+((taux$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux$`L=[0-3] H=[7-10]`[c(6)])-(taux1$`L=[0-3] H=[7-10]`[c(6)]+ taux$`L=[0-3] H=[7-10]`[c(6)])/2 )
   # # variancia_20 <- (taux$`L=[0-3] H=[7-10]`[c(12)]-(taux1$`L=[0-3] H=[7-10]`[c(12)]+ taux$`L=[0-3] H=[7-10]`[c(12)])/2 )^2+(taux1$`L=[0-3] H=[7-10]`[c(12)]-(taux1$`L=[0-3] H=[7-10]`[c(12)]+ taux$`L=[0-3] H=[7-10]`[c(12)])/2 )^2
   # #
   # # covDR_20 <- ((taux1$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux1$`L=[0-3] H=[7-10]`[c(2)])-(taux1$`L=[0-3] H=[7-10]`[c(2)]+ taux$`L=[0-3] H=[7-10]`[c(2)])/2 )+((taux$`L=[0-3] H=[7-10]`[c(11)])-(taux1$`L=[0-3] H=[7-10]`[c(11)]+ taux$`L=[0-3] H=[7-10]`[c(11)])/2 )*((taux$`L=[0-3] H=[7-10]`[c(2)])-(taux1$`L=[0-3] H=[7-10]`[c(2)]+ taux$`L=[0-3] H=[7-10]`[c(2)])/2 )
   # #
   # # covCF_30 <- ((taux1$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux1$`L=[0-4] H=[6-10]`[c(6)])-(taux1$`L=[0-4] H=[6-10]`[c(6)]+ taux$`L=[0-4] H=[6-10]`[c(6)])/2 )+((taux$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux$`L=[0-4] H=[6-10]`[c(6)])-(taux1$`L=[0-4] H=[6-10]`[c(6)]+ taux$`L=[0-4] H=[6-10]`[c(6)])/2 )
   # # variancia_30 <- (taux$`L=[0-4] H=[6-10]`[c(12)]-(taux1$`L=[0-4] H=[6-10]`[c(12)]+ taux$`L=[0-4] H=[6-10]`[c(12)])/2 )^2+(taux1$`L=[0-4] H=[6-10]`[c(12)]-(taux1$`L=[0-4] H=[6-10]`[c(12)]+ taux$`L=[0-4] H=[6-10]`[c(12)])/2 )^2
   # #
   # # covDR_30 <- ((taux1$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux1$`L=[0-4] H=[6-10]`[c(2)])-(taux1$`L=[0-4] H=[6-10]`[c(2)]+ taux$`L=[0-4] H=[6-10]`[c(2)])/2 )+((taux$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux$`L=[0-4] H=[6-10]`[c(2)])-(taux1$`L=[0-4] H=[6-10]`[c(2)]+ taux$`L=[0-4] H=[6-10]`[c(2)])/2 )
   # #
   # BetaCF_V_weight <- covCF_10/variancia_10
   # BetaDR_V_weight <- covDR_10/variancia_10
   # #
   # # BetaCF_20 <- covCF_20/variancia_20
   # # BetaDR_20 <- covDR_20/variancia_20
   #
   # # BetaCF_30 <- covCF_30/variancia_30
   # # BetaDR_30 <- covDR_30/variancia_30
   #
   #
   # tbaux_10 <- rbind(BetaCF_V_weight ,BetaDR_V_weight)
   # # tbaux_20 <- rbind(BetaCF_20 ,BetaDR_20)
   # # tbaux_30 <- rbind(BetaCF_30 ,BetaDR_30)
   #
   # # print(sprintf("beta calculado"))
   # # covCF_10 <- ((taux1$'ICC_GLS' [c(11)]))
   # # print(BetaDR_10)
   #
   # return(tbaux_10)
}
treatTab6 <- function(ano, dadosn, dtICC_D_6){

   dadosR_6_0 <- calculaRetorno(dadosn,ano)

   tb6_1 <- decomposicaoRetornoOutroArquivo(dtICC_D_6,dadosR_6_0)[ICC == "ICC_GLS"]
   # tb6_1 <- midprocessing_table(tb6_1)


   tb6_1_E <- decomposicaoRetornoE(dtICC_D_6,dadosR_6_0)[ICC == "ICC_GLS"]
   # tb6_1_E <- midprocessing_table(tb6_1_E)


   ###tabela Social score

   tb6_1_S <- decomposicaoRetornoS(dtICC_D_6,dadosR_6_0)[ICC == "ICC_GLS"]
   # tb6_1_S <- midprocessing_table(tb6_1_S)


   ###tabela Governance score

   tb6_1_G <- decomposicaoRetornoG(dtICC_D_6,dadosR_6_0)[ICC == "ICC_GLS"]
   # tb6_1_G <- midprocessing_table(tb6_1_G)


   ###tabela E+S+G score

   tb6_1_ESG <- decomposicaoRetornoESG(dtICC_D_6,dadosR_6_0)[ICC == "ICC_GLS"]
   # tb6_1_ESG <- midprocessing_table(tb6_1_ESG)

   tb6_1[,CASE := "Base Case"]
   tb6_1_E[,CASE := "E"]
   tb6_1_S[,CASE := "S"]
   tb6_1_G[,CASE := "G"]
   tb6_1_ESG[,CASE := "E+S+G"]

   tb6<-rbind(tb6_1_E,tb6_1_S,tb6_1_G,tb6_1_ESG,tb6_1)

   tb6 <- midprocessing_table6(tb6)


   return(tb6)
}

midprocessing_table6 <- function(tb6_1){
   tb6_1[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
   tb6_1[,Valor := round(Valor*100,2)]
   tb6_1 <- dcast(tb6_1,VARF~CASE,value.var = "Valor")
   tb6_1 <- as.data.frame(tb6_1)
   rownames(tb6_1) <- tb6_1$VARF
   return(tb6_1)
}
