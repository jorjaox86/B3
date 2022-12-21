tab5Build <- function(anos,dadosn,dtICC_0,dtICC_1,dtICC_D,BetaNames){

   # dtICC_D5 <- GeraDeltaDAno(anos)
   # dtICC_05_0 <- dtICC_D5[[1]]
   # dtICC_05_1 <- dtICC_D5[[2]]
   # dtICC_05_D <- dtICC_D5[[3]]

   #gerador de tabelas tb5 de Retornos, NDR e NCF para anos de 2013 a 2017
   for(i in 1:5){
      ano_selecionado <- anos+i-1

      ###gerador de deltas e ICCs para cada ano de 2013 a 2017###
      dtICC_D5 <- GeraDeltaDAno(ano_selecionado)
      ###separando a lista de dtICCs
      dtICC_0 <- dtICC_D5[[1]]
      dtICC_1 <- dtICC_D5[[2]]
      dtICC_D <- dtICC_D5[[3]]
      ###########################################################

      ###marcando as ESGs de acordo com os Cutoffs##############
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

      ####Gerando os deltasICC cutoffs para o ano(i) ######################
      dtICC_D_clean <- deltaICCcut(dtICC_0,dtICC_1)
      dtICC_D_37 <- deltaICCcut(cutoff37_0,cutoff37_1)
      dtICC_D_46 <- deltaICCcut(cutoff46_0,cutoff46_1)

      ###gerando as tabelas de retornos intituladas tb5_1, tb5_2, tb5_3, tb5_4 e tb5_5 para calcular os Betas.
      assign(paste0("tb5_",i),treatTab5(ano_selecionado, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46))
   # tb5_1 <- treatTab5(anos2, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46)
   }

   # # ano <- ano_0
   # # dadosR_5_0 <- calculaRetorno(dadosn,ano)
   # #
   # # tb5_clean <- decomposicaoRetornoOutroArquivo(dtICC_D_clean,dadosR_5_0)[ICC == "ICC_GLS"]
   # # tb5_L <- decomposicaoRetornoOutroArquivo(dtICC_D_37,dadosR_5_0)[ICC == "ICC_GLS"]
   # # tb5_H <- decomposicaoRetornoOutroArquivo(dtICC_D_46,dadosR_5_0)[ICC == "ICC_GLS"]
   #
   # ano <- anos+1
   # dadosR_5_1 <- calculaRetorno(dadosn,ano)
   #
   # tb5_clean_1 <- decomposicaoRetornoOutroArquivo(dtICC_D_clean,dadosR_5_1)[ICC == "ICC_GLS"]
   # tb5_L_1 <- decomposicaoRetornoOutroArquivo(dtICC_D_37,dadosR_5_1)[ICC == "ICC_GLS"]
   # tb5_H_1 <- decomposicaoRetornoOutroArquivo(dtICC_D_46,dadosR_5_1)[ICC == "ICC_GLS"]
   #
   # # tb5_clean[,CASE := "Base Case"]
   # # tb5_L[,CASE := "L=[0-3] H=[7-10]"]
   # # tb5_H[,CASE := "L=[0-4] H=[6-10]"]
   # #
   # # tb5_1 <- rbind(tb5_clean,tb5_H,tb5_L)
   #
   # tb5_clean_1[,CASE := "Base Case"]
   # tb5_L_1[,CASE := "L=[0-3] H=[7-10]"]
   # tb5_H_1[,CASE := "L=[0-4] H=[6-10]"]
   #
   # tb5_2 <- rbind(tb5_clean_1,tb5_H_1,tb5_L_1)
   #
   # # tb5_1[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
   # # tb5_1[,Valor := round(Valor*100,2)]
   # # tb5_1 <- dcast(tb5_1,VARF~CASE,value.var = "Valor")
   # # tb5_1 <- as.data.frame(tb5_1)
   # # rownames(tb5_1) <- tb5_1$VARF
   #
   # tb5_2[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
   # tb5_2[,Valor := round(Valor*100,2)]
   # tb5_2 <- dcast(tb5_2,VARF~CASE,value.var = "Valor")
   # tb5_2 <- as.data.frame(tb5_2)
   # rownames(tb5_2) <- tb5_2$VARF
   #
   # tb5_3 <- treatTab5(anos+2, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46)
   # tb5_4 <- treatTab5(anos+3, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46)
   # tb5_5 <- treatTab5(anos+4, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46)
   # tb5_6 <- treatTab5(anos+5, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46)


   #começando a chamar as funções necessarias para montar o beta da tabela 5###################
   tb5 <- BetaTab5Calc(tb5_1,tb5_2,tb5_3,tb5_4,tb5_5, BetaNames)


   return(tb5)
}

BetaTab5Calc <- function(taux, taux1, taux2, taux3, taux4, BetaNames){

   ###calculo das variâncias

   #vetorizando os valores das tabelas - c(12) = UR_M
   URM_1 <- c(taux$`Base Case`[c(12)],taux1$`Base Case`[c(12)],taux2$`Base Case`[c(12)],taux3$`Base Case`[c(12)],taux4$`Base Case`[c(12)])
   media1 <- mean(URM_1)

   URM_2 <- c(taux$`L=[0-3] H=[7-10]`[c(12)],taux1$`L=[0-3] H=[7-10]`[c(12)],taux2$`L=[0-3] H=[7-10]`[c(12)],taux3$`L=[0-3] H=[7-10]`[c(12)],taux4$`L=[0-3] H=[7-10]`[c(12)])
   media2 <- mean(URM_2)

   URM_3 <- c(taux$`L=[0-4] H=[6-10]`[c(12)],taux1$`L=[0-4] H=[6-10]`[c(12)],taux2$`L=[0-4] H=[6-10]`[c(12)],taux3$`L=[0-4] H=[6-10]`[c(12)],taux4$`L=[0-4] H=[6-10]`[c(12)])
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

   URHL_1 <- c(taux$`Base Case`[c(11)],taux1$`Base Case`[c(11)],taux2$`Base Case`[c(11)],taux3$`Base Case`[c(11)],taux4$`Base Case`[c(11)])
   URHL_2 <- c(taux$`L=[0-3] H=[7-10]`[c(11)],taux1$`L=[0-3] H=[7-10]`[c(11)],taux2$`L=[0-3] H=[7-10]`[c(11)],taux3$`L=[0-3] H=[7-10]`[c(11)],taux4$`L=[0-3] H=[7-10]`[c(11)])
   URHL_3 <- c(taux$`L=[0-4] H=[6-10]`[c(11)],taux1$`L=[0-4] H=[6-10]`[c(11)],taux2$`L=[0-4] H=[6-10]`[c(11)],taux3$`L=[0-4] H=[6-10]`[c(11)],taux4$`L=[0-4] H=[6-10]`[c(11)])
   URHL <- list(URHL_1,URHL_2,URHL_3)

   mediaURHL1 <- mean(URHL_1)
   mediaURHL2 <- mean(URHL_2)
   mediaURHL3 <- mean(URHL_3)
   mediaURHL <- list(mediaURHL1,mediaURHL2,mediaURHL3)

   NDRM_1 <- c(taux$`Base Case`[c(2)],taux1$`Base Case`[c(2)],taux2$`Base Case`[c(2)],taux3$`Base Case`[c(2)],taux4$`Base Case`[c(2)])
   NDRM_2 <- c(taux$`L=[0-3] H=[7-10]`[c(2)],taux1$`L=[0-3] H=[7-10]`[c(2)],taux2$`L=[0-3] H=[7-10]`[c(2)],taux3$`L=[0-3] H=[7-10]`[c(2)],taux4$`L=[0-3] H=[7-10]`[c(2)])
   NDRM_3 <- c(taux$`L=[0-4] H=[6-10]`[c(2)],taux1$`L=[0-4] H=[6-10]`[c(2)],taux2$`L=[0-4] H=[6-10]`[c(2)],taux3$`L=[0-4] H=[6-10]`[c(2)],taux4$`L=[0-4] H=[6-10]`[c(2)])
   NDRM <- list(NDRM_1,NDRM_2,NDRM_3)

   mediaNDRM1 <- mean(NDRM_1)
   mediaNDRM2 <- mean(NDRM_2)
   mediaNDRM3 <- mean(NDRM_3)
   mediaNDRM <- list(mediaNDRM1,mediaNDRM2,mediaNDRM3)

   NCFM_1 <- c(taux$`Base Case`[c(6)],taux1$`Base Case`[c(6)],taux2$`Base Case`[c(6)],taux3$`Base Case`[c(6)],taux4$`Base Case`[c(6)])
   NCFM_2 <- c(taux$`L=[0-3] H=[7-10]`[c(6)],taux1$`L=[0-3] H=[7-10]`[c(6)],taux2$`L=[0-3] H=[7-10]`[c(6)],taux3$`L=[0-3] H=[7-10]`[c(6)],taux4$`L=[0-3] H=[7-10]`[c(6)])
   NCFM_3 <- c(taux$`L=[0-4] H=[6-10]`[c(6)],taux1$`L=[0-4] H=[6-10]`[c(6)],taux2$`L=[0-4] H=[6-10]`[c(6)],taux3$`L=[0-4] H=[6-10]`[c(6)],taux4$`L=[0-4] H=[6-10]`[c(6)])
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


   ######################################################

   BetaCF_p10 <- covCF_p10/variancia_p10
   BetaDR_p10 <- covDR_p10/variancia_p10

   BetaCF_p20 <- covCF_p10/variancia_p20
   BetaDR_p20 <- covDR_p10/variancia_p20

   BetaCF_p30 <- covCF_p30/variancia_p30
   BetaDR_p30 <- covDR_p30/variancia_p30

   ############################################################



   BetaCF_10 <- covCF_10/variancia_10
   BetaDR_10 <- covDR_10/variancia_10

   BetaCF_20 <- covCF_20/variancia_20
   BetaDR_20 <- covDR_20/variancia_20

   BetaCF_30 <- covCF_30/variancia_30
   BetaDR_30 <- covDR_30/variancia_30


   BetaCF_p10[is.na(BetaCF_p10)] <- 0.000
   BetaCF_p20[is.na(BetaCF_p20)] <- 0.000
   BetaCF_p30[is.na(BetaCF_p30)] <- 0.000


   BetaDR_p10[is.na(BetaDR_p10)] <- 0.000
   BetaDR_p20[is.na(BetaDR_p20)] <- 0.000
   BetaDR_p30[is.na(BetaDR_p30)] <- 0.000


   tbaux_10 <- rbind(BetaCF_10 ,BetaDR_10, BetaCF_p10 ,BetaDR_p10)
   tbaux_20 <- rbind(BetaCF_20 ,BetaDR_20, BetaCF_p20 ,BetaDR_p20)
   tbaux_30 <- rbind(BetaCF_30 ,BetaDR_30, BetaCF_p30 ,BetaDR_p30)


   nameTab <- rbind(BetaNames,BetaNamesP)
   aux <- cbind(nameTab,tbaux_10,tbaux_20, tbaux_30)
   colnames(aux)<-c("VARF",	"Base Case",	"L=[0-3] H=[7-10]",	"L=[0-4] H=[6-10]")
   return(rbind(taux,aux))
}

treatTab5 <- function(ano, dadosn, dtICC_D_clean,  dtICC_D_37,  dtICC_D_46){
   dadosR_5_0 <- calculaRetorno(dadosn,ano)

   tb5_clean <- decomposicaoRetornoOutroArquivo(dtICC_D_clean,dadosR_5_0)[ICC == "ICC_GLS"]
   tb5_L <- decomposicaoRetornoOutroArquivo(dtICC_D_37,dadosR_5_0)[ICC == "ICC_GLS"]
   tb5_H <- decomposicaoRetornoOutroArquivo(dtICC_D_46,dadosR_5_0)[ICC == "ICC_GLS"]

   tb5_clean[,CASE := "Base Case"]
   tb5_L[,CASE := "L=[0-3] H=[7-10]"]
   tb5_H[,CASE := "L=[0-4] H=[6-10]"]

   tb5_1 <- rbind(tb5_clean,tb5_H,tb5_L)

   tb5_1[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
   tb5_1[,Valor := round(Valor*100,2)]
   tb5_1 <- dcast(tb5_1,VARF~CASE,value.var = "Valor")
   tb5_1 <- as.data.frame(tb5_1)
   rownames(tb5_1) <- tb5_1$VARF

   return(tb5_1)
}
