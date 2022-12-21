betasTab2 <- function(dadosn,anos){

   for(i in 1:5){
      ano_selecionado <- anos+i-1

      ###gerador de deltas e ICCs para cada ano de 2013 a 2017###
      dtICC_D6 <- GeraDeltaDAno(ano_selecionado)
      ###separando a lista de dtICCs
      dtICC_0 <- dtICC_D6[[1]]
      dtICC_1 <- dtICC_D6[[2]]
      dtICC_D <- dtICC_D6[[3]]

      # dadosR_6_0 <- calculaRetorno(dadosn,ano)

      assign(paste0("tb2_",i),treatTab2(ano_selecionado, dadosn, dtICC_D))

   }
   # dtICC_D <- copy(dt)
   # ano<-ano_0
   # dadosR_b <- dadosn[VAR %in% c("PRC","DPS") & Ano >= ano & Ano < ano + 5 & EmpCode %in% stocks]
   # dadosR_b <- dcast(dadosR_b,EmpCode + Ano ~ VAR,value.var = "Valor")
   # dadosR_b[, `:=` (P0 = shift(PRC),D0 = shift(DPS)),.(EmpCode)]
   # dadosR_b <- dadosR_b[Ano != ano]
   # dadosR_b[,R := ((PRC - D0)/P0)-1]
   # dadosR_ano0 <- dadosR_b[,.(R = mean(R)),.(EmpCode)]
   #
   # ano <- ano_0+1
   # dadosR_b <- dadosn[VAR %in% c("PRC","DPS") & Ano >= ano & Ano < ano + 5 & EmpCode %in% stocks]
   # dadosR_b <- dcast(dadosR_b,EmpCode + Ano ~ VAR,value.var = "Valor")
   # dadosR_b[, `:=` (P0 = shift(PRC),D0 = shift(DPS)),.(EmpCode)]
   # dadosR_b <- dadosR_b[Ano != ano]
   # dadosR_b[,R := ((PRC - D0)/P0)-1]
   # dadosR_ano1 <- dadosR_b[,.(R = mean(R)),.(EmpCode)]
   #
   # taux <- decomposicaoRetornoOutroArquivo(dtICC_D, dadosR_ano0)
   # taux[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
   # taux[,Valor := round(Valor*100,2)]
   # taux <- dcast(taux,VAR+VARF~ICC,value.var = "Valor")
   #
   # taux1 <- decomposicaoRetornoOutroArquivo(dtICC_D, dadosR_ano1)
   # taux1[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
   # taux1[,Valor := round(Valor*100,2)]
   # taux1 <- dcast(taux1,VAR+VARF~ICC,value.var = "Valor")

   tb2t <- BetaTab2Calc(tb2_1, tb2_2, tb2_3, tb2_4, tb2_5, BetaNames)

   return(tb2t)
}

BetaTab2Calc <- function(taux, taux1, taux2, taux3, taux4, BetaNames){

   URM_1 <- c(taux$`GLS(%)`[c(12)],taux1$`GLS(%)`[c(12)],taux2$`GLS(%)`[c(12)],taux3$`GLS(%)`[c(12)],taux4$`GLS(%)`[c(12)])
   media1 <- mean(URM_1)

   URM_2 <- c(taux$`E(%)`[c(12)],taux1$`E(%)`[c(12)],taux2$`E(%)`[c(12)],taux3$`E(%)`[c(12)],taux4$`E(%)`[c(12)])
   media2 <- mean(URM_2)

   URM_3 <- c(taux$`OJ(%)`[c(12)],taux1$`OJ(%)`[c(12)],taux2$`OJ(%)`[c(12)],taux3$`OJ(%)`[c(12)],taux4$`OJ(%)`[c(12)])
   media3 <- mean(URM_3)

   URM_4 <- c(taux$`CT(%)`[c(12)],taux1$`CT(%)`[c(12)],taux2$`CT(%)`[c(12)],taux3$`CT(%)`[c(12)],taux4$`CT(%)`[c(12)])
   media4 <- mean(URM_2)


   u <- as.numeric(length(URM_1))

   URM <-list( URM_1,URM_2,URM_3,URM_4)
   media <- list(media1,media2,media3,media4)

   for (i in 1:4){

      varia <- 0
      for(n in 1:5) {
         varia <- varia+(URM[[i]][n]-media[[i]])^2
      }
      varia <- varia/(u-1)
      assign(paste0("variancia_",i*10),varia)
   }

   URHL_1 <- c(taux$`GLS(%)`[c(11)],taux1$`GLS(%)`[c(11)],taux2$`GLS(%)`[c(11)],taux3$`GLS(%)`[c(11)],taux4$`GLS(%)`[c(11)])
   URHL_2 <- c(taux$`E(%)`[c(11)],taux1$`E(%)`[c(11)],taux2$`E(%)`[c(11)],taux3$`E(%)`[c(11)],taux4$`E(%)`[c(11)])
   URHL_3 <- c(taux$`OJ(%)`[c(11)],taux1$`OJ(%)`[c(11)],taux2$`OJ(%)`[c(11)],taux3$`OJ(%)`[c(11)],taux4$`OJ(%)`[c(11)])
   URHL_4 <- c(taux$`CT(%)`[c(11)],taux1$`CT(%)`[c(11)],taux2$`CT(%)`[c(11)],taux3$`CT(%)`[c(11)],taux4$`CT(%)`[c(11)])

   URHL <- list(URHL_1,URHL_2,URHL_3,URHL_4)

   mediaURHL1 <- mean(URHL_1)
   mediaURHL2 <- mean(URHL_2)
   mediaURHL3 <- mean(URHL_3)
   mediaURHL4 <- mean(URHL_4)

   mediaURHL <- list(mediaURHL1,mediaURHL2,mediaURHL3,mediaURHL4)

   NDRM_1 <- c(taux$`GLS(%)`[c(6)],taux1$`GLS(%)`[c(6)],taux2$`GLS(%)`[c(6)],taux3$`GLS(%)`[c(6)],taux4$`GLS(%)`[c(6)])
   NDRM_2 <- c(taux$`E(%)`[c(6)],taux1$`E(%)`[c(6)],taux2$`E(%)`[c(6)],taux3$`E(%)`[c(6)],taux4$`E(%)`[c(6)])
   NDRM_3 <- c(taux$`OJ(%)`[c(6)],taux1$`OJ(%)`[c(6)],taux2$`OJ(%)`[c(6)],taux3$`OJ(%)`[c(6)],taux4$`OJ(%)`[c(6)])
   NDRM_4 <- c(taux$`CT(%)`[c(6)],taux1$`CT(%)`[c(6)],taux2$`CT(%)`[c(6)],taux3$`CT(%)`[c(6)],taux4$`CT(%)`[c(6)])

   NDRM <- list(NDRM_1,NDRM_2,NDRM_3,NDRM_4)

   mediaNDRM1 <- mean(NDRM_1)
   mediaNDRM2 <- mean(NDRM_2)
   mediaNDRM3 <- mean(NDRM_3)
   mediaNDRM4 <- mean(NDRM_4)

   mediaNDRM <- list(mediaNDRM1,mediaNDRM2,mediaNDRM3,mediaNDRM4)

   NCFM_1 <- c(taux$`GLS(%)`[c(4)],taux1$`GLS(%)`[c(4)],taux2$`GLS(%)`[c(4)],taux3$`GLS(%)`[c(4)],taux4$`GLS(%)`[c(4)])
   NCFM_2 <- c(taux$`E(%)`[c(4)],taux1$`E(%)`[c(4)],taux2$`E(%)`[c(4)],taux3$`E(%)`[c(4)],taux4$`E(%)`[c(4)])
   NCFM_3 <- c(taux$`OJ(%)`[c(4)],taux1$`OJ(%)`[c(4)],taux2$`OJ(%)`[c(4)],taux3$`OJ(%)`[c(4)],taux4$`OJ(%)`[c(4)])
   NCFM_4 <- c(taux$`CT(%)`[c(4)],taux1$`CT(%)`[c(4)],taux2$`CT(%)`[c(4)],taux3$`CT(%)`[c(4)],taux4$`CT(%)`[c(4)])

   NCFM <- list(NCFM_1,NCFM_2,NCFM_3,NCFM_4)

   mediaNCFM1 <- mean(NCFM_1)
   mediaNCFM2 <- mean(NCFM_2)
   mediaNCFM3 <- mean(NCFM_3)
   mediaNCFM4 <- mean(NCFM_4)

   mediaNCFM <- list(mediaNCFM1,mediaNCFM2,mediaNCFM3,mediaNCFM4)

   ##repetindo so por desencargo de consciência
   u <- as.numeric(length(URHL_1))
   ####iteração para calculo da covariancia_10, covariancia_20 e covariancia_30##################################
   for (i in 1:4){

      covaria <- 0
      for(n in 1:5) {
         covaria <- covaria+(URHL[[i]][n]-mediaURHL[[i]])*(NCFM[[i]][n]-mediaNCFM[[i]])
      }
      covaria <- covaria/(u-1)
      assign(paste0("covCF_",i*10),covaria)
   }

   for (i in 1:4){

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

   ######################################################

   BetaCF_p10 <- covCF_p10/variancia_p10
   BetaDR_p10 <- covDR_p10/variancia_p10

   BetaCF_p20 <- covCF_p10/variancia_p20
   BetaDR_p20 <- covDR_p10/variancia_p20

   BetaCF_p30 <- covCF_p30/variancia_p30
   BetaDR_p30 <- covDR_p30/variancia_p30

   BetaCF_p40 <- covCF_p40/variancia_p40
   BetaDR_p40 <- covDR_p40/variancia_p40

   BetaCF_p10[is.na(BetaCF_p10)] <- 0.000
   BetaCF_p20[is.na(BetaCF_p20)] <- 0.000
   BetaCF_p30[is.na(BetaCF_p30)] <- 0.000
   BetaCF_p40[is.na(BetaCF_p40)] <- 0.000

   BetaDR_p10[is.na(BetaDR_p10)] <- 0.000
   BetaDR_p20[is.na(BetaDR_p20)] <- 0.000
   BetaDR_p30[is.na(BetaDR_p30)] <- 0.000
   BetaDR_p40[is.na(BetaDR_p40)] <- 0.000

   BetaCFAVGp <- (BetaCF_p10+BetaCF_p20+BetaCF_p30+BetaCF_p40)/4
   BetaDRAVGp <- (BetaDR_p10+BetaDR_p20+BetaDR_p30+BetaDR_p40)/4
###############################################################################


   BetaCF_GLS <- covCF_10/variancia_10
   BetaDR_GLS <- covDR_10/variancia_10

   BetaCF_E <- covCF_20/variancia_20
   BetaDR_E <- covDR_20/variancia_20

   BetaCF_OJ <- covCF_30/variancia_30
   BetaDR_OJ <- covDR_30/variancia_30

   BetaCF_CT <- covCF_40/variancia_40
   BetaDR_CT <- covDR_40/variancia_40

   BetaCFAVG <- (BetaCF_GLS+BetaCF_CT+BetaCF_E+BetaCF_OJ)/4
   BetaDRAVG <- (BetaDR_GLS+BetaDR_CT+BetaDR_E+BetaDR_OJ)/4

   tbaux_G <- rbind(BetaCF_GLS ,BetaDR_GLS, BetaCF_p10, BetaDR_p10)
   tbaux_E <- rbind(BetaCF_E ,BetaDR_E, BetaCF_p20, BetaDR_p20)
   tbaux_C <- rbind(BetaCF_CT ,BetaDR_CT, BetaCF_p30, BetaDR_p30)
   tbaux_O <- rbind(BetaCF_OJ ,BetaDR_OJ, BetaCF_p40, BetaDR_p40)
   tbauxAVG <- rbind(BetaCFAVG, BetaDRAVG, BetaCFAVGp, BetaDRAVGp)

   nameTab <- rbind(BetaNames,BetaNamesP)
   tbBetas <- cbind(nameTab,tbaux_G,tbaux_C,tbaux_O,tbaux_E, tbauxAVG)
return(tbBetas)
}

treatTab2 <- function(ano, dadosn, dtICC_D_6){

   dadosR_6_0 <- calculaRetorno(dadosn,ano)

   tb2 <- decomposicaoRetornoOutroArquivo(dtICC_D_6, dadosR_6_0)
   # tb6_1 <- midprocessing_table(tb6_1)

   tb2 <- midprocessing_table2(tb2)


   return(tb2)
}

midprocessing_table2 <- function(tb){
   tb[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
   tb[,Valor := round(Valor*100,2)]
   tb <- dcast(tb,VAR+VARF~ICC,value.var = "Valor")
   tb <- as.data.frame(tb)
   rownames(tb) <- tb$VARF
   return(tb)
}
