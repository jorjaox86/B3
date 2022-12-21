tab7Build <- function(dadosn, anos){

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

      assign(paste0("tb7_",i),treatTab7(ano_selecionado, dadosn, dtICC_D))

   }

   print("saiu função")
   # dtICCg_1 <- dtICC_1_mod7[,lapply(.SD,mean,na.rm=T),by=.(Group50),.SDcols=grep('ICC.*',colnames(dtICC_0_mod7),value = T)]

   # dtICC_D_7 <- deltaICCcut(dtICC_0,dtICC_1)
   #
   # ano <- ano_0
   # dadosR_7_0 <- calculaRetorno(dadosn,ano)
   #
   # tb7_1 <- decomposicaoRetornoValueWeight(dtICC_D_7,dadosR_7_0)[ICC == "ICC_GLS"]
   #
   # ano <- ano_0+1
   # dadosR_7_1 <- calculaRetorno(dadosn,ano)
   #
   # tb7_2 <- decomposicaoRetornoValueWeight(dtICC_D_7,dadosR_7_1)[ICC == "ICC_GLS"]
   #
   # tb7_1[,Valor := round(Valor*100,2)]
   # tb7_1 <- dcast(tb7_1,VAR+VARF~ICC,value.var = "Valor")
   # tb7_1 <- as.data.frame(tb7_1)
   # rownames(tb7_1) <- tb7_1$VARF
   # #kable(tb7_1,align=c(rep('c', 5)))
   #
   # tb7_2[,Valor := round(Valor*100,2)]
   # tb7_2 <- dcast(tb7_2,VAR+VARF~ICC,value.var = "Valor")
   # tb7_2 <- as.data.frame(tb7_2)
   # rownames(tb7_2) <- tb7_2$VARF

   tb7 <- BetaTab7Calc(tb7_1, tb7_2, tb7_3, tb7_4, tb7_5, BetaNames)

   # return(list(tb7_1, tb7_2, tb7_3, tb7_4, tb7_5))
   return(tb7)
}

BetaTab7Calc <- function(taux, taux1, taux2, taux3, taux4, BetaNames){




   URM_1 <- c(taux$`(base case)`[c(12)],taux1$`(base case)`[c(12)],taux2$`(base case)`[c(12)],taux3$`(base case)`[c(12)],taux4$`(base case)`[c(12)])
   media1 <- mean(URM_1)

   URM_2 <- c(taux$`value-weighted`[c(12)],taux1$`value-weighted`[c(12)],taux2$`value-weighted`[c(12)],taux3$`value-weighted`[c(12)],taux4$`value-weighted`[c(12)])
   media2 <- mean(URM_2)

   # URM_3 <- c(taux$`L=[0-4] H=[6-10]`[c(12)],taux1$`L=[0-4] H=[6-10]`[c(12)],taux2$`L=[0-4] H=[6-10]`[c(12)],taux3$`L=[0-4] H=[6-10]`[c(12)],taux4$`L=[0-4] H=[6-10]`[c(12)])
   # media3 <- mean(URM_3)

   u <- as.numeric(length(URM_1))

   URM <-list( URM_1,URM_2)#,URM_3)
   media <- list(media1,media2)#,media3)

   var_p <- var(URM[[1]],y=NULL)
   # var_p <- var(URM_1)
   # print(sprintf("loop var"))

   for (i in 1:2){

      varia <- 0
      for(n in 1:5) {
         varia <- varia+(URM[[i]][n]-media[[i]])^2
      }
      varia <- varia/(u-1)
      assign(paste0("variancia_",i*10),varia)
   }

   ######variancia_11 <- var(x, y=NULL, use = "everything")

   ####expressão da covariancia###############################################

   URHL_1 <- c(taux$`(base case)`[c(11)],taux1$`(base case)`[c(11)],taux2$`(base case)`[c(11)],taux3$`(base case)`[c(11)],taux4$`(base case)`[c(11)])
   URHL_2 <- c(taux$`value-weighted`[c(11)],taux1$`value-weighted`[c(11)],taux2$`value-weighted`[c(11)],taux3$`value-weighted`[c(11)],taux4$`value-weighted`[c(11)])
   # URHL_3 <- c(taux$`L=[0-4] H=[6-10]`[c(11)],taux1$`L=[0-4] H=[6-10]`[c(11)],taux2$`L=[0-4] H=[6-10]`[c(11)],taux3$`L=[0-4] H=[6-10]`[c(11)],taux4$`L=[0-4] H=[6-10]`[c(11)])
   URHL <- list(URHL_1,URHL_2)#,URHL_3)

   mediaURHL1 <- mean(URHL_1)
   mediaURHL2 <- mean(URHL_2)
   # mediaURHL3 <- mean(URHL_3)
   mediaURHL <- list(mediaURHL1,mediaURHL2)#,mediaURHL3)

   NDRM_1 <- c(taux$`(base case)`[c(2)],taux1$`(base case)`[c(2)],taux2$`(base case)`[c(2)],taux3$`(base case)`[c(2)],taux4$`(base case)`[c(2)])
   NDRM_2 <- c(taux$`value-weighted`[c(2)],taux1$`value-weighted`[c(2)],taux2$`value-weighted`[c(2)],taux3$`value-weighted`[c(2)],taux4$`value-weighted`[c(2)])
   # NDRM_3 <- c(taux$`L=[0-4] H=[6-10]`[c(2)],taux1$`L=[0-4] H=[6-10]`[c(2)],taux2$`L=[0-4] H=[6-10]`[c(2)],taux3$`L=[0-4] H=[6-10]`[c(2)],taux4$`L=[0-4] H=[6-10]`[c(2)])
   NDRM <- list(NDRM_1,NDRM_2)#,NDRM_3)

   mediaNDRM1 <- mean(NDRM_1)
   mediaNDRM2 <- mean(NDRM_2)
   # mediaNDRM3 <- mean(NDRM_3)
   mediaNDRM <- list(mediaNDRM1,mediaNDRM2)#,mediaNDRM3)

   NCFM_1 <- c(taux$`(base case)`[c(6)],taux1$`(base case)`[c(6)],taux2$`(base case)`[c(6)],taux3$`(base case)`[c(6)],taux4$`(base case)`[c(6)])
   NCFM_2 <- c(taux$`value-weighted`[c(6)],taux1$`value-weighted`[c(6)],taux2$`value-weighted`[c(6)],taux3$`value-weighted`[c(6)],taux4$`value-weighted`[c(6)])
   # NCFM_3 <- c(taux$`L=[0-4] H=[6-10]`[c(6)],taux1$`L=[0-4] H=[6-10]`[c(6)],taux2$`L=[0-4] H=[6-10]`[c(6)],taux3$`L=[0-4] H=[6-10]`[c(6)],taux4$`L=[0-4] H=[6-10]`[c(6)])
   NCFM <- list(NCFM_1,NCFM_2)#,NCFM_3)

   mediaNCFM1 <- mean(NCFM_1)
   mediaNCFM2 <- mean(NCFM_2)
   # mediaNCFM3 <- mean(NCFM_3)
   mediaNCFM <- list(mediaNCFM1,mediaNCFM2)#,mediaNCFM3)

   ##repetindo so por desencargo de consciência
   u <- as.numeric(length(URHL_1))

   # print(sprintf("loop covar2"))
   ####iteração para calculo da covariancia_10, covariancia_20 e covariancia_30##################################
   for (i in 1:2){

      covaria <- 0
      for(n in 1:5) {
         covaria <- covaria+(URHL[[i]][n]-mediaURHL[[i]])*(NCFM[[i]][n]-mediaNCFM[[i]])
      }
      covaria <- covaria/(u-1)
      assign(paste0("covCF_",i*10),covaria)
   }

   # print(sprintf("loop covar3"))

   for (i in 1:2){

      covaria <- 0
      for(n in 1:5) {
         covaria <- covaria+(URHL[[i]][n]-mediaURHL[[i]])*(NDRM[[i]][n]-mediaNDRM[[i]])
      }
      covaria <- covaria/(u-1)
      assign(paste0("covDR_",i*10),covaria)
   }

   covDR_p <- cov(URHL_1[URHL_1>0],NDRM_1[URHL_1>0])
   covCF_p <- cov(URHL_1[URHL_1>0],NCFM_1[URHL_1>0])

   # covCF_10 <- ((taux1$`(base case)`[c(11)])-(taux1$`(base case)`[c(11)]+ taux$`(base case)`[c(11)])/2 )*((taux1$`(base case)`[c(6)])-(taux1$`(base case)`[c(6)]+ taux$`(base case)`[c(6)])/2 )+((taux$`(base case)`[c(11)])-(taux1$`(base case)`[c(11)]+ taux$`(base case)`[c(11)])/2 )*((taux$`(base case)`[c(6)])-(taux1$`(base case)`[c(6)]+ taux$`(base case)`[c(6)])/2 )
   # variancia_10 <- (taux$`(base case)`[c(12)]-(taux1$`(base case)`[c(12)]+ taux$`(base case)`[c(12)])/2 )^2+(taux1$`(base case)`[c(12)]-(taux1$`(base case)`[c(12)]+ taux$`(base case)`[c(12)])/2 )^2
   #
   # covDR_10 <- ((taux1$`(base case)`[c(11)])-(taux1$`(base case)`[c(11)]+ taux$`(base case)`[c(11)])/2 )*((taux1$`(base case)`[c(2)])-(taux1$`(base case)`[c(2)]+ taux$`(base case)`[c(2)])/2 )+((taux$`(base case)`[c(11)])-(taux1$`(base case)`[c(11)]+ taux$`(base case)`[c(11)])/2 )*((taux$`(base case)`[c(2)])-(taux1$`(base case)`[c(2)]+ taux$`(base case)`[c(2)])/2 )
   #
   # covCF_20 <- ((taux1$`value-weighted`[c(11)])-(taux1$`value-weighted`[c(11)]+ taux$`value-weighted`[c(11)])/2 )*((taux1$`value-weighted`[c(6)])-(taux1$`value-weighted`[c(6)]+ taux$`value-weighted`[c(6)])/2 )+((taux$`value-weighted`[c(11)])-(taux1$`value-weighted`[c(11)]+ taux$`value-weighted`[c(11)])/2 )*((taux$`value-weighted`[c(6)])-(taux1$`value-weighted`[c(6)]+ taux$`value-weighted`[c(6)])/2 )
   # variancia_20 <- (taux$`value-weighted`[c(12)]-(taux1$`value-weighted`[c(12)]+ taux$`value-weighted`[c(12)])/2 )^2+(taux1$`value-weighted`[c(12)]-(taux1$`value-weighted`[c(12)]+ taux$`value-weighted`[c(12)])/2 )^2
   #
   # covDR_20 <- ((taux1$`value-weighted`[c(11)])-(taux1$`value-weighted`[c(11)]+ taux$`value-weighted`[c(11)])/2 )*((taux1$`value-weighted`[c(2)])-(taux1$`value-weighted`[c(2)]+ taux$`value-weighted`[c(2)])/2 )+((taux$`value-weighted`[c(11)])-(taux1$`value-weighted`[c(11)]+ taux$`value-weighted`[c(11)])/2 )*((taux$`value-weighted`[c(2)])-(taux1$`value-weighted`[c(2)]+ taux$`value-weighted`[c(2)])/2 )
   #
   # covCF_30 <- ((taux1$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux1$`L=[0-4] H=[6-10]`[c(6)])-(taux1$`L=[0-4] H=[6-10]`[c(6)]+ taux$`L=[0-4] H=[6-10]`[c(6)])/2 )+((taux$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux$`L=[0-4] H=[6-10]`[c(6)])-(taux1$`L=[0-4] H=[6-10]`[c(6)]+ taux$`L=[0-4] H=[6-10]`[c(6)])/2 )
   # variancia_30 <- (taux$`L=[0-4] H=[6-10]`[c(12)]-(taux1$`L=[0-4] H=[6-10]`[c(12)]+ taux$`L=[0-4] H=[6-10]`[c(12)])/2 )^2+(taux1$`L=[0-4] H=[6-10]`[c(12)]-(taux1$`L=[0-4] H=[6-10]`[c(12)]+ taux$`L=[0-4] H=[6-10]`[c(12)])/2 )^2
   #
   # covDR_30 <- ((taux1$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux1$`L=[0-4] H=[6-10]`[c(2)])-(taux1$`L=[0-4] H=[6-10]`[c(2)]+ taux$`L=[0-4] H=[6-10]`[c(2)])/2 )+((taux$`L=[0-4] H=[6-10]`[c(11)])-(taux1$`L=[0-4] H=[6-10]`[c(11)]+ taux$`L=[0-4] H=[6-10]`[c(11)])/2 )*((taux$`L=[0-4] H=[6-10]`[c(2)])-(taux1$`L=[0-4] H=[6-10]`[c(2)]+ taux$`L=[0-4] H=[6-10]`[c(2)])/2 )

   ###Cálculo dos Betas##############################################

   BetaCF_10 <- covCF_10/variancia_10
   BetaDR_10 <- covDR_10/variancia_10

   BetaCF_20 <- covCF_20/variancia_20
   BetaDR_20 <- covDR_20/variancia_20

   # BetaCF_30 <- covCF_30/variancia_30
   # BetaDR_30 <- covDR_30/variancia_30

   BetaCF_p <- covCF_p/var_p
   BetaDR_p <- covDR_p/var_p


   tbaux_10 <- rbind(BetaCF_10 ,BetaDR_10, BetaCF_p, BetaDR_p)
   tbaux_20 <- rbind(BetaCF_20 ,BetaDR_20, BetaCF_p, BetaDR_p)
   # tbaux_30 <- rbind(BetaCF_30 ,BetaDR_30)

   nameTab <- rbind(BetaNames,BetaNamesP)
   # print(sprintf("beta calculado"))
   # covCF_10 <- ((taux1$'(base case)' [c(11)]))
   # print(BetaDR_10)
   aux <- cbind(nameTab,tbaux_10,tbaux_20)#, tbaux_30)
   colnames(aux)<-c("VARF",	"(base case)",	"value-weighted")#,	"L=[0-4] H=[6-10]")
   return(rbind(taux,aux))
   # return(aux)

   # nameTab <- data.frame(x1 = c("BetaCf","BetaDR"))
   # aux <- cbind(nameTab,nameTab, tbaux_10)
   # colnames(aux)<-c("VARF","VAR",	"Value-weighted portfolios")
   #
   # colnames(taux)<-c("VARF","VAR",	"Value-weighted portfolios")
   #
   #
   # #aux2 <- rbind(taux,aux)
   # return(rbind(taux,aux))
}

treatTab7 <- function(ano, dadosn, dtICC_D_6){

   dadosR_7_0 <- calculaRetorno(dadosn,ano)

   tb7_0 <- decomposicaoRetornoOutroArquivo(dtICC_D_6,dadosR_7_0)[ICC == "ICC_GLS"]
   tb7_1 <- decomposicaoRetornoValueWeight(dtICC_D_6,dadosR_7_0)[ICC == "ICC_GLS"]
   tb7_0[,CASE := "(base case)"]
   tb7_1[,CASE := "value-weighted"]

   tb7<-rbind(tb7_0,tb7_1)

   tb7 <- midprocessing_table7(tb7)


   return(tb7)
}

midprocessing_table7 <- function(tb7_1){
   tb7_1[,Valor := round(Valor*100,2)]
   # tb7_1 <- dcast(tb7_1,VAR+VARF~ICC,value.var = "Valor")
   tb7_1 <- dcast(tb7_1,VARF~CASE,value.var = "Valor")
   tb7_1 <- as.data.frame(tb7_1)
   rownames(tb7_1) <- tb7_1$VARF
   return(tb7_1)
}
