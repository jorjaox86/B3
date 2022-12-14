
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


   ano <- ano_0
   dadosR_5_0 <- calculaRetorno(dadosn,ano)

   tb5_clean <- decomposicaoRetornoOutroArquivo(dtICC_D_clean,dadosR_5_0)[ICC == "ICC_GLS"]
   tb5_L <- decomposicaoRetornoOutroArquivo(dtICC_D_37,dadosR_5_0)[ICC == "ICC_GLS"]
   tb5_H <- decomposicaoRetornoOutroArquivo(dtICC_D_46,dadosR_5_0)[ICC == "ICC_GLS"]

   ano <- ano_0+1
   dadosR_5_1 <- calculaRetorno(dadosn,ano)

   tb5_clean_1 <- decomposicaoRetornoOutroArquivo(dtICC_D_clean,dadosR_5_1)[ICC == "ICC_GLS"]
   tb5_L_1 <- decomposicaoRetornoOutroArquivo(dtICC_D_37,dadosR_5_1)[ICC == "ICC_GLS"]
   tb5_H_1 <- decomposicaoRetornoOutroArquivo(dtICC_D_46,dadosR_5_1)[ICC == "ICC_GLS"]

   tb5_clean[,CASE := "Base Case"]
   tb5_L[,CASE := "L=[0-3] H=[7-10]"]
   tb5_H[,CASE := "L=[0-4] H=[6-10]"]

   tb5_1 <- rbind(tb5_clean,tb5_H,tb5_L)

   tb5_clean_1[,CASE := "Base Case"]
   tb5_L_1[,CASE := "L=[0-3] H=[7-10]"]
   tb5_H_1[,CASE := "L=[0-4] H=[6-10]"]

   tb5_2 <- rbind(tb5_clean_1,tb5_H_1,tb5_L_1)

   tb5_1[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
   tb5_1[,Valor := round(Valor*100,2)]
   tb5_1 <- dcast(tb5_1,VARF~CASE,value.var = "Valor")
   tb5_1 <- as.data.frame(tb5_1)
   rownames(tb5_1) <- tb5_1$VARF

   tb5_2[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
   tb5_2[,Valor := round(Valor*100,2)]
   tb5_2 <- dcast(tb5_2,VARF~CASE,value.var = "Valor")
   tb5_2 <- as.data.frame(tb5_2)
   rownames(tb5_2) <- tb5_2$VARF
   #começando a chamar as funções necessarias para montar o beta da tabela 7###################
   tb5 <- BetaTab5Calc(tb5_1,tb5_2)




