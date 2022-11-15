decomposicaoRetornoOutroArquivo <- function(dtICC_D,dadosR,interv=list(H=c(.5,1),L=c(0,.5)),rho = .96^(1/12)){

   ER_H <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$H[1]) & ESG_Score <= quantile(ESG_Score,interv$H[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_L <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$L[1]) & ESG_Score <= quantile(ESG_Score,interv$L[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_M <- dtICC_D[ESG_Score == ESG_Score,.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_HL <- copy(ER_H)
   ER_HL[,Valor := -1*Valor]
   ER_HL <- rbind(ER_HL,ER_L)
   ER_HL <- ER_HL[,.(Valor = sum(Valor)),.(ICC)]
   ER_HL[,VAR := "ER_HL"]
   ER_HL[,VARF := "$ER^{HL}$"]
   ER_M[,VAR := "ER_M"]
   ER_M[,VARF := "$ER^{M}$"]

   dtR <- dtICC_D[dadosR,on="EmpCode",nomatch=0]
   R_H <- dtR[ESG_Score >= quantile(ESG_Score,interv$H[1]) & ESG_Score <= quantile(ESG_Score,interv$H[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_L <- dtR[ESG_Score >= quantile(ESG_Score,interv$L[1]) & ESG_Score <= quantile(ESG_Score,interv$L[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_M <- dtR[ESG_Score == ESG_Score,.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_HL <- copy(R_L)
   R_HL[,Valor := -1*Valor]
   R_HL <- rbind(R_HL,R_H)
   R_HL <- R_HL[,.(Valor = sum(Valor)),.(ICC)]
   R_HL[,VAR := "R_HL"]
   R_H[,VAR := "R_H"]
   R_L[,VAR := "R_L"]
   R_M[,VAR := "R_M"]
   R_HL[,VARF := "$R^{HL}$"]
   R_H[,VARF := "$R^{H}$"]
   R_L[,VARF := "$R^{L}$"]
   R_M[,VARF := "$R^{M}$"]

   NDR_H <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$H[1]) & ESG_Score <= quantile(ESG_Score,interv$H[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_L <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$L[1]) & ESG_Score <= quantile(ESG_Score,interv$L[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_M <- dtICC_D[ESG_Score == ESG_Score,.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_HL <- copy(NDR_L)
   NDR_HL[,Valor := -1*Valor]
   NDR_HL <- rbind(NDR_HL,NDR_H)
   NDR_HL <- NDR_HL[,.(Valor = -sum(Valor)),.(ICC)]
   NDR_HL[,VAR := "NDR_HL"]
   NDR_HL[,VARF := "$-NDR^{HL}$"]
   NDR_M[,VAR := "NDR_M"]
   NDR_M[,VARF := "$-NDR^{M}$"]

   UR_HL <- rbind(R_HL,ER_HL)
   UR_M <- rbind(R_M,ER_M)
   UR_HL[VAR == "ER_HL",Valor := -1*Valor]
   UR_HL[,VAR := "UR_HL"]
   UR_HL <- UR_HL[,.(Valor = sum(Valor)),.(ICC,VAR)]
   UR_HL[,VARF := "$UR^{HL}$"]
   UR_M[VAR == "ER_M",Valor := -1*Valor]
   UR_M[,VAR := "UR_M"]
   UR_M <- UR_M[,.(Valor = sum(Valor)),.(ICC,VAR)]
   UR_M[,VARF := "$UR^{M}$"]

   NCF_HL <- rbind(UR_HL,NDR_HL)
   NCF_M <- rbind(UR_M,NDR_M)
   NCF_HL[VAR == "NDR_HL",Valor := -1*Valor]
   NCF_HL[,VAR := "NCF_HL"]
   NCF_HL <- NCF_HL[,.(Valor = sum(Valor)),.(ICC,VAR)]
   NCF_HL[,VARF := "$NCF^{HL}$"]
   NCF_M[VAR == "NDR_M",Valor := -1*Valor]
   NCF_M[,VAR := "NCF_M"]
   NCF_M <- NCF_M[,.(Valor = sum(Valor)),.(ICC,VAR)]
   NCF_M[,VARF := "$NCF^{M}$"]

   return(rbind(R_H,R_L,R_M,R_HL,ER_HL,ER_M,UR_HL,UR_M,NCF_HL,NCF_M,NDR_HL,NDR_M))
}
