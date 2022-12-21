decomposicaoRetornoOutroArquivo <- function(dtICC_D,dadosR,interv=list(H=c(.5,1),L=c(0,.5)),rho = .96^(1/12)){

   ER_H <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$H[1]) & ESG_Score <= quantile(ESG_Score,interv$H[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_L <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$L[1]) & ESG_Score <= quantile(ESG_Score,interv$L[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_M <- dtICC_D[ESG_Score == ESG_Score ,.(Valor=mean(Valor,na.rm=T)),.(ICC)]
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
   R_M <- dtR[ESG_Score == ESG_Score ,.(Valor = mean(R,na.rm=T)),.(ICC)]
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

decomposicaoRetornoE <- function(dtICC_D,dadosR,interv=list(H=c(.5,1),L=c(0,.5)),rho = .96^(1/12)){

   ER_H <- dtICC_D[Environmental_Score >= quantile(Environmental_Score,interv$H[1]) & Environmental_Score <= quantile(Environmental_Score,interv$H[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_L <- dtICC_D[Environmental_Score >= quantile(Environmental_Score,interv$L[1]) & Environmental_Score <= quantile(Environmental_Score,interv$L[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_M <- dtICC_D[Environmental_Score == Environmental_Score,.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_HL <- copy(ER_H)
   ER_HL[,Valor := -1*Valor]
   ER_HL <- rbind(ER_HL,ER_L)
   ER_HL <- ER_HL[,.(Valor = sum(Valor)),.(ICC)]
   ER_HL[,VAR := "ER_HL"]
   ER_HL[,VARF := "$ER^{HL}$"]
   ER_M[,VAR := "ER_M"]
   ER_M[,VARF := "$ER^{M}$"]

   dtR <- dtICC_D[dadosR,on="EmpCode",nomatch=0]
   R_H <- dtR[Environmental_Score >= quantile(Environmental_Score,interv$H[1]) & Environmental_Score <= quantile(Environmental_Score,interv$H[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_L <- dtR[Environmental_Score >= quantile(Environmental_Score,interv$L[1]) & Environmental_Score <= quantile(Environmental_Score,interv$L[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_M <- dtR[Environmental_Score == Environmental_Score,.(Valor = mean(R,na.rm=T)),.(ICC)]
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

   NDR_H <- dtICC_D[Environmental_Score >= quantile(Environmental_Score,interv$H[1]) & Environmental_Score <= quantile(Environmental_Score,interv$H[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_L <- dtICC_D[Environmental_Score >= quantile(Environmental_Score,interv$L[1]) & Environmental_Score <= quantile(Environmental_Score,interv$L[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_M <- dtICC_D[Environmental_Score == Environmental_Score,.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
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

decomposicaoRetornoS <- function(dtICC_D,dadosR,interv=list(H=c(.5,1),L=c(0,.5)),rho = .96^(1/12)){

   ER_H <- dtICC_D[Social_Score >= quantile(Social_Score,interv$H[1]) & Social_Score <= quantile(Social_Score,interv$H[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_L <- dtICC_D[Social_Score >= quantile(Social_Score,interv$L[1]) & Social_Score <= quantile(Social_Score,interv$L[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_M <- dtICC_D[Social_Score == Social_Score,.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_HL <- copy(ER_H)
   ER_HL[,Valor := -1*Valor]
   ER_HL <- rbind(ER_HL,ER_L)
   ER_HL <- ER_HL[,.(Valor = sum(Valor)),.(ICC)]
   ER_HL[,VAR := "ER_HL"]
   ER_HL[,VARF := "$ER^{HL}$"]
   ER_M[,VAR := "ER_M"]
   ER_M[,VARF := "$ER^{M}$"]

   dtR <- dtICC_D[dadosR,on="EmpCode",nomatch=0]
   R_H <- dtR[Social_Score >= quantile(Social_Score,interv$H[1]) & Social_Score <= quantile(Social_Score,interv$H[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_L <- dtR[Social_Score >= quantile(Social_Score,interv$L[1]) & Social_Score <= quantile(Social_Score,interv$L[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_M <- dtR[Social_Score == Social_Score,.(Valor = mean(R,na.rm=T)),.(ICC)]
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

   NDR_H <- dtICC_D[Social_Score >= quantile(Social_Score,interv$H[1]) & Social_Score <= quantile(Social_Score,interv$H[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_L <- dtICC_D[Social_Score >= quantile(Social_Score,interv$L[1]) & Social_Score <= quantile(Social_Score,interv$L[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_M <- dtICC_D[Social_Score == Social_Score,.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
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

decomposicaoRetornoG <- function(dtICC_D,dadosR,interv=list(H=c(.5,1),L=c(0,.5)),rho = .96^(1/12)){

   ER_H <- dtICC_D[Governance_Score >= quantile(Governance_Score,interv$H[1]) & Governance_Score <= quantile(Governance_Score,interv$H[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_L <- dtICC_D[Governance_Score >= quantile(Governance_Score,interv$L[1]) & Governance_Score <= quantile(Governance_Score,interv$L[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_M <- dtICC_D[Governance_Score == Governance_Score,.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_HL <- copy(ER_H)
   ER_HL[,Valor := -1*Valor]
   ER_HL <- rbind(ER_HL,ER_L)
   ER_HL <- ER_HL[,.(Valor = sum(Valor)),.(ICC)]
   ER_HL[,VAR := "ER_HL"]
   ER_HL[,VARF := "$ER^{HL}$"]
   ER_M[,VAR := "ER_M"]
   ER_M[,VARF := "$ER^{M}$"]

   dtR <- dtICC_D[dadosR,on="EmpCode",nomatch=0]
   R_H <- dtR[Governance_Score >= quantile(Governance_Score,interv$H[1]) & Governance_Score <= quantile(Governance_Score,interv$H[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_L <- dtR[Governance_Score >= quantile(Governance_Score,interv$L[1]) & Governance_Score <= quantile(Governance_Score,interv$L[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_M <- dtR[Governance_Score == Governance_Score,.(Valor = mean(R,na.rm=T)),.(ICC)]
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

   NDR_H <- dtICC_D[Governance_Score >= quantile(Governance_Score,interv$H[1]) & Governance_Score <= quantile(Governance_Score,interv$H[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_L <- dtICC_D[Governance_Score >= quantile(Governance_Score,interv$L[1]) & Governance_Score <= quantile(Governance_Score,interv$L[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_M <- dtICC_D[Governance_Score == Governance_Score,.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
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

decomposicaoRetornoESG <- function(dtICC_D,dadosR,interv=list(H=c(.5,1),L=c(0,.5)),rho = .96^(1/12)){

   ER_H <- dtICC_D[E_S_G >= quantile(E_S_G,interv$H[1]) & E_S_G <= quantile(E_S_G,interv$H[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_L <- dtICC_D[E_S_G >= quantile(E_S_G,interv$L[1]) & E_S_G <= quantile(E_S_G,interv$L[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_M <- dtICC_D[E_S_G == E_S_G,.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_HL <- copy(ER_H)
   ER_HL[,Valor := -1*Valor]
   ER_HL <- rbind(ER_HL,ER_L)
   ER_HL <- ER_HL[,.(Valor = sum(Valor)),.(ICC)]
   ER_HL[,VAR := "ER_HL"]
   ER_HL[,VARF := "$ER^{HL}$"]
   ER_M[,VAR := "ER_M"]
   ER_M[,VARF := "$ER^{M}$"]

   dtR <- dtICC_D[dadosR,on="EmpCode",nomatch=0]
   R_H <- dtR[E_S_G >= quantile(E_S_G,interv$H[1]) & E_S_G <= quantile(E_S_G,interv$H[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_L <- dtR[E_S_G >= quantile(E_S_G,interv$L[1]) & E_S_G <= quantile(E_S_G,interv$L[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_M <- dtR[E_S_G == E_S_G,.(Valor = mean(R,na.rm=T)),.(ICC)]
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

   NDR_H <- dtICC_D[E_S_G >= quantile(E_S_G,interv$H[1]) & E_S_G <= quantile(E_S_G,interv$H[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_L <- dtICC_D[E_S_G >= quantile(E_S_G,interv$L[1]) & E_S_G <= quantile(E_S_G,interv$L[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_M <- dtICC_D[E_S_G >= quantile(E_S_G,interv$H[1]) & E_S_G <= quantile(E_S_G,interv$H[2]) & E_S_G >= quantile(E_S_G,interv$L[1]) & E_S_G <= quantile(E_S_G,interv$L[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
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

decomposicaoRetornoDeltaESG <- function(dtICC_D,dadosR,interv=list(H=c(.5,1),L=c(0,.5)),rho = .96^(1/12)){

   ER_H <- dtICC_D[DeltaESG >= quantile(DeltaESG,interv$H[1]) & DeltaESG <= quantile(DeltaESG,interv$H[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_L <- dtICC_D[DeltaESG >= quantile(DeltaESG,interv$L[1]) & DeltaESG <= quantile(DeltaESG,interv$L[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_M <- dtICC_D[DeltaESG == DeltaESG,.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_HL <- copy(ER_H)
   ER_HL[,Valor := -1*Valor]
   ER_HL <- rbind(ER_HL,ER_L)
   ER_HL <- ER_HL[,.(Valor = sum(Valor)),.(ICC)]
   ER_HL[,VAR := "ER_HL"]
   ER_HL[,VARF := "$ER^{HL}$"]
   ER_M[,VAR := "ER_M"]
   ER_M[,VARF := "$ER^{M}$"]

   dtR <- dtICC_D[dadosR,on="EmpCode",nomatch=0]
   R_H <- dtR[DeltaESG >= quantile(DeltaESG,interv$H[1]) & DeltaESG <= quantile(DeltaESG,interv$H[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_L <- dtR[DeltaESG >= quantile(DeltaESG,interv$L[1]) & DeltaESG <= quantile(DeltaESG,interv$L[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_M <- dtR[DeltaESG == DeltaESG,.(Valor = mean(R,na.rm=T)),.(ICC)]
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

   NDR_H <- dtICC_D[DeltaESG >= quantile(DeltaESG,interv$H[1]) & DeltaESG <= quantile(DeltaESG,interv$H[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_L <- dtICC_D[DeltaESG >= quantile(DeltaESG,interv$L[1]) & DeltaESG <= quantile(DeltaESG,interv$L[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_M <- dtICC_D[DeltaESG == DeltaESG,.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
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

decomposicaoRetornoValueWeight <- function(dtICC_D,dadosR,interv=list(H=c(.5,1),L=c(0,.5)),rho = .96^(1/12)){

   ER_H <- dtICC_D[ESG_Score >= 50,.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_L <- dtICC_D[ESG_Score < 50,.(Valor=mean(Valor,na.rm=T)),.(ICC)]
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
   R_H <- dtR[ESG_Score >= 50,.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_L <- dtR[ESG_Score < 50,.(Valor = mean(R,na.rm=T)),.(ICC)]
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

   NDR_H <- dtICC_D[ESG_Score >= 50,.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_L <- dtICC_D[ESG_Score < 50,.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
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

Tab3DeltaIndex <- function(dtICC_D,esgTEMP, anos){
   ESGtime2014 <- esgTEMP[Ano == anos]
   ESGtime2015 <- esgTEMP[Ano == anos+1]
   Score2014_15 <- ESGtime2014[, ESG_Score ,.(Stock)]
   colnames(Score2014_15) <- c("Stock","ESG_2014")
   auxscore <- ESGtime2015[, ESG_Score ,.(Stock)]
   colnames(auxscore) <- c("Stock","ESG_2015")

   auxscore <- auxscore[!duplicated(auxscore$Stock)]
   Score2014_15 <- Score2014_15[!duplicated(Score2014_15$Stock)]
   datamerged <- merge(Score2014_15, auxscore, by="Stock")

   EMPList <- as.data.frame(dtICC_D$RIC)
   colnames(EMPList) <- c("Stock")
   datamerged2 <- merge(datamerged, EMPList, by = "Stock", all.y = TRUE)
   colnames(datamerged2) <- c("RIC","ESG_2014","ESG_2015")
   datamerged2$ESG_2014 <- -1*datamerged2$ESG_2014
   datamerged3 <- as.data.table(rowSums(datamerged2[, c("ESG_2015", "ESG_2014")]), value.name = "DeltaESG")
   datamerged2[,DeltaESG := datamerged3$V1]
   datamerged2[DeltaESG >= 0, DeltaHL := "Alto"]
   datamerged2[DeltaESG < 0, DeltaHL := "Baixo"]
   datamerged2[DeltaESG >= quantile(DeltaESG,.5, na.rm=TRUE), HLAbove := "Alto"]
   datamerged2[DeltaESG < quantile(DeltaESG,.5, na.rm=TRUE), HLBelow := "Baixo"]
   datamerged2 <- datamerged2[!duplicated(datamerged2$RIC)]
   dtICCCopy <- copy(dtICC_D)
   dtICCCopy <- dtICCCopy[!duplicated(dtICCCopy$RIC)]
   merge_aux <- merge(dtICCCopy, datamerged2, by = "RIC" ,all = TRUE)
   merge_aux <- merge_aux[!is.na(merge_aux$DeltaESG)]
   return(merge_aux)
}

GeraDeltaDAno <- function(ano_0){
   dadosF_0 <- trataPrevisao(dadosn,ref,ano = ano_0,vars = c("BPS","DPS","EPS","ROE"))
   dadosF_1 <- trataPrevisao(dadosn,ref,ano = ano_0+1,vars = c("BPS","DPS","EPS","ROE"))


   stocks <- unique(dadosF_1$EmpCode)
   dadosI_0 <- dadosn[VAR %in% c("PRC","BPS","EPS","DPS") & Ano == ano_0 & EmpCode %in% stocks]
   dadosI_0 <- dcast(dadosI_0,EmpCode ~ VAR,value.var = "Valor")
   dadosI_1 <- dadosn[VAR %in% c("PRC","BPS","EPS","DPS") & Ano == ano_0+1 & EmpCode %in% stocks]
   dadosI_1 <- dcast(dadosI_1,EmpCode ~ VAR,value.var = "Valor")

   dadosR <- calculaRetorno(dadosn,ano_0+1)

   dadosr <- data.table(read_xlsx("dados/datastream.xlsx",sheet="dadosROE"))
   ROE_historio_mediana <- ROEestimado(dadosr)

   dtGLS_ICC5_0 <- rbindlist(lapply(stocks,ICC_GLS,dadosF_0,dadosI_0,ROE_historio_mediana))
   dtGLS_ICC5_1 <- rbindlist(lapply(stocks,ICC_GLS,dadosF_1,dadosI_1,ROE_historio_mediana))

   dtICC_0 <- comparaICCs(dtGLS_ICC5_0,dtCT_ICC_0,dtOJ_ICC_0,dtE_ICC_0,dsBR,esgB3)
   dtICC_1 <- comparaICCs(dtGLS_ICC5_1,dtCT_ICC_1,dtOJ_ICC_1,dtE_ICC_1,dsBR,esgB3)

   dtICC_0 <- dtICC_0[EmpCode != "BR:RG3"]
   dtICC_1 <- dtICC_1[EmpCode != "BR:RG3"]

   dtICCg_1 <- dtICC_1[,lapply(.SD,mean,na.rm=T),by=.(Group50),.SDcols=grep('ICC.*',colnames(dtICC_0),value = T)]

   colid <- names(dtICC_1)[-grep('ICC.*',names(dtICC_1))]
   dtICC_D <- melt(dtICC_1,id.vars = colid,variable.name = "ICC",variable.factor = F,value.name = "Valor")
   dtICC_D_0 <- melt(dtICC_0,id.vars = "EmpCode",measure.vars = grep('ICC.*',names(dtICC_0),value = T),variable.name = "ICC",variable.factor = F,value.name = "V0")
   dtICC_D_0 <- dtICC_D_0[!is.na(V0)]
   dtICC_D <- dtICC_D[dtICC_D_0,on = c("EmpCode","ICC")]
   dtICC_D[,Delta := Valor-V0]

   return(list( dtICC_0, dtICC_1,dtICC_D))
}
