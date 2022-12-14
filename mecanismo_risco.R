## Tabela 2 invenção própria
## Expected Return

tb2 <- decomposicaoRetorno(dtICC_D, dadosR)
tb2[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
tb2[,Valor := round(Valor*100,2)]
tb2 <- dcast(tb2,VAR+VARF~ICC,value.var = "Valor")
tb2 <- as.data.frame(tb2)
rownames(tb2) <- tb2$VARF
#tb2[,"CT(%)" := 1]
kable(tb2[c(4,6,5,1,7,2,3),-c(1,2)],align=c(rep('c', 4)))
aleluia <- 50
aleluia /10
vetor <- c(10 ,20, 30, 40 ,50, 60, 70, 80, 90, 100)
str(vetor)
tb2/aleluia
vetor[1]
vetor[1:5]
vetor[c(1,2,10)]
1:3
mean(vetor)
var(vetor)
sum(vetor)
vec3 <- as.vector(tb2["CT(%)"])
vator2 <- tb2["CT(%)"]
vator2[1]
sapply(tb2, class)
vec4 <- tb2$`CT(%)`*4
mean(vec4)
vec5 <-mean(tb2$`CT(%)`)
mean(vec4)/vec5
vator2[,CASADO := 10]
interv <- list(H=c(.5,1),L=c(0,.5))
ER_H <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$H[1]) & ESG_Score <= quantile(ESG_Score,interv$H[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
ER_L <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$L[1]) & ESG_Score <= quantile(ESG_Score,interv$L[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
ER_HL <- copy(ER_H)
ER_HL[,Valor := -1*Valor]
ER_HL <- rbind(ER_HL,ER_L)
ER_HL <- ER_HL[,.(Valor = sum(Valor)),.(ICC)]
ER_M <- dtICC_D[ESG_Score == ESG_Score,.(Valor=mean(Valor,na.rm=T)),.(ICC)]

dtR <- dtICC_D[dadosR,on="EmpCode",nomatch=0]
R_H <- dtR[ESG_Score >= quantile(ESG_Score,interv$H[1]) & ESG_Score <= quantile(ESG_Score,interv$H[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
R_L <- dtR[ESG_Score >= quantile(ESG_Score,interv$L[1]) & ESG_Score <= quantile(ESG_Score,interv$L[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
R_M <- dtR[ESG_Score == ESG_Score,.(Valor = mean(R,na.rm=T)),.(ICC)]
R_H_L <- rbind(R_H,R_L)
R_H_L <- R_H_L[,.(Valor = mean(Valor)),.(ICC)]

tb5 <- decomposicaoRetornoTeste(dtICC_D, dadosR)
tb5[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
tb5[,Valor := round(Valor*100,2)]
tb5 <- dcast(tb5,VAR+VARF~ICC,value.var = "Valor")







interv <- list(H=c(.5,1),L=c(0,.5))
rho = .96^(1/12)

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

tb4 <- rbind(R_H,R_L,R_M,R_HL,ER_HL,ER_M,UR_HL,UR_M,NCF_HL,NCF_M,NDR_HL,NDR_M)
tb4[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
tb4[,Valor := round(Valor*100,2)]
tb4 <- dcast(tb4,VAR+VARF~ICC,value.var = "Valor")

   ano<-ano_0
   dadosR_b <- dadosn[VAR %in% c("PRC","DPS") & Ano >= ano & Ano < ano + 5 & EmpCode %in% stocks]
   dadosR_b <- dcast(dadosR_b,EmpCode + Ano ~ VAR,value.var = "Valor")
   dadosR_b[, `:=` (P0 = shift(PRC),D0 = shift(DPS)),.(EmpCode)]
   dadosR_b <- dadosR_b[Ano != ano]
   dadosR_b[,R := ((PRC - D0)/P0)-1]
   dadosR_ano0 <- dadosR_b[,.(R = mean(R)),.(EmpCode)]

   ano <- ano_0+1
   dadosR_b <- dadosn[VAR %in% c("PRC","DPS") & Ano >= ano & Ano < ano + 5 & EmpCode %in% stocks]
   dadosR_b <- dcast(dadosR_b,EmpCode + Ano ~ VAR,value.var = "Valor")
   dadosR_b[, `:=` (P0 = shift(PRC),D0 = shift(DPS)),.(EmpCode)]
   dadosR_b <- dadosR_b[Ano != ano]
   dadosR_b[,R := ((PRC - D0)/P0)-1]
   dadosR_ano1 <- dadosR_b[,.(R = mean(R)),.(EmpCode)]

taux <- decomposicaoRetornoOutroArquivo(dtICC_D, dadosR_ano0)
taux[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
taux[,Valor := round(Valor*100,2)]
taux <- dcast(taux,VAR+VARF~ICC,value.var = "Valor")
tabela_URM <- dcast(taux, VAR~VAR, value.var = "GLS(%)")

taux1 <- decomposicaoRetornoOutroArquivo(dtICC_D, dadosR_ano1)
taux1[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
taux1[,Valor := round(Valor*100,2)]
taux1 <- dcast(taux1,VAR+VARF~ICC,value.var = "Valor")
tabela_URM <- dcast(taux, VAR~VAR, value.var = "GLS(%)")

taux1$`GLS(%)`[c(2)]
class(taux$`GLS(%)`[c(2)])

covCF_GLS <- ((taux1$`GLS(%)`[c(11)])-(taux1$`GLS(%)`[c(11)]+ taux$`GLS(%)`[c(11)])/2 )*((taux1$`GLS(%)`[c(4)])-(taux1$`GLS(%)`[c(4)]+ taux$`GLS(%)`[c(4)])/2 )+((taux$`GLS(%)`[c(11)])-(taux1$`GLS(%)`[c(11)]+ taux$`GLS(%)`[c(11)])/2 )*((taux$`GLS(%)`[c(4)])-(taux1$`GLS(%)`[c(4)]+ taux$`GLS(%)`[c(4)])/2 )
variancia <- (taux$`GLS(%)`[c(12)]-(taux1$`GLS(%)`[c(12)]+ taux$`GLS(%)`[c(12)])/2 )^2+(taux1$`GLS(%)`[c(12)]-(taux1$`GLS(%)`[c(12)]+ taux$`GLS(%)`[c(12)])/2 )^2

covDR_GLS <- ((taux1$`GLS(%)`[c(11)])-(taux1$`GLS(%)`[c(11)]+ taux$`GLS(%)`[c(11)])/2 )*((taux1$`GLS(%)`[c(6)])-(taux1$`GLS(%)`[c(6)]+ taux$`GLS(%)`[c(6)])/2 )+((taux$`GLS(%)`[c(11)])-(taux1$`GLS(%)`[c(11)]+ taux$`GLS(%)`[c(11)])/2 )*((taux$`GLS(%)`[c(6)])-(taux1$`GLS(%)`[c(6)]+ taux$`GLS(%)`[c(6)])/2 )

tbnext <- cbind(taux$`GLS(%)`,taux1$`GLS(%)`,make.row.names = TRUE)
class(tbnext)

tbnext[2, 2]
BetaCF <- covCF/variancia
BetaDR <- covDR/variancia

tableBeta <- rbind(BetaCF,BetaDR)

P10 <- decomposicaoRetornoOutroArquivo(dtICC_D,dadosR,interv = list(H=c(.9,1),L=c(0,.1)))[ICC == "ICC_GLS"]
P20 <- decomposicaoRetorno(dtICC_D,dadosR,interv = list(H=c(.8,1),L=c(0,.2)))[ICC == "ICC_GLS"]
P30 <- decomposicaoRetorno(dtICC_D,dadosR,interv = list(H=c(.7,1),L=c(0,.3)))[ICC == "ICC_GLS"]
P40 <- decomposicaoRetorno(dtICC_D,dadosR,interv = list(H=c(.6,1),L=c(0,.4)))[ICC == "ICC_GLS"]
P50 <- decomposicaoRetorno(dtICC_D,dadosR,interv = list(H=c(.5,1),L=c(0,.5)))[ICC == "ICC_GLS"]

P10[,CASE := "10%"]
P20[,CASE := "20%"]
P30[,CASE := "30%"]
P40[,CASE := "40%"]
P50[,CASE := "50% (base case)"]


tb2 <- rbind(P10,P20,P30,P40,P50)
tb2[,Valor := round(Valor*100,2)]
tb2 <- dcast(tb2,VARF~CASE,value.var = "Valor")
tb2 <- as.data.frame(tb2)
rownames(tb2) <- tb2$VARF
kable(tb2[c(5,6,4,2,7,3,1),6:2],align=c(rep('c', 5)))
print(tb2)
