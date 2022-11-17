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

taux1 <- decomposicaoRetornoOutroArquivo(dtICC_D, dadosR_ano1)
taux1[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
taux1[,Valor := round(Valor*100,2)]
taux1 <- dcast(taux1,VAR+VARF~ICC,value.var = "Valor")

covCF_GLS <- ((taux1$`GLS(%)`[c(11)])-(taux1$`GLS(%)`[c(11)]+ taux$`GLS(%)`[c(11)])/2 )*((taux1$`GLS(%)`[c(4)])-(taux1$`GLS(%)`[c(4)]+ taux$`GLS(%)`[c(4)])/2 )+((taux$`GLS(%)`[c(11)])-(taux1$`GLS(%)`[c(11)]+ taux$`GLS(%)`[c(11)])/2 )*((taux$`GLS(%)`[c(4)])-(taux1$`GLS(%)`[c(4)]+ taux$`GLS(%)`[c(4)])/2 )
variancia_GLS <- (taux$`GLS(%)`[c(12)]-(taux1$`GLS(%)`[c(12)]+ taux$`GLS(%)`[c(12)])/2 )^2+(taux1$`GLS(%)`[c(12)]-(taux1$`GLS(%)`[c(12)]+ taux$`GLS(%)`[c(12)])/2 )^2

covDR_GLS <- ((taux1$`GLS(%)`[c(11)])-(taux1$`GLS(%)`[c(11)]+ taux$`GLS(%)`[c(11)])/2 )*((taux1$`GLS(%)`[c(6)])-(taux1$`GLS(%)`[c(6)]+ taux$`GLS(%)`[c(6)])/2 )+((taux$`GLS(%)`[c(11)])-(taux1$`GLS(%)`[c(11)]+ taux$`GLS(%)`[c(11)])/2 )*((taux$`GLS(%)`[c(6)])-(taux1$`GLS(%)`[c(6)]+ taux$`GLS(%)`[c(6)])/2 )

covCF_E <- ((taux1$`E(%)`[c(11)])-(taux1$`E(%)`[c(11)]+ taux$`E(%)`[c(11)])/2 )*((taux1$`E(%)`[c(4)])-(taux1$`E(%)`[c(4)]+ taux$`E(%)`[c(4)])/2 )+((taux$`E(%)`[c(11)])-(taux1$`E(%)`[c(11)]+ taux$`E(%)`[c(11)])/2 )*((taux$`E(%)`[c(4)])-(taux1$`E(%)`[c(4)]+ taux$`E(%)`[c(4)])/2 )
variancia_E <- (taux$`E(%)`[c(12)]-(taux1$`E(%)`[c(12)]+ taux$`E(%)`[c(12)])/2 )^2+(taux1$`E(%)`[c(12)]-(taux1$`E(%)`[c(12)]+ taux$`E(%)`[c(12)])/2 )^2

covDR_E <- ((taux1$`E(%)`[c(11)])-(taux1$`E(%)`[c(11)]+ taux$`E(%)`[c(11)])/2 )*((taux1$`E(%)`[c(6)])-(taux1$`E(%)`[c(6)]+ taux$`E(%)`[c(6)])/2 )+((taux$`E(%)`[c(11)])-(taux1$`E(%)`[c(11)]+ taux$`E(%)`[c(11)])/2 )*((taux$`E(%)`[c(6)])-(taux1$`E(%)`[c(6)]+ taux$`E(%)`[c(6)])/2 )

covCF_OJ <- ((taux1$`OJ(%)`[c(11)])-(taux1$`OJ(%)`[c(11)]+ taux$`OJ(%)`[c(11)])/2 )*((taux1$`OJ(%)`[c(4)])-(taux1$`OJ(%)`[c(4)]+ taux$`OJ(%)`[c(4)])/2 )+((taux$`OJ(%)`[c(11)])-(taux1$`OJ(%)`[c(11)]+ taux$`OJ(%)`[c(11)])/2 )*((taux$`OJ(%)`[c(4)])-(taux1$`OJ(%)`[c(4)]+ taux$`OJ(%)`[c(4)])/2 )
variancia_OJ <- (taux$`OJ(%)`[c(12)]-(taux1$`OJ(%)`[c(12)]+ taux$`OJ(%)`[c(12)])/2 )^2+(taux1$`OJ(%)`[c(12)]-(taux1$`OJ(%)`[c(12)]+ taux$`OJ(%)`[c(12)])/2 )^2

covDR_OJ <- ((taux1$`OJ(%)`[c(11)])-(taux1$`OJ(%)`[c(11)]+ taux$`OJ(%)`[c(11)])/2 )*((taux1$`OJ(%)`[c(6)])-(taux1$`OJ(%)`[c(6)]+ taux$`OJ(%)`[c(6)])/2 )+((taux$`OJ(%)`[c(11)])-(taux1$`OJ(%)`[c(11)]+ taux$`OJ(%)`[c(11)])/2 )*((taux$`OJ(%)`[c(6)])-(taux1$`OJ(%)`[c(6)]+ taux$`OJ(%)`[c(6)])/2 )

covCF_CT <- ((taux1$`CT(%)`[c(11)])-(taux1$`CT(%)`[c(11)]+ taux$`CT(%)`[c(11)])/2 )*((taux1$`CT(%)`[c(4)])-(taux1$`CT(%)`[c(4)]+ taux$`CT(%)`[c(4)])/2 )+((taux$`CT(%)`[c(11)])-(taux1$`CT(%)`[c(11)]+ taux$`CT(%)`[c(11)])/2 )*((taux$`CT(%)`[c(4)])-(taux1$`CT(%)`[c(4)]+ taux$`CT(%)`[c(4)])/2 )
variancia_CT <- (taux$`CT(%)`[c(12)]-(taux1$`CT(%)`[c(12)]+ taux$`CT(%)`[c(12)])/2 )^2+(taux1$`CT(%)`[c(12)]-(taux1$`CT(%)`[c(12)]+ taux$`CT(%)`[c(12)])/2 )^2

covDR_CT <- ((taux1$`CT(%)`[c(11)])-(taux1$`CT(%)`[c(11)]+ taux$`CT(%)`[c(11)])/2 )*((taux1$`CT(%)`[c(6)])-(taux1$`CT(%)`[c(6)]+ taux$`CT(%)`[c(6)])/2 )+((taux$`CT(%)`[c(11)])-(taux1$`CT(%)`[c(11)]+ taux$`CT(%)`[c(11)])/2 )*((taux$`CT(%)`[c(6)])-(taux1$`CT(%)`[c(6)]+ taux$`CT(%)`[c(6)])/2 )

BetaCF_GLS <- covCF_GLS/variancia_GLS
BetaDR_GLS <- covDR_GLS/variancia_GLS

BetaCF_E <- covCF_E/variancia_E
BetaDR_E <- covDR_E/variancia_E

BetaCF_OJ <- covCF_OJ/variancia_OJ
BetaDR_OJ <- covDR_OJ/variancia_OJ

BetaCF_CT <- covCF_CT/variancia_CT
BetaDR_CT <- covDR_CT/variancia_CT

BetaCFAVG <- (BetaCF_GLS+BetaCF_CT+BetaCF_E+BetaCF_OJ)/4
BetaDRAVG <- (BetaDR_GLS+BetaDR_CT+BetaDR_E+BetaDR_OJ)/4

tbaux_G <- rbind(BetaCF_GLS ,BetaDr_GLS)
tbaux_E <- rbind(BetaCF_E ,BetaDr_E)
tbaux_C <- rbind(BetaCF_CT ,BetaDr_CT)
tbaux_O <- rbind(BetaCF_OJ ,BetaDr_OJ)
tbauxAVG <- rbind(BetaCFAVG, BetaDRAVG)

tbBetas <- cbind(tbaux_G,tbaux_C,tbaux_O,tbaux_E, tbauxAVG)
