### Tratamento de dados

trataPrevisao <- function(dadosn,ref,ano,vars = c("BPS","DPS","EPS","ROE")){
   ## Filtrando dados e desnormalizando os anos
   dt <- copy(dadosn)[Ano %in% ano:(ano+3)]
   dt[,Ano := paste0("Ano",Ano-ano)]
   dtY <- dcast(dt,EmpCode + VAR ~ Ano,value.var = "Valor")
   ## Gerando referencia com todas as variaveis previstas para todas as empresas
   dtref <- data.table(expand.grid(unique(dtY$EmpCode),ref$VAR))
   names(dtref) <- c("EmpCode","VAR")
   ## Mapeamento com referencia para identificar dados faltantes
   dtY <- dtY[dtref,on=c("EmpCode","VAR")]
   ## Ordena dados
   dtY <- dtY[order(EmpCode,VAR)]
   ## Filtro de variaveis previstas
   dtYF <- dtY[VAR %in% paste0(expand.grid(1:5,vars)[,2],expand.grid(1:5,vars)[,1])]
   ## Coluna com os primeiros 3 caracteres da variavel (cortando o numero de ano a frente)
   dtYF[,RefVAR := substr(VAR,1,3)]
   ## Filtra empresa que possuem dados
   dtYF[!is.na(Ano0),VC := .N,.(EmpCode,RefVAR)]
   ## Filtra empresas que possuam ao menos 3 anos de dados previstos em todas as variaveis
   dtYF[,VC := mean(VC,na.rm=T),.(EmpCode,RefVAR)]
   dtYF <- dtYF[,EVC := all(VC >= 3),.(EmpCode)]
   dtYF <- dtYF[EVC == T]
   ## Shifta as colunas prevista no numero de anos a frente
   dtYF[,LagAno1 := shift(Ano1,n = 1,type = "lag"),.(EmpCode,RefVAR)]
   dtYF[,LagAno2 := shift(Ano2,n = 2,type = "lag"),.(EmpCode,RefVAR)]
   dtYF[,LagAno3 := shift(Ano3,n = 3,type = "lag"),.(EmpCode,RefVAR)]
   dtYF[is.na(Ano0),Ano0 := ifelse(!is.na(LagAno1),LagAno1,ifelse(!is.na(LagAno2),LagAno2,ifelse(!is.na(LagAno3),LagAno3,NA_real_)))]
   dtYF <- dtYF[!EmpCode %in% unique(dtYF[is.na(Ano0)]$EmpCode)]
   dtYF[,Ano := as.numeric(substr(VAR,4,4))+ano]
   setnames(dtYF,"Ano0","Valor")

   dadosF <- dtYF[,.(EmpCode,RefVAR,VAR,Ano,Valor)]
   dadosF <- dcast(dadosF,EmpCode + Ano ~ RefVAR,value.var = "Valor")
   dadosF_chk <- dadosF[Ano %in% c(ano+1,ano+2),.(EmpCode,Ano,EPS)]
   dadosF_chk[,chk := all(EPS>0),.(EmpCode)]
   dadosF_chk <- dadosF_chk[chk==T]
   dadosF_chk[Ano == ano+1,EPS := 1/EPS]
   dadosF_chk <- dadosF_chk[,.(EPS = prod(EPS)),.(EmpCode)]
   dadosF_chk <- dadosF_chk[EPS > 1]
   dadosF <- dadosF[EmpCode %in% dadosF_chk$EmpCode]
   return(dadosF)
}

calculaRetorno <- function(dadosn,ano){
   dadosR_0 <- dadosn[VAR %in% c("PRC","DPS") & Ano >= ano & Ano < ano + 5 & EmpCode %in% stocks]
   dadosR_0 <- dcast(dadosR_0,EmpCode + Ano ~ VAR,value.var = "Valor")
   dadosR_0[, `:=` (P0 = shift(PRC),D0 = shift(DPS)),.(EmpCode)]
   dadosR_0 <- dadosR_0[Ano != ano]
   dadosR_0[,R := ((PRC - D0)/P0)-1]
   return(dadosR_0[,.(R = mean(R)),.(EmpCode)])
}

ROEestimado <- function(dadosr){
   suppressWarnings({
      dadosrn <- data.table::melt(dadosr,id.vars = c("Name","Code"),measure = 3:ncol(dadosr),
                                  value.name = "Valor",variable.name = "Ano",variable.factor = F)
   })
   dadosrn[,Empresa := unlist(lapply(strsplit(dadosrn$Name," - "),function(x)x[1]))]
   dadosrn[,VarCode := gsub('^.*\\(|\\)','',Code)]
   dadosrn[,EmpCode := gsub('\\(.*','',Code)]
   dadosrn[,Ano := as.numeric(Ano)]
   dadosrn[Valor == "NA",Valor := NA_real_]
   dadosrn[,Valor := as.numeric(Valor)]
   dadosrn[VarCode == "WC03995",VarCode := "SHE"]
   dadosrn[VarCode == "WC01651",VarCode := "NI"]
   dadosrn <- dcast(dadosrn, EmpCode + Ano ~ VarCode,value.var = "Valor")
   dadosrn <- dadosrn[complete.cases(dadosrn)]
   dadosrn[,ROE := NI/SHE]
   ROEtip <- median(dadosrn$ROE)
   return(ROEtip)
}

comparaICCs <- function(dtGLS_ICC,dtCT_ICC,dtOJ_ICC,dtE_ICC,dsBR,esgB3){
   dtICC <- copy(dtGLS_ICC)
   dtICC <- dtICC[dtCT_ICC,on="EmpCode",nomatch=0]
   dtICC <- dtICC[dtOJ_ICC,on="EmpCode",nomatch=0]
   dtICC <- dtICC[dtE_ICC,on="EmpCode",nomatch=0]
   dtICC <- dtICC[dsBR[,.(Symbol,RIC)],on=.(EmpCode=Symbol),nomatch=0]
   dtICC <- dtICC[esgB3,on = .(RIC = Stock),nomatch=0]
   dtICC[ESG_Score >= 50, value_weight := "Alto"]
   dtICC[ESG_Score <  50, value_weight := "Baixo"]
   dtICC[ESG_Score >= median(ESG_Score),Group50 := "Alto"]
   dtICC[ESG_Score < median(ESG_Score),Group50 := "Baixo"]
   dtICC[ESG_Score >= quantile(ESG_Score,.6),Group40 := "Alto"]
   dtICC[ESG_Score <= quantile(ESG_Score,.4),Group40 := "Baixo"]
   dtICC[ESG_Score >= quantile(ESG_Score,.7),Group30 := "Alto"]
   dtICC[ESG_Score <= quantile(ESG_Score,.3),Group30 := "Baixo"]
   dtICC[ESG_Score >= quantile(ESG_Score,.8),Group20 := "Alto"]
   dtICC[ESG_Score <= quantile(ESG_Score,.2),Group20 := "Baixo"]
   dtICC[ESG_Score >= quantile(ESG_Score,.9),Group10 := "Alto"]
   dtICC[ESG_Score <= quantile(ESG_Score,.1),Group10 := "Baixo"]
   dtICC[ESG_Score >= quantile(ESG_Score,.6) & ESG_Score <= quantile(ESG_Score,.9),Group40_10 := "Alto"]
   dtICC[ESG_Score <= quantile(ESG_Score,.4) & ESG_Score >= quantile(ESG_Score,.1),Group40_10 := "Baixo"]
   return(dtICC)
}


decomposicaoRetorno <- function(dtICC_D,dadosR,interv=list(H=c(.5,1),L=c(0,.5)),rho = .96^(1/12)){

   ER_H <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$H[1]) & ESG_Score <= quantile(ESG_Score,interv$H[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_L <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$L[1]) & ESG_Score <= quantile(ESG_Score,interv$L[2]),.(Valor=mean(Valor,na.rm=T)),.(ICC)]
   ER_HL <- copy(ER_H)
   ER_HL[,Valor := -1*Valor]
   ER_HL <- rbind(ER_HL,ER_L)
   ER_HL <- ER_HL[,.(Valor = sum(Valor)),.(ICC)]
   ER_HL[,VAR := "ER_HL"]
   ER_HL[,VARF := "$ER^{HL}$"]

   dtR <- dtICC_D[dadosR,on="EmpCode",nomatch=0]
   R_H <- dtR[ESG_Score >= quantile(ESG_Score,interv$H[1]) & ESG_Score <= quantile(ESG_Score,interv$H[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_L <- dtR[ESG_Score >= quantile(ESG_Score,interv$L[1]) & ESG_Score <= quantile(ESG_Score,interv$L[2]),.(Valor = mean(R,na.rm=T)),.(ICC)]
   R_HL <- copy(R_L)
   R_HL[,Valor := -1*Valor]
   R_HL <- rbind(R_HL,R_H)
   R_HL <- R_HL[,.(Valor = sum(Valor)),.(ICC)]
   R_HL[,VAR := "R_HL"]
   R_H[,VAR := "R_H"]
   R_L[,VAR := "R_L"]
   R_HL[,VARF := "$R^{HL}$"]
   R_H[,VARF := "$R^{H}$"]
   R_L[,VARF := "$R^{L}$"]

   NDR_H <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$H[1]) & ESG_Score <= quantile(ESG_Score,interv$H[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_L <- dtICC_D[ESG_Score >= quantile(ESG_Score,interv$L[1]) & ESG_Score <= quantile(ESG_Score,interv$L[2]),.(Valor=mean(Delta/rho,na.rm=T)),.(ICC)]
   NDR_HL <- copy(NDR_L)
   NDR_HL[,Valor := -1*Valor]
   NDR_HL <- rbind(NDR_HL,NDR_H)
   NDR_HL <- NDR_HL[,.(Valor = -sum(Valor)),.(ICC)]
   NDR_HL[,VAR := "NDR_HL"]
   NDR_HL[,VARF := "$-NDR^{HL}$"]

   UR_HL <- rbind(R_HL,ER_HL)
   UR_HL[VAR == "ER_HL",Valor := -1*Valor]
   UR_HL[,VAR := "UR_HL"]
   UR_HL <- UR_HL[,.(Valor = sum(Valor)),.(ICC,VAR)]
   UR_HL[,VARF := "$UR^{HL}$"]

   NCF_HL <- rbind(UR_HL,NDR_HL)
   NCF_HL[VAR == "NDR_HL",Valor := -1*Valor]
   NCF_HL[,VAR := "NCF_HL"]
   NCF_HL <- NCF_HL[,.(Valor = sum(Valor)),.(ICC,VAR)]
   NCF_HL[,VARF := "$NCF^{HL}$"]

   return(rbind(R_H,R_L,R_HL,ER_HL,UR_HL,NCF_HL,NDR_HL))
}



### ICC GLS

sumGLS <- function(k,est,bv,ICC){
   return(bv[k]*(est[k]-ICC)/(1+ICC)^k)
}

ICC_GLS_fun <- function(ICC,estROE,estBPS,P,B,root = T){
   sm <- sum(vapply(1:11,FUN.VALUE = 0.1,FUN = sumGLS,est = estROE,bv = estBPS,ICC = ICC))
   sm <- sm + estBPS[12]*(estROE[12]-ICC)/(ICC*(1+ICC)^12)
   sm <- sm + B
   if(root) sm <- sm - P
   return(sm)
}

ICC_GLS <- function(stock,dadosF,dadosI,ROEtip){
   estROE <- abs(c(dadosF[EmpCode == stock][order(Ano)]$ROE/100,rep(ROEtip,7)))
   E <- dadosI[EmpCode == stock]$EPS[1]
   D <- dadosI[EmpCode == stock]$DPS[1]
   K <- 1-(E-D)/E
   K <- ifelse(K < 1 | is.infinite(K),1,1-(E-D)/E)
   P <- dadosI[EmpCode == stock]$PRC[1]
   B <- dadosI[EmpCode == stock]$BPS[1]
   estEPS <- log(dadosF[EmpCode == stock][order(Ano)]$EPS)
   ltg <- median(estEPS[-1]/estEPS[-5])
   for(i in 6:12) estEPS[i] <- estEPS[i-1]*ltg
   estBPS <- c(B,dadosF[EmpCode == stock][order(Ano)]$BPS)
   for(i in 7:12) estBPS[i] <- estBPS[i-1]+(estEPS[i]-estEPS[i]*K)
   suppressWarnings({
   rt <- tryCatch({
      uniroot(ICC_GLS_fun,c(0.0001,10000),estROE = estROE,estBPS = estBPS,P=P,B=B)$root
   }, error = function(e){

      return(NA_real_)
   })
   })
   return(data.table(EmpCode = stock, ICC_GLS = rt))
}

### ICC CT
sumCT <- function(i,e,b,ICC){
   return((e[i]-ICC*b[i])/(1+ICC)^i)
}
ICC_CT_fun <- function(ICC,estEarn,estBV,P,B,gae,ny = 5,root = T){
   sm <- sum(vapply(1:ny,FUN.VALUE = 0.1,FUN = sumCT,e = estEarn,b = estBV,ICC = ICC))
   tm <- (estEarn[ny]*(1+gae))/((ICC-gae)*((1+ICC)^ny))
   sm <- sm+B + tm
   if(root) sm <- sm - P
   return(sm)
}

ICC_CT <- function(stock,dadosF,dadosI,gae = 0.05,ny = 3){
   estEPS <- dadosF[EmpCode == stock][order(Ano)]$EPS
   P <- dadosI[EmpCode == stock]$PRC[1]
   B <- dadosI[EmpCode == stock]$BPS[1]
   estBPS <- c(B,dadosF[EmpCode == stock][order(Ano)]$BPS)
   suppressWarnings({
      rt <- tryCatch({
         uniroot(ICC_CT_fun,c(gae+0.001,10000),estEarn = estEPS,estBV = estBPS,P=P,B=B,gae=gae,ny=ny)$root
      }, error = function(e){
         return(NA_real_)
      })
   })
   return(data.table(EmpCode = stock, ICC_CT = rt))
}

### ICC OJ

ICC_OJ_fun <- function(estEarn,estDiv,P,step=1,gma = 1.02){
   g2 <- (estEarn[2]-estEarn[1])/estEarn[1]
   A <- .5*((gma-1)+estDiv[step]/P)
   RE <- A + sqrt(A^2 + (g2-(gma-1))*estEarn[step]/P)
   return(RE)
}

ICC_OJ <- function(stock,dadosF,dadosI,gma = 1.02){

   estEPS <- dadosF[EmpCode == stock][order(Ano)]$EPS
   estDPS <- dadosF[EmpCode == stock][order(Ano)]$DPS
   P <- dadosI[EmpCode == stock]$PRC[1]
   rt <- ICC_OJ_fun(estEPS,estDPS,P,gma = gma)
   return(data.table(EmpCode = stock, ICC_OJ = rt))
}

### ICC E

ICC_E_fun <- function(estEarn,estDiv,P){
   b = -1*log(estDiv[1])/P
   c = -1*(log(estEarn[2]) - log(estEarn[1]))/P
   r1 = (-1*b+sqrt(b^2-4*c))/2
   return(r1)
}

ICC_E <- function(stock,dadosF,dadosI){
   estEPS <- dadosF[EmpCode == stock][order(Ano)]$EPS
   estDPS <- dadosF[EmpCode == stock][order(Ano)]$DPS
   P <- dadosI[EmpCode == stock]$PRC[1]
   rt <- tryCatch({
      if(estEPS[2]<estEPS[1]) warning("Easton nao aplicavel: Ganhos decrescentes")
         ICC_E_fun(estEPS,estDPS,P)
      }, error = function(e){
         return(NA_real_)
      }, warning = function(w){
         return(NA_real_)
      } )

   return(data.table(EmpCode = stock, ICC_E = rt))
}


plotGLS <- function(stock,dadosF,dadosI,ROEtip,x = seq(0.001,1,0.001),svgplt = T){
   estROE <- abs(c(dadosF[EmpCode == stock][order(Ano)]$ROE/100,rep(ROEtip,7)))
   E <- dadosI[EmpCode == stock]$EPS[1]
   D <- dadosI[EmpCode == stock]$DPS[1]
   K <- 1-(E-D)/E
   K <- ifelse(K < 1 | is.infinite(K),1,1-(E-D)/E)
   P <- dadosI[EmpCode == stock]$PRC[1]
   B <- dadosI[EmpCode == stock]$BPS[1]
   estEPS <- log(dadosF[EmpCode == stock][order(Ano)]$EPS)
   ltg <- median(estEPS[-1]/estEPS[-5])
   for(i in 6:12) estEPS[i] <- estEPS[i-1]*ltg
   estBPS <- c(B,dadosF[EmpCode == stock][order(Ano)]$BPS)
   for(i in 7:12) estBPS[i] <- estBPS[i-1]+(estEPS[i]-estEPS[i]*K)
   try({
      y <- vapply(x,ICC_GLS_fun,.1,estROE,estBPS,P,B,F)
      rt <- uniroot(ICC_GLS_fun,c(0.0001,10000),estROE = estROE,estBPS = estBPS,P=P,B=B)$root
      if(svgplt){
         svglite::xmlSVG(code = {
            plot(x = x,y=y,xlab = "ICC GLS",ylab = "Current Price",main= stock,sub = paste("Target Price",round(P,2),"- BV",round(B,2),"- ICC",round(rt,3)))
            abline(h = P,col="red")
            abline(v = rt, col = "blue")
         },standalone = T)
      } else {
         jpeg(paste0("ICC/GLS/",sub('BR:','',stock),"_GLS.jpeg"),width = 640)
         plot(x = x,y=y,xlab = "ICC GLS",ylab = "Current Price",main= stock,sub = paste("Target Price",round(P,2),"- BV",round(B,2),"- ICC",round(rt,3)))
         abline(h = P,col="red")
         abline(v = rt, col = "blue")
         dev.off()
      }
   })
}

plotCT <- function(stock,dadosF,dadosI,gae = 0.05,x = seq(0.001,1,0.001)){
   estEPS <- dadosF[EmpCode == stock][order(Ano)]$EPS
   P <- dadosI[EmpCode == stock]$PRC[1]
   B <- dadosI[EmpCode == stock]$BPS[1]
   estBPS <- c(B,dadosF[EmpCode == stock][order(Ano)]$BPS)
   try({
      y <- vapply(x,ICC_CT_fun,.1,estEPS,estBPS,P,B,gae,root=F)
      rt <- uniroot(ICC_CT_fun,c(gae+0.001,10000),estEarn = estEPS,estBV = estBPS,P=P,B=B,gae=gae)$root
      jpeg(paste0("ICC/CT/",sub('BR:','',stock),"_CT.jpeg"),width = 640)
      plot(x = x,y=y,xlab = "ICC CT",ylab = "Current Price",main= stock,sub = paste("Target Price",round(P,2),"- BV",round(B,2),"- ICC",round(rt,3)))
      abline(h = P,col="red")
      abline(v = rt, col = "blue")
      dev.off()
   })
}

decomposicaoRetornoTeste <- function(dtICC_D,dadosR,interv=list(H=c(.5,1),L=c(0,.5)),rho = .96^(1/12)){

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

midprocessing_table <- function(tab){
   tab[,Valor := round(Valor*100,2)]
   tab <- dcast(tab,VAR+VARF~ICC,value.var = "Valor")
   tab <- as.data.frame(tab)
   rownames(tab) <- tab$VARF
   return(tab)
}
