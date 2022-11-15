tb3 <- decomposicaoRetorno(dtICC_D, dadosR)
tb3[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
tb3[,Valor := round(Valor*100,2)]
tb3 <- dcast(tb3,VAR+VARF~ICC,value.var = "Valor")
tb3 <- as.data.frame(tb3)
rownames(tb3) <- tb3$VARF
tb3[,"CT(%)" := 1]
kable(tb3[c(4,6,5,1,7,2,3),-c(1,2)],align=c(rep('c', 4)))



