## Tabela 2 invenção própria
## Expected Return

#tb4 <- decomposicaoRetornoTeste(dtICC_D, dadosR)
tb4 <- decomposicaoRetorno(dtICC_D, dadosR)
tb4[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
tb4[,Valor := round(Valor*100,2)]
tb4 <- dcast(tb4,VAR+VARF~ICC,value.var = "Valor")
tb4 <- as.data.frame(tb2)
rownames(tb4) <- tb4$VARF
#tb2[,"CT(%)" := 1]
kable(tb4[c(4,6,5,1,7,2,3),-c(1,2)],align=c(rep('c', 4)))


