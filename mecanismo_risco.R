## Tabela 2 invenção própria
## Expected Return

tb2 <- decomposicaoRetornoTeste(dtICC_D, dadosR)
tb2[,ICC := paste0(gsub('ICC_','',ICC),"(%)")]
tb2[,Valor := round(Valor*100,2)]
tb2 <- dcast(tb2,VAR+VARF~ICC,value.var = "Valor")
tb2 <- as.data.frame(tb2)
rownames(tb2) <- tb2$VARF
#tb2[,"CT(%)" := 1]
kable(tb2[c(4,6,5,1,7,2,3),-c(1,2)],align=c(rep('c', 4)))
aleluia <- 50



