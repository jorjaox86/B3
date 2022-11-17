deltaICCcut <- function(dt0,dt1){
   colid <- names(dt1)[-grep('ICC.*',names(dt1))]
   dtICC_D <- melt(dt1,id.vars = colid,variable.name = "ICC",variable.factor = F,value.name = "Valor")
   dtICC_D_0 <- melt(dt0,id.vars = "EmpCode",measure.vars = grep('ICC.*',names(dt0),value = T),variable.name = "ICC",variable.factor = F,value.name = "V0")
   dtICC_D_0 <- dtICC_D_0[!is.na(V0)]
   dtICC_D <- dtICC_D[dtICC_D_0,on = c("EmpCode","ICC")]
   dtICC_D[,Delta := Valor-V0]
   return(dtICC_D)
}
