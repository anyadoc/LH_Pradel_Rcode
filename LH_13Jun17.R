mydata = read.csv("femaletyp0508.csv",header=T)#maletyani2005/maletyani2006/maletypoanifin/
y <- mydata
pasty <- function(x)
{
  k<-ncol(x)
  n<-nrow(x)
  out<-array(dim=n)
  for(i in 1:n)
  {
    y<-(x[i,]>0)*1
    out[i]<-paste(y[2],y[3],y[4],y[5],y[6],y[7],y[8],y[9],y[10],y[11],y[12],y[13],sep="")#,y[13],y[14],y[15],y[16],y[2],....,y[12],[y1],sep
  }
  return(out)
}
capt.hist<-data.frame(ch=pasty(y[,1:13]),cov=y$best)
as.character(capt.hist$cov)
closed.proc=process.data(data = capt.hist, model = "Pradrec",groups=("cov"))#pradrec/lambda
export.MARK(closed.proc, "femaletyp0508", replace=TRUE, chat = 1, title = "LHdata 13Jun17", ind.covariates ="all")#"finalanimod1"
rm(list=ls())
pradel=convert.inp('femaletyp0508.inp',group.df=data.frame(cov=c("1","2","3","4","5")))#sex=c('M','F',U')finalanimod1.inp'
pradel[1:5,]
pradel.processed=process.data(pradel,model="Pradrec",groups="cov")  #Pradlambda/rec
pradel.ddl=make.design.data(pradel.processed)

Phi.s=list(formula=~cov)
Phi.s.t=list(formula=~cov+time)  #additive sex and time
Phi.s.T=list(formula=~cov*time)  #interactive sex and time

p.t=list(formula=~time)

f.s=list(formula=~cov)
#f.t=list(formula=~time)
f.s.t=list(formula=~cov+time)
f.s.T=list(formula=~cov*time)
#lambda.s=list(formula=~cov)
#lambda.t=list(formula=~time)
#lambda.s.t=list(formula=~cov+time)
#lambda.s.T=list(formula=~cov*time)

mod.list=create.model.list("Pradrec")  #Pradlambda/rec
mark.wrapper(mod.list,data=pradel.processed,ddl=pradel.ddl)
mod.out=mark.wrapper(mod.list,data=pradel.processed,ddl=pradel.ddl,output=FALSE)
#sink("LH_output_3Nov16",append=FALSE,split=TRUE)
tbl=mod.out$model.table
cat('\nTable of model results:\n')
print(tbl[,-1:-3,])