modeles.LM <- function(Xfinal, Y,R){

size.CV <- floor(N/10)
CV.err <- numeric(10)
N <- nrow(Xfinal)    
n <- ncol(Xfinal) 
    
Y.hats.ts <- numeric(10)
  
Y.scale <- attr(Y, 'scaled:scale')
Y.center <- attr(Y, 'scaled:center')

for (i in 1:10) {
    # testing
    i.ts<-(((i-1)*size.CV+1):(i*size.CV))  
    X.ts<-Xfinal[i.ts,]  
    Y.ts<-Y[i.ts]  
      
    i.tr<-setdiff(1:N,i.ts)                
    
    Y.hat.ts.R<-matrix(0,nrow=nrow(X.ts),ncol=R)
    
    for (r in 1:R){
        # training
        i.tr.resample<-sample(i.tr,rep=T)
        X.tr<-Xfinal[i.tr.resample,]
        Y.tr<-Y[i.tr.resample]
        
        DS<-cbind(X.tr,SalePrice=Y.tr)    
        model <- lm(SalePrice~.,DS)
                              
        Y.hat.ts<- predict(model,X.ts)  
        Y.hat.ts[Y.hat.ts <= 0] <- 1 
        Y.hat.ts.R[,r] <- Y.hat.ts
    }
    Y.hat.ts[,i]<-apply(Y.hat.ts.R,1,mean)
    CV.err[i]<-sqrt(mean((log(Y.hat.ts)-log(Y.ts))^2))
}

print(paste("CV error=",round(mean(CV.err),digits=4), " ; std dev=",round(sd(CV.err),digits=4)))
}