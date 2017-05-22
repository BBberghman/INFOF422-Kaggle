modeles.NN <- function(Xfinal,Y,R){
    size.CV<-floor(N/10)
    CV.err<-numeric(10)
    N<-nrow(Xfinal)    
    n<-ncol(Xfinal) 

    for (i in 1:10) {
        # testing
        i.ts<-(((i-1)*size.CV+1):(i*size.CV))  
        X.ts<-Xfinal[i.ts,]  
        Y.ts<-Y[i.ts]  

        i.tr<-setdiff(1:N,i.ts)                

        Y.hat.ts.R<-matrix(0,nrow=nrow(X.ts),ncol=R)

        for (r in 1:R) {
            # training
            i.tr.resample<-sample(i.tr,rep=T)
            X.tr<-Xfinal[i.tr.resample,]
            Y.tr<-Y[i.tr.resample]     

            # scaling parameters
            X.sc <- scale(X.tr)
            X.sd <- attr(X.sc, 'scaled:scale')
            X.mean <- attr(X.sc, 'scaled:center')
            Y.sc <- scale(Y.tr)
            Y.sd <- attr(Y.sc, 'scaled:scale')
            Y.mean <- attr(Y.sc, 'scaled:center')

            X.sc <- data.frame(X.sc)

            X.ts.sc <- t(apply(sweep(X.ts,2,X.mean,"-"), 1, function(x) x/X.sd)) 
            X.ts.sc <- data.frame(X.ts.sc)

            # training = dataset
            DS <- cbind(X.sc, SalePrice=Y.sc)
            DS<- DS[,colSums(!is.na(DS)) > 0]  # if one column has only NAs due to std = 0 

            model.nn <- nnet(SalePrice~.,DS, size = 2, linout=T, maxit = 500, trace=F)

            # infere Y.testing scaled
            Y.hat.sc <- predict(model.nn,X.ts.sc)

            # unscale
            Y.hat <- Y.hat.sc*Y.sd + Y.mean

            # prevent log error
            Y.hat[Y.hat <= 0] <- 1    

            Y.hat.ts.R[,r] <- Y.hat
        }

        Y.hat.ts<-apply(Y.hat.ts.R,1,mean)
        CV.err[i]<-sqrt(mean((log(Y.hat.ts)-log(Y.ts))^2))
    }

    print(paste("CV error=",round(mean(CV.err),digits=4), " ; std dev=",round(sd(CV.err),digits=4)))
}