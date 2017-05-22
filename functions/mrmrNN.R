mrmrNN <- function(X,Y){
    N<-nrow(X)
    n<-ncol(X)
    size.CV<-floor(N/10)
    scale <- F

    CV.err<-matrix(0,nrow=n,ncol=10)

    for (i in 1:10) {
        # testing set
        i.ts<-(((i-1)*size.CV+1):(i*size.CV))  
        X.ts<-X[i.ts,]  
        Y.ts<-Y[i.ts]  
        
        # training set
        i.tr<-setdiff(1:N,i.ts)
        X.tr<-X[i.tr,]
        Y.tr<-Y[i.tr]

        # scaling parameters
        X.sc <- scale(X.tr)
        X.sd <- attr(X.sc, 'scaled:scale')
        X.mean <- attr(X.sc, 'scaled:center')
        Y.sc <- scale(Y.tr)
        Y.sd <- attr(Y.sc, 'scaled:scale')
        Y.mean <- attr(Y.sc, 'scaled:center')

        X.sc <- data.frame(X.sc)

        # scaling testing set
        if (dim(X.ts)[2] == 1){
            X.ts.sc <- (X.ts - X.mean)/X.sd
        }else {
            X.ts.sc <- t(apply(sweep(X.ts,2,X.mean,"-"), 1, function(x) x/X.sd))
                }
        Y.ts.sc <- cbind(Y.ts - Y.mean)/Y.sd 
        X.ts.sc <- data.frame(X.ts.sc)
                            
        #correlation on training set
        correlation <- abs(cor(X.tr,Y.tr))
        correlation[is.na(correlation)] <- 0

        selected<-c()
        candidates<-1:n

        #mRMR ranks the variables by taking account not only the correlation with the output, but also by avoiding redudant variables
        for (j in 1:n) {
            redudancy.score<-numeric(length(candidates))
            if (length(selected)>0) {
                cor.selected.candidates <- cor(X.tr[,selected,drop=F],X.tr[,candidates,drop=F])
                cor.selected.candidates[is.na(cor.selected.candidates)] <- 0
                redudancy.score <- apply(cor.selected.candidates,2,mean)
            }

            mRMR.score<-correlation[candidates]-redudancy.score

            selected_current <- candidates[which.max(mRMR.score)]
            selected <- c(selected,selected_current)
            candidates <- setdiff(candidates,selected_current)
        }

        ranking<-selected

        for (nb_features in 1:n) {
            #create model
            DS<-cbind(X.tr[,ranking[1:nb_features],drop=F],SalePrice=Y.tr)
            model.nn <- nnet(SalePrice~.,DS, size = 2, linout=T, maxit = 500, trace=F)
            
            # infere value of Y.ts
            Y.hat.ts <- predict(model.nn,X.ts[,ranking[1:nb_features],drop=F])
            
            if (scale){
                # unscale Y.hat.ts
                Y.hat.ts <- cbind((Y.hat.ts*Y.scale) + Y.center) 
            }
           
            Y.hat.ts[Y.hat.ts <= 0] <- 1

            # compute log error
            CV.err[nb_features,i] <- sqrt( mean( (log(Y.hat.ts)-log(Y.ts))^2 ))    
        }
    }  

    print(paste("#Features: ",c(1:n)," CV error=",round(apply(CV.err,1,mean),digits=4), " std dev=",round(apply(CV.err,1,sd),digits=4)))
    
    nb_features.best <- which.min(round(apply(CV.err,1,mean),digits=4))
    
    return(ranking[1:nb_features.best])    
}