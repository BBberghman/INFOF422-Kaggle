wrapperNN <- function(X,Y){
    N <- nrow(X)
    n <- ncol(X)
    size.CV<-floor(N/10)
    
    selected<-NULL

    for (round in 1:n) { 
        candidates<-setdiff(1:n,selected)

        CV.err <- matrix(0,nrow=length(candidates),ncol=10)

        for (j in 1:length(candidates)) {
            features_to_include <- c(selected,candidates[j])

            for (i in 1:10) {
                i.ts<-(((i-1)*size.CV+1):(i*size.CV))  
                X.ts<-X[i.ts,features_to_include,drop=F]  
                Y.ts<-Y[i.ts]  

                i.tr<-setdiff(1:N,i.ts)
                X.tr<-X[i.tr,features_to_include,drop=F]
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
                                   
                colnames(X.ts.sc) <- colnames(X[features_to_include])
                colnames(X.sc) <- colnames(X[features_to_include])

                # training = dataset
                DS <- cbind(X.sc, SalePrice=Y.sc)
                DS<- DS[,colSums(!is.na(DS)) > 0]  # if one column has only NAs due to std = 0 
                                       
                model.nn <- nnet(SalePrice~.,DS, size = 2, linout=T, maxit = 100, trace=F)

                # infere Y.testing scaled
                Y.hat.sc <- predict(model.nn,X.ts.sc)
                # unscale
                Y.hat <- Y.hat.sc*Y.sd + Y.mean
                Y.unsc <- Y.ts.sc*Y.sd + Y.mean

                # prevent log error
                Y.hat[Y.hat <= 0] <- 1    
                Y.unsc[Y.unsc <= 0] <- 1

                CV.err[j,i] <- sqrt( mean( (log(Y.hat)-log(Y.unsc))^2 ))                  
                }
        }
                                    
        CV.err.mean <- apply(CV.err,1,mean)
        CV.err.sd <- apply(CV.err,1,sd)
        selected_current <- which.min(CV.err.mean)              
        selected <- c(selected,candidates[selected_current])
                                    
        message(paste(
            "Round ",round," ; Selected feature: ",candidates[selected_current]," ; CV error = ", round(CV.err.mean[selected_current],digits=4), " ; std dev=", round(CV.err.sd[selected_current], digits=4)
        ))

    }
    #nb_features.best <- which.min(round(CV.err.mean,digits=4))
    return(selected)
}