wrapperLM <- function(X,Y){
    N <- nrow(X)
    n <- ncol(X)
    size.CV<-floor(N/10)
    scale <- F
    
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
                
                if (scale){
                     # scaling factors
                    X.center <- colMeans(X.tr)
                    X.scale <- apply(X.tr,2,sd)                

                    Y.center <- mean(Y.tr)
                    Y.scale <- sd(Y.tr)  

                    #scaling training set 
                    X.tr <- data.frame(scale(X.tr))
                    Y.tr <- scale(Y.tr) 

                    X.ts <- t(apply(sweep(X.ts,2,X.center,"-"), 1, function(x) x/X.scale))
                    X.ts <-data.frame(X.ts)
                }
                                    
                colnames(X.ts) <- colnames(X[features_to_include])
                colnames(X.tr) <- colnames(X[features_to_include])

                DS<-cbind(X.tr,SalePrice=Y.tr)
                                   
                model<- lm(SalePrice~.,DS)

                Y.hat.ts<- predict(model,X.ts)
                
                if (scale){
                    # unscale Y.hat.ts
                    Y.hat.ts <- cbind((Y.hat.ts*Y.scale) + Y.center)   
                }
                                    
                # compute log error
                Y.hat.ts[Y.hat.ts <= 0] <- 1                    
                CV.err[j,i] <- sqrt( mean( (log(Y.hat.ts)-log(Y.ts))^2 ))    
            }
        }
                                    
        CV.err.mean<-apply(CV.err,1,mean)
        CV.err.sd<-apply(CV.err,1,sd)
        selected_current<-which.min(CV.err.mean)              
        selected<-c(selected,candidates[selected_current])
                                    
        message(paste("Round ",round," ; Selected feature: ",candidates[selected_current]," ; CV error=",round(CV.err.mean[selected_current],digits=4), " ; std dev=",round(CV.err.sd[selected_current],digits=4)))

    }
    #nb_features.best <- which.min(round(CV.err.mean,digits=4))
    return(selected)
}