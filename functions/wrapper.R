wrapper <- function(X,Y){
    N<-nrow(X)
    n<-ncol(X)
    size.CV<-floor(N/10)

    selected<-NULL

    for (round in 1:n) 
    { 
        candidates<-setdiff(1:n,selected)

        CV.err<-matrix(0,nrow=length(candidates),ncol=10)
        
        for (j in 1:length(candidates)) 
        {
            features_to_include<-c(selected,candidates[j])
            print(colnames(X[features_to_include]))
            
            for (i in 1:10) 
            {
                # testing set
                i.ts<-(((i-1)*size.CV+1):(i*size.CV))
                X.ts<-X[i.ts,features_to_include,drop=F] 
                Y.ts<-Y[i.ts] 

                # training set
                i.tr<-setdiff(1:N,i.ts)
                X.tr<-X[i.tr,features_to_include,drop=F]
                Y.tr<-Y[i.tr]

                # scaling factors
                if (is.null(dim(X.tr))){ # if X.tr is a vector
                    X.center <- mean(X.tr)
                    X.scale <- sd(X.tr)
                }    
                else{  # if X.tr is a matrix
                    X.center <- colMeans(X.tr)
                    X.scale <- apply(X.tr,2,sd)                
                }
                Y.center <- mean(Y.tr)
                Y.scale <- sd(Y.tr)  

                #scaling training set 
                X.tr <-data.frame(scale(X.tr))
                Y.tr <- scale(Y.tr) 

                #scaling the testing test by the same scaling as of the training set
                if (is.null(dim(X.ts))){  # if X.ts is a vector
                    X.ts <- cbind(X.ts - X.mean)/X.scale
                }
                else {  # X.ts is a matrix
                    X.ts <- t(apply(sweep(X.ts,2,X.mean,"-"), 1, function(x) x/X.scale))
                    X.ts <-data.frame(X.ts)
                }
                colnames(X.ts) <- colnames(X[features_to_include])
                colnames(X.tr) <- colnames(X[features_to_include])

                #print(X.ts[1:2,])
                                
                # create model
                DS <- cbind(X.tr, SalePrice=Y.tr)
                model <- lm(SalePrice~.,DS)
                
                #infere value of Y.ts
                Y.hat.ts <- predict(model, X.ts)

                # unscale Y.hat.ts
                Y.hat.ts <- cbind((Y.hat.ts*Y.scale) + Y.center)
                Y.hat.ts[Y.hat.ts <= 0] <- 1

                # compute log error
                CV.err[j,i] <- sqrt( mean( (log(Y.hat.ts)-log(Y.ts))^2 ))     
                    
            }
        }
        
        CV.err.mean <- apply(CV.err,1,mean)
        CV.err.sd <- apply(CV.err,1,sd)
        
        selected_current <- which.min(CV.err.mean)              
        selected <- c(selected,candidates[selected_current])
        
        print(paste("Round ",round," ; Selected feature: ",candidates[selected_current]," CV error = ",round(CV.err.mean[selected_current],digits=4), "  std dev = ",round(CV.err.sd[selected_current],digits=4)))
    
    }
    
    #nb_features.best <- which.min(round(CV.err.mean,digits=4))
    #print(nb_features.best)
 return(selected)                 
}