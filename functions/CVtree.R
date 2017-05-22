CVtree <- function(X,Y){
    N <- nrow(X)
    n <- ncol(X)

    size.CV <- floor(N/10)
    CV.err <- numeric(10)
   
    for (i in 1:10) {
        # 1/10 for testing
         i.ts<-(((i-1)*size.CV+1):(i*size.CV))  ### i.ts = indices of the test set for the i-th fold
         X.ts<-X[i.ts,]  
         Y.ts<-Y[i.ts]    

        #9/10 for training
         i.tr<-setdiff(1:N,i.ts)                ###i.tr = indices of the training set for the i-th fold
         X.tr<-X[i.tr,]
         Y.tr<-Y[i.tr] 
                              
         DS<-cbind(X.tr,SalePrice=Y.tr)    
        
         model <- rpart(SalePrice~.,DS)      # create model with the training set
          
         # predict value for the test set                    
         Y.hat.ts<- predict(model,X.ts)  
         Y.hat.ts[Y.hat.ts <= 0] <- 1
         
         # compute log error
         CV.err[i] <- sqrt( mean( (log(Y.hat.ts)-log(Y.ts))^2 ))                 
    }
    
    message(paste("Tree decision : CV error = ", round(mean(CV.err),digits=4), " ; std dev = ",round(sd(CV.err),digits=4)))
    #return (CV_error)
}