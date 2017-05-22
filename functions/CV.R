CV <- function(X,Y, model, nbNeurones){
    N <- nrow(X)
    n <- ncol(X)

    size.CV <- floor(N/10)
    CV.err <- numeric(10)
   
    for (i in 1:1) {
        # 1/10 for testing
         i.ts<-(((i-1)*size.CV+1):(i*size.CV))  ### i.ts = indices of the test set for the i-th fold
         X.ts<-X[i.ts,]  
         Y.ts<-Y[i.ts]    

        #9/10 for training
         i.tr<-setdiff(1:N,i.ts)                ###i.tr = indices of the training set for the i-th fold
         X.tr<-X[i.tr,]
         Y.tr<-Y[i.tr] 
        
        if (TRUE){
            #scaling
             X.tr.mean <- colMeans(X.tr)
             X.tr.sd <- apply(X.tr,2,sd)
             Y.tr.mean <- mean(Y.tr)
             Y.tr.sd <- sd(Y.tr)

            #Y.tr <- cbind(Y.tr - Y.tr.mean)/Y.tr.sd 
             X.tr <- data.frame(scale(X.tr))

            #scaling the testing test by the same scaling as of the training set
             X.ts <- t(apply(sweep(X.ts,2,X.tr.mean,"-"), 1, function(x) x/X.tr.sd))
             Y.ts <- cbind(Y.ts - Y.tr.mean)/Y.tr.sd 

             X.ts <-data.frame(X.ts)
        } 
                         
         DS<-cbind(X.tr,SalePrice=Y.tr)

         # delete column with only NAs (due to dividing by a std of 0)    
         DS<- DS[,colSums(!is.na(DS)) > 0]    
        
         if (model == "lm"){
             model <- lm(SalePrice~.,DS)      # create model with the training set
             message(paste("linear model"))
         }else if (model == "nnet"){
             model <- nnet(SalePrice~.,DS, size = nbNeurones, linout=T, maxit = 1000)
             message(paste("nnet"))
         }else if (model == "tree"){
             model <- rpart(SalePrice~.,DS)
             message(paste("tree"))
         }
                             
         # predict value for the test set                    
         Y.hat.ts<- predict(model,X.ts)  
                         
         # unscale Y.hat.ts
         message(paste(Y.tr.sd))                    
         Y.hat.ts <- cbind((Y.hat.ts*Y.tr.sd) + Y.tr.mean)
                            
         Y.hat.ts[Y.hat.ts <= 0] <- 1
         
         # compute log error
         CV.err[i] <- sqrt( mean( (log(Y.hat.ts)-log(Y.ts))^2 ))                 
    }
    CV_error <- round(mean(CV.err),digits=4)
    message(paste("CV error = ", CV_error, " ; std dev = ",round(sd(CV.err),digits=4)))
    return (CV_error)
}