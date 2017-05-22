CVneural <- function(X,Y,vector){
    N <- nrow(X)
    n <- ncol(X)
    size.CV<-floor(N/10)
    CV.err<-numeric(10)
    mean.size<-numeric(max(vector))

    for (j in vector){   
        for (i in 1:10) {
            i.ts<-(((i-1)*size.CV+1):(i*size.CV)) 
            X.ts<-X[i.ts,]  
            Y.ts<-Y[i.ts]    

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
            X.ts.sc <- t(apply(sweep(X.ts,2,X.mean,"-"), 1, function(x) x/X.sd))
            Y.ts.sc <- cbind(Y.ts - Y.mean)/Y.sd 

            DS <- cbind(X.sc, SalePrice=Y.sc)
            DS<- DS[,colSums(!is.na(DS)) > 0]  # if one column has only NAs due to std = 0  

            model.nn <- nnet(SalePrice~.,DS, size = j, linout=T, maxit = 500, trace = F)

            Y.hat.sc <- predict(model.nn,X.ts.sc)
            Y.hat <- Y.hat.sc*Y.sd + Y.mean
            Y.unsc <- Y.ts.sc*Y.sd + Y.mean

            # prevent log error
            Y.hat[Y.hat <= 0] <- 1    
            Y.unsc[Y.unsc <= 0] <- 1

            CV.err[i] <- sqrt( mean( (log(Y.hat)-log(Y.unsc))^2 ))
        }
        mean.size[j] <- round( mean(CV.err),digits=4 )
        message(paste("Size: ", j, " CV error = ",mean.size[j], " std dev = ",round(sd(CV.err),digits=4)))
    }
    return(mean.size)
}  