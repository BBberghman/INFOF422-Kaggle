filtre <- function(X,Y){
    N<-nrow(X)
    n<-ncol(X)
    size.CV<-floor(N/10)

    CV.err<-matrix(0,nrow=n,ncol=10)

    for (i in 1:10) {
        i.ts<-(((i-1)*size.CV+1):(i*size.CV))  
        X.ts<-X[i.ts,]  
        Y.ts<-Y[i.ts]  

        i.tr<-setdiff(1:N,i.ts)
        X.tr<-X[i.tr,]
        Y.tr<-Y[i.tr]

        correlation<-abs(cor(X.tr,Y.tr))
        correlation[is.na(correlation)] <- 0
        
        ranking <- sort(correlation,dec=T,index.return=T)$ix

        for (nb_features in 1:n) {            
            DS<-cbind(X.tr[,ranking[1:nb_features],drop=F],imdb_score=Y.tr)
            model<- lm(imdb_score~.,DS)

            Y.hat.ts<- predict(model,X.ts[,ranking[1:nb_features],drop=F])

            CV.err[nb_features,i]<-mean((Y.hat.ts-Y.ts)^2)
        }
    }  

    # print(paste("#Features: ",c(1:n)," ; CV error=",round(apply(CV.err,1,mean),digits=4), " ; std dev=",round(apply(CV.err,1,sd),digits=4)))

    nb_features.best <- which.min(round(apply(CV.err,1,mean),digits=4))
    
    return(ranking[1:nb_features.best])
}