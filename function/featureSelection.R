pca <- function(X,Y){
    
    N<-nrow(X)
    n<-ncol(X)
    size.CV<-floor(N/10)

    CV.err<-matrix(0,nrow=n,ncol=10)

    X_pca<-data.frame(prcomp(X,retx=T)$x)
    
    for (i in 1:10) {
        i.ts<-(((i-1)*size.CV+1):(i*size.CV))  
        X.ts<-X_pca[i.ts,]  
        Y.ts<-Y[i.ts]  

        i.tr<-setdiff(1:N,i.ts)
        X.tr<-X_pca[i.tr,]
        Y.tr<-Y[i.tr]

        for (nb_features in 1:n) {
            DS<-cbind(X.tr[,1:nb_features,drop=F],imdb_score=Y.tr)
            model<- lm(imdb_score~.,DS)

            Y.hat.ts<- predict(model,X.ts[,1:nb_features,drop=F])

            CV.err[nb_features,i]<-mean((Y.hat.ts-Y.ts)^2)
        }
    }  

    #print(paste("# features: ",c(1:n),"  CV error = ",round(apply(CV.err,1,mean),digits=4), "  std dev = ",round(apply(CV.err,1,sd),digits=4)))

    nb_features.best <- which.min(round(apply(CV.err,1,mean),digits=4))
    return(X_pca[1:nb_features.best])
}

wrapper <- function(X,Y){
    N<-nrow(X)
    n<-ncol(X)
    size.CV<-floor(N/10)

    selected<-NULL

    for (round in 1:n) { 
        candidates<-setdiff(1:n,selected)

        CV.err<-matrix(0,nrow=length(candidates),ncol=10)

        for (j in 1:length(candidates)) {
            features_to_include<-c(selected,candidates[j])

            for (i in 1:10) {
                i.ts<-(((i-1)*size.CV+1):(i*size.CV))  
                X.ts<-X[i.ts,features_to_include,drop=F]  
                Y.ts<-Y[i.ts]  

                i.tr<-setdiff(1:N,i.ts)
                X.tr<-X[i.tr,features_to_include,drop=F]
                Y.tr<-Y[i.tr]

                DS<-cbind(X.tr,imdb_score=Y.tr)
                model<- lm(imdb_score~.,DS)

                Y.hat.ts<- predict(model,X.ts)

                CV.err[j,i]<-mean((Y.hat.ts-Y.ts)^2)
            }
        }
        CV.err.mean<-apply(CV.err,1,mean)
        CV.err.sd<-apply(CV.err,1,sd)
        selected_current<-which.min(CV.err.mean)              
        selected<-c(selected,candidates[selected_current])
        
        #print(paste("Round ",round," ; Selected feature: ",candidates[selected_current]," ; CV error = ",round(CV.err.mean[selected_current],digits=4), " ; std dev = ",round(CV.err.sd[selected_current],digits=4)))
    }
 return(selected)                 
}
           
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

mrmr <- function(X,Y){
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
            DS<-cbind(X.tr[,ranking[1:nb_features],drop=F],imdb_score=Y.tr)
            model <- lm(imdb_score~.,DS)

            Y.hat.ts <- predict(model,X.ts[,ranking[1:nb_features],drop=F])

            CV.err[nb_features,i] <- mean((Y.hat.ts-Y.ts)^2)
        }
    }  

    #print(paste("#Features: ",c(1:n)," ; CV error=",round(apply(CV.err,1,mean),digits=4), " ; std dev=",round(apply(CV.err,1,sd),digits=4)))
    
    nb_features.best <- which.min(round(apply(CV.err,1,mean),digits=4))
    
    return(ranking[1:nb_features.best])    
}