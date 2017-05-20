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
        
        CV.err.mean <- apply(CV.err,1,mean)
        CV.err.sd <- apply(CV.err,1,sd)
        
        selected_current <- which.min(CV.err.mean)              
        selected <- c(selected,candidates[selected_current])
        
        print(paste("Round ",round," ; Selected feature: ",candidates[selected_current]," ; CV error = ",round(CV.err.mean[selected_current],digits=4), " ; std dev = ",round(CV.err.sd[selected_current],digits=4)))
    
    }
 return(selected)                 
}