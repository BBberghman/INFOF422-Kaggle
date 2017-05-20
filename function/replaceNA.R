replace_na_with_mean_value<-function(vec) {
    mean_vec<-mean(vec,na.rm=T)
    vec[is.na(vec)]<-mean_vec
    vec
}

replace_na_with_median_value<-function(vec) {
    median_vec<-median(vec,na.rm=T)
    vec[is.na(vec)]<-median_vec
    vec
}