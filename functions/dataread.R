data.type <- function(path) {
    data<-read.csv(path)

    factor_variables <- which(sapply(data[1,],class)=="factor")
    nofactor_variables <- which(sapply(data[1,],class)=="numeric")
    data.nofactor <- data[,-factor_variables]
    data.factor <- data[,factor_variables]

  return(list(
      "numeric" = data.nofactor,
      "factor" = data.factor,
      "factor.variables" = factor_variables,
      "variables.factor" = nofactor_variables,
      "data" = data))
}