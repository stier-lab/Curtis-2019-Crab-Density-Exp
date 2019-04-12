## Function for extracting parameters from functional response data output by frair package
#Author: J. Curtis

fr_param <- function(dat) {
  temp <- summary(dat$fit)
  st_err <- as.data.frame(temp@coef[,c(1,2)])
  result <- as.data.frame(t(as.numeric(as.character(c(st_err[1,],st_err[2,])))))
  colnames(result) <- c("a", "st_err_a", "h", "st_err_h")
  return(result)
}

