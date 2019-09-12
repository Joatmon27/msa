library(data.table)

data <- fread('data.csv')

q1 <- function (data) 
{
  data<-as.matrix(data)
  n <- nrow(data)
  Z <- as.matrix(cbind(rep(1,n),data[,1:2]))
  beta_hat <- solve(t(Z)%*%Z)%*%t(Z)%*%data[,3]
  y_hat <- Z%*%beta_hat
  hat_mat <- Z%*%solve(t(Z)%*%Z)%*%t(Z)
  err <- data[,3]-y_hat
  SSE <- t(err)%*%err
  SSR <- t(y_hat-(mean(data[,3])*(rep(1,n))))%*%(y_hat-(mean(data[,3])*(rep(1,n))))
  SST <- t(data[,3]-(mean(data[,3])*(rep(1,n))))%*%(data[,3]-(mean(data[,3])*(rep(1,n))))
  R_sq <- SSR/SST
  s_sq <- SSE/(n-ncol(data[,1:2])-1)
  list("beta_hat"=beta_hat,"y_hat"=y_hat,"hat_matrix"=hat_mat,"SSE"=SSE,"SSR"=SSR,"SST"=SST,
       "R_squared"=R_sq,"sigma_squared"=s_sq)
}

q1(data)
