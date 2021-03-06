library(data.table)
library(ggplot2)

data <- table_8_5

pw_pca <- function (data) 
{
  ## function to do principle component analysis
  ## param data: data on which to do PCA
  ## returns: list with eigenvectors, correlation coefficients, variance and cumulative variances
  
  p <- ncol(data)
  sigma <- cov(data)
  xbar <- colMeans(data)
  
  eigen_vectors <- eigen(sigma)$vectors
  eigen_values <- eigen(sigma)$values
  cum_var <- matrix(,ncol=1,nrow=p)
  cum_var[1,] <- (eigen_values[1]/sum(eigen_values))
  
  for (i in 2:p){
    cum_var[i,] <- (eigen_values[i]/sum(eigen_values))+cum_var[i-1]
  }
  
  n <- sum(cum_var<0.97)
  rho <- matrix(,ncol=n,nrow=p)
  
  for (i in 1:p){
    for (j in 1:n){
      rho[i,]<-(eigen_vectors[i,j]*sqrt(eigen_values[j]))/sqrt(sigma[i,i])
    }
  }
  
  par(mfrow=c(2,1))
  
  y1<-(eigen_vectors[1,1]*(data[,1]-xbar[1])) + (eigen_vectors[2,1]*(data[,2]-xbar[2]))+(eigen_vectors[3,1]*(data[,3]-xbar[3]))+(eigen_vectors[4,1]*(data[,4]-xbar[4]))+(eigen_vectors[5,1]*(data[,5]-xbar[5]))
  y2<-(eigen_vectors[1,2]*(data[,1]-xbar[1])) + (eigen_vectors[2,2]*(data[,2]-xbar[2]))+(eigen_vectors[3,2]*(data[,3]-xbar[3]))+(eigen_vectors[4,2]*(data[,4]-xbar[4]))+(eigen_vectors[5,2]*(data[,5]-xbar[5]))
  
  pca_plot <- ggplot() + geom_point(aes(x=y2$V1,y=y1$V1)) + ggtitle("Plot of first two principal components") + ylab("yhat 2") + xlab("yhat 1")
  print(pca_plot)
  
  scree_plot <- ggplot() + geom_point(aes(y=eigen_values, x=seq(1,length(eigen_values))))+ geom_line(aes(y=eigen_values, x=seq(1,length(eigen_values)))) + xlab("i") + ggtitle("Scree plot") + ylab("eigen values")
  print(scree_plot)
  
  return(list("eigen vectors"=eigen_vectors,"correlation coefficients"=rho, "variance"=eigen_values,"cumulative variances"=cum_var))
}

table_8_5 <- fread('T8-5.dat')

results <- pw_pca(table_8_5)

pw_pca_2 <- function (data){
  ## function to do principle component analysis
  ## param data: data on which to do PCA
  ## returns: list with eigenvectors, correlation coefficients, variance and cumulative variances
  
  p <- ncol(data)
  sigma <- cov(data)
  xbar <- colMeans(data)
  
  eigen_vectors <- eigen(sigma)$vectors
  eigen_values <- eigen(sigma)$values
  cum_var <- matrix(,ncol=1,nrow=p)
  cum_var[1,] <- (eigen_values[1]/sum(eigen_values))
  
  for (i in 2:p){
    cum_var[i,] <- (eigen_values[i]/sum(eigen_values))+cum_var[i-1]
  }
  
  n <- sum(cum_var<0.97)
  rho <- matrix(,ncol=n,nrow=p)
  
  for (i in 1:p){
    for (j in 1:n){
      rho[i,]<-(eigen_vectors[i,j]*sqrt(eigen_values[j]))/sqrt(sigma[i,i])
    }
  }
  
  par(mfrow=c(2,1))
  
  y1<-(eigen_vectors[1,1]*(data[,1]-xbar[1])) + (eigen_vectors[2,1]*(data[,2]-xbar[2]))+(eigen_vectors[3,1]*(data[,3]-xbar[3]))
  y2<-(eigen_vectors[1,2]*(data[,1]-xbar[1])) + (eigen_vectors[2,2]*(data[,2]-xbar[2]))+(eigen_vectors[3,2]*(data[,3]-xbar[3]))
  
  pca_plot <- ggplot() + geom_point(aes(x=y2$V1,y=y1$V1)) + ggtitle("Plot of first two principal components") + ylab("yhat 2") + xlab("yhat 1")
  print(pca_plot)
  
  scree_plot <- ggplot() + geom_point(aes(y=eigen_values, x=seq(1,length(eigen_values))))+ geom_line(aes(y=eigen_values, x=seq(1,length(eigen_values)))) + xlab("i") + ggtitle("Scree plot") + ylab("eigen values")
  print(scree_plot)
  
  return(list("eigen vectors"=eigen_vectors,"correlation coefficients"=rho, "variance"=eigen_values,"cumulative variances"=cum_var))
}

table_6_9 <- fread('T6-9.dat')
table_6_9 <- table_6_9[,c(1:3)]

pw_pca_2(log(table_6_9))

ex_8_3<-princomp(table_8_5,cor=F,scores=T)
summary(ex_8_3)

pca_plot_8_3 <- ggplot() + geom_line(aes(y=ex_8_3$sdev^2,x=seq(1,length(ex_8_3$sdev)))) + geom_point(aes(y=ex_8_3$sdev^2,x=seq(1,length(ex_8_3$sdev))))
print(pca_plot_8_3)

loadings(ex_8_3)

ex_8_4 <- princomp(table_6_9,cor=F,scores=T)
summary(ex_8_4)

pca_plot_8_4 <- ggplot() + geom_line(aes(y=ex_8_4$sdev^2,x=seq(1,length(ex_8_4$sdev)))) + geom_point(aes(y=ex_8_4$sdev^2,x=seq(1,length(ex_8_4$sdev))))
print(pca_plot_8_4)

loadings(ex_8_4)

example_8_5 <- function (data){
  p <- ncol(data)
  n <- nrow(data)
  sigma <- cov(data)
  xbar <- colMeans(data)
  yhat <- matrix(,ncol=p,nrow=n)
  
  eigen_vectors <- eigen(sigma)$vectors
  eigen_values <- eigen(sigma)$values
  
  for(i in 1:p){
    yhat[,i] <- (eigen_vectors[1,i]*(data[,1]-xbar[1])) + (eigen_vectors[2,i]*(data[,2]-xbar[2]))+(eigen_vectors[3,i]*(data[,3]-xbar[3]))+(eigen_vectors[4,i]*(data[,4]-xbar[4]))+(eigen_vectors[5,i]*(data[,5]-xbar[5]))
  }
  
  data_subset <- yhat[,1:2]
  x_bar <- as.matrix(colMeans(data_subset))
  sinv <- solve(var(data_subset))
  Q <- qchisq(0.05,2,lower.tail=FALSE)
  xmin <- -5000;xmax<-5000
  ymin <- -5000;ymax<-5000
  x1 <- seq(xmin,xmax,length=100)
  x2 <- seq(ymin,ymax,length=100)
  grid_data <- as.matrix(expand.grid(x1,x2))
  ng <- nrow(grid.data)
  np <- length(x1)
  signgrid <- matrix(rep(0,ng),nrow=ng,ncol=1)
  
  for (i in 1:ng){
    signgrid[i,] <- sign((t(grid_data[i,]-x_bar)%*%sinv%*%(grid_data[i,]-x_bar))-Q) 
  }
  
  plot(data_subset[,1],data_subset[,2],xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="yhat1",ylab="yhat2",col="red",pch=16,cex=1.25)
  
  par(new=T)
  contour(x1,x2,matrix(signgrid,np),add=TRUE,drawlabels=FALSE,levels=0,lty=2,lwd=2,col="black")
  
  mat<-cbind(signgrid,grid_data)
  
  region<-matrix(mat[mat[,1]==-1],ncol=3)
  par(new=T)
  plot(region[,2:3],xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="red",xlab="yhat1",ylab="yhat2",pch=".",main="Ellipse" )
  
  list("eigen vectors"=eigen_vectors,"variance"=eigen_values,"y hat"=yhat)
}

table_5_8 <- fread('T5-8.dat')
data <- table_5_8

example_8_5(table_5_8)

example_8_11 <- function (data){
  p<-ncol(data)
  n<-nrow(data)
  sigma<-cov(data)
  eig.vecs<-eigen(sigma)$vectors
  vars<-eigen(sigma)$values
  xbar<-apply(data,2,mean)
  yhat<-matrix(,ncol=p,nrow=n)
  for ( i in 1:p){
    yhat[,i]<-(eig.vecs[1,i]*(data[,1]-xbar[1])) + (eig.vecs[2,i]*(data[,2]-xbar[2]))+(eig.vecs[3,i]*(data[,3]-xbar[3]))+(eig.vecs[4,i]*(data[,4]-xbar[4]))+(eig.vecs[5,i]*(data[,5]-xbar[5]))
  }
  vals<-(yhat[,3]^2/vars[3])+(yhat[,4]^2/vars[4])+(yhat[,5]^2/vars[5])
  plot(vals,xlab="Period",ylab="T^2",ylim=c(0,10))
  lines(vals,col="black")
  Ch1<-qchisq(0.05,3,lower=FALSE)
  abline(h=Ch1,lty=2)
  abline(h=0,lty=3)
}

example_8_11(table_5_8)

s<-matrix(c(5,2,2,2),byrow = T,ncol=2)

eig_vec<-eigen(s)$vectors

var<-eigen(s)$values

cum_var<-(var[1]/sum(var))

s<-matrix(c(5,2,2,2),byrow = T,ncol=2)

pho<-cov2cor(s)

eigen(pho)$vector

eigen_val<-eigen(pho)$values

eigen_val[1]/(eigen_val[1]+eigen_val[2])

eigen_vec<-eigen(pho)$vector

eigen_vec[1,1]*sqrt(eigen_val[1])

eigen_vec[2,1]*sqrt(eigen_val[1])

eigen_vec[1,2]*sqrt(eigen_val[2])

data <- matrix(c(108.28, 17.05, 1484.10,152.36, 16.59,  750.33,95.04, 10.91,  766.42,65.45, 14.14, 1110.46,62.97,  9.52, 1031.29,263.99, 25.33,  195.26,265.19, 18.54,  193.83,285.06, 15.73,  191.11,92.01,  8.10, 1175.16,165.68, 11.13,  211.15), byrow = T, ncol = 3)

function (data){
  p<-ncol(data)
  n<-nrow(data)
  sigma<-cov(data)
  eig.vecs<-eigen(sigma)$vectors
  vars<-eigen(sigma)$values
  xbar<-apply(data,2,mean)
  cum.var<-matrix(,ncol=1,nrow=p)
  cum.var[1,]<-(vars[1]/sum(vars))
  for (i in 2:p){
    cum.var[i,]<-(vars[i]/sum(vars))+cum.var[i-1]
  }
  yhat<-matrix(,ncol=p,nrow=n)
  for ( i in 1:p){
    yhat[,i]<-(eig.vecs[1,i]*(data[,1]-xbar[1])) + (eig.vecs[2,i]*(data[,2]-xbar[2]))#+(eig.vecs[3,i]*(data[,3]-xbar[3]))+(eig.vecs[4,i]*(data[,4]-xbar[4]))+(eig.vecs[5,i]*(data[,5]-xbar[5]))
  }
  yhat <<- yhat
  #Ellipse
  data.use<-yhat[,1:2]
  sinv<-solve(var(data))
  Q<-1.4
  xmin<-0;xmax<-max(data[,1])
  ymin<-min(data[,2]);ymax<-max(data[,2])
  x1<-seq(xmin,xmax,length=100)
  x2<-seq(ymin,ymax,length=100)
  grid.data<-as.matrix(expand.grid(x1,x2))
  ng<-nrow(grid.data)
  np<-length(x1)
  signgrid<-matrix(rep(0,ng),nrow=ng,ncol=1)
  for (i in 1:ng){
    signgrid[i,]<-sign((t(grid.data[i,]-xbar)%*%sinv%*%(grid.data[i,]-xbar))-Q) 
  }
  plot(data[,1],data[,2],xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="x1",ylab="x2",
       col="red",pch=16,cex=1.25)
  par(new=T)
  contour(x1,x2,matrix(signgrid,np),add=TRUE,drawlabels=FALSE,levels=0,lty=2,lwd=2,
          col="black")
  mat<-cbind(signgrid,grid.data)
  region<-matrix(mat[mat[,1]==-1],ncol=3)
  par(new=T)
  plot(region[,2:3],xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="red",xlab="x1",
       ylab="x2",pch=".",main="Ellipse" )	 
  c<-sqrt(1.4)
  one<-c*sqrt(vars[1])*eig.vecs[,1]
  two<-c*sqrt(vars[2])*eig.vecs[,2]
  lines(c(xbar[1]-one[1],xbar[1]+one[1]),c(xbar[2]-one[2],xbar[2]+one[2]),lty=1)
  lines(c(xbar[1]-two[1],xbar[1]+two[1]),c(xbar[2]-two[2],xbar[2]+two[2]),lty=1)
  yone<-eig.vecs[2,1]/eig.vecs[1,1]
  xone<-xbar[2]-yone*xbar[1]
  abline(xone,yone,lty=2)
  ytwo<-eig.vecs[2,2]/eig.vecs[1,2]
  xtwo<-xbar[2]-ytwo*xbar[1]
  abline(xtwo,ytwo,lty=2)
  text(150,25,"yhat1")
  text(275,19,"yhat2")
  
  list("eigen vectors"=eig.vecs,"variance"=vars,"cumulative variance"=cum.var,"y hat"=yhat)
}

cor(yhat[,1],data[,1:2])