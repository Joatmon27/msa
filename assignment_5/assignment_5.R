library(data.table)
library(dplyr)
library(magrittr)

pca_method <- function(df, conversion = 0){
  ## PCA method for factor analysis
  ## param df: data to be used for pca method for factor analysis
  ## param conversion: conversion to apply, 1 then take correlation matrix,
  ##                   2 then take covariance matrix, else use as is
  ## returns: estimated factor loadings, estimated cov matrix, 
  ##          communalities and proportion of total variance by each factor
  
  if(conversion == 1){
    conv_data <- cor(df)
  }else if(conversion == 2){
    conv_data <- cov(df)
  }else{
    conv_data <- df
  }
  
  conv_eigen <- eigen(conv_data)
  
  row_nums <- sum(conv_eigen$values > 1)
  
  loadings <- matrix(row_nums,ncol=ncol(conv_data),nrow=row_nums)
  
  for (i in 1:row_nums){
     for (j in 1:ncol(conv_data)){
       loadings[i,j] <- sqrt(conv_eigen$values[i])*t(conv_eigen$vectors[j,i])
     }}
  
  loadings <- t(loadings)
  
  est_cov_matrix <- diag(diag(conv_data-loadings%*%t(loadings)))
  communalities <- rowSums(loadings^2)
  prop_tot_var_exp <- conv_eigen$values[1:row_num]/ncol(conv_data)
  
  return(list("est_fact_loadings" = loadings, "est_cov_matrix" = est_cov_matrix, "communalities" = communalities, "prop_tot_var_exp" = prop_tot_var_exp))
}

table_8_4 <- fread('T8-4.dat')

cor_9_3 <- matrix(c(1, 0.02, 0.96, 0.42, 0.01, 0.02, 
                  1, 0.13, 0.71, 0.85, 0.96, 0.13, 
                  1, 0.5, 0.11, 0.42, 0.71, 0.5, 
                  1, 0.79, 0.01, 0.85, 0.11, 0.79, 1),
                  ncol = 5, byrow = T)

pca_method(cor_9_3, 0)

pca_method(table_8_4, 1)

factanal(table_8_4, 2)

factanal(table_8_4, 2)$uniqueness

factanal(table_8_4, 2, scores = 'regression')$scores

factanal(table_8_4,2,scores="regression",rotation = "varimax")$loadings

factanal(table_8_4,2,scores="regression",rotation = "varimax")$scores

factors <- factanal(table_8_4,2,scores="regression",rotation = "varimax")$scores
factor1 <- factors[,1]
factor2 <- factors[,2]
plot(factor1,factor2,main="Factor Scores",xlab="Factor 1",ylab="Factor 2",col="blue")
abline(v=0)
abline(h=0)

q_5a <- function (rho, eigen_vecs, eigen_vals) {
## A function to answer question 5
## param rho: correlation matrix of data
## param eiven_vecs: eigen vector to be used
## param eigen_vals: eigen values associated with eigen vecs
## returns: estimated factor loadings, estimated cov matrix, 
##           and proportion of total variance by each factor

  row_nums <- sum(eigen_vals>1)
  loadings <- matrix(row_nums,ncol=ncol(eigen_vecs),nrow=row_nums)
  
  for (i in 1:row_nums){
      for (j in 1:ncol(eigen_vecs)){
        loadings[i,j]<-sqrt(eigen_vals[i])*t(eigen_vecs[j,i])
      }}
  
  loadings <- t(loadings)
  est_cov_matrix <- diag(diag(rho-loadings%*%t(loadings)))
  prop_tot_var_exp <- eigen_vals[1:row_nums]/ncol(eigen_vecs)
  
  list("est_fact_loadings"=loadings,"est_cov_matrix"=est_cov_matrix,"prop_tot_var_exp"=prop_tot_var_exp) 
}

rho <- matrix(c(1,0.63,0.45,0.63,1,0.35,0.45,0.35,1),ncol=3,byrow=TRUE)
eigen_vals <- c(1.96,0.68,0.36)
eigen_vecs <- matrix(c(0.625,-0.219,0.749,0.593,-0.491,-0.638,0.507,0.843,-0.177),ncol=3,byrow=TRUE)

q_5a(rho, eigen_vecs, eigen_vals)

chick_bone <-matrix(c(1.000, 0.505, 0.569, 0.602, 0.621, 0.603, 
                     0.505, 1.000, 0.422, 0.467, 0.482, 0.450,
                     0.569, 0.422, 1.000, 0.926, 0.877, 0.878,
                     0.602, 0.467, 0.926, 1.000, 0.874, 0.894,
                     0.621, 0.482, 0.877, 0.874, 1.000, 0.937,
                     0.603, 0.450, 0.878, 0.894, 0.937, 1.000)
                    ,ncol=6, nrow=6, byrow=TRUE)

loadings <-matrix(c(0.602,0.200,0.467,0.154,0.926,0.143,1,000,0.874,0.476,0.894,0.327),ncol=2,byrow=2)

psi <- as.matrix(chick_bone-(loadings%*%t(loadings)))

spec_var <- diag(psi)

communalities <- rowSums(loadings^2)

eigen(chick_bone)$values[1:2]/6

chick_bone-diag(diag(as.matrix(chick_bone)-loadings%*%t(loadings)))-loadings%*%t(loadings)

table_4_3 <- fread('T4-3.dat')

factanal(table_4_3 %>% select(-'V5'),1)

factanal(table_4_3 %>% select(-'V5'),1,scores="regression")$scores

detect_outliers <- function (data) {
## Detects outliers from data passed through
## param df: dataframe to inspect for outliers
  
  data <- as.matrix(data)
  xbar <- matrix(apply(data,2,mean),nrow=1)  
  cov_mat <- var(data) 
  inv <- solve(cov_mat) 
  row_num <- nrow(data)
  num_cols <- ncol(data)
  distance <- matrix(rep(0,row_num),ncol=1)
  
  for (i in 1:row_num){
    distance[i,] <- (data[i,]-xbar)%*%inv%*%t(data[i,]-xbar)
  }
  
  j <- seq(1:row_num)
  prob <- (row_num-j+0.5)/row_num
  chi_value <- sort(qchisq(prob,num_cols))
  sorted <- sort(distance)
  
  plot(sorted,chi_value,ylab="Chi-square",xlab="Distance",main="Chi-Square plot",col="blue")
}

detect_outliers(table_4_3 %>% select(-'V5'))
