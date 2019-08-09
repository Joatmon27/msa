library(data.table)
library(here)
library(MASS)

hypothesis_test <- function(df, significance){
  dbar <- colMeans(df)
  sd <- cov(df)
  n <- nrow(df)
  p <- ncol(df)
  
  t_square <- n*dbar%*%solve(sd)%*%dbar
  crit <- ((n-1)*p/(n-p))*qf(1-significance,p,n-p)
  
  p_val <- 1-pf(t_square, p, n-p)
  
  if(t_square > crit){
    print(paste0('T^2 = ',round(t_square, 4),' > ', round(crit, 4),
                 ' We thus reject the null hypothesis and conclude that there is a nonzero mean difference between the measurements with significance level alpha = '
                 , significance))
  }else{
    
    print(paste0('T^2 = ',round(t_square, 4),' < ', round(crit, 4),
                 ' We thus do not reject the null hypothesis and conclude that there is not a significant difference between the measurements at significance level alpha = '
                 , significance))
  }
  
  print(paste0('p-value = ', p_val))
}


table_6_1 <- as.data.frame(fread(file = here('/assignment_1/T6-1.dat')))

d1 <- table_6_1$V1 - table_6_1$V3
d2 <- table_6_1$V2 - table_6_1$V4

d <- data.frame(d1 = d1, d2 = d2)

hypothesis_test(d, 0.05)

confidence_interval <- function(df, a, significance){
  dbar <- colMeans(df)
  sd <- cov(df)
  n <- nrow(df)
  p <- ncol(df)
  
  t_conf_int <- sqrt((((n-1)*p/(n-p))*qf(1-significance,p,n-p))*(a%*%sd%*%a/n))
  
  bonf_conf_int <- qt(1-significance/2*p, n-1)*sqrt((a%*%sd%*%a/n))
  
  return(list('t_conf_int' = c(a%*%dbar - t_conf_int, a%*%dbar + t_conf_int),
              'bonf_conf_int' = c(a%*%dbar - bonf_conf_int, a%*%dbar + bonf_conf_int)))
}

a <- c(1,1)

confidence_interval(d, c(1,0),0.05)

draw_ellipse <- function(){
  
}
