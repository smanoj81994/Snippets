#Description : Manual Implementation of gradient desc for simple linear regression
#Author: Manoj
#Date : 18 April,2019

# Loading libraries
library(dplyr)
library(plotly)

# Mtcars data for model fit
x_orig=mtcars$wt
y_orig=mtcars$hp
n<-length(x_orig)

# Standardizing the data
sds<-apply(cbind(x_orig,y_orig),2, sd)
means<-apply(cbind(x_orig,y_orig),2, mean)

x<-(x_orig-means["x_orig"])/sds["x_orig"]
y<-(y_orig-means["y_orig"])/sds["y_orig"]



# Bench mark with lm formula
mod<-lm(y_orig~x_orig)
yhat_bench<-predict(mod,mtcars)
mse_bench<-sum((y_orig-yhat_bench)^2)/n

# Getting coefficients with standardized data
mod_nm<-lm(y~x)
bm<-coefficients(mod_nm)

# Gradient Descent Function
gradient_Descent <- function(x, y, learning_rate, converge_threshold,m_start,c_start) {
  
  #ggplot(mtcars,aes(x=disp,y=mpg))+geom_point()
  
  m <- m_start
  c <- c_start
  yhat <- m * x + c
  
  
  MSE <- sum((y - yhat) ^ 2)/length(x) 
  converge = F
  iterator = 0
  while(converge == F) {
    ## Implement the gradient descent algorithm
    m_new <<- m - learning_rate * sum(-2*(y-yhat)*x)/length(x) 
    c_new <<- c - learning_rate * sum(-2*(y-yhat))/length(x)
    
    step_m_diff<-abs(learning_rate * sum(-2*(y-yhat)*x))
    step_c_diff<-abs(learning_rate * sum(-2*(y-yhat)))
    #print(paste0(step_m_diff," : ",step_c_diff))
    print(paste0(c_new," : ",m_new))
    
    
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    
    
    if(iterator==0){
      
      diff<-MSE-sum((y - yhat) ^ 2)/length(x)
      # if(diff<0){
      #   print("try lower learning rate..")
      #   stop()
      # }
    }else{
      diff<-MSE_new-sum((y - yhat) ^ 2)/length(x)
      # if(diff<0){
      #   print("try lower learning rate..")
      #   stop()
      # }
    }
    
    MSE_new <- sum((y - yhat) ^ 2)/length(x)
    
    print(paste0("Iterator : ",iterator,"_diff mse : ",diff))
    
    if(diff<converge_threshold) {
      #iterator>max_iter 
      converge = T
      print(paste0("best intercept:", c," best slope:", m))
      return(yhat)
    }
    iterator = iterator + 1
    
  }
  
}

# Calling the function
yhat<-gradient_Descent(x,y,0.0001,10^-10,0,0)

# Checking how denormalized predictions compare to benchmark 
yhat_dn<-yhat*sds["y_orig"]+means["y_orig"]
mse_dn<-sum((y_orig-yhat_dn)^2)/n


print(paste0("Original MSE:",mse_bench," gradient desc fit MSE:",mse_dn))
print(paste0("Original coeff:",bm["x"]," gradient desc coeff:",m_new))
print(paste0("Original coeff:",bm["(Intercept)"]," gradient desc coeff:",c_new))


# Visual examination of the fit

# green dots are the bench mark fit
# red dots are the grad desc fit 
p<-ggplot(mtcars,aes(x=x,y=y))+geom_point()+
  geom_point(aes(x=x,y=(m_new*x+c_new)),size=2,color="red")+
  geom_point(aes(x=x,y=(bm['x']*x+bm['(Intercept)'])),size=0.9,color="green")

ggplotly(p)



