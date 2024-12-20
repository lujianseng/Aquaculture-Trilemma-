rm(list=ls())

library(openxlsx)
library(dplyr)
library(rstanarm)

library(bayestestR)
library(ggplot2)
library(insight)
library(modelbased)
data<-read.xlsx(file.choose())
head(data)
year<-as.numeric(unlist(data[2:11,1]))

###设定数据集
a<-vector(length = 13)
b<-vector(length = 13)
a1<-vector(length = 13)
a2<-vector(length = 13)
b1<-vector(length = 13)
b2<-vector(length = 13)

pv_a<-vector(length = 13)
pv_b<-vector(length = 13)
for(i in 2:14){
  mar<-as.numeric(unlist(data[2:11,i]))
  
  production<-cbind(year,mar)
  
  
  colnames(production)[1:2] <- c("x","y")
  production<-as.data.frame(production)
  mar_p <- stan_glm(y ~ x, data = production,chains = 4, iter = 11000, warmup = 1000)
  mar_s <- get_parameters(mar_p)
  
  a[i]<-mean(mar_s$x)
  b[i]<-mean(mar_s$`(Intercept)`)
  
  
  performance<-model_performance(mar_p)
  des<-describe_posterior(mar_p, test = c("pd", "ROPE", "BF"))
  
  par<-des$CI_low
  par1<-des$CI_high
  a1[i]<-par[2]
  a2[i]<-par1[2]
  b1[i]<-par[1]
  b2[i]<-par1[1]
  
  
  ###p值
  pv<-p_direction(mar_p)
  pv<-p_direction(mar_p)
  pv_b[i]<-pv[1,2]
  pv_a[i]<-pv[2,2]
 
}

par<-rbind(a,b,a1,a2,b1,b2,pv_a,pv_b)




