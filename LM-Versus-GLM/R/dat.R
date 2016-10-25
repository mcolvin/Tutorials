dat<-data.frame(
    sex=c(rep("female",35),rep("male",43)),
    weight=runif(35+43,68,158))
dat$sex<-factor(mydat$sex, levels=c("male","female"))    

  
b0<- -15
b1<- 0.5 # effect of sex
b2<- 0.1 # 
b3<- 0.0001
er<- 0.5

# model matrix
mm<- model.matrix(~sex+weight+sex:weight,mydat)

dat$homerange<- mm %*% c(b0,b1,b2,b3)
dat$homerange<- exp(dat$homerange+rnorm(35+43,0,er))
