
set.seed(1234)
dat<-data.frame(
    sex=c(rep("female",35),rep("male",43)),
    weight=runif(35+43,68,158))
dat$sex<-factor(dat$sex, levels=c("male","female"))    

  
b0<- -15
b1<- 0.5 # effect of sex
b2<- 0.1 # 
b3<- 0.01
er<- 0.5

# model matrix
mm<- model.matrix(~sex+weight+sex:weight,dat)

dat$homerange<- mm %*% c(b0,b1,b2,b3)
dat$homerange<- exp(dat$homerange+rnorm(35+43,0,er))

plot(homerange~weight,dat)

write.csv(dat,
    "C:/Users/mcolvin/Google Drive/Tutorials/LM-Versus-GLM/output/homerange-data.csv")