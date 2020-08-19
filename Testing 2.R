dat <- read.csv(file ="D:/RIZA/rmr.csv",head=TRUE)
dat
complete.cases(dat)
dat.complete <- dat[complete.cases(dat), ] 
dat.complete
attach(dat.complete) 
plot(body.weight, metabolic.rate) #a)Plot metabolic rate vs body weight dan berikan interpretasi.
sum.x <- sum(body.weight) #b)x=variabel independen
sum.x
sum.x2 <- sum(body.weight^2)
sum.x2
sum.y <- sum(metabolic.rate) #b)y=variabel dependen
sum.y
sum.xy <- sum(body.weight*metabolic.rate)
sum.xy
n <- nrow(dat.complete)
n
beta1.hat <- (n*sum.xy-sum.x*sum.y)/(n*sum.x2-(sum.x)^2) 
beta1.hat
beta0.hat <- (sum.y-beta1.hat*sum.x)/n
beta0.hat
#c)persamaan regresi dugaan
y.hat=(beta0.hat+beta1.hat*body.weight)
y.hat

#prediksi metabolik rate bagi body.wight sebesar 70kg
y.hat=(beta0.hat+beta1.hat*70)
y.hat
#interval kepercayaan 95% bagi slope dan interpretasinya

# menggunakan fungsi lm di R
lm(metabolic.rate~body.weight)
plot(body.weight,metabolic.rate)
abline(lm(metabolic.rate~body.weight))
Sxx <- sum((body.weight-mean(body.weight))^2)
Sxx
Syy <- sum((metabolic.rate-mean(metabolic.rate))^2)
Syy
Sxy <- sum((body.weight-mean(body.weight))*(metabolic.rate-mean(metabolic.rate)))
Sxy
SST <- Syy
SST
SSR <- beta1.hat*(Sxy)
SSR
SSE <- SST-SSR
SSE
MSR <- SSR/1
MSR
MSE <- SSE/(n-2)
MSE
F <- MSR/MSE
F
p.value <- 1-pf(F,df1=1,df2=n-2)
p.value
R2 <- SSR/SST
R2
mod.reg <- lm(metabolic.rate~body.weight)
mod.aov <- anova(mod.reg)
mod.aov
sbetal.hat=sqrt(MSE/Sxx)
sbetal.hat
t_hit=beta1.hat/sbetal.hat
t_hit
summary(mod.reg)
confint(mod.reg)
r=(Sxy)/sqrt(Sxx*Syy)
r
cor(body.weight,metabolic.rate)
par(mfrow=c(1,2))
plot(fitted(mod.reg),resid(mod.reg),xlab="Nilai Dugaan",ylab="Residual")
cor.test(body.weight,metabolic.rate)
abline(h=0,col="blue",lty=2)
plot(body.weight,resid(mod.reg),xlab="X",ylab="Residual")
abline(h=0,col="blue",lty=2)
qqnorm(resid(mod.reg))


str(dat)
rmr <- table(dat$Type)
rmr
chisq <- chisq.test(rmr)
chisq
p.value = 1-pchisq(q=8.870968,df=5)
p.value
qf(0.95,df1=1,df2=42)

t=t_hit*sbetal.hat
t
beta1.hat
interval=beta1.hat+t_hit
interval
