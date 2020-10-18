library(VIM)
library(MASS)
set.seed(1)

Z_1=rnorm(500)
Z_2=rnorm(500)
Z_3=rnorm(500)

Y_1=1+Z_1
Y_2=5+2*Z_1+Z_2

#3a
a_1=2
b_1=0


R_1=ifelse(a_1*(Y_1-1)+b_1*(Y_2-5)+Z_3<0,1,0)
missing_index_1=which(R_1==1)

Y_df_1=Y_df
Y_df_1[missing_index_1,2]=NA

plot(density(Y_2)$x,density(Y_2)$y,ylim=c(0,0.25),xlab="y",ylab="f(y)",type="l",main="Density plot of Y_2")
lines(density(Y_2[-missing_index_1])$x,density(Y_2[-missing_index_1])$y,col="grey")
legend("topright", legend=c("Complete", "Observed"),
       col=c("black", "grey"), lty=c(1,1))

#3b
lm.fit_1=lm(Y_2[-missing_index_1]~Y_1[-missing_index_1])
imputed_1=cbind(rep(1,sum(R_1)),Y_1[missing_index_1])%*%as.numeric(lm.fit_1$coefficients)+rnorm(sum(R_1),0,sd(lm.fit_1$residuals))

#This might look bad as I first set the completed data as the complete.
#However, then I replace the values that should have been missing with the ones computed above
completed_Y_2=Y_2
completed_Y_2[missing_index_1]=imputed_1

plot(density(Y_2)$x,density(Y_2)$y,ylim=c(0,0.25),xlab="y",ylab="f(y)",type="l",main="Density plot of Y_2")
lines(density(completed_Y_2)$x,density(completed_Y_2)$y,col="grey")
legend("topright", legend=c("Complete", "Completed"),
       col=c("black", "grey"), lty=c(1,1))

#3c
a_2=0
b_2=2


R_2=ifelse(a_2*(Y_1-1)+b_2*(Y_2-5)+Z_3<0,1,0)
missing_index_2=which(R_2==1)

plot(density(Y_2)$x,density(Y_2)$y,ylim=c(0,0.3),xlab="y",ylab="f(y)",type="l",main="Density plot of Y_2")
lines(density(Y_2[-missing_index_2])$x,density(Y_2[-missing_index_2])$y,col="grey")
legend("topright", legend=c("Complete", "Observed"),
       col=c("black", "grey"), lty=c(1,1))

#3d
lm.fit_2=lm(Y_2[-missing_index_2]~Y_1[-missing_index_2])
imputed_2=cbind(rep(1,sum(R_2)),Y_1[missing_index_2])%*%as.numeric(lm.fit_2$coefficients)+rnorm(sum(R_2),0,sd(lm.fit_2$residuals))

#This might look bad as I first set the completed data as the complete.
#However, then I replace the values that should have been missing with the ones computed above
completed_Y_2_d=Y_2
completed_Y_2_d[missing_index_2]=imputed_2

plot(density(Y_2)$x,density(Y_2)$y,ylim=c(0,0.3),xlab="y",ylab="f(y)",type="l",main="Density plot of Y_2")
lines(density(completed_Y_2_d)$x,density(completed_Y_2_d)$y,col="grey")
legend("topright", legend=c("Complete", "Completed"),
       col=c("black", "grey"), lty=c(1,1))

