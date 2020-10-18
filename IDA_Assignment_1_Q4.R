library(VIM)
library(MASS)

set.seed(1)
load('databp.Rdata')
missing=which(is.na(databp$recovtime)==TRUE)

#4a
mean_CCA=mean(databp$recovtime,na.rm=TRUE)
std_err_CCA=sd(databp$recovtime,na.rm=TRUE)/sqrt(length(databp$recovtime[-missing]))

cor_CCA_RT_DOSE=cor(databp$recovtime[-missing],databp$logdose[-missing])
cor_CCA_RT_BP=cor(databp$recovtime[-missing],databp$bloodp[-missing])

#4b
databp_MI=databp
databp_MI$recovtime[missing]=mean_CCA

mean_MI=mean(databp_MI$recovtime)
std_err_MI=sd(databp_MI$recovtime)/sqrt(length(databp_MI$recovtime))

cor_MI_RT_DOSE=cor(databp_MI$recovtime,databp_MI$logdose)
cor_MI_RT_BP=cor(databp_MI$recovtime,databp_MI$bloodp)

#4c
lm_fit_RI=lm(recovtime ~ logdose+bloodp, data=databp[-missing,])
ww=as.numeric(lm_fit_RI$coefficients)
imp_RI=1*ww[1]+databp$logdose[missing]*ww[2]+databp$bloodp[missing]*ww[3]
databp_RI=databp
databp_RI$recovtime[missing]=imp_RI

mean_RI=mean(databp_RI$recovtime)
std_err_RI=sd(databp_RI$recovtime)/sqrt(length(databp_RI$recovtime))

cor_RI_RT_DOSE=cor(databp_RI$recovtime,databp_RI$logdose)
cor_RI_RT_BP=cor(databp_RI$recovtime,databp_RI$bloodp)

#4c Residuals have very large standard deviation!
imp_SRI=1*ww[1]+databp$logdose[missing]*ww[2]+databp$bloodp[missing]*ww[3]+
  rnorm(length(missing),0,sd(lm_fit_RI$residuals))

databp_SRI=databp
databp_SRI$recovtime[missing]=imp_SRI

mean_SRI=mean(databp_SRI$recovtime)
std_err_SRI=sd(databp_SRI$recovtime)/sqrt(length(databp_SRI$recovtime))

cor_SRI_RT_DOSE=cor(databp_SRI$recovtime,databp_SRI$logdose)
cor_SRI_RT_BP=cor(databp_SRI$recovtime,databp_SRI$bloodp)

#4d
imp_PMM=c()
for (k in 1:length(missing)){
  imp_PMM[k]=as.numeric(databp_PMM$recovtime[-missing][
    (lm_fit_RI$fitted.values-imp_RI[k])^2==min((lm_fit_RI$fitted.values-imp_RI[k])^2)])
}

databp_PMM=databp
databp_PMM$recovtime[missing]=imp_PMM

mean_PMM=mean(databp_PMM$recovtime)
std_err_PMM=sd(databp_PMM$recovtime)/sqrt(length(databp_PMM$recovtime))

cor_PMM_RT_DOSE=cor(databp_PMM$recovtime,databp_PMM$logdose)
cor_PMM_RT_BP=cor(databp_PMM$recovtime,databp_PMM$bloodp)

#Summarizing results
table_df=data.frame(CCA=c(mean_CCA,std_err_CCA,cor_CCA_RT_DOSE,cor_CCA_RT_BP),
                    MI=c(mean_MI,std_err_MI,cor_MI_RT_DOSE,cor_MI_RT_BP),
                    RI=c(mean_RI,std_err_RI,cor_RI_RT_DOSE,cor_RI_RT_BP),
                    SRI=c(mean_SRI,std_err_SRI,cor_SRI_RT_DOSE,cor_SRI_RT_BP),
                    PMM=c(mean_PMM,std_err_PMM,cor_PMM_RT_DOSE,cor_PMM_RT_BP)) %>%
  round(4)
row.names(table_df)=c("Mean","SE","Corr RT,D","Corr RT,BP")




