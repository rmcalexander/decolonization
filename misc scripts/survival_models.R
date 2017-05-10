library(tidyverse)
survival_df<-select(decolonization,country,year,rr_std,literacy,ns5emp,centercap,Netherlands,Spain,Portugal,French,British,any_resist,violent_resist,nonviolent_resist,middleeast,africa,lamerica,asia,camp_size_est,settler_pop)



survival_df<-summarise(group_by(survival_df,country),
                       time=length(year),
                       rr_std=mean(na.omit(rr_std)),
                       literacy=mean(na.omit(literacy)),
                       centercap=mean(na.omit(centercap)),
                       Netherlands=max(na.omit(Netherlands)),
                       Spain=max(na.omit(Spain)),
                       Portugal=max(na.omit(Portugal)),
                       French=max(na.omit(French)),
                       British=max(na.omit(British)),
                       any_resist_sum=sum(any_resist),
                       violent_resist_sum=sum(violent_resist),
                       nonviolent_resist_sum=sum(nonviolent_resist),
                       middleeast=max(middleeast),
                       africa=max(africa),
                       lamerica=max(lamerica),
                       asia=max(asia),
                       camp_size_est=max(na.omit(camp_size_est)),
                      settler_pop=mean((settler_pop))
)

survival_df$camp_size_est[is.infinite(survival_df$camp_size_est)]<-0

survival_df$share_violent<-survival_df$violent_resist_sum/survival_df$time
survival_df$share_nonviolent<-survival_df$nonviolent_resist_sum/survival_df$time
survival_df$share_all_resist<-survival_df$any_resist_sum/survival_df$time

#try survival model
library(survival)
survival_df$start<-0
survival_df$end<-as.integer(survival_df$time)

S<-Surv(survival_df$start,survival_df$time)
model<-coxph(S~rr_std,data=na.omit(survival_df))

summary(MASS::glm.nb(time~rr_std+literacy+centercap+settler_pop+
                       Netherlands+Portugal+French+British+
                       middleeast+africa+lamerica+as.numeric(share_all_resist>0),data=survival_df))


