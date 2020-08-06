rm(list=ls())

setwd("xxx")

folder_name<-"xxx/results/"


####################################################################################################################

library(tidyverse)

firstup<-function(x) {
  substr(x,1,1)<-toupper(substr(x,1,1))
  x
}

####################################################################################################################
### READ DATA ###

outp_agg<-read.csv(paste0(getwd(),folder_name,"outp_agg.csv")) %>%
  mutate(scenario=factor(scenario,levels=c("unmitigated","lockdown","cyclical"))) %>%
  mutate(crit=case_when(
    crit=="TRUE_TRUE_TRUE_TRUE_TRUE_TRUE" ~ "baseline",
    crit=="TRUE_TRUE_TRUE_TRUE_TRUE_FALSE" ~ "80",
    crit=="TRUE_TRUE_TRUE_TRUE_FALSE_FALSE" ~ "70",
    crit=="TRUE_TRUE_TRUE_FALSE_FALSE_FALSE" ~ "60",
    crit=="TRUE_TRUE_FALSE_FALSE_FALSE_FALSE" ~ "50")) %>%
  group_by(scenario,policy,policy_param,crit,metric,currency) %>%
  summarise(value=sum(value)) %>%
  pivot_wider(names_from=metric,values_from=value)

outp_raw<-read.csv(paste0(getwd(),folder_name,"outp_raw.csv")) %>%
  mutate(scenario=factor(scenario,levels=c("unmitigated","lockdown","cyclical"))) %>%
  mutate(crit=case_when(
    crit=="TRUE_TRUE_TRUE_TRUE_TRUE_TRUE" ~ "baseline",
    crit=="TRUE_TRUE_TRUE_TRUE_TRUE_FALSE" ~ "80",
    crit=="TRUE_TRUE_TRUE_TRUE_FALSE_FALSE" ~ "70",
    crit=="TRUE_TRUE_TRUE_FALSE_FALSE_FALSE" ~ "60",
    crit=="TRUE_TRUE_FALSE_FALSE_FALSE_FALSE" ~ "50")) %>%
  group_by(scenario,policy,policy_param,crit,dates,group,metric,currency,type) %>%
  summarise(value=sum(value)) %>%
  pivot_wider(names_from=metric,values_from=value)


####################################################################################################################
### TABLE 3 ###

outp3<-outp_agg %>%
  pivot_longer(cols=-c(scenario,policy,policy_param,crit,currency),names_to="metric",values_to="value") %>%
  pivot_wider(names_from=c("currency","metric"),values_from=value)

fn1<-function(y1,y2,y3) {
  y<-data.frame(y1=y1,y2=y2,y3=y3)
  sapply(1:nrow(y), function(z) {
    if (y$y2[z]<y$y3[z]) {
      paste0(round(y$y1[z],0)," (",round(y$y2[z],0)," to ",round(y$y3[z],0),")")
    } else {
      paste0(round(y$y1[z],0)," (",round(y$y3[z],0)," to ",round(y$y2[z],0),")")
    }
  })
}

fn2<-function(y1,y2,y3) {
  y<-data.frame(y1=-y1,y2=-y2,y3=-y3)
  sapply(1:nrow(y), function(z) {
    tx1<-ifelse(y$y1[z]>0,TRUE,FALSE)
    if (y$y2[z]<y$y3[z]) {
      paste0(ifelse(tx1==TRUE,"+",""),round(y$y1[z],0)," (",round(y$y2[z],0)," to ",round(y$y3[z],0),")")
    } else {
      paste0(ifelse(tx1==TRUE,"+",""),round(y$y1[z],0)," (",round(y$y3[z],0)," to ",round(y$y2[z],0),")")
    }
  })
}


table3<-do.call("rbind",lapply(unique(outp3$scenario),function(x) {
  toutp3<-outp3[which(outp3$scenario==x),]
  tmp1<-as.data.frame(toutp3[which(toutp3$crit=="baseline"),])
  tmp1a<-tmp1[,1:4]
  tmp1b<-cbind(tmp1a,
               data.frame(type1=fn1(tmp1$mean_rej_died,tmp1$q025_rej_died,tmp1$q975_rej_died)),
               data.frame(type2=fn1(tmp1$mean_int_died,tmp1$q025_int_died,tmp1$q975_int_died)),
               data.frame(type3=fn1(tmp1$mean_dch_died,tmp1$q025_dch_died,tmp1$q975_dch_died)),
               data.frame(total=fn1(tmp1$mean_tot_died,tmp1$q025_tot_died,tmp1$q975_tot_died)),
               data.frame(total_ly=fn1(tmp1$mean_tot_died_ly,tmp1$q025_tot_died_ly,tmp1$q975_tot_died_ly)))
  tmp2<-toutp3[which(toutp3$crit!="baseline"),]
  tmp3<-data.frame(tmp2[,1:4])
  tmp4<-cbind(tmp3,
              data.frame(mean_dch_died=tmp1$mean_dch_died-tmp2$mean_dch_died),
              data.frame(mean_dch_died_ly=tmp1$mean_dch_died_ly-tmp2$mean_dch_died_ly),
              data.frame(mean_int_died=tmp1$mean_int_died-tmp2$mean_int_died),
              data.frame(mean_int_died_ly=tmp1$mean_int_died_ly-tmp2$mean_int_died_ly),
              data.frame(mean_rej_died=tmp1$mean_rej_died-tmp2$mean_rej_died),
              data.frame(mean_rej_died_ly=tmp1$mean_rej_died_ly-tmp2$mean_rej_died_ly),
              data.frame(mean_tot_died=tmp1$mean_tot_died-tmp2$mean_tot_died),
              data.frame(mean_tot_died_ly=tmp1$mean_tot_died_ly-tmp2$mean_tot_died_ly),
              
              data.frame(q025_dch_died=tmp1$q025_dch_died-tmp2$q025_dch_died),
              data.frame(q025_dch_died_ly=tmp1$q025_dch_died_ly-tmp2$q025_dch_died_ly),
              data.frame(q025_int_died=tmp1$q025_int_died-tmp2$q025_int_died),
              data.frame(q025_int_died_ly=tmp1$q025_int_died_ly-tmp2$q025_int_died_ly),
              data.frame(q025_rej_died=tmp1$q025_rej_died-tmp2$q025_rej_died),
              data.frame(q025_rej_died_ly=tmp1$q025_rej_died_ly-tmp2$q025_rej_died_ly),
              data.frame(q025_tot_died=tmp1$q025_tot_died-tmp2$q025_tot_died),
              data.frame(q025_tot_died_ly=tmp1$q025_tot_died_ly-tmp2$q025_tot_died_ly),
              
              data.frame(q975_dch_died=tmp1$q975_dch_died-tmp2$q975_dch_died),
              data.frame(q975_dch_died_ly=tmp1$q975_dch_died_ly-tmp2$q975_dch_died_ly),
              data.frame(q975_int_died=tmp1$q975_int_died-tmp2$q975_int_died),
              data.frame(q975_int_died_ly=tmp1$q975_int_died_ly-tmp2$q975_int_died_ly),
              data.frame(q975_rej_died=tmp1$q975_rej_died-tmp2$q975_rej_died),
              data.frame(q975_rej_died_ly=tmp1$q975_rej_died_ly-tmp2$q975_rej_died_ly),
              data.frame(q975_tot_died=tmp1$q975_tot_died-tmp2$q975_tot_died),
              data.frame(q975_tot_died_ly=tmp1$q975_tot_died_ly-tmp2$q975_tot_died_ly)
  )
  tmp5<-cbind(tmp3,
              data.frame(type1=fn2(tmp4$mean_rej_died,tmp4$q025_rej_died,tmp4$q975_rej_died)),
              data.frame(type2=fn2(tmp4$mean_int_died,tmp4$q025_int_died,tmp4$q975_int_died)),
              data.frame(type3=fn2(tmp4$mean_dch_died,tmp4$q025_dch_died,tmp4$q975_dch_died)),
              data.frame(total=fn2(tmp4$mean_tot_died,tmp4$q025_tot_died,tmp4$q975_tot_died)),
              data.frame(total_ly=fn2(tmp4$mean_tot_died_ly,tmp4$q025_tot_died_ly,tmp4$q975_tot_died_ly)))
  return(rbind(tmp1b,tmp5))
}))

table3<-table3 %>%
  arrange(scenario,policy,desc(policy_param))


write.csv(table3,paste0(getwd(),"/table3.csv"),row.names=FALSE)


####################################################################################################################
### FIGURE 4 ###


outp6a<-outp_agg %>%
  pivot_longer(cols=-c(scenario,policy,policy_param,crit,currency),names_to="metric",values_to="value") %>%
  pivot_wider(names_from=c("currency","metric"),values_from=value)

outp6b<-do.call("rbind",lapply(unique(outp6a$scenario),function(x) {
  tmp0<-outp6a[which(outp6a$scenario==x),]
  tmp1<-as.data.frame(tmp0[which(tmp0$crit=="baseline"),])
  tmp2<-tmp0[which(tmp0$crit!="baseline"),]
  tmp3<-data.frame(tmp2[,1:4])
  tmp4<-cbind(tmp3,
              data.frame(mean_tot_lives_saved=100*(tmp2$mean_tot_died-tmp1$mean_tot_died)/tmp1$mean_tot_died),
              data.frame(mean_tot_ly_saved=100*(tmp2$mean_tot_died_ly-tmp1$mean_tot_died_ly)/tmp1$mean_tot_died_ly)
  )
  return(tmp4)
}))


fig4<-outp6b %>%
  mutate(Strategy=case_when(policy=="1" ~"Cut-off",
                            policy=="2" & policy_param=="2" ~"Tolerance (2)",
                            policy=="2" & policy_param=="3" ~"Tolerance (3)",
                            policy=="2" & policy_param=="4" ~"Tolerance (4)",
                            policy=="2" & policy_param=="6" ~"Tolerance (6)",
                            policy=="3" ~"Interrupt")) %>%
  rename(Trajectory=scenario) %>%
  mutate(Trajectory=firstup(as.character(Trajectory))) %>%
  select(-policy,-policy_param) %>%
  pivot_longer(cols=-c("Trajectory","Strategy","crit"),names_to="metric",values_to="value") %>%
  mutate(metric=case_when(metric=="mean_tot_lives_saved" ~"Deaths",
                          metric=="mean_tot_ly_saved" ~"Life-years lost")) %>%
  mutate(metric=factor(metric,levels=c("Deaths","Life-years lost"))) %>%
  mutate(Trajectory=factor(Trajectory,levels=c("Unmitigated","Lockdown","Cyclical"))) %>%
  mutate(Strategy=factor(Strategy,levels=c("Cut-off","Tolerance (6)","Tolerance (4)","Tolerance (3)","Tolerance (2)","Interrupt"))) %>%
  ggplot(aes(x=crit,y=value,linetype=Strategy,group=Strategy,shape=Strategy)) +
  geom_line() +
  geom_point() +
  facet_grid(metric~Trajectory,scales="free") +
  labs(title="Deaths and life-years lost relative to baseline (mean)") +
  labs(x="Age threshold (years)") +
  scale_y_continuous(labels=function(x) paste0(ifelse(x>0,paste0("+",x),x),"%"),limits=c(-15,15)) +
  scale_linetype_manual(values=c("solid","longdash","dotdash","dotted")) +
  theme(axis.title.y=element_blank(),
        plot.title=element_text(size=12),
        plot.subtitle=element_text(size=12,face="italic"),
        legend.position="bottom"
  ) + 
  guides(fill=guide_legend(nrow=1))

pdf(paste0(getwd(),"/fig4.pdf"),width=6,height=4.5)
fig4
dev.off()

png(paste0(getwd(),"/fig4.png"),width=6,height=4.5,units="in",res=800)
fig4
dev.off()


####################################################################################################################
### FIGURE 5 ###

outp4a<-outp_agg %>%
  pivot_longer(cols=-c(scenario,policy,policy_param,crit,currency),names_to="metric",values_to="value") %>%
  pivot_wider(names_from=c("currency","metric"),values_from=value)

outp4b<-do.call("rbind",lapply(unique(outp4a$scenario),function(x) {
  toutp3<-outp4a[which(outp4a$scenario==x),]
  tmp1<-as.data.frame(toutp3[which(toutp3$crit=="baseline"),])
  tmp2<-toutp3[which(toutp3$crit!="baseline"),]
  tmp3<-data.frame(tmp2[,1:4])
  tmp4<-cbind(tmp3,
              data.frame(mean_dch_died=tmp2$mean_dch_died-tmp1$mean_dch_died),
              data.frame(mean_int_died=tmp2$mean_int_died-tmp1$mean_int_died),
              data.frame(mean_rej_died=tmp2$mean_rej_died-tmp1$mean_rej_died),
              data.frame(mean_tot_died=tmp2$mean_tot_died-tmp1$mean_tot_died),
              data.frame(mean_dch_died_ly=tmp2$mean_dch_died_ly-tmp1$mean_dch_died_ly),
              data.frame(mean_int_died_ly=tmp2$mean_int_died_ly-tmp1$mean_int_died_ly),
              data.frame(mean_rej_died_ly=tmp2$mean_rej_died_ly-tmp1$mean_rej_died_ly),
              data.frame(mean_tot_died_ly=tmp2$mean_tot_died_ly-tmp1$mean_tot_died_ly)
  )
  return(tmp4)
}))

ly_scaler<-50

fig5<-outp4b %>%
  filter(crit==60) %>%
  select(-crit) %>%
  mutate(Strategy=case_when(policy=="1" ~"Cut-off",
                            policy=="2" & policy_param=="2" ~"Tolerance (2)",
                            policy=="2" & policy_param=="3" ~"Tolerance (3)",
                            policy=="2" & policy_param=="4" ~"Tolerance (4)",
                            policy=="2" & policy_param=="6" ~"Tolerance (6)",
                            policy=="3" ~"Interrupt")) %>%
  rename(Trajectory=scenario) %>%
  mutate(Trajectory=firstup(as.character(Trajectory))) %>%
  select(-policy,-policy_param) %>%
  pivot_longer(cols=-c("Trajectory","Strategy"),names_to="metric",values_to="value") %>%
  mutate(type=case_when(grepl("mean_dch_died",metric) ~"Type 3",
                           grepl("mean_rej_died",metric) ~"Type 1",
                           grepl("mean_int_died",metric) ~"Type 2",
                           grepl("mean_tot_died",metric) ~"Total")) %>%
  mutate(metric=case_when(grepl("_ly",metric)==FALSE ~"Deaths",
                           grepl("_ly",metric)==TRUE ~"Life-years lost")) %>%
  mutate(type=factor(type,levels=c("Type 1","Type 2","Type 3","Total"))) %>%
  mutate(Trajectory=factor(Trajectory,levels=c("Unmitigated","Lockdown","Cyclical"))) %>%
  mutate(Strategy=factor(Strategy,levels=c("Cut-off","Tolerance (6)","Tolerance (4)","Tolerance (3)","Tolerance (2)","Interrupt"))) %>%
  rename(Metric=metric) %>%
  mutate(Metric=factor(Metric,levels=c("Deaths","Life-years lost"))) %>%
  mutate(value=case_when(Metric=="Deaths" ~ value,
         Metric=="Life-years lost" ~ value/ly_scaler)) %>%
  mutate(Metric=recode(Metric,'Deaths'='Deaths','Life-years lost'=paste0("Life-years lost (÷",ly_scaler,")"))) %>%
  ggplot(aes(x=type,y=value,fill=Metric)) +
  geom_bar(stat="identity",position="dodge",colour="black",alpha=0.6) +
  scale_fill_grey(start=0.75,end=0.2) +
  facet_grid(Trajectory~Strategy) +
  labs(title="Deaths and life-years lost relative to baseline (mean)",subtitle="Age threshold at 60 years") +
  scale_y_continuous(labels=function(x) ifelse(x>0,paste0("+",x),x)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(size=12),
        plot.subtitle=element_text(size=12,face="italic"),
        legend.position="bottom",
        axis.text.x=element_text(angle=45,vjust=0.5,hjust=1),
        plot.margin=unit(c(5.5, 5.5, 12, 5.5), "points")
  )

pdf(paste0(getwd(),"/fig5.pdf"),width=8,height=8)
fig5
dev.off()

png(paste0(getwd(),"/fig5.png"),width=8,height=8,units="in",res=800)
fig5
dev.off()


####################################################################################################################
### FIGURE 6 ###

s.out<-outp_raw %>%
  ungroup() %>%
  filter(scenario=="lockdown",crit %in% c("baseline","60")) %>%
  pivot_longer(cols=-c(scenario, policy, policy_param,crit,dates,group,currency,type),names_to="metric",values_to="value") %>%
  rename(Age=group) %>%
  mutate(Age=case_when(Age=="age16to39" ~"16-39",
                       Age=="age40to49" ~"40-49",
                       Age=="age50to59" ~"50-59",
                       Age=="age60to69" ~"60-69",
                       Age=="age70to79" ~"70-79",
                       Age=="age80plus" ~"80+")) %>%
  mutate(Strategy=case_when(crit=="baseline" ~"Baseline",
                          policy=="1" ~"Cut-off",
                          policy=="2" & policy_param=="2" ~"Tolerance (2)",
                          policy=="2" & policy_param=="3" ~"Tolerance (3)",
                          policy=="2" & policy_param=="4" ~"Tolerance (4)",
                          policy=="2" & policy_param=="6" ~"Tolerance (6)",
                          policy=="3" ~"Interrupt")) %>%
  mutate(Strategy=factor(Strategy,levels=c("Baseline","Cut-off","Tolerance (6)","Tolerance (3)","Interrupt"))) %>%
  rename(Trajectory=scenario) %>%
  mutate(Trajectory=firstup(as.character(Trajectory))) %>%
  select(-policy,-policy_param) %>%
  filter(currency=="mean") %>%
  select(-currency) %>%
  filter({type=="value" & metric=="occupancy"} | {type=="value_cum" & metric %in% c("admitted","dch_died","rej_died","int_died","tot_died")}) %>%
  select(-type) %>%
  mutate(metric=case_when(metric=="occupancy" ~"Bed occupancy",
                          metric=="admitted" ~"Admissions",
                          metric=="dch_died" ~"Type 3 deaths",
                          metric=="rej_died" ~"Type 1 deaths",
                          metric=="int_died" ~"Type 2 deaths",
                          metric=="tot_died" ~"Total deaths")) %>%
  mutate(metric=factor(metric,levels=c("Bed occupancy","Admissions","Type 1 deaths","Type 2 deaths","Type 3 deaths","Total deaths")))
  

fig6<-s.out %>%
  ggplot(aes(x=dates,y=value)) +
  geom_area(aes(fill=Age),linetype=1,size=0.5,colour="black",alpha=0.6) +
  scale_fill_grey(start=0.95,end=0) +
  facet_grid(metric~Strategy,scales="free") +
  labs(x="Day") +
  labs(title="Bed occupancy, admissions, and deaths by type (mean, stacked)",subtitle="Lockdown trajectory with age threshold at 60 years") +
  theme(axis.title.y=element_blank(),
        plot.title=element_text(size=12),
        plot.subtitle=element_text(size=12,face="italic"),
        legend.position="bottom",
        axis.text.x=element_text(angle=45,vjust=0.5,hjust=1)
        #legend.justification=c("right","top"),
        #legend.box.just="right",
        #legend.margin=margin(6,6,6,6)
  ) + 
  guides(fill=guide_legend(nrow=1))

pdf(paste0(getwd(),"/fig6.pdf"),width=8,height=8)
fig6
dev.off()

png(paste0(getwd(),"/fig6.png"),width=8,height=8,units="in",res=800)
fig6
dev.off()





