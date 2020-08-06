library("tidyverse")
library("ggplot2")
library("cowplot")

setwd("xxx")

firstup<-function(x) {
  substr(x,1,1)<-toupper(substr(x,1,1))
  x
}

dat<-do.call("rbind",lapply(seq(10,200,10), function(x) {
  tdat1<-read.csv(paste0(getwd(),"/final results/capacity - fig7/zresults_cap",x,"/outp_agg.csv"))
  tdat1$cap<-x
  tdat2<-read.csv(paste0(getwd(),"/final results/capacity - fig7/zresults_lockdown_cap",x,"/outp_agg.csv"))
  tdat2$cap<-x
  tdat<-rbind(tdat1,tdat2)
  return(tdat)
}))

fig7<-dat %>%
  filter(policy==3) %>%
  filter(currency=="mean") %>%
  filter(metric %in% c("dch_died","dch_died_ly","int_died","int_died_ly",
                       "rej_died","rej_died_ly")) %>%
  select(-c(policy,policy_param,currency,crit)) %>%
  mutate(type=case_when(grepl("dch_died",metric) ~"Type 3",
                        grepl("rej_died",metric) ~"Type 1",
                        grepl("int_died",metric) ~"Type 2",
                        grepl("tot_died",metric) ~"Total")) %>%
  mutate(metric=case_when(grepl("_ly",metric)==FALSE ~"Deaths",
                          grepl("_ly",metric)==TRUE ~"Life-years lost")) %>%
  mutate(metric=factor(metric,levels=c("Deaths","Life-years lost"))) %>%
  rename(Trajectory=scenario) %>%
  mutate(Trajectory=firstup(as.character(Trajectory))) %>%
  mutate(Trajectory=factor(Trajectory,levels=c("Unmitigated","Lockdown","Cyclical"))) %>%
  ggplot(aes(x=cap,y=value)) +
  geom_area(aes(fill=type),linetype=1,size=0.5,colour="black",alpha=0.6) +
  scale_fill_grey(start=0.75,end=0.25) +
  facet_grid(metric~Trajectory,scales="free") +
  geom_vline(xintercept=20,linetype="dashed",colour="darkgrey") +
  labs(title="Deaths and life-years lost (mean, stacked)",subtitle="Interrupt triage strategy with age threshold at 60 years") +
  labs(x="Number of intensive care beds") +
  scale_linetype_manual(values=c("solid","longdash","dotdash","dotted")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(axis.title.y=element_blank(),
        plot.title=element_text(size=12),
        plot.subtitle=element_text(size=12,face="italic"),
        legend.position="bottom",
        #legend.justification=c("right","top"),
        #legend.box.just="right",
        #legend.margin=margin(6,6,6,6)
  ) + 
  guides(fill=guide_legend(nrow=1)) +
  guides(fill=guide_legend(title="Type of death"))


pdf(paste0(getwd(),"/fig7.pdf"),width=6,height=4.5)
fig7
dev.off()

png(paste0(getwd(),"/fig7.png"),width=6,height=4.5,units="in",res=800)
fig7
dev.off()
  
  


