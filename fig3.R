library("tidyverse")
library("ggplot2")
library("cowplot")

setwd("xxx")

firstup<-function(x) {
  substr(x,1,1)<-toupper(substr(x,1,1))
  x
}

dat<-read.csv("inputs-demand.csv") %>%
  rename(Trajectory=scenario) %>%
  mutate(Trajectory=firstup(as.character(Trajectory))) %>%
  mutate(Trajectory=factor(Trajectory,levels=c("Unmitigated","Lockdown","Cyclical")))

####################################################################################################################

dat1<-dat %>%
  group_by(Trajectory,dates) %>%
  summarise(mean=mean(value),lower=quantile(value,0.025),upper=quantile(value,0.975))

dat2<-dat1 %>%
  select(-lower,-upper)


fig3a<-dat2 %>%
  ggplot(aes(x=dates,linetype=Trajectory)) + 
  geom_ribbon(data=dat1,aes(ymin=lower,ymax=upper,fill=Trajectory),alpha=0.2) +
  scale_fill_grey(start=0,end=0.7) +
  geom_line(aes(y=mean,linetype=Trajectory)) +
  #ylab("Requiring admission per day") +
  xlab("Day") +
  labs(title="Projected daily demand for intensive care") +
  theme(axis.title.y=element_blank(),
        plot.title=element_text(size=11),
        #axis.text.x=element_text(angle=45,hjust=1),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        #text=element_text(family="serif"),
        legend.title=element_text(size=10)
  )  

####################################################################################################################

fig3b<-dat %>%
  group_by(Trajectory,iter) %>%
  summarise(value=sum(value)) %>%
  mutate(grps=cut(value,breaks=seq(0,2800,400),labels=c("0-399","400-799","800-1199","1200-1599","1600-1999","2000-2399","2400-2800"))) %>%
  select(-value) %>%
  group_by(Trajectory,grps,.drop=FALSE) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  select(-n) %>%
  ggplot(aes(x=grps,y=freq,fill=Trajectory)) +
  geom_bar(stat="identity",position="dodge",alpha=0.5) +
  scale_fill_grey(start=0,end=0.7) +
  ylab("Probability") +
  labs(title="Total intensive care demand over pandemic") +
  theme(axis.title.x=element_blank(),
        plot.title=element_text(size=11),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        #text=element_text(family="serif"),
        legend.title=element_text(size=10)
  )  



####################################################################################################################

pdf("fig3.pdf",height=3.5,width=7)
plot_grid(fig3a,fig3b,nrow=1)
dev.off()

png("fig3.png",height=3.5,width=7,units="in",res=800)
plot_grid(fig3a,fig3b,nrow=1)
dev.off()




