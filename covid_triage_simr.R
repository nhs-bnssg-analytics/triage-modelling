
####################################################################################################################################################################

library(parallel)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

####################################################################################################################################################################

covid_simr2<-function(name,cases,groups,los,cap,policy,policy_param,crit,nreps) {

cases<-cases %>%
  select(-scenario)

groups<-cbind(groups,crit)

####################################################################################################################################################################

simfn<-function(rep) {
  set.seed(rep)
  dmax<-length(unique(cases$dates))
  traj_rep<-sample(unique(cases$iter),1)
  fn1<-function(y){
    sapply(y,function(x) {
    q=abs(x-trunc(x))
    adj=sample(0:1,size=1,prob=c(1-q,q))
    if(x<0) adj=adj*-1
    trunc(x)+adj
    })
  }
  narr<-cases %>%
    filter(iter==traj_rep) %>%
    select(-iter) %>%
    rename(narr=value) %>%
    mutate(narr=fn1(narr))
  arr_times<-unlist(sapply(1:nrow(narr),function(x) {
    sort(runif(narr$narr[x],0,1)+narr$dates[x]-1)
  }))
  cal<-data.frame(id=1:length(arr_times),group=sample(groups$group,size=length(arr_times),replace=TRUE,prob=groups$demand_props),time=arr_times,event="arrival")
  tx<-0
  res<-expand_grid(time=1:round(dmax*1.33),
    group=groups$group,metric=c("occupancy","admitted",
    "rej_died","rej_surv","dch_died","dch_surv","int_died","int_surv","tot_died","tot_surv")) %>%
    mutate(value=ifelse(metric=="occupancy",NA,0))
  occ<-rep(0,nrow(groups))  #occupancy (number in unit)
  while (nrow(cal)>0 & tx<max(res$time)) {
    ind<-which(cal$time>tx & cal$event %in% c("arrival","endsrv-died","endsrv-surv"))
    ind<-ind[which.min(cal$time[ind])][1]
    id<-cal$id[ind]
    grp<-cal$group[ind]
    grp_ind<-which(groups$group==grp)
    event<-cal$event[ind]
    tx_old<-tx
    occ_old<-occ
    tx<-cal$time[ind]
    tx_day<-ceiling(tx)
    if (tx_day>max(res$time)) break
    if (event=="arrival") {
      #1. Arrival-admission: if there is a free bed, they are admitted to the hospital
      if(policy==1) {
        condition<-sum(occ)<cap & groups$crit[which(groups$group==grp)]
      } else if (policy==2) {
        condition<-sum(occ)<(cap-policy_param) | {groups$crit[which(groups$group==grp)] & sum(occ)<cap}
      } else if (policy==3) {
        if (sum(occ)<cap) {
          # there is spare capacity: admit regardless
          condition<-TRUE
        } else if (groups$crit[which(groups$group==grp)] & sum(occ[groups$crit==FALSE])>0) {
          # there is no spare capacity, and arrival meets criteria and someone in service doesn't: interrupt service and reject
          id_adm<-unique(cal$id[which(cal$time<tx)])
          id_adm_suitable<-unique(cal$id[which({cal$id %in% id_adm} & {cal$group %in% groups$group[which(groups$crit==FALSE)]})])
          id_int<-sample(as.list(id_adm_suitable),1)[[1]]
          grp_int<-unique(cal$group[which(cal$id==id_int)])
          grp_int_ind<-which(groups$group==grp_int)
          cal<-cal[-which(cal$id==id_int),]
          occ[grp_int_ind]<-occ[grp_int_ind]-1
          if (runif(1,0,1)<groups$pfat_rej[which(groups$group==grp_int)]) {
            # interruption-died
            res_ind<-which(res$time==tx_day & res$group==grp_int & res$metric=="int_died")
            res$value[res_ind]<-res$value[res_ind]+1     
            res_ind<-which(res$time==tx_day & res$group==grp_int & res$metric=="tot_died")
            res$value[res_ind]<-res$value[res_ind]+1  
          } else {
            # interruption-survived
            res_ind<-which(res$time==tx_day & res$group==grp_int & res$metric=="int_surv")
            res$value[res_ind]<-res$value[res_ind]+1   
            res_ind<-which(res$time==tx_day & res$group==grp_int & res$metric=="tot_surv")
            res$value[res_ind]<-res$value[res_ind]+1 
          }
          condition<-TRUE
        } else {
          # otherwise: reject
          condition<-FALSE
        }
        occ[which(groups$group==grp)]
      }
      if (condition==TRUE) {
        cal<-rbind(cal,data.frame(id=id,group=grp,time=tx,event="startsrv"))
        toutcome<-ifelse(runif(1,0,1)<groups$pfat_admit[which(groups$group==grp)],"died","surv")
        tlos<-do.call(paste0("r",los$los_dist[which(los$outcome==toutcome)]),
                     c(list(n=1),c(los$los_par1[which(los$outcome==toutcome)],los$los_par2[which(los$outcome==toutcome)])))
        cal<-rbind(cal,data.frame(id=id,group=grp,time=tx+tlos,event=paste0("endsrv-",toutcome)))
        occ[grp_ind]<-occ[grp_ind]+1
        res_ind<-which(res$time==tx_day & res$group==grp & res$metric=="admitted")
        res$value[res_ind]<-res$value[res_ind]+1
      } else {
        ##2. Arrival-rejection: There is not a free bed, so the patient is rejected
        cal<-cal[-which(cal$id==id),]
        if (runif(1,0,1)<groups$pfat_rej[which(groups$group==grp)]) {
          #2.1 Arrival-rejection-died
          res_ind<-which(res$time==tx_day & res$group==grp & res$metric=="rej_died")
          res$value[res_ind]<-res$value[res_ind]+1  
          res_ind<-which(res$time==tx_day & res$group==grp & res$metric=="tot_died")
          res$value[res_ind]<-res$value[res_ind]+1 
        } else {
          #2.2 Arrival-rejection-survived
          res_ind<-which(res$time==tx_day & res$group==grp & res$metric=="rej_surv")
          res$value[res_ind]<-res$value[res_ind]+1   
          res_ind<-which(res$time==tx_day & res$group==grp & res$metric=="tot_surv")
          res$value[res_ind]<-res$value[res_ind]+1 
        }
      }
      #3. End of service
    } else if (event=="endsrv-died") {
      cal<-cal[-which(cal$id==id),]  
      occ[grp_ind]<-occ[grp_ind]-1
      res_ind<-which(res$time==tx_day & res$group==grp & res$metric=="dch_died")
      res$value[res_ind]<-res$value[res_ind]+1  
      res_ind<-which(res$time==tx_day & res$group==grp & res$metric=="tot_died")
      res$value[res_ind]<-res$value[res_ind]+1 
    } else if (event=="endsrv-surv") {
      cal<-cal[-which(cal$id==id),]  
      occ[grp_ind]<-occ[grp_ind]-1
      res_ind<-which(res$time==tx_day & res$group==grp & res$metric=="dch_surv")
      res$value[res_ind]<-res$value[res_ind]+1   
      res_ind<-which(res$time==tx_day & res$group==grp & res$metric=="tot_surv")
      res$value[res_ind]<-res$value[res_ind]+1  
    }
    cal<-cal[order(cal$time),]
    res_ind<-which(res$time==tx_day & res$group==grp & res$metric=="occupancy")
    wt_new<-(tx-tx_old)/tx
    res$value[res_ind]<-ifelse(is.na(res$value[res_ind]),(tx-floor(tx))*occ_old[grp_ind]+(ceiling(tx)-tx)*occ[grp_ind],wt_new*occ[grp_ind]+(1-wt_new)*res$value[res_ind])
  }
  res<-res %>%
    mutate(value=ifelse(time==1 & metric=="occupancy" & is.na(value),0,value)) %>%
    group_by(group,metric) %>%
    fill(value) %>%
    mutate(rep=rep)
  return(res)
}

cl<-makeCluster(detectCores()-1)
clusterExport(cl=cl,varlist=c("cases","groups","los","cap","policy","policy_param"),envir=environment())
clusterEvalQ(cl=cl,c(library(tidyr),library(dplyr)))
RES<-parLapply(cl,1:nreps,simfn)
stopCluster(cl)

outputs_sim<-do.call("rbind",RES) %>%
  mutate(dates=min(cases$dates)+time-1) %>%
  select(-time)

#######################################

outputs_agg<-outputs_sim %>%
  left_join(.,groups,by="group") %>%
  select(-c("demand_props","pfat_rej","pfat_admit","crit")) %>%
  pivot_wider(names_from=metric,values_from=value) %>%
  #group_by(group,currency) %>%
  mutate(dch_died_ly=dch_died*lyrs_remain) %>%
  mutate(int_died_ly=int_died*lyrs_remain) %>%
  mutate(rej_died_ly=rej_died*lyrs_remain) %>%
  mutate(tot_died_ly=tot_died*lyrs_remain) %>%
  select(-lyrs_remain) %>%
  pivot_longer(names_to="metric",values_to="value",cols=-c(dates,group,rep)) %>%
  group_by(metric,rep) %>%
  summarise(value=sum(value)) %>%
  group_by(metric) %>%
  summarise(mean=mean(value),median=median(value),
            q25=quantile(value,0.25),
            q025=quantile(value,0.025),
            q75=quantile(value,0.75),
            q975=quantile(value,0.975),
  ) %>%
  pivot_longer(cols=-c(metric),names_to="currency",values_to="value")


outputs_raw1<-outputs_sim %>%
  group_by(dates,group,metric) %>%
  summarise(mean=mean(value),median=median(value),
            q25=quantile(value,0.25),
            q025=quantile(value,0.025),
            q75=quantile(value,0.75),
            q975=quantile(value,0.975),
            ) %>%
  pivot_longer(cols=-c(dates,group,metric),names_to="currency",values_to="value")


outputs_raw2<-outputs_sim %>%
  group_by(metric,rep,group) %>%
  mutate(value_cum=cumsum(value)) %>%
  select(-value) %>%
  group_by(dates,group,metric) %>%
  summarise(mean=mean(value_cum),
            median=median(value_cum),
            q25=quantile(value_cum,0.25),
            q025=quantile(value_cum,0.025),
            q75=quantile(value_cum,0.75),
            q975=quantile(value_cum,0.975),
            ) %>%
  pivot_longer(cols=-c(dates,group,metric),names_to="currency",values_to="value_cum") 

outputs_raw<-left_join(outputs_raw1,outputs_raw2) %>%
  pivot_longer(cols=c(value,value_cum),names_to="type",values_to="value") %>%
  left_join(.,groups,by="group") %>%
  select(-c("demand_props","pfat_rej","pfat_admit","crit")) %>%
  pivot_wider(names_from=metric,values_from=value) %>%
  group_by(group,currency) %>%
  mutate(dch_died_ly=dch_died*lyrs_remain) %>%
  mutate(int_died_ly=int_died*lyrs_remain) %>%
  mutate(rej_died_ly=rej_died*lyrs_remain) %>%
  mutate(tot_died_ly=tot_died*lyrs_remain) %>%
  select(-lyrs_remain) %>%
  pivot_longer(names_to="metric",values_to="value",cols=-c(dates,group,currency,type))



out<-list()
out[[1]]<-outputs_agg
out[[2]]<-outputs_raw


out<-lapply(out,function(x) {
  x %>%
    mutate(scenario=scenario) %>%
    mutate(policy=policy) %>%
    mutate(policy_param=policy_param) %>%
    mutate(crit=paste(crit,collapse="_")) %>%
    select(scenario,policy,policy_param,crit,everything())
})


return(out)
}



