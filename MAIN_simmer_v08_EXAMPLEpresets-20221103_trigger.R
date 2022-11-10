## Date: 02/11/2022
## Overview: Discrete event simulation model for ambulance setting
## Author: Martina Fonseca, Digital Analytics and Research Team (DART)
## Stage: alpha / rapid prototype
## Current script: main script from which libraries are loaded, parameters set, runs made and results processed
## Dependencies: 01-packages.R, 03-replication_v08.R
## Called by:
##
################################################


############################################### ##
######### PREPARATORY AND PRESETS ##############
############################################### ##

# Clear vars in environment ----------------------------------------------------------------
rm(list = ls())
set.seed(999)

# Source libraries ----------------------------------------------------------------
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)),   # Unload add-on packages
                 detach,
                 character.only = TRUE, unload = TRUE))# to not have e.g. bupaR mask select from simmer
source("01-packages.R")
source("03-replication_v08.R")

# Set batch run presets ----------------------------------------------------------------
nreps = 10 # number of replications
mydate="20221103_trigger_ppt" # name of Output subfolder
flag_resourcesaveplots=T # whether to save per batch res_mon plots (slows down considerably when writing to file)
flag_utilsaveplots = T # whether to save per batch util plots
flag_sitequeuesaveplots = T # whether to save site queueing plots
flag_patientpathplot = FALSE # plot(trajectory)
g.debug=0 # how many levels of depth of log detail to print. #0 for least.

ambu_day <- 180 #120 is typical daily avg per most affected trusts, but varies considerably.
n_days_warm <- 2 # days of warm-up
n_days_study <- 7 # days to observe
ndays <- n_days_warm + n_days_study # days of study (warm-up + observe)
g.tmin=0
g.tmax=24*60*ndays; # one day
g.tday = 24*60 # minutes in a day
flag_RRS <- TRUE # whether to allow for static
#RRS_protocol <- "schedule"
RRS_protocol <- "trigger"

# Create directory for results if necessary
if (!dir.exists(here::here("Output", mydate))){
  dir.create(here::here("Output", mydate))
}

# Schedule outline ----------------------------------------------------------------

# Schedule 8 to 16 h
#          16 to 24 h
#          24 to 8 h
# Schedule D2 8 to 16 h
#          16 to 24 h
#          24 to 8 h        
#t_sch = c(0, 8, 16, 24, 32, 40)*60 # time intervals
T_window_hours<- 8 # hours per window
n_windows <- ndays*24/T_window_hours # number windows (warm-up + observe)
n_warmup_windows <- n_days_warm*24/T_window_hours  # nr warm-up windows
t_sch = seq(0,(n_windows-1)*T_window_hours,T_window_hours)*60 # time intervals (6 - 2 days, 3 windows of 8 hours)
period_sch= Inf#24*60*2 # the periodicity of the schedule motif 
t_sch_ones = rep(1,n_windows)


# Resource Redeplyoment to Site Bays (RRS) Trigger specifics -------------------------------------------------------
trigger_avsitequeue <- 5 # time averaged site queue for most shift, that triggers Resource Redeployment to Site (RRS) incremental standing up
trigger_avcallqueue <- 10 # time averaged call queue (call to ambulance allocation), that triggers, together with site underutilisation, Resource Redeployment to Site (RRS) incremental standing down

# old approach. Not time averaged, instead sets threshold Y ambulances arriving in shift that would see a queue of X or more on arrival
no_ambulancesqueueingbreach <- 10  # no ambulances queueing in site for it to count as breach from new ambulance arrival perspective
no_arrivalbreaches <- 10 # trigger point: nr of arrivals in time window that observe queue length breach.


# Resource info -----------------------------------------------------------
n_ru0 <- 46  # 30 # number of staff (pool) - ideally twice of n_ru (assuming all DSA vehicles and desired full use)
n_ru <- t_sch_ones*n_ru0 # 30 # number of staff (pool) - ideally twice of n_ru (assuming all DSA vehicles and desired full use)
n_dsav0 <- 23 # 15  #number of double-staff vehicles (pool)
n_dsav <- t_sch_ones*n_dsav0 # 15  #number of double-staff vehicles (pool)
n_staticru <- 2 # number of staff for static (only used if flag_RRS is true)
c_bays_per_staticstaff <- 3 # bays per staff (indication of 4), twice that per double vehicle
n_mobile0 <- min(floor(n_ru0/2),n_dsav0)

# Define globals on job cycle time ----------------------------------------------------------
#unit minute
g.T_allocatetomobilise=0 # Time period from allocate to mobilise. Assume most of 1/5*25 above is from call to allocate, not allocate to mobilise. Allow for 2 minutes.
g.T_traveltoscene <- 22# Time period to travel to scene. 22 to match travel to hospital. Under assumption that ambulances can't travel from their optimal spot. All pivot back and forth from site.
g.T_atscene <- 36 # Time period at scene.
#g.T_atscene_TRI <- c(0,95,20) # min, max, mode
#g.T_atscene <- rtri(10000,min=g.T_atscene_TRI[1],g.T_atscene_TRI[2],g.T_atscene_TRI[3]) %>% mean()
g.T_traveltohospital <- 22 # Time period to travel to hospital site.
g.T_prehandover_LN <- c(3.218467,1.414410,10) # Time period for pre-handover. Define the logmean, logsd, offset
g.T_clear=5

# Auxiliary calcs on job cycle time ----------------------------------------------------------
(g.T_prehandover_LN_median <- exp(g.T_prehandover_LN[1]) + g.T_prehandover_LN[3])  #Time period for pre-handover. Median from distribution
(g.T_prehandover_LN_mean <- exp(g.T_prehandover_LN[1]+g.T_prehandover_LN[2]^2/2)+ g.T_prehandover_LN[3]) #Time period for pre-handover. Mean of distribution
#exp(g.T_prehandover_LN[1]-g.T_prehandover_LN[2]^2)+ g.T_prehandover_LN[3] # Time period for pre-handover. Mode from distribution
ph_sample <- rlnorm(100000,g.T_prehandover_LN[1],g.T_prehandover_LN[2])+g.T_prehandover_LN[3] # Time period for pre-handover. Draw from distribution
ph_sample %>% summary()

# Levers / scenarios . What to vary over runs - initialise results matrix ----------------------

flag_scenarios <- FALSE
if (flag_scenarios){
  vec_prehandoverNAperc = c(0.1) # percentage of pre-handover that is not-avoidable by using RRS. To understand how RRS effect changes with decreasing portion of vehicle handover time avoided
  vec_JCToffsiteincrement = c(0) # additional JCT time. Added stylistically to 'at scene' time . To understand trade-off of %JCT off-site on effectiviness of standing down DSA vehicle for RRS.
  vec_demand = c(170,180,190)
  vec_RRS = c(FALSE,TRUE)
} else{
  vec_prehandoverNAperc = c(0.1) # percentage of pre-handover that is not-avoidable by using RRS. To understand how RRS effect changes with decreasing portion of vehicle handover time avoided
  vec_JCToffsiteincrement = c(0)
  vec_demand = c(ambu_day)
  vec_RRS = (flag_RRS)
}

n_prehandoverNAperc = length(vec_prehandoverNAperc)
n_JCToffsiteincrement = length(vec_JCToffsiteincrement)
n_demand <- length(vec_demand)
n_RRS <- length(vec_RRS)
n_scenarios <- n_prehandoverNAperc * n_JCToffsiteincrement * n_demand * n_RRS

names_levers <- c("demand","prehandoverNAperc","JCToffsiteincrement","RRS")
df_scenarios <-expand.grid(vec_demand,vec_prehandoverNAperc,vec_JCToffsiteincrement,vec_RRS)
colnames(df_scenarios) <- names_levers
df_scenarios <- df_scenarios %>% mutate(id=row_number())
rownames(df_scenarios) <- 1:nrow(df_scenarios)

#scenarioids_toplot <- c(1,2,3)
scenarioids_toplot <- df_scenarios$id

dff_rc=data.frame() # attributes log initialised (all patients, all iterations, all parameter variation batch runs)
dfr_rc=data.frame() # resources log initialised (all event timepoints, all iterations, all parameter variation batch runs)


############################################################################################ ##
######### RUN AND PER-BATCH POST-PROCESSING - Scenarios x Batch of replications x Individual replication (DES) ##############
############################################################################################ ##

# Runs - including environment, pathway, generators ---------------------------------------------
# tba: possibly some of these can pulled out of the for loop.

for (myr in 1:nrow(df_scenarios)) { 
    
    row <- df_scenarios[myr,]
    id<-row$id
    # Employ
    now_prehandoverNAperc <- row$prehandoverNAperc
    now_JCToffsiteincrement <- row$JCToffsiteincrement
    now_demand <- row$demand
    flag_RRS <- row$RRS
    
    g.T_prehandoverNAperc = now_prehandoverNAperc # NA - portion of pre-handover that is non-avoidable (as %)
    g.T_atscene <- 36 + now_JCToffsiteincrement # idem - #make this lognormal/triangular?
    ambu_day <- now_demand #120 # typical daily avg per RRS trust is 120
    
    print(paste("Case where portion prehandover non-avoidable is",round(now_prehandoverNAperc*100,0),"% and JCT off-site incremented by ",now_JCToffsiteincrement,"minutes")) 

    
    # RRS deployment and resulting schedule ---------------------------------------------------------
    
    
    if (flag_RRS && RRS_protocol=="schedule"){
      n_staticru_init = n_staticru
    } else {
      n_staticru_init=0
      n_staticru = 2 # i.e. the increment
    }
    
    toRRS_sch <- c(rep(0,n_warmup_windows),c(rep(1,n_windows-n_warmup_windows)))*n_staticru_init # nr staff to site (from DSA) - per schedule above
    bays_sch <- toRRS_sch * c_bays_per_staticstaff # nr bays - per schedule
    n_ru_net <- n_ru - toRRS_sch # net staff resource units for DSA vehicle mobile
    mobile_sch <- pmin(n_dsav, floor(n_ru_net/2)) # number of DSA vehicles mobile - per schedule. Pairwise min. Smallest of no physical vehicles and floor of no available staff (assumes site doesn't need the physical vehicle)
    
    bays_schedule <- schedule(t_sch,bays_sch, period = period_sch) # nr bays
    mobile_schedule <- schedule(t_sch,mobile_sch,period = period_sch) # nr mobile vehicles
    
    
    # heuristic
    (JCT <- (g.T_allocatetomobilise + g.T_traveltoscene + g.T_atscene + g.T_traveltohospital + median(ph_sample) + g.T_clear))
    JCT_mu <-(g.T_allocatetomobilise + g.T_traveltoscene + g.T_atscene + g.T_traveltohospital + mean(ph_sample) + g.T_clear)
    pathours <- ambu_day * JCT
    pathours_mu <- ambu_day * JCT_mu
    ambhours <- sum(mobile_sch[1:3] * lead(t_sch)[1])
    paste0("Ratio patient hours to ambulance hours in day 1 is ",round(pathours/ambhours,2),"/n Excludes queueing for bays, queueing for allocation, uses median for pre-handover")
    paste0("Ratio patient hours to ambulance hours in day 1 is ",round(pathours_mu/ambhours,2),"/n Excludes queueing for bays, queueing for allocation, uses mean for pre-handover")
    
    
    # Run simulation (multiple runs) - not parallelised ----------------------------------------------------------
    CADS <- lapply(1:nreps, CAD_rep) # CAD_rep defined in 03-replication_v08.R
    #CADS <- CAD_rep(1)

    ### Post-processing - log of attributes ####
    log_attributes <- CADS %>% get_mon_attributes()
    dff <- as.data.frame(log_attributes) %>% mutate(id=id) # add identifiers of scenario
    dff_rc <- dff_rc %>% bind_rows(dff) # save all in common log
    
    ### Post-processing - log of resources ####
    log_resources <- CADS %>% get_mon_resources()
    dfr <- as.data.frame(log_resources) %>% mutate(id=id)
    dfr_rc <- dfr_rc %>% bind_rows(dfr) # save all in common log
    
    ### Post-processing - call queue, single scenario. Operations to give equal steps to average over ###
    log_callqueue <- dfr %>% filter(resource=="ambulance") %>% dplyr::select(time,queue,replication)
    my_step <- 120
    log_callqueue <- log_callqueue %>% mutate(step = (time %/% my_step)*my_step) %>% rename(value=queue)
    
    aux <- log_callqueue %>% group_by(replication,step) %>% summarise(value = sum(value*(time==max(time)))) %>%
      ungroup() %>% mutate(step=step+my_step,
                           time=step)
    log_callqueue_step <- log_callqueue %>% bind_rows(aux) %>% arrange(replication,step,time)
    
    log_callqueue_avstep <- log_callqueue_step %>% group_by(replication) %>%
      mutate(dtime = lead(time)-time) %>%
      group_by(replication,step) %>%
      summarise(queue = sum(dtime*value,na.rm=T)/sum(dtime,na.rm=T)) %>%
      ungroup()
    
    p_cq <- ggplot(data = log_callqueue_avstep, aes(x=factor(step),y=queue)) +
      geom_boxplot() +
      labs(title="Calls queued - non allocated (across replications)",x="Time (days)",y="Nr (time window avg)")+
      theme(axis.text.x = element_text(angle = 0),text = element_text(size=10))+
      scale_x_discrete(breaks = round(seq(min(log_callqueue_avstep$step), max(log_callqueue_avstep$step), by = 1440),1),
                       labels = seq(0,9,1));p_cq
    
    if (flag_resourcesaveplots && id %in% scenarioids_toplot){
    
      ggsave(here::here("Output",mydate,paste0("callqueueplot",mydate,"_id",id,".png")),
             plot = p_cq,
             unit="cm",
             dpi=300,
             width=20,
             height=20)
      
      ggsave(here::here("Output",mydate,paste0("callqueueplot",mydate,"_id",id,".svg")),
             plot = p_cq,
             unit="cm",
             dpi=300,
             width=20,
             height=20)
      
      ggplot(data = log_callqueue, aes(x=time,y=value)) + geom_line(aes(group=replication))
    }
    
    ### Post-processing - site queue, single scenario. Operations to give equal steps to average over ###
    log_sitequeue <- dff %>% filter(key=="site_queue") %>% dplyr::select(id,time,value,replication)
    my_step <- 240
    log_sitequeue <- log_sitequeue %>% mutate(step = (time %/% my_step)*my_step)
    
    aux <- log_sitequeue %>% group_by(id,replication,step) %>% summarise(value = sum(value*(time==max(time)))) %>%
      ungroup() %>% mutate(step=step+my_step,
                           time=step)
    aux2 <- log_sitequeue %>% group_by(id,replication) %>% summarise(step=0,
                                                                  value=0,
                                                                  time=0) %>% ungroup()
    log_sitequeue_step <- log_sitequeue %>% bind_rows(aux) %>% bind_rows(aux2) %>% arrange(id,replication,step,time)
    log_sitequeue_avstep <- log_sitequeue_step %>% group_by(replication) %>%
      mutate(dtime = lead(time)-time) %>%
      group_by(id,replication,step) %>%
      summarise(sitequeue = sum(dtime*value,na.rm=T)/sum(dtime,na.rm=T)) %>%
      ungroup()
    
    p_sq <- ggplot(data = log_sitequeue_avstep, aes(x=factor(step),y=sitequeue)) +
      geom_boxplot() +
      facet_wrap(.~id) +
      labs(title="Ambulances queueing at site over time (across replications)",x="Time (days)",y="Nr (time window avg)")+
      theme(axis.text.x = element_text(angle = 0),text = element_text(size=10))+
      scale_x_discrete(breaks = round(seq(min(log_callqueue_avstep$step), max(log_callqueue_avstep$step), by = 1440),1),
                   labels = seq(0,9,1));p_sq
    
    if (flag_sitequeuesaveplots && id %in% scenarioids_toplot){
      
      ggsave(here::here("Output",mydate,paste0("sitequeueplot",mydate,"_id",id,".png")),
             plot=p_sq,
             unit="cm",
             dpi=300,
             width=20,
             height=20)
      
      ggsave(here::here("Output",mydate,paste0("sitequeueplot",mydate,"_id",id,".svg")),
             plot=p_sq,
             unit="cm",
             dpi=300,
             width=20,
             height=20)
      
      ggplot(data = log_sitequeue, aes(x=time,y=value)) + geom_line(aes(group=replication))
    }
    
    ### Post-processing - plots per batch
    
    ### Post-processing - plot ###
   
    # new way
    p<-plot(get_mon_resources(CADS), #%>% filter(time>24*60),
            metric = "usage",
            c("ambulance","bay","bed"),
            items = c("queue","server"),
            steps=FALSE)#+
      #labs(subtitle = paste0("% pre-handover unavoidable: ",now_prehandoverNAperc*100,"%\n Increment to off-site JCT (min): ",now_JCToffsiteincrement));
    p
    if (flag_resourcesaveplots && id %in% scenarioids_toplot){
      
      ggsave(here::here("Output",mydate,paste0("resplot",mydate,"_id",id,".png")),
             plot=p,
             unit="cm",
             dpi=300,
             width=20,
             height=20)
      
      ggsave(here::here("Output",mydate,paste0("resplot",mydate,"_id",id,".svg")),
             plot=p,
             unit="cm",
             dpi=300,
             width=20,
             height=20)
    }
    
    p_u <- plot(CADS %>% get_mon_resources(), metric="utilization") # latest only , incl warmup
    if (flag_utilsaveplots && id %in% scenarioids_toplot){
      
      ggsave(here::here("Output",mydate,paste0("utilmu_",mydate,"_id",id,".png")),
             plot = p_u,
             unit="cm",
             dpi=300,
             width=20,
             height=20) 
      
      ggsave(here::here("Output",mydate,paste0("utilmu_",mydate,"_id",id,".svg")),
             plot = p_u,
             unit="cm",
             dpi=300,
             width=20,
             height=20)  
    
  }
}


################################################################## ##
###### CROSS-SCENARIO POST-PROCESSING, VISUALISATION AND SAVING #####
################################################################## ##


### High-level plots (last scenario)  --------------------------------------------------------------

p_u # plotting utilisation (%)
p # plotting resource usage
p_sq # plotting site queueing
p_cq # plotting call queueing


### Parameters used, for reference  --------------------------------------------------------------

# A table recalling schedule (for Markdown)
df_sch <- data.frame(time=mobile_schedule$timetable,
                        mobile_ambulances=mobile_schedule$values,
                        RRS_bays=bays_schedule$values)

# A table recalling presets (for Markdown)
df_presets <- tribble(
  ~Parameter, ~value,
  "Warm-up time (days)",n_days_warm,
  "Study time (days)",n_days_study,
  "Period - travel to scene (min)",g.T_traveltoscene,
  "Period - time at scene (min)",g.T_atscene,
  "Period - travel to hospital (min)",g.T_traveltohospital,
  "Period - hospital arrival to handover - median (min)",g.T_prehandover_LN_median,
  "Period - hospital arrival to handover - mean (min)",g.T_prehandover_LN_mean,
  "Period - time to clear (min)",g.T_clear,
  "Bays per static staff",c_bays_per_staticstaff,
  "Double-staffed vehicles",max(n_dsav)
)

# A list recalling presets (for Markdown)
df_params <- list()
df_params$df_presets <- df_presets
df_params$df_sch <- df_sch

### Save the raw post-processing logs  --------------------------------------------------------------

saveRDS(dff_rc,here::here("Output", mydate,"attribs_rc.RDS"))
saveRDS(dfr_rc,here::here("Output", mydate,"resources_rc.RDS"))
save(df_scenarios,file=here::here("Output",mydate,"df_scenarios.RData"))
save(df_params,file=here::here("Output",mydate,"df_params.RData"))

# Patient Attributes > KPIs per replication > KPIs per batch (scenario) --------------------------------------------------------------

# Log attributes - Pivot wider
log_attributes_l <- dff_rc %>%
  filter(name!="") %>% # to remove globals
  filter(substr(name,1,5)!="Contr") %>% # remove control vars
  dplyr::select(-time) %>% pivot_wider(names_from = key,values_from=value) # remember: select from simmer is masking select from tidyverse/dplyr
#View(log_attributes_l)

# Log attributes - Add further scenario id info
log_attributes_l <- log_attributes_l %>% left_join(df_scenarios,by=c("id"))

## Log attributes - Add primary metric (per patient)
log_attributes_l <- log_attributes_l %>% mutate(Kp_Tresponsetime=t_Rclockstop - t_Rclockstart,
                                                Kp_Tallocation = t_ambulanceseized - t_Rclockstart)

## Log attributes - Calculate run-level summary for metrics . Filter to exclude warm-up
v_quantile <- function(vector,probs=c(0.25,0.5,0.75)){ # function to obtain quantiles as dataframe
  data.frame(as.list(quantile(vector,probs,na.rm=T)))
}  

# Summary KPI per replication (across batch x scenarios)
log_attributes_l_runsum_l <- log_attributes_l %>%
  filter(t_Rclockstart > n_days_warm*g.tday) %>% 
  group_by(id,replication) %>%
  summarise(
    quant_n = c(names(quantile(Kp_Tresponsetime,c(0.25,0.5,0.75,0.9),na.rm=T)),"mean"),
    quant=c(quantile(Kp_Tresponsetime,c(0.25,0.5,0.75,0.9),na.rm=T),mean(Kp_Tresponsetime,na.rm=T)))

# Summary KPI per batch (across scenarios)
log_attributes_l_runsum_l_batch <- log_attributes_l_runsum_l %>%
  group_by(id,quant_n) %>% 
  #mutate(prehandoverNAperc = factor(prehandoverNAperc),
  #       JCToffsiteincrement = factor(JCToffsiteincrement)) %>% 
  summarise(Kp_Tresponsetime_Rq_Bmu=mean(quant,na.rm=T)) %>% 
  ungroup()

## Plot per-replication KPIs into per batch distribution
ggplot(data=log_attributes_l_runsum_l) +
  geom_boxplot(aes(x=quant_n , y = quant)) +
  facet_wrap(.~id)+
  labs(x="Call response time Replication KPI", y="Distribution across replications")

## Save batch KPI
save(log_attributes_l_runsum_l_batch,file=here::here("Output",mydate,"log_attributes_l_runsum_l_batch.RData"))

# Save plot
ggsave(here::here("Output",mydate,paste0("responsetimeplot",mydate,"_id",id,".png")),
       unit="cm",
       dpi=300,
       width=20,
       height=20)


# Log attributes - per-run KPIs
# https://stackoverflow.com/questions/22240816/dplyr-summarise-with-multiple-return-values-from-a-single-function
log_attributes_l_runsum <- log_attributes_l %>%
  filter(t_Rclockstart > n_days_warm*g.tday) %>% 
  group_by(prehandoverNAperc,JCToffsiteincrement,replication) %>%
  summarise(quant=v_quantile(Kp_Tresponsetime,c(0.25,0.5,0.75,0.9)),
            desc=psych::describe(Kp_Tresponsetime,na.rm=T)) %>% 
  unpack(cols = c(quant,desc))

## Log attributes - per-batch level KPIs
log_attributes_l_runsum_batch <- log_attributes_l_runsum %>%
  group_by(prehandoverNAperc,JCToffsiteincrement) %>% 
  mutate(prehandoverNAperc = factor(prehandoverNAperc),
         JCToffsiteincrement = factor(JCToffsiteincrement)) %>% 
  summarise(Kp_Tresponsetime_Rq50_Bmu = mean(X50.),
            Kp_Tresponsetime_Rq75_Bmu = mean(X75.),
            Kp_Tresponsetime_Rq90_Bmu = mean(X90.)) %>% 
  ungroup()

# Resources --------------------------------------------------------------

      
# Log attributes - Add further scenario id info
dfr_rc <-  dfr_rc %>% left_join(df_scenarios,by=c("id"))      
      
      
### Resource: utilisation ##
resources_runsum <- dfr_rc %>%
  group_by(id,JCToffsiteincrement, prehandoverNAperc,resource,replication) %>%
  filter(time > n_days_warm*g.tday) %>% 
  mutate(dtime = lead(time)-time) %>%
  summarise(utilization = sum(dtime*server,na.rm=T)/sum(dtime*capacity,na.rm=T)*100,
            queue = sum(dtime*queue,na.rm=T)/sum(dtime,na.rm=T)) %>%
  summarise(mean_util=mean(utilization),sd_util=sd(utilization),mean_queue=mean(queue),sd_queue=sd(queue))


### Distribution of response times per scenario/batcj. Flatten replications and patients ###

mymsg <- paste0(max(n_dsav)-n_staticru,"active ambulances,\n ",
                n_staticru*c_bays_per_staticstaff," bays staffed")

p <- ggplot(data=log_attributes_l %>%  filter(t_Rclockstart > n_days_warm*g.tday),
            aes(x=Kp_Tresponsetime)) +
  geom_histogram(aes(y=..density..),alpha=0.5,position="identity",binwidth=9,fill="lightblue",colour="black")+
  #geom_density(alpha=0.4) +
  geom_vline(data=log_attributes_l_runsum_batch,aes(xintercept=Kp_Tresponsetime_Rq50_Bmu),colour=c("blue"),linetype="dashed")+
  geom_vline(data=log_attributes_l_runsum_batch,aes(xintercept=Kp_Tresponsetime_Rq75_Bmu),colour=c("red"),linetype="dashed")+
  #facet_wrap(. ~ id,scales="free_y",ncol=3)+
  facet_grid(RRS ~ demand,scales="free_y")+
  xlim(0, 120)+
  labs(x="Response time in minutes (across patients and replications)",
       caption="Individual replication 50%ile (blue) and 75%ile (red) - averaged across across replications.",
       title="Distribution of response times - across patients and replications");p

ggsave(here::here("Output",mydate,paste0("Rq_hist_",mydate,".png")),
       unit="cm",
       dpi=300,
       width=20,
       height=20)  

