## Date: 02/11/2022
## Overview: Discrete event simulation model for ambulance setting
## Author: Martina Fonseca, Digital Analytics and Research Team (DART)
## Stage: alpha / rapid prototype
## Current script: function that instantiates environment, pathways and processes. Sets the run. When used in wrapper can be used to enable multiple replications.
## Dependencies:
## Called by: MAIN_simmer_v08.R
##
################################################


#https://stackoverflow.com/questions/55364348/simmer-in-r-modelling-changes-in-server-capacity-based-on-queue-length-and-dura
#https://stackoverflow.com/questions/67089345/r-simmer-reallocate-resource-between-pots-based-on-queue-size
CAD_rep <- function(i){
  
  CAD <- simmer("CAD",log_level=g.debug)
  
  if (RRS_protocol=="schedule" | !flag_RRS){
    CAD <- CAD %>%
      add_resource("ambulance",mobile_schedule) %>%
      add_resource("bay",bays_schedule,queue_size=0) %>%
      add_resource("bed",1) # not used! Delay used for ED entry, not a queue.
  } else{
    CAD <- CAD %>%
      add_resource("ambulance",n_mobile0) %>%
      add_resource("bay",0,queue_size=0) %>%
      add_resource("bed",1) # not used! Delay used for ED entry, not a queue.
  }
  
  
  choice_arrivals = "Poisson" # poisson or custom or uniform
  #choice_arrivals = "Uniform"
  #choice_arrivals = "Custom"
  if (choice_arrivals =="Poisson"){
    patient_gen = function(){c(0,rexp(ambu_day*ndays,ambu_day/(24*60)),-1)}  # https://r-simmer.org/articles/simmer-04-bank-1.html
  } else if (choice_arrivals == "Uniform"){
    patient_gen = function(){(24*60)/(ambu_day)}
  } else if (choice_arrivals == "Custom"){
    patient_gen = at(seq(2,g.tmax,(24*60)/ambu_day))
    # patient_gen = at(c(30,50,101))
  }
  
  
  # Branch where patient goes direct to ED
  patient_directED <- trajectory() %>%
    log_("Direct AE",level=1) %>%
    set_attribute("RRS_use",function(){FALSE}) %>%
    timeout(function() get_attribute(CAD,"T_BNprehandover")) %>% 
    set_attribute("t_EDhandover", function(){now(CAD)}) %>%
    set_attribute("t_ambhandover", function(){now(CAD)}) %>%
    set_global("site_queue",-1,mod="+") %>% # track queue size outside ED, decrement by 1
    timeout(function() g.T_clear) %>%
    release("ambulance",1) %>% 
    set_attribute("t_ambclear", function(){now(CAD)})
  
  # Branch where patient goes via RRSBays
  patient_RRSBays <- trajectory() %>%
    #select(c("bay"),"first-available") %>% 
    select(c("bay")) %>%
    seize_selected(1,continue=FALSE,reject=patient_directED) %>%
    log_("Bay grabbed",level=1) %>%
    set_attribute("RRS_use",function(){TRUE}) %>%
    set_global("site_queue",-1,mod="+") %>% # track queue size outside ED, decrement by 1
    set_attribute("t_ambhandover", function(){now(CAD)}) %>%
    branch(
      function() ((g.T_clear < get_attribute(CAD,"T_BNprehandover")))+1,
      continue=c(T,T),
      trajectory() %>%
        timeout(function() get_attribute(CAD,"T_BNprehandover")) %>%
        set_attribute("t_EDhandover", function(){now(CAD)}) %>%
        log_("AE via bay",level=1) %>%
        release_selected(1) %>%
        timeout(function() g.T_clear - get_attribute(CAD,"T_BNprehandover")) %>%
        release("ambulance",1) %>% 
        set_attribute("t_ambclear", function(){now(CAD)}),
      trajectory() %>%
        timeout(function() g.T_clear) %>%
        release("ambulance",1) %>% 
        set_attribute("t_ambclear", function(){now(CAD)}) %>% 
        timeout(function() get_attribute(CAD,"T_BNprehandover") - g.T_clear) %>%
        release_selected(1) %>%
        set_attribute("t_EDhandover", function(){now(CAD)}) %>%
        log_("AE via bay",level=1)
    )
  
  ### trajectory ###
  patient <-
    trajectory("Patient path") %>%
    log_("Patient waiting to be assigned",level=1) %>%
    set_attribute("t_Rclockstart", function(){now(CAD)}) %>%
    seize("ambulance",1) %>% # Q...
    set_attribute("t_ambulanceseized", function(){now(CAD)}) %>%
    log_(function(){paste("Allocated after: ",round(now(CAD) - get_attribute(CAD,"t_Rclockstart"),1))},level=2) %>% 
    timeout(g.T_allocatetomobilise + g.T_traveltoscene) %>%
    set_attribute("t_Rclockstop", function(){now(CAD)}) %>%
    log_(function(){paste("Clock-stop: ",round(now(CAD) - get_attribute(CAD,"t_clockstart"),))},level=2) %>% 
    #timeout(function() {rtri(1,min=g.T_atscene_TRI[1],g.T_atscene_TRI[2],g.T_atscene_TRI[3]) + g.T_traveltohospital}) %>%
    timeout(function() {g.T_atscene + g.T_traveltohospital}) %>%
    set_attribute("t_hospitalarrival", function(){now(CAD)}) %>%
    log_("Hospital arrival",level=1) %>%
    set_attribute("T_prehandover", function() rlnorm(1,g.T_prehandover_LN[1],g.T_prehandover_LN[2])+g.T_prehandover_LN[3]) %>%
    set_attribute("T_NAprehandover", function() {get_attribute(CAD,"T_prehandover")*g.T_prehandoverNAperc}) %>% 
    set_attribute("T_BNprehandover", function() {get_attribute(CAD,"T_prehandover")*(1-g.T_prehandoverNAperc)}) %>% 
    #set_attribute("T_NAprehandover", function() rexp(1,1/(g.T_NAprehandover))) %>%  # NA - non-avoidable; BN - bottle neck
    #set_attribute("T_BNprehandover", function() rexp(1,1/(g.T_prehandover-g.T_NAprehandover))) %>% 
    log_(function(){paste0("T_prehandover: ",get_capacity(CAD, "bay"))},level=2) %>%
    log_(function(){paste0("Period - prehandover: ",get_attribute(CAD,"T_prehandover"))},level=2) %>%
    log_(function(){paste0("Period - non avoidable prehandover: ",get_attribute(CAD,"T_NAprehandover"))},level=2) %>%
    log_(function(){paste0("Period - bottle neck prehandover: ",get_attribute(CAD,"T_BNprehandover"))},level=2) %>%
    set_global("site_queue",1,mod="+") %>% # track queue size outside ED, increment by 1
    set_global("events10pinshift",
               function(){
                 ifelse(get_global(CAD,"site_queue")>=no_ambulancesqueueingbreach,1,0)
               },
               mod="+"
                 ) %>%
    timeout(function() get_attribute(CAD,"T_NAprehandover")) %>%
    branch(
      function() {
        #RRSBays_full <- get_server_count(CAD, "bay") >= get_capacity(CAD, "bay") # check if RRSBays is full. If so, go 'direct to ED' (no RRSBays queueing)
        RRSBays_full = FALSE
        (flag_RRS&&get_capacity(CAD, "bay")>0 && !RRSBays_full )+1
        }, # If RRSBays on and if RRSBays capacity positive
      continue=c(T,T),
      patient_directED,
      patient_RRSBays
    )
  
  # KPIs for performance over shift that then dictate triggers (up/down) use.
  control_q_KPIS <- trajectory() %>%
    set_attribute(c("K_RRSBaysutilisation","K_RRSBaysqueue","K_RRSBaysserver","Kflag_RRSBaysunderutil","Kflag_callexcess","Kroute_triggerdown","Ksite_queue"), function() {
      mon_res <- get_mon_resources(CAD)
      mon_res <- mon_res %>%
        filter(resource=="bay") %>%
        filter(time>=(now(CAD)%/% (T_window_hours*60))*(T_window_hours*60)) %>%
        mutate(dtime = lead(time)-time) %>% 
        summarise(utilization = sum(dtime*server,na.rm=T)/sum(dtime*capacity,na.rm=T)*100,
                  server = sum(dtime*server,na.rm=T)/sum(dtime,na.rm=T),
                  queue = sum(dtime*queue,na.rm=T)/sum(dtime,na.rm=T))
      under_util <- mon_res$server < (get_capacity(CAD,"bay")-n_staticru*c_bays_per_staticstaff) # checking whether utilisation of server in shift is on average less than that if 2 RU were removed
      call_queue_excess <- (get_queue_size(CAD,"ambulance") > trigger_avcallqueue) # whether calls waiting allocation exceeds a certain level
      x <- ifelse(under_util && call_queue_excess,1,2)
      
      mon_attr <- get_mon_attributes(CAD)
      mon_attr <- mon_attr %>%
        filter(key=="site_queue") %>%
        filter(time>=(now(CAD)%/% (T_window_hours*60))*(T_window_hours*60)) %>%
        mutate(dtime = lead(time)-time) %>%
        summarise(site_queue_av = sum(dtime*value,na.rm=T)/sum(dtime,na.rm=T))
      K_sitequeue <- mon_attr$site_queue_av
      return(c(mon_res$utilization,mon_res$queue,mon_res$server,under_util,call_queue_excess,x,K_sitequeue))
    }
    )
  
    
  # Control whether to trigger RRSBays resource up or down
  control_q <- trajectory() %>%
    log_("Control") %>%
    join(control_q_KPIS) %>%
    log_(function() {paste0("Site queue: ",get_attribute(CAD,"Ksite_queue"))}) %>%
    branch(
      function(){
        #if(get_global(CAD,"events10pinshift")>=no_arrivalbreaches){1} else{2} # old approach.
        if(is.na(get_attribute(CAD,"Ksite_queue"))){ # need to debug this, shouldn't give NA
          2} else if(get_attribute(CAD,"Ksite_queue")>=trigger_avsitequeue){
          1} else{2} # time-averaged KPI
      },
      continue=c(T,T),
      trajectory()  %>%
        timeout( function(){T_window_hours*60-(now(CAD)%% (T_window_hours*60))}) %>%
        #timeout(1000) %>%
        set_capacity("ambulance",
                     -ceiling(n_staticru/2),
                     mod="+") %>%
        set_capacity("bay",
                     n_staticru * c_bays_per_staticstaff,
                     mod="+") %>%
        log_("Triggered up"),
      trajectory()  %>%
        branch(
          function(){
            if(get_capacity(CAD,"bay")>=n_staticru*c_bays_per_staticstaff){
              return(get_attribute(CAD,"Kroute_triggerdown"))
            } else{
              return(2) # not possible to decrease RRSBays any more.
            }
          },
          continue=c(T,T),
          trajectory()  %>%
            timeout( function(){T_window_hours*60-(now(CAD)%% (T_window_hours*60))}) %>%
            set_capacity("ambulance",
                         n_staticru/2,
                         mod="+") %>%
            set_capacity("bay",
                         - n_staticru * c_bays_per_staticstaff,
                         mod="+") %>%
            log_("Triggered down"),
          trajectory() %>%
            timeout( function(){T_window_hours*60-(now(CAD)%% (T_window_hours*60))})%>%
            log_("Not triggered")

        )
    ) %>%
    set_global("events10pinshift",0) # reset the count of 10+ in queue per site arrival in shift
  

  
  # Path plotted ------------------------------------------------------------
  if (flag_patientpathplot){
    p<- patient %>% plot()
    saveWidget(p, here::here("Output",mydate,paste0("v05_patient_pathway_generic.html")))
    #webshot::webshot(here::here("Output",mydate,paste0("v05_patient_pathway_generic.html")),here::here("Output",mydate,paste0("v05_patient_pathway_generic.png")))
    flag_patientpathplot = FALSE # only do this once.
  }
  
  
  ### Add generator ###
  CAD <- CAD %>% add_generator("Patient",
                               patient,
                               patient_gen,
                               mon=2) # https://stackoverflow.com/questions/50367760/r-simmer-using-set-attribute-and-get-attribute-and-replication-using-lapply
  
  
  ### Add trigger generator
  if (RRS_protocol=="trigger" && flag_RRS){
    
    CAD <- CAD %>% add_generator("Control_Q",
                                 control_q,
                                 #at(1500),
                                 at(seq(n_warmup_windows*T_window_hours*60-60,g.tmax,T_window_hours*60)),
                                 mon=2
                                 )
 
  }
  
  ### Run ###
  CAD %>% run(until=g.tmax)
}
  
  