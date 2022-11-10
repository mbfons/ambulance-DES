library(here)
library(bupaR)
library(processmapR)
library(tidyverse)
library(processanimateR)
library(webshot)
library(htmlwidgets)
mydate <- "20221103_A_ppt"

attrib_log <- readRDS(here::here("Output", mydate,"attribs_rc.RDS"))

log_attributes_l <- attrib_log %>%
  filter(name!="") %>% # to remove globals
  dplyr::select(-time) %>% pivot_wider(names_from = key,values_from=value) # remember: select from simmer is masking select from tidyverse/dplyr
#View(log_attributes_l)

myid = 2
myrep = 1

forevent <- attrib_log %>%
  filter(id==myid,replication==myrep) %>%
  group_by(id,replication,name) %>%
  mutate(RRS=sum((key=="RRS_use")*value)>0) %>% # flag for patients that utilised RRS bay
  mutate(complete =sum(key=="t_ambclear" )>0) %>% # flag for incomplete pathways
  mutate(value = ifelse(key=="t_ambhandover" & RRS==FALSE,NA_integer_,value), 
         value=ifelse(key=="t_EDhandover" & RRS==TRUE,NA_integer_,value)) %>% # if used RRS bay, NA the ED handover log (to rather follow-through vehicle to clearance in viz)
  ungroup() %>%
  filter(key %in% c('t_Rclockstart','t_ambulanceseized',"t_Rclockstop","t_hospitalarrival","t_EDhandover","t_ambhandover","t_ambclear")) %>%
  mutate(activity_instance=1:nrow(.),
         resource_id = "ambulance",
         status="complete")


forevent <- forevent %>% filter(!is.na(value)) # remove those NAs (non-used route)
forevent <- forevent %>% filter(complete==TRUE) # remove incomplete pathways

my_origin = "2023-05-01" # ficitonal start date
forevent$time <- as.POSIXct(forevent$value*60,origin=my_origin)

event_DES <- eventlog(forevent,
                      case_id="name",
                      activity_id="key",
                      activity_instance_id = "activity_instance",
                      timestamp="time",
                      lifecycle_id="status",
                      resource_id="resource_id"
                      )

event_DES %>% summary() # event log summary statistics

event_DES %>% process_map() # event log process map

mapping(event_DES)

n_activities(event_DES) # nr of activities

activity_labels(event_DES)

activities(event_DES)# absolute and relative frequency per activity

# function to support with saving graphics
saveviewer <- function(aux,mymetric){
  saveWidget(aux,"temp.html",selfcontained=FALSE)
  webshot("temp.html",file=here::here("Output", mydate,paste0("map-",mymetric,"rep",myrep,"id",myid,".png")))
}


### Process maps


det=5
aa <- event_DES #%>%
  #filter_trim(start_activities = "New", end_activities =  c("New",paste0("FUp ",1:det)))

mymetric = "median"
aux <- aa %>% process_map(performance(median, "mins")) ; aux
#htmlwidgets::saveWidget(aux, file = "./Process mapping/outputs/apr17_1000_perf_median_.html")
saveviewer(aux,mymetric)


mymetric="absolute"
aux <- aa %>% process_map();aux
#saveviewer(aux,mycohort,mymetric,det)

mymetric="relative"
aux<- aa %>% process_map(type = frequency(mymetric));aux
#saveviewer(aux,mycohort,mymetric,det)

mymetric="relative_case"
aux <- aa %>% process_map(type = frequency(mymetric));aux
#saveviewer(aux,mycohort,mymetric,det)


### Animated

saveviewerhtml <- function(aux,mymetric="absolute"){
  
  saveWidget(aux,file=here::here("Output", mydate,paste0("map-",mymetric,"rep",myrep,"id",myid,"_",Sys.Date(),".html")),selfcontained=FALSE)
  
}

saveviewerhtml_self <- function(aux,mymetric="absolute"){
  
  saveWidget(aux,file=here::here("Output", mydate,paste0("map-",mymetric,"rep",myrep,"id",myid,"_",Sys.Date(),"_self.html")),selfcontained=TRUE)
  
}


ap_aa <- animate_process(aa, mode = "absolute",epsilon_time=0.1)
ap_aa

#saveviewerhtml(ap_aa)

ap_aa_2c <- animate_process(aa,
                             mode = "absolute",
                             legend="color",
                             #duration=15,
                             epsilon_time=0.1,
                             #jitter=10,
                             mapping = token_aes(color = token_scale("RRS", 
                                                                     scale = "ordinal", 
                                                                     range = RColorBrewer::brewer.pal(3, "Set1"))))




saveviewerhtml(ap_aa_2c,"absolute_c")
saveviewerhtml_self(ap_aa_2c,"absolute_c")
ap_aa_2c



#animate_process(patients, mode = "absolute",epsilon_time=0.1)
#View(patients)
#View(patients %>% group_by(handling,registration_type) %>% summarise(n()))