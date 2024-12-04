# 1.9 plot treatment trajectory
plotTrtTrajectory <- function(dfRegimen, dfEvent, keepID=NULL){
  
  # data
  ## regimen info
  regimen <- dfRegimen %>% 
    setNames(c("ID", "LoT", "Start_t", "End_t")) %>% 
    filter(ID%in%keepID) %>% 
    add_count(ID) %>% arrange(desc(n)) %>%
    mutate_at(vars(Start_t, End_t), as.numeric) %>% 
    mutate(LoT = as.factor(LoT),
           ID = factor(ID,levels=unique(ID)))
  ## event info
  event <- dfEvent %>% 
    setNames(c("ID", "eventType", "time")) %>% 
    filter(ID%in%keepID) %>% 
    drop_na(time) %>% 
    mutate_at(vars(time), as.numeric) %>% 
    mutate(ID = factor(ID,levels=unique(ID)))
  
  # plot
  g_desc <- ggplot() +
    ## lines
    geom_segment(data=regimen,aes(x=Start_t, xend=End_t,
                                  y=ID, yend=ID, color=LoT),
                 linetype=1,size=2) +
    ## events
    geom_point(event,
               mapping=aes(x=time,y=ID,
                           shape=eventType),size=2.5)+
    scale_shape_manual(values=c(1:length(unique(event$eventType)))) +
    
    ylab("Patients") +
    labs(color="Lines",shape="Event type") +
    theme_classicM()
  
  return(g_desc)
}