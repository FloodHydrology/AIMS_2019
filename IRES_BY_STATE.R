#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: IRES by State
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 12/31/2019
#Purpose: Develop plot that shows IRES by state for 2019 AIMS Proposal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Datasets: 
#  (1) NHD Flowlines 
#     -Shapefiles obtained from http://prd-tnm.s3-website-us-west-2.amazonaws.com/?prefix=StagedProducts/Hydrography/NHD/State/HighResolution/Shape/
#     -FCODE Definitions Table Obtained from: https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Hydrography_Dataset/Complete_FCode_List.htm
#     -unzip files into data_dir, rename from "shape" to state abbreviation
#  (2) NHD USGS Gage File
#     -Files obtained from http://www.horizon-systems.com/NHDPlus/V2NationalData.php
#     -Download and unzip national streamgage data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear Memorry
remove(list=ls())

#Load Requred Libraries
library(gridExtra)
library(foreign)
library(sf)
library(tidyverse)

#Define directories
data_dir<-"C:\\Users/cnjones7/Box Sync/My Folders/Research Proposals/2020_DryingMicrobes_EPSCOR/spatial_data/"
results_dir<-"C:\\Users/cnjones7/Box Sync/My Folders/Research Proposals/2020_DryingMicrobes_EPSCOR/figures"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Gather and Organize Data---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Download KS Files
KS<-  #List all files in AL NHD folder
  list.files(paste0(data_dir, "/NHD/KS")) %>% 
  #Convert to tibble
  as_tibble() %>% 
  #Filter to flowline dbf files
  filter(str_detect(string = value, pattern='Flowline') &
           str_detect(string = value, pattern='.dbf') &
           !str_detect(string = value, pattern='VAA')) %>% 
  #Add path information
  mutate(value=paste0(data_dir, "/NHD/KS/", value)) %>% 
  #Donvert path names to values
  pull %>% 
  #Read data files and bind rows
  map_df(., read.dbf) %>% bind_rows() %>% 
  #Add state col
  mutate(state="KS")

#2.2 Download ID Files
ID<-  #List all files in AL NHD folder
  list.files(paste0(data_dir, "/NHD/ID")) %>% 
  #Convert to tibble
  as_tibble() %>% 
  #Filter to flowline dbf files
  filter(str_detect(string = value, pattern='Flowline') &
           str_detect(string = value, pattern='.dbf') &
           !str_detect(string = value, pattern='VAA')) %>% 
  #Add path information
  mutate(value=paste0(data_dir, "/NHD/ID/", value)) %>% 
  #Donvert path names to values
  pull %>% 
  #Read data files and bind rows
  map_df(., read.dbf) %>% bind_rows() %>% 
  #Add state col
  mutate(state="ID")

#2.3 Download MS Files
MS<-  #List all files in AL NHD folder
  list.files(paste0(data_dir, "/NHD/MS")) %>% 
  #Convert to tibble
  as_tibble() %>% 
  #Filter to flowline dbf files
  filter(str_detect(string = value, pattern='Flowline') &
           str_detect(string = value, pattern='.dbf') &
           !str_detect(string = value, pattern='VAA')) %>% 
  #Add path information
  mutate(value=paste0(data_dir, "/NHD/MS/", value)) %>% 
  #Donvert path names to values
  pull %>% 
  #Read data files and bind rows
  map_df(., read.dbf) %>% bind_rows() %>% 
  #Add state col
  mutate(state="MS")

#2.4 Download OK Files
OK<-  #List all files in AL NHD folder
  list.files(paste0(data_dir, "/NHD/OK")) %>% 
  #Convert to tibble
  as_tibble() %>% 
  #Filter to flowline dbf files
  filter(str_detect(string = value, pattern='Flowline') &
           str_detect(string = value, pattern='.dbf') &
           !str_detect(string = value, pattern='VAA')) %>% 
  #Add path information
  mutate(value=paste0(data_dir, "/NHD/OK/", value)) %>% 
  #Donvert path names to values
  pull %>% 
  #Read data files and bind rows
  map_df(., read.dbf) %>% bind_rows() %>% 
  #Add state col
  mutate(state="OK")

#2.5 Download AL Files
AL<-  #List all files in AL NHD folder
  list.files(paste0(data_dir, "/NHD/AL")) %>% 
  #Convert to tibble
  as_tibble() %>% 
  #Filter to flowline dbf files
  filter(str_detect(string = value, pattern='Flowline') &
           str_detect(string = value, pattern='.dbf') &
           !str_detect(string = value, pattern='VAA')) %>% 
  #Add path information
  mutate(value=paste0(data_dir, "/NHD/AL/", value)) %>% 
  #Donvert path names to values
  pull %>% 
  #Read data files and bind rows
  map_df(., read.dbf) %>% bind_rows() %>% 
  #Add state col
  mutate(state="AL")

#2.6 Download AL Files
NC<-  #List NCl files in NC NHD folder
  list.files(paste0(data_dir, "/NHD/NC")) %>% 
  #Convert to tibble
  as_tibble() %>% 
  #Filter to flowline dbf files
  filter(str_detect(string = value, pattern='Flowline') &
           str_detect(string = value, pattern='.dbf') &
           !str_detect(string = value, pattern='VAA')) %>% 
  #Add path information
  mutate(value=paste0(data_dir, "/NHD/NC/", value)) %>% 
  #Donvert path names to vNCues
  pull %>% 
  #Read data files and bind rows
  map_df(., read.dbf) %>% bind_rows() %>% 
  #Add state col
  mutate(state="NC")

#2.6 Combine files and tidy
df<-bind_rows(AL, ID) %>% 
  bind_rows(.,KS) %>% 
  bind_rows(.,OK) %>% 
  bind_rows(.,MS) %>% 
  bind_rows(.,NC) %>%
  #Tidy dataframe
  as_tibble() %>% 
  select(state, ReachCode, FCode, Shape_Leng)

#2.7 Clean up workspace
remove(list=ls()[ls()!='df' &
                 ls()!='data_dir' &
                 ls()!='results_dir'])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Estimate stats on IRES Streams---------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Convert FCODE data
dist <- df %>% 
  mutate(
    stream_type = 
      if_else(FCode==46007, 'IRES', 
        if_else(FCode==46003, 'IRES', 'PERENNIAL')
        )
  )

#Estimate streamdist by state
dist<-dist %>% 
  #Create IRES Col for summation
  mutate(IRES_leng = if_else(stream_type=='IRES', Shape_Leng, 0)) %>% 
  #Group By State
  group_by(state) %>% 
  #Estimate Metrics
  summarise(tot_stream_dist = sum(Shape_Leng), 
            IRES_stream_dist = sum(IRES_leng)) %>% 
  #Estimate Proportion
  mutate(prop_stream_dist = IRES_stream_dist/tot_stream_dist*100)

#Create plot
dist %>% 
  #Arrange by prop_stream_dist
  arrange(-prop_stream_dist) %>% 
  ggplot(aes(x=state, y=prop_stream_dist)) +
    geom_bar(stat='identity')+
    coord_cartesian(ylim = c(35,97))+
    xlab(NULL) +
    ylab(expression(
      atop("Intermittent and Ephemeral", 
           "Stream Length [% of Total Length]"))) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))

#Export plot
# ggsave(paste0(results_dir,"/IRES_LENGTH_BY_STATE.jpg"), 
#        a,
#        dpi=300, 
#        width = 4, 
#        height = 3.5, 
#        units="in")
                
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Estimate stats on IRES Streams---------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 download gage data and tidy
gages<-
  left_join(
    read.dbf(paste0(data_dir,"/NHDPlusV2/GageLoc.dbf")) %>% rename(GAGEID = SOURCE_FEA),
    read.dbf(paste0(data_dir,"/NHDPlusV2/GageInfo.dbf"))
    )%>% 
  #convert to tibble
  as_tibble() %>% 
  #Grab relevant collumns
  select(GAGEID, REACHCODE, STATE, Active)

#Filter to to states of interestand active gages
gages <- gages %>% 
  filter(Active==1) %>% 
  filter(STATE == "KS" |
         STATE == "ID" |
         STATE == "OK" |
         STATE == "MS" |
         STATE == "NC" |   
         STATE == "AL") 

#Create function to estimate mode
mode <- function(codes){
  if(is.factor(codes)==TRUE){
    levels(codes)[which.max(tabulate(codes))]
  }else{
    which.max(tabulate(codes))
  }
}

#Merge with master dataframe to estimate number of IRES gages
gages<-left_join(gages %>% mutate(REACHCODE = paste(REACHCODE)), 
          df %>% rename(REACHCODE = ReachCode)) %>% 
  #Deal with duplicate gage
  group_by(GAGEID) %>% 
  summarise(
    STATE = mode(STATE),
    FCode = mode(FCode)
  )
  
#Convert FCODE data
gages <- gages %>% 
  mutate(
    stream_type = 
      if_else(FCode==46007, 'IRES', 
              if_else(FCode==46003, 'IRES', 'PERENNIAL')
      ), 
    n_gages = 1,
    IRES = if_else(stream_type == 'PERENNIAL', 0, 1)
  )

#Count number of gages
gages<-gages %>% 
  #group by state
  group_by(STATE) %>% 
  #count number of gages
  summarise(
    #n_gages = count(IRES), 
    n_IRES_gages = sum(IRES), 
    n_gages = sum(n_gages), 
    prop = (n_IRES_gages/n_gages)*100
  )

#Create plot
gages %>% 
  #Arrange by prop_stream_dist
  arrange(-prop) %>% 
  ggplot(aes(x=STATE, y=prop)) +
  geom_bar(stat='identity')+
  geom_text(aes(label=paste0('n=', n_IRES_gages), 
                x=STATE, 
                y=rep(13,6))) +
  geom_text(aes(label=paste0('(', n_gages,')'), 
                x=STATE, 
                y=rep(12,6))) +
  coord_cartesian(ylim = c(0,13))+
  xlab(NULL) +
  ylab(expression(
    atop("Intermittent and Ephemeral", 
         "Stream Gages [% of Total Gages]"))) +  
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#Export plot
# ggsave(paste0(results_dir,"/IRES_GAGES_BY_STATE.png"), 
#        b,
#        dpi=300, 
#        width = 4, 
#        height = 3.5, 
#        units="in")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Plot-----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define Point Colors
col<-tibble(
  state = c("AL", "ID", "KS", "MS", "NC", "OK"), 
  shade = c("steelblue", "darkgreen", "darkorange", "steelblue", "steelblue", "darkorange")
) 

#Wrangle data
t<-left_join(dist, 
             gages %>% rename(state=STATE, 
                              prop_gage = prop)) %>% 
  left_join(., col) %>% 
  select(state, prop_stream_dist, prop_gage, shade) %>% 
  filter(state!="NC")

#Plot
SE<-t %>% subset(shade=='steelblue')
W<-t %>% subset(shade=='darkgreen')
PL<-t %>% subset(shade=='darkorange')
cols = c("Southeastern forest" = 'steelblue', 
         "Western mountains" = "darkgreen", 
         "Central plains" = "darkorange") 
ggplot() +
  geom_point(aes(x=SE$prop_stream_dist, 
                 y=SE$prop_gage, 
                 label = SE$state, 
                 fill = 'Southeastern forest'),
             shape = 21, 
             size=4, 
             lwd=3) +
  geom_point(aes(x=W$prop_stream_dist, 
                 y=W$prop_gage, 
                 label = W$state, 
                 fill = 'Western mountains'),
             shape = 21, 
             size=4, 
             lwd=3) +
  geom_point(aes(x=PL$prop_stream_dist, 
                 y=PL$prop_gage, 
                 label = PL$state, 
                 fill = 'Central plains'),
             shape = 21, 
             size=4, 
             lwd=3) +
  geom_text(aes(x=t$prop_stream_dist, 
                y=t$prop_gage, 
                label = t$state), 
            hjust=c(0.25, -0.5, 1, 0.25, 1), 
            vjust=-1, 
            size = 4) +
  scale_fill_manual(name=NULL, values=cols) +
  theme_bw() +
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1),
        legend.title = element_text(size=10),
        axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=14,face="bold")) +
  ylab(expression(atop("Intermittent Gages", "[% of USGS Gages]"))) +
  xlab(expression(atop("Intermittent Length","[% Total Length]"))) +
  coord_cartesian(ylim = c(0,10))
   
ggsave(paste0(results_dir,"/IRES_BIPLOT.png"),
       dpi=300,
       width = 3.5,
       height = 3,
       units="in")
     