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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear Memorry
remove(list=ls())

#Load Requred Libraries
library(foreign)
library(sf)
library(tidyverse)

#Define directories
data_dir<-"C:\\Users/cnjones7/Box Sync/My Folders/Research Proposals/2020_DryingMicrobes_EPSCOR/spatial_data/NHD"
results_dir<-"C:\\Users/cnjones7/Box Sync/My Folders/Research Proposals/2020_DryingMicrobes_EPSCOR/figures"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Gather and Organize Data---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Download KS Files
KS<-  #List all files in AL NHD folder
  list.files(paste0(data_dir, "/KS")) %>% 
  #Convert to tibble
  as_tibble() %>% 
  #Filter to flowline dbf files
  filter(str_detect(string = value, pattern='Flowline') &
           str_detect(string = value, pattern='.dbf') &
           !str_detect(string = value, pattern='VAA')) %>% 
  #Add path information
  mutate(value=paste0(data_dir, "/KS/", value)) %>% 
  #Donvert path names to values
  pull %>% 
  #Read data files and bind rows
  map_df(., read.dbf) %>% bind_rows() %>% 
  #Add state col
  mutate(state="KS")

#2.2 Download ID Files
ID<-  #List all files in AL NHD folder
  list.files(paste0(data_dir, "/ID")) %>% 
  #Convert to tibble
  as_tibble() %>% 
  #Filter to flowline dbf files
  filter(str_detect(string = value, pattern='Flowline') &
           str_detect(string = value, pattern='.dbf') &
           !str_detect(string = value, pattern='VAA')) %>% 
  #Add path information
  mutate(value=paste0(data_dir, "/ID/", value)) %>% 
  #Donvert path names to values
  pull %>% 
  #Read data files and bind rows
  map_df(., read.dbf) %>% bind_rows() %>% 
  #Add state col
  mutate(state="ID")

#2.3 Download NM Files
NM<-  #List all files in AL NHD folder
  list.files(paste0(data_dir, "/NM")) %>% 
  #Convert to tibble
  as_tibble() %>% 
  #Filter to flowline dbf files
  filter(str_detect(string = value, pattern='Flowline') &
           str_detect(string = value, pattern='.dbf') &
           !str_detect(string = value, pattern='VAA')) %>% 
  #Add path information
  mutate(value=paste0(data_dir, "/NM/", value)) %>% 
  #Donvert path names to values
  pull %>% 
  #Read data files and bind rows
  map_df(., read.dbf) %>% bind_rows() %>% 
  #Add state col
  mutate(state="NM")

#2.4 Download AL Files
AL<-  #List all files in AL NHD folder
  list.files(paste0(data_dir, "/AL")) %>% 
  #Convert to tibble
  as_tibble() %>% 
  #Filter to flowline dbf files
  filter(str_detect(string = value, pattern='Flowline') &
           str_detect(string = value, pattern='.dbf') &
           !str_detect(string = value, pattern='VAA')) %>% 
  #Add path information
  mutate(value=paste0(data_dir, "/AL/", value)) %>% 
  #Donvert path names to values
  pull %>% 
  #Read data files and bind rows
  map_df(., read.dbf) %>% bind_rows() %>% 
  #Add state col
  mutate(state="AL")

#2.5 Combine files and cleanup workspae
df<-bind_rows(AL, ID) %>% bind_rows(.,KS) %>% bind_rows(.,NM)
remove(list=ls()[ls()!='df' &
                 ls()!='data_dir' &
                 ls()!='results_dir'])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Estimate stats on IRES Streams---------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Tidy dataframe
df<-df %>% as_tibble() %>% select(state, FCode, Shape_Leng)
  
#Convert FCODE data
df <- df %>% 
  mutate(
    stream_type = 
      if_else(FCode==46007, 'IRES', 
        if_else(FCode==46003, 'IRES', 'PERENNIAL')
        )
  )


#Estimate streamlength by state
output<-df %>% 
  #Create IRES Col for summation
  mutate(IRES_leng = if_else(stream_type=='IRES', Shape_Leng, 0)) %>% 
  #Group By State
  group_by(state) %>% 
  #Estimate Metrics
  summarise(tot_stream_length = sum(Shape_Leng), 
            IRES_stream_length = sum(IRES_leng)) %>% 
  #Estimate Proportion
  mutate(prop_stream_length = IRES_stream_length/tot_stream_length)


#Create plot
output %>% 
  #Arrange by prop_stream_length
  arrange(-prop_stream_length) %>% 
  ggplot(aes(x=state, y=prop_stream_length)) +
    geom_bar(stat='identity')+
    coord_cartesian(ylim = c(.35,.97))+
    xlab(NULL) +
    ylab(expression(paste("Proportion of Intermittent and\nEphemeral Streams [km/km]"))) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"), 
          plot.margin = margin(0.5, 0.5, 1, 2.1, "line"))

#Export plot
ggsave(paste0(results_dir,"/IRES_BY_STATE.jpg"), 
       dpi=300, width = 3.5, height = 3, units="in")
               
                                    