#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Site Map + Hydrographs
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 12/31/2019
#Purpose: Plot Sites + Hydrograph
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
library(dataRetrieval)
library(lubridate)

#Define directories
data_dir<-"C:\\Users/cnjones7/Box Sync/My Folders/Research Proposals/2020_DryingMicrobes_EPSCOR/spatial_data/"
results_dir<-"C:\\Users/cnjones7/Box Sync/My Folders/Research Proposals/2020_DryingMicrobes_EPSCOR/figures"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 CONUS----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read CONUS Object
states<-st_read(paste0(data_dir,"states/cb_2018_us_state_500k.shp")) %>% 
  filter(NAME!='Alaska') %>% 
    filter(NAME!="Hawaii") %>% 
    filter(NAME!="Puerto Rico") %>% 
    filter(NAME!="American Samoa") %>% 
    filter(NAME!="Commonwealth of the Northern Mariana Islands") %>% 
    filter(NAME!="United States Virgin Islands") %>% 
    filter(NAME!="Guam") %>% 
  st_transform(crs=5070)

#Define Primary Institutions
bama<-states %>% filter(NAME == "Alabama")
kansas<-states %>% filter(NAME=="Kansas")
idaho<-states %>% filter(NAME=='Idaho')

#Define Secondary Institutions
oklahoma<-states %>% filter(NAME %in% c("Oklahoma"))
miss<-states %>% filter(NAME %in% c("Mississippi"))

#Plot
png(paste0(results_dir,"/sitemap_states.png"), width = 7, height = 6, units = 'in', res=300)
states %>% st_geometry() %>% plot(col="grey85")
bama %>% st_geometry() %>% plot(add=T, col="steelblue4")
kansas %>% st_geometry() %>% plot(add=T,col='darkorange')
oklahoma %>% st_geometry() %>% plot(add=T, col="darkgoldenrod1")
idaho %>% st_geometry() %>% plot(add=T,col="darkgreen")
miss %>% st_geometry() %>% plot(add=T,col="steelblue1")
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Hydrographs----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Downlaod Hydrograph data---------------------------------------------------
ms<-readNWISdv('02429980', "00060") %>% 
  rename(Q = X_00060_00003) %>% 
  mutate(day= yday(Date)) %>% 
  group_by(day) %>% 
  summarise(upr = quantile(Q, 0.75), 
            med = quantile(Q, 0.50),
            lwr = quantile(Q, 0.25))

ks<-readNWISdv('06879650', "00060") %>% 
  rename(Q = X_00060_00003) %>% 
  mutate(day= yday(Date)) %>% 
  group_by(day) %>% 
  summarise(upr = quantile(Q, 0.75), 
            med = quantile(Q, 0.50),
            lwr = quantile(Q, 0.25))  

id<-readNWISdv('12392155', "00060") %>% 
  rename(Q = X_00060_00003) %>% 
  mutate(day= yday(Date)) %>% 
  group_by(day) %>% 
  summarise(upr = quantile(Q, 0.75), 
            med = quantile(Q, 0.50),
            lwr = quantile(Q, 0.25))  


#2.2 Plot hydrographs-----------------------------------------------------------
#Southeast
ms %>% 
  ggplot() +
  geom_ribbon(
    aes(ymax=upr, ymin=lwr, x = day), 
    fill='steelblue1', 
    alpha = 0.6) +
  geom_line(aes(x = day, y = med), 
            col='steelblue4', 
            lwd=0.75, 
            lty=1)+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_y_log10() +
  xlab("Julian Day") + 
  ylab("Flow [cfs]")
  
ggsave(paste0(results_dir,"/hydrograph_se.png"),
       dpi=300,
       width = 3.5,
       height = 2.75,
       units="in")


#Plains
ks %>% 
  ggplot() +
  geom_ribbon(
    aes(ymax=upr, ymin=lwr, x = day), 
    fill='darkgoldenrod1', 
    alpha = 0.6) +
  geom_line(aes(x = day, y = med), 
            col='darkorange', 
            lwd=0.75, 
            lty=1)+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  #scale_y_log10() +
  xlab("Julian Day") + 
  ylab("Flow [cfs]")

ggsave(paste0(results_dir,"/hydrograph_plains.png"),
       dpi=300,
       width = 3.5,
       height = 2.75,
       units="in")

#Mountains
id %>% 
  ggplot() +
  geom_ribbon(
    aes(ymax=upr, ymin=lwr, x = day), 
    fill='darkgreen', 
    alpha = 0.3) +
  geom_line(aes(x = day, y = med), 
            col='darkgreen', 
            lwd=0.75, 
            lty=1)+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  #scale_y_log10() +
  xlab("Julian Day") + 
  ylab("Flow [cfs]")

ggsave(paste0(results_dir,"/hydrograph_mts.png"),
       dpi=300,
       width = 3.5,
       height = 2.75,
       units="in")

