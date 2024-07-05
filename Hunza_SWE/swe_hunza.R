
library(sf)
library(raster)
library(tidyverse)
library(lubridate)

library(riversCentralAsia)
data_path <- "data/"
# Load additional necessary data
dem <- raster(paste0(data_path, "clipped_dem.tif"))
basin <- st_read(paste0(data_path, "hunza_basin.shp"), 
                 quiet = TRUE)

load(paste0(data_path, "SWE_1999_2010.RData"))
#save(list = "swel", file = "SWE_1999_2010_7elev.RData")
swel02<-swel%>%
  filter(!grepl('09|10',`Elevation band`))


swe_obs<- read.csv('swel02.csv')
library(RColorBrewer)
SWE_OBS<-ggplot(swel02)+
  geom_line(aes(Date, value, colour = `Elevation band`), alpha = 0.9, size = 0.6) + 
  theme_bw()+
  scale_fill_gradientn(name = "ppt(mm)", colours = brewer.pal(9, "YlGn")) + 
  labs(x='',y='SWE (m)')+
  facet_wrap("Subbasin")
SWE_OBS
#####################################################################################################


swe_sim <- readResultCSV(
  paste0(data_path, 
         "data/SWE_2010_DAILY02.csv")) |>  
  mutate(Subbasin = gsub("\\_Subbasin\\_\\d+$", "", model), 
         "Elevation band" = str_extract(model, "\\d+$") |> as.numeric(), 
         "Elevation band" = factor(`Elevation band`, levels = c(1:20)))



#swe_sim02<-swe_sim%>%
# filter(!grepl('basin04_07|basin02_07|basin03_08|basin02_08|basin01_09|',Subbasin))

swe_sim03<-swe_sim%>%
  separate(Subbasin, into = c("Subbasin", "Elevation band"), sep = "_")

SWE_SIM<-ggplot(swe_sim03) + 
  geom_line(aes(date, value, colour = `Elevation band`), alpha = 0.9, size = 0.6)+
  theme_bw()+
  scale_fill_gradientn(name = "ppt(mm)", colours = brewer.pal(9, "YlGn")) +
  #theme(legend.position = c(0.5, 0.09), legend.direction = 'horizontal')+
  labs(x='',y='SWE (m)')+
  facet_wrap("Subbasin")

SWE_SIM

################################################

#swe_obs<- read.csv('swel02.csv')

#swe_obs<- read.csv('swel02.csv')

swe_obs<-swe_obs%>%
  filter(!grepl('8|9|10',Elevation))

swe_obs$Month_str <- month(swe_obs$date, label=TRUE, abbr=TRUE)

#swe_obs$Month <- factor(format(swe_obs$date, "%b"), month.abb, ordered = TRUE)

compare_swe <- swe_obs |> 
  mutate(Month = month(date), 
         Year = hyear(date),
         "Obs-Sim" = Obs - Sim)

#swe_obs$Month = month(swe_obs$date, label=TRUE, abbr=TRUE),
#"Obs-Sim" = Obs - Sim)


library(viridis)
p<-ggplot(compare_swe) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(Obs, Sim, colour=Elevation), size = 0.1) + 
  scale_color_viridis(discrete = FALSE) +
  facet_wrap('Month_str')+
  xlab("Observed SWE [m]") + 
  ylab("Simulated SWE [m]")+
  coord_fixed() + xlim(0, 1.5) + ylim(0, 1.5) + 
  theme_bw()
p2<-p+labs(color='Elevation band')

p2

################


library(reshape2)

swe<-read.csv('data/mon_swe_sim_mean.csv', colClasses = c('Date'= 'Date'))

tas_ann02<-melt(swe, id='Date')

tas_8.5<-ggplot(data=tas_ann02,aes(x=Date, y=value, colour=variable))+
  scale_colour_discrete(name = "Variables")+labs(x='Year',y='SWE (m)')+  theme_bw()+
  #geom_smooth(method='loess',se=FALSE)+
  geom_line()+
  #stat_summary(fun.y = "median", geom = "line", color = "black", size = 1)+
  theme(
    axis.title = element_text( face='bold'),
    axis.line = element_line(size = 1, colour = "black", linetype=1),
    axis.text = element_text( face='bold', size = 12)
  )

tas_8.5

library(ggpubr)

ggarrange(SWE_OBS, SWE_SIM, p2,tas_8.5,
          labels = c("(a)", "(b)", "(c)", "(d)"),
          ncol = 2, nrow = 2)

